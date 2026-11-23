"""
Shared configuration, console, and download helpers used across all scripts.

Environment variables (loaded from scripts/.env via python-dotenv):
    GITHUB_OWNER       GitHub org or user  [default: masgeek]  — used in Zenodo metadata
    GITHUB_REPO        Repository name     [default: akilimo-recommendations]
    ZENODO_RECORD_ID   Zenodo record ID    [default: ""]
    ZENODO_USE_SANDBOX "1" for sandbox     [default: "0"]
    OSF_NODE_ID        OSF project node ID [default: ""]
    OSF_TOKEN          OSF personal access token (required only for private nodes)
"""

from __future__ import annotations

import fnmatch
import hashlib
import json
import os
import tarfile
import tempfile
from dataclasses import dataclass, field
from pathlib import Path

import requests
from dotenv import load_dotenv
from rich.console import Console
from tqdm import tqdm

# ---------------------------------------------------------------------------
# Project layout
# ---------------------------------------------------------------------------

# scripts/akilimo_data/common.py → akilimo_data/ → scripts/ → project root
SCRIPTS_DIR: Path = Path(__file__).resolve().parent.parent
PROJECT_ROOT: Path = SCRIPTS_DIR.parent
DIST_DIR: Path = PROJECT_ROOT / "dist"

# Load .env from the scripts/ directory.
# override=False means real shell env vars always win over .env values.
load_dotenv(SCRIPTS_DIR / ".env", override=False)

console = Console()

# What never travels in a data bundle, and what an extraction never overwrites (files tracked
# in git ship with the code; the price database is refreshed on the server), comes from the
# exclusion list in scripts/assets.json, so a data pull never replaces the repo's
# translations or the server's prices.
ASSET_CONFIG_FILE: Path = SCRIPTS_DIR / "assets.json"
_DEFAULT_ARCHIVE_SUFFIX = "-data.tar.gz"
_asset_config: dict | None = None


def load_asset_config() -> dict:
    """scripts/assets.json with defaults filled in (read once)."""
    global _asset_config
    if _asset_config is None:
        try:
            raw = json.loads(ASSET_CONFIG_FILE.read_text(encoding="utf-8"))
        except (OSError, ValueError) as exc:
            raise SystemExit(f"Cannot read {ASSET_CONFIG_FILE}: {exc}") from exc
        _asset_config = {
            "root": str(raw.get("root", "data")).strip("/\\"),
            "exclude": [str(p) for p in raw.get("exclude", [])],
            "required": [str(n) for n in raw.get("required", [])],
            "optional": [str(n) for n in raw.get("optional", [])],
            "on_demand": [str(n) for n in raw.get("on_demand", [])],
            "archives": {str(k): str(v) for k, v in (raw.get("archives") or {}).items()},
        }
        # Listed as an asset but also on the exclusion list: the exclusion wins, so say so
        clashes = [n for n in _asset_config["required"] + _asset_config["optional"]
                   if is_excluded_name(n)]
        if clashes:
            console.print(
                f"[yellow]Warning:[/] {', '.join(clashes)} in scripts/assets.json is listed as an asset "
                "and also matches the exclusion list; it is NOT bundled. Remove it from one of them."
            )
    return _asset_config


def is_excluded_name(rel: str) -> bool:
    """
    True when a path relative to the data root is on the exclusion list. A name without a
    slash matches that name anywhere in the path (so a folder is left out whole); a pattern
    with a slash matches that path or anything below it.
    """
    parts = [p for p in rel.replace("\\", "/").removeprefix("./").split("/") if p]
    if not parts:
        return False
    for pattern in load_asset_config()["exclude"]:
        if "/" in pattern:
            if any(fnmatch.fnmatchcase("/".join(parts[:i]), pattern) for i in range(1, len(parts) + 1)):
                return True
        elif any(fnmatch.fnmatchcase(part, pattern) for part in parts):
            return True
    return False


def is_bundle_excluded(name: str) -> bool:
    """True for a tar member / project-relative path that must not be bundled or extracted."""
    path = name.replace("\\", "/").removeprefix("./")
    root = load_asset_config()["root"] + "/"
    if not path.startswith(root):
        return False
    return is_excluded_name(path[len(root):])


# The manifest published next to the bundles (written by bundle-assets, uploaded with
# them, read by setup-data): for every asset its archive, SHA-256 and the exact files
# inside, so a download can be verified and only the listed files are extracted.
MANIFEST_NAME = "manifest.json"
MANIFEST_SCHEMA = 1


def sha256_of(path: Path) -> str:
    digest = hashlib.sha256()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1_048_576), b""):
            digest.update(chunk)
    return digest.hexdigest()


# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

@dataclass
class Config:
    """
    Runtime configuration.  Defaults come from environment variables (populated
    by .env via python-dotenv), so callers rarely need to pass anything explicitly.
    CLI flags in each script override these values at the point of use.
    """

    github_owner: str = field(
        default_factory=lambda: os.environ.get("GITHUB_OWNER", "omilika")
    )
    github_repo: str = field(
        default_factory=lambda: os.environ.get("GITHUB_REPO", "akilimo-recommendations")
    )
    zenodo_id: str = field(
        default_factory=lambda: os.environ.get("ZENODO_RECORD_ID", "")
    )
    sandbox: bool = field(
        default_factory=lambda: os.environ.get("ZENODO_USE_SANDBOX", "0") == "1"
    )
    osf_node_id: str = field(
        default_factory=lambda: os.environ.get("OSF_NODE_ID", "")
    )
    # Needed for the OSF file listing; sent to api.osf.io only
    osf_token: str = field(
        default_factory=lambda: os.environ.get("OSF_TOKEN", "")
    )
    failed: list[str] = field(default_factory=list)

    @property
    def zenodo_base(self) -> str:
        return "https://sandbox.zenodo.org" if self.sandbox else "https://zenodo.org"

    @property
    def gh_repo_url(self) -> str:
        return f"https://github.com/{self.github_owner}/{self.github_repo}"

    @property
    def zenodo_files_base(self) -> str:
        return f"{self.zenodo_base}/record/{self.zenodo_id}/files"

    @property
    def osf_api_base(self) -> str:
        return "https://api.osf.io/v2"

    @property
    def osf_storage_url(self) -> str:
        return f"{self.osf_api_base}/nodes/{self.osf_node_id}/files/osfstorage/"


# ---------------------------------------------------------------------------
# Download helpers
# ---------------------------------------------------------------------------

def _is_osf_api(url: str) -> bool:
    host = url.split("/")[2] if "://" in url else ""
    return host == "api.osf.io"


def auth_headers(url: str, cfg: "Config") -> dict[str, str]:
    """
    A bearer header for api.osf.io when OSF_TOKEN is set, never for any other host. The file
    listing needs it (OSF answers 401 without), but the file downloads (osf.io and
    files.osf.io) must not carry it: with the token they answer 403, without it 200.
    """
    return {"Authorization": f"Bearer {cfg.osf_token}"} if cfg.osf_token and _is_osf_api(url) else {}


def download_file(url: str, dest: Path, description: str = "",
                  headers: dict[str, str] | None = None) -> bool:
    """
    Stream-download *url* to *dest* with a tqdm progress bar.
    Returns True on success, False on any network error (never raises).
    """
    dest.parent.mkdir(parents=True, exist_ok=True)
    label = description or dest.name

    try:
        with requests.get(url, stream=True, timeout=60, headers=headers) as response:
            response.raise_for_status()
            total = int(response.headers.get("content-length", 0))

            with (
                open(dest, "wb") as fh,
                tqdm(
                    total=total or None,
                    unit="B",
                    unit_scale=True,
                    desc=f"  {label}",
                    leave=False,
                ) as bar,
            ):
                for chunk in response.iter_content(chunk_size=65_536):
                    fh.write(chunk)
                    bar.update(len(chunk))

        return True

    except requests.RequestException as exc:
        console.print(f"  [bold red][fail][/] {label}: {exc}")
        dest.unlink(missing_ok=True)
        return False


def fetch_tarball(
    url: str,
    asset_name: str,
    cfg: Config,
    sha256: str | None = None,
    files: set[str] | None = None,
) -> bool:
    """
    Download a tarball from *url* and extract it into PROJECT_ROOT.

    With *sha256* the download must match it. With *files* (from the manifest) only
    those paths are extracted, besides folders; anything else in the archive is left out.
    """
    with tempfile.NamedTemporaryFile(suffix=".tar.gz", delete=False) as tmp:
        tmp_path = Path(tmp.name)

    console.print(f"  Downloading [cyan]{asset_name}[/] ...")
    ok = download_file(url, tmp_path, asset_name, auth_headers(url, cfg))

    if not ok:
        cfg.failed.append(asset_name)
        return False

    if sha256 and sha256_of(tmp_path) != sha256:
        console.print(f"  [bold red][fail][/] {asset_name}: checksum does not match the manifest")
        cfg.failed.append(asset_name)
        tmp_path.unlink(missing_ok=True)
        return False

    console.print(f"  Extracting  [cyan]{asset_name}[/] ...")
    try:
        with tarfile.open(tmp_path, "r:gz") as tar:
            everything = tar.getmembers()
            members = [m for m in everything if not is_bundle_excluded(m.name)]
            kept_local = len(everything) - len(members)
            not_listed = 0
            if files is not None:
                listed = [m for m in members if m.isdir() or m.name.replace("\\", "/").removeprefix("./") in files]
                not_listed = len(members) - len(listed)
                members = listed
            tar.extractall(path=PROJECT_ROOT, members=members)
        if kept_local:
            console.print(f"  [dim]kept local copies of {kept_local} tracked/runtime file(s)[/]")
        if not_listed:
            console.print(f"  [dim]left out {not_listed} file(s) the manifest does not list[/]")
        console.print(f"  [green][ok][/]   {asset_name}")
        return True
    except tarfile.TarError as exc:
        console.print(f"  [bold red][fail][/] extracting {asset_name}: {exc}")
        cfg.failed.append(asset_name)
        return False
    finally:
        tmp_path.unlink(missing_ok=True)


class ProgressReader:
    """Wraps a binary file object and advances a tqdm bar on every read."""

    def __init__(self, fh, bar: tqdm) -> None:
        self._fh = fh
        self._bar = bar

    def read(self, size: int = -1) -> bytes:
        data = self._fh.read(size)
        self._bar.update(len(data))
        return data


def zenodo_url(filename: str, cfg: Config) -> str | None:
    """Download URL of a file in the Zenodo record, or None when no record is configured."""
    if not cfg.zenodo_id:
        console.print(
            f"  [yellow][skip][/] {filename} — "
            "ZENODO_RECORD_ID not set (add it to .env or pass --zenodo-id)"
        )
        return None
    return f"{cfg.zenodo_files_base}/{filename}?download=1"


def osf_url(filename: str, cfg: Config, *, quiet: bool = False) -> str | None:
    """Download URL of a file in the OSF project's storage, or None (reported unless *quiet*)."""
    if not cfg.osf_node_id:
        if not quiet:
            console.print(
                f"  [yellow][skip][/] {filename} — "
                "OSF_NODE_ID not set (add it to .env or pass --osf-node-id)"
            )
        return None

    # List files in the node's osfstorage to find the download URL.
    try:
        resp = requests.get(cfg.osf_storage_url, headers=auth_headers(cfg.osf_storage_url, cfg), timeout=30)
        resp.raise_for_status()
        files = resp.json().get("data", [])
    except requests.RequestException as exc:
        if not quiet:
            console.print(f"  [bold red][fail][/] listing OSF node {cfg.osf_node_id}: {exc}")
        return None

    entry = next(
        (f for f in files if f.get("attributes", {}).get("name") == filename),
        None,
    )
    url = (entry or {}).get("links", {}).get("download", "")
    if not url and not quiet:
        console.print(f"  [bold red][fail][/] {filename} not found in OSF node {cfg.osf_node_id}")
    return url or None


def fetch_manifest(source: str, cfg: Config) -> dict | None:
    """
    The published manifest, or None when there is none (older publishes) or it cannot be
    read. Never raises: setup-data then falls back to the asset registry alone.
    """
    url = zenodo_url(MANIFEST_NAME, cfg) if source == "zenodo" else osf_url(MANIFEST_NAME, cfg, quiet=True)
    if not url:
        return None
    try:
        resp = requests.get(url, headers=auth_headers(url, cfg), timeout=30)
        resp.raise_for_status()
        manifest = resp.json()
    except (requests.RequestException, ValueError):
        return None
    if not isinstance(manifest, dict) or not isinstance(manifest.get("assets"), dict):
        return None
    return manifest


def fetch_zenodo_tarball(filename: str, cfg: Config, sha256: str | None = None,
                         files: set[str] | None = None) -> bool:
    url = zenodo_url(filename, cfg)
    return bool(url) and fetch_tarball(url, filename, cfg, sha256, files)


def fetch_osf_tarball(filename: str, cfg: Config, sha256: str | None = None,
                      files: set[str] | None = None) -> bool:
    """Download a tarball from an OSF project node and extract it into PROJECT_ROOT."""
    if not cfg.osf_node_id:
        osf_url(filename, cfg)  # says why it is skipped
        return False
    url = osf_url(filename, cfg)
    if not url:
        cfg.failed.append(filename)
        return False
    return fetch_tarball(url, filename, cfg, sha256, files)
