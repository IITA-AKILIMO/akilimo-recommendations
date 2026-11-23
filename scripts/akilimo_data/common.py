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
import re
import time
from concurrent.futures import ThreadPoolExecutor
import tarfile
import tempfile
from dataclasses import dataclass, field
from pathlib import Path
from typing import Callable

import requests
from dotenv import load_dotenv
from requests.adapters import HTTPAdapter
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
            "split_over_mb": float(raw.get("split_over_mb", 50)),
            "compress_level": int(raw.get("compress_level", 6)),
            "human_sizes": bool(raw.get("human_sizes", True)),
            "already_compressed": tuple(str(x).lower() for x in raw.get("already_compressed", [])),
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


_run_ignore: list[str] = []


def add_ignore_patterns(patterns) -> None:
    """Patterns to leave out for this run only (the --ignore option), on top of the config list."""
    _run_ignore.extend(p.strip() for p in patterns if p and p.strip())


def _env_ignore() -> list[str]:
    """ASSETS_IGNORE: comma or newline separated patterns, always applied (env or scripts/.env)."""
    return [p.strip() for p in re.split(r"[,\n]", os.environ.get("ASSETS_IGNORE", "")) if p.strip()]


def is_excluded_name(rel: str) -> bool:
    """
    True when a path relative to the data root is on the exclusion list (assets.json), on
    ASSETS_IGNORE, or on this run's --ignore list. A name without a slash matches that name
    anywhere in the path (so a folder is left out whole); a pattern with a slash matches that
    path or anything below it. * and ? work, and * also crosses folders.
    """
    parts = [p for p in rel.replace("\\", "/").removeprefix("./").split("/") if p]
    if not parts:
        return False
    for pattern in [*load_asset_config()["exclude"], *_env_ignore(), *_run_ignore]:
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
MANIFEST_SCHEMA = 2


def human_size(n: int | float) -> str:
    """1536 -> '1.5 KiB' (binary units, one decimal; plain bytes below 1 KiB)."""
    value = float(n)
    for unit in ("B", "KiB", "MiB", "GiB", "TiB"):
        if value < 1024 or unit == "TiB":
            return f"{int(value)} B" if unit == "B" else f"{value:.1f} {unit}"
        value /= 1024
    return f"{value:.1f} TiB"  # unreachable


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


# ---------------------------------------------------------------------------
# Upload helper shared by upload-osf and upload-zenodo
# ---------------------------------------------------------------------------

class _BigBlockAdapter(HTTPAdapter):
    """Send file bodies in 1 MiB blocks; the default is 16 KiB, which is a lot of Python per byte."""

    def init_poolmanager(self, connections, maxsize, block=False, **pool_kwargs):
        pool_kwargs.setdefault("blocksize", 1_048_576)
        super().init_poolmanager(connections, maxsize, block, **pool_kwargs)


_RETRY_STATUS = {429, 500, 502, 503, 504}


def put_file(url: str, filepath: Path, headers: dict[str, str], *, show_bar: bool = True,
             attempts: int = 4) -> requests.Response:
    """
    PUT *filepath* to *url* (Content-Length set, body streamed from disk).

    The read timeout grows with the file (a 470 MB file on a slow link needs far more than a
    fixed 10 minutes), and a dropped connection or a 429/5xx answer is retried with a growing
    pause, reopening the file each time. The last response is returned for the caller to judge.
    Neither OSF nor Zenodo can resume a partial upload, so a retry starts the file again.
    """
    size = filepath.stat().st_size
    # Assume at least ~100 KB/s; never less than 10 minutes
    timeout = (30, max(600, size // 100_000))
    session = requests.Session()
    session.mount("https://", _BigBlockAdapter())
    session.mount("http://", _BigBlockAdapter())

    for attempt in range(1, attempts + 1):
        try:
            with (
                open(filepath, "rb") as fh,
                tqdm(total=size, unit="B", unit_scale=True, desc=f"  {filepath.name}",
                     leave=False, disable=not show_bar) as bar,
            ):
                response = session.put(
                    url,
                    data=ProgressReader(fh, bar),
                    headers={**headers, "Content-Length": str(size)},
                    timeout=timeout,
                )
        except (requests.ConnectionError, requests.Timeout) as exc:
            if attempt == attempts:
                raise
            wait = 2 ** attempt * 2
            console.print(f"  [yellow][retry][/] {filepath.name}: {type(exc).__name__}, "
                          f"attempt {attempt + 1} of {attempts} in {wait}s")
            time.sleep(wait)
            continue
        if response.status_code in _RETRY_STATUS and attempt < attempts:
            wait = 2 ** attempt * 2
            console.print(f"  [yellow][retry][/] {filepath.name}: HTTP {response.status_code}, "
                          f"attempt {attempt + 1} of {attempts} in {wait}s")
            time.sleep(wait)
            continue
        return response
    raise RuntimeError("unreachable")  # the loop always returns or raises


def upload_plan(assets) -> list[tuple[str, bool]]:
    """
    [(archive file name, optional)] to upload for these assets: the archives (parts) the local
    dist/manifest.json lists for each, or the asset's single archive when it has no entry.
    """
    manifest_path = DIST_DIR / MANIFEST_NAME
    listed: dict = {}
    if manifest_path.exists():
        try:
            listed = json.loads(manifest_path.read_text(encoding="utf-8")).get("assets", {})
        except (OSError, ValueError):
            listed = {}

    plan: list[tuple[str, bool]] = []
    for asset in assets:
        entry = listed.get(asset.name) or {}
        names = [p["archive"] for p in entry.get("parts", [])] or [asset.archive]
        plan.extend((name, asset.optional) for name in names)
    return plan


def run_uploads(files: list[Path], upload: Callable[[Path], bool], jobs: int) -> list[str]:
    """
    Upload *files* with *jobs* at a time and return the names that failed. An upload that
    raises (a network error after its retries) counts as failed; it never stops the others.
    """
    failed: list[str] = []

    def _one(path: Path) -> bool:
        try:
            return upload(path)
        except Exception as exc:  # noqa: BLE001 - report and carry on with the other files
            console.print(f"  [bold red][fail][/] {path.name}: {type(exc).__name__}: {exc}")
            return False

    if jobs <= 1 or len(files) <= 1:
        results = [_one(f) for f in files]
    else:
        with ThreadPoolExecutor(max_workers=jobs) as pool:
            results = list(pool.map(_one, files))
    failed.extend(f.name for f, ok in zip(files, results) if not ok)
    return failed


class ProgressReader:
    """Wraps a binary file object and advances a tqdm bar on every read."""

    def __init__(self, fh, bar: tqdm) -> None:
        self._fh = fh
        self._bar = bar
        self._total = os.fstat(fh.fileno()).st_size

    def __len__(self) -> int:
        # requests takes the body length from here (and so sends Content-Length, not chunked)
        return self._total

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
