"""
Shared configuration, console, and download helpers used across all scripts.

Environment variables (loaded from scripts/.env via python-dotenv):
    GITHUB_OWNER      GitHub org or user  [default: masgeek]  — used in Zenodo metadata
    GITHUB_REPO       Repository name     [default: akilimo-recommendations]
    ZENODO_RECORD_ID  Zenodo record ID    [default: ""]
    ZENODO_USE_SANDBOX "1" for sandbox    [default: "0"]
"""

from __future__ import annotations

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


# ---------------------------------------------------------------------------
# Download helpers
# ---------------------------------------------------------------------------

def download_file(url: str, dest: Path, description: str = "") -> bool:
    """
    Stream-download *url* to *dest* with a tqdm progress bar.
    Returns True on success, False on any network error (never raises).
    """
    dest.parent.mkdir(parents=True, exist_ok=True)
    label = description or dest.name

    try:
        with requests.get(url, stream=True, timeout=60) as response:
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


def fetch_tarball(url: str, asset_name: str, cfg: Config) -> bool:
    """Download a tarball from *url* and extract it into PROJECT_ROOT."""
    with tempfile.NamedTemporaryFile(suffix=".tar.gz", delete=False) as tmp:
        tmp_path = Path(tmp.name)

    console.print(f"  Downloading [cyan]{asset_name}[/] ...")
    ok = download_file(url, tmp_path, asset_name)

    if not ok:
        cfg.failed.append(asset_name)
        return False

    console.print(f"  Extracting  [cyan]{asset_name}[/] ...")
    try:
        with tarfile.open(tmp_path, "r:gz") as tar:
            tar.extractall(path=PROJECT_ROOT)
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


def fetch_zenodo_tarball(filename: str, cfg: Config) -> bool:
    if not cfg.zenodo_id:
        console.print(
            f"  [yellow][skip][/] {filename} — "
            "ZENODO_RECORD_ID not set (add it to .env or pass --zenodo-id)"
        )
        return False
    url = f"{cfg.zenodo_files_base}/{filename}?download=1"
    return fetch_tarball(url, filename, cfg)
