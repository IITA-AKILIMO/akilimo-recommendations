"""
GitHub Releases as a place to keep the data bundles.

One release holds the current bundles as assets (up to 2 GiB each) plus manifest.json:

    tag      GITHUB_DATA_TAG, default "data-latest" (a pre-release that is never marked
             "latest", so it does not interfere with the version releases of the deploy)
    repo     GITHUB_DATA_REPO ("owner/name"), else GITHUB_REPOSITORY (set in GitHub Actions),
             else the owner and name of this checkout's origin remote
    token    GITHUB_TOKEN or GH_TOKEN, else `gh auth token`. Needed to upload and to read a
             private repository; a public one downloads without it. Sent to api.github.com
             and uploads.github.com only (the redirect to the file storage is pre-signed)

upload-github publishes (see upload_github.py); setup-data --source github downloads. GitHub
reports a SHA-256 for each asset, which is used to skip unchanged uploads and to verify a
download when the manifest is missing.
"""

from __future__ import annotations

import os
import re
import subprocess
from urllib.parse import quote

import requests

from .common import MANIFEST_NAME, PROJECT_ROOT, Config, console, fetch_tarball

API = "https://api.github.com"
TOKEN_HOSTS = ("api.github.com", "uploads.github.com")
DEFAULT_TAG = "data-latest"
_API_VERSION = "2022-11-28"


def token() -> str:
    """GITHUB_TOKEN or GH_TOKEN, else the GitHub CLI's login; empty when there is none."""
    for name in ("GITHUB_TOKEN", "GH_TOKEN"):
        if os.environ.get(name):
            return os.environ[name]
    try:
        out = subprocess.run(["gh", "auth", "token"], capture_output=True, text=True, timeout=10)
        return out.stdout.strip() if out.returncode == 0 else ""
    except (OSError, subprocess.SubprocessError):
        return ""


def tag_name() -> str:
    return os.environ.get("GITHUB_DATA_TAG", "").strip() or DEFAULT_TAG


def repo_slug(cfg: Config | None = None) -> str:
    """owner/name of the repository that holds the release (see the module doc)."""
    for name in ("GITHUB_DATA_REPO", "GITHUB_REPOSITORY"):
        value = os.environ.get(name, "").strip()
        if re.fullmatch(r"[\w.-]+/[\w.-]+", value):
            return value
    try:
        url = subprocess.run(["git", "remote", "get-url", "origin"], cwd=PROJECT_ROOT, capture_output=True,
                             text=True, timeout=10).stdout.strip()
        found = re.search(r"github\.com[:/]([\w.-]+)/([\w.-]+?)(?:\.git)?/?$", url)
        if found:
            return f"{found.group(1)}/{found.group(2)}"
    except (OSError, subprocess.SubprocessError):
        pass
    cfg = cfg or Config()
    return f"{cfg.github_owner}/{cfg.github_repo}"


def headers_for(url: str, tok: str, accept: str = "application/vnd.github+json") -> dict[str, str]:
    """Request headers; the token goes to the GitHub API hosts only."""
    headers = {"Accept": accept, "X-GitHub-Api-Version": _API_VERSION}
    host = url.split("/")[2] if "://" in url else ""
    if tok and host in TOKEN_HOSTS:
        headers["Authorization"] = f"Bearer {tok}"
    return headers


_releases: dict[tuple[str, str], dict | None] = {}


def get_release(slug: str, tag: str, tok: str, refresh: bool = False) -> dict | None:
    """The release with this tag, or None when there is none (or it cannot be read)."""
    key = (slug, tag)
    if refresh or key not in _releases:
        url = f"{API}/repos/{slug}/releases/tags/{quote(tag)}"
        try:
            resp = requests.get(url, headers=headers_for(url, tok), timeout=30)
        except requests.RequestException as exc:
            console.print(f"  [bold red][fail][/] reading release {tag} of {slug}: {exc}")
            return None
        if resp.status_code == 404:
            _releases[key] = None
        elif resp.ok:
            _releases[key] = resp.json()
        else:
            console.print(f"  [bold red][fail][/] release {tag} of {slug}: HTTP {resp.status_code} — {resp.text[:200]}")
            return None
    return _releases[key]


def ensure_release(slug: str, tag: str, tok: str) -> dict | None:
    """The release with this tag, created (as a pre-release that is not 'latest') when missing."""
    release = get_release(slug, tag, tok, refresh=True)
    if release is not None:
        return release
    url = f"{API}/repos/{slug}/releases"
    body = {
        "tag_name": tag,
        "name": "Data assets",
        "body": "Runtime data bundles for the AKILIMO recommendations engine, published by "
                "`poetry run upload-github`. See manifest.json for the files in each archive and their SHA-256.",
        "draft": False,
        "prerelease": True,
        "make_latest": "false",
    }
    resp = requests.post(url, headers=headers_for(url, tok), json=body, timeout=30)
    if not resp.ok:
        console.print(f"  [bold red][fail][/] creating release {tag} in {slug}: HTTP {resp.status_code} — {resp.text[:300]}")
        return None
    console.print(f"  [green][ok][/]   created release {tag} in {slug}")
    _releases[(slug, tag)] = resp.json()
    return _releases[(slug, tag)]


def asset_named(release: dict | None, name: str) -> dict | None:
    return next((a for a in (release or {}).get("assets", []) if a.get("name") == name), None)


def remote_sha256(asset: dict) -> str | None:
    """The SHA-256 GitHub reports for an asset ('sha256:<hex>'), when it reports one."""
    digest = asset.get("digest") or ""
    return digest.removeprefix("sha256:") if digest.startswith("sha256:") else None


def upload_base(release: dict) -> str:
    """The release's upload URL without its {?name,label} template."""
    return release["upload_url"].split("{")[0]


def upload_url(release: dict, filename: str) -> str:
    return f"{upload_base(release)}?name={quote(filename)}"


def fetch_github_tarball(filename: str, cfg: Config, sha256: str | None = None,
                         files: set[str] | None = None) -> bool:
    """Download an asset of the data release and extract it into PROJECT_ROOT."""
    tok = token()
    release = get_release(repo_slug(cfg), tag_name(), tok)
    asset = asset_named(release, filename)
    if asset is None:
        where = f"release {tag_name()} of {repo_slug(cfg)}"
        console.print(f"  [bold red][fail][/] {filename} not found in {where}"
                      + ("" if release else " (no such release, or no access: a private repository needs a token)"))
        cfg.failed.append(filename)
        return False
    # The asset's API URL with Accept: octet-stream streams the file (and works for a private repo)
    url = asset["url"]
    return fetch_tarball(url, filename, cfg, sha256 or remote_sha256(asset), files,
                         headers=headers_for(url, tok, accept="application/octet-stream"))


def fetch_github_manifest(cfg: Config) -> dict | None:
    """manifest.json of the data release, or None (never raises)."""
    tok = token()
    asset = asset_named(get_release(repo_slug(cfg), tag_name(), tok), MANIFEST_NAME)
    if asset is None:
        return None
    try:
        resp = requests.get(asset["url"], headers=headers_for(asset["url"], tok, accept="application/octet-stream"),
                            timeout=30)
        resp.raise_for_status()
        manifest = resp.json()
    except (requests.RequestException, ValueError):
        return None
    if not isinstance(manifest, dict) or not isinstance(manifest.get("assets"), dict):
        return None
    return manifest
