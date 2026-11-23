"""
GitHub Releases as a place to keep the data bundles.

One release holds the current bundles as assets (up to 2 GiB each) plus manifest.json:

    tag      GITHUB_DATA_TAG, else "github.tag" of scripts/assets.json, else "data-latest" (a
             pre-release that is never marked "latest", so it does not interfere with the
             version releases of the deploy)
    repo     GITHUB_DATA_REPO ("owner/name"), else the "github.repo" of scripts/assets.json
             (the private data repository), else GITHUB_REPOSITORY (set in GitHub Actions),
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

from .common import (
    MANIFEST_NAME,
    PROJECT_ROOT,
    Config,
    ManifestUnavailable,
    console,
    fetch_tarball,
    http_get,
    load_asset_config,
)

API = "https://api.github.com"

READ_HINT = ("The token needs read access to the repository's contents: a fine-grained token with "
             "Contents: Read-only on it (Metadata: Read-only is added automatically), or a classic token "
             "with the repo scope.")
WRITE_HINT = ("Publishing needs write access to the repository's contents: a fine-grained token with "
              "Contents: Read and write on it, or a classic token with the repo scope.")


def hint_for(status: int, hint: str) -> str:
    """The permission hint for a status that usually means a missing permission, else nothing."""
    return f"\n         {hint}" if status in (401, 403, 404) else ""
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
    return os.environ.get("GITHUB_DATA_TAG", "").strip() or load_asset_config()["github_tag"] or DEFAULT_TAG


def repo_slug(cfg: Config | None = None) -> str:
    """owner/name of the repository that holds the release (see the module doc)."""
    for value in (os.environ.get("GITHUB_DATA_REPO", ""), load_asset_config()["github_repo"],
                  os.environ.get("GITHUB_REPOSITORY", "")):
        value = value.strip()
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


def lookup_release(slug: str, tag: str, tok: str, refresh: bool = False) -> tuple[dict | None, bool]:
    """(release, failed): the release with this tag, or None; failed is True when GitHub could not be asked."""
    key = (slug, tag)
    if refresh or key not in _releases:
        url = f"{API}/repos/{slug}/releases/tags/{quote(tag)}"
        try:
            resp = http_get(url, headers=headers_for(url, tok))
        except requests.RequestException as exc:
            console.print(f"  [bold red][fail][/] reading release {tag} of {slug}: {exc}")
            return None, True
        if resp.status_code == 404:
            _releases[key] = None
        elif resp.ok:
            _releases[key] = resp.json()
        else:
            console.print(f"  [bold red][fail][/] release {tag} of {slug}: HTTP {resp.status_code} — {resp.text[:200]}"
                          + hint_for(resp.status_code, READ_HINT))
            return None, True
    return _releases[key], False


def get_release(slug: str, tag: str, tok: str, refresh: bool = False) -> dict | None:
    """The release with this tag, or None when there is none (or it cannot be read)."""
    return lookup_release(slug, tag, tok, refresh)[0]


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
        console.print(f"  [bold red][fail][/] creating release {tag} in {slug}: HTTP {resp.status_code} — {resp.text[:300]}"
                      + hint_for(resp.status_code, WRITE_HINT))
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
                      + ("" if release else f" (no such release, or no access).\n         {READ_HINT}"))
        cfg.failed.append(filename)
        return False
    # The asset's API URL with Accept: octet-stream streams the file (and works for a private repo)
    url = asset["url"]
    return fetch_tarball(url, filename, cfg, sha256 or remote_sha256(asset), files,
                         headers=headers_for(url, tok, accept="application/octet-stream"))


def fetch_github_manifest(cfg: Config) -> dict | None:
    """manifest.json of the data release; None when there is none; ManifestUnavailable when GitHub fails."""
    tok = token()
    release, failed = lookup_release(repo_slug(cfg), tag_name(), tok)
    if failed:
        raise ManifestUnavailable(f"could not read release {tag_name()} of {repo_slug(cfg)}")
    asset = asset_named(release, MANIFEST_NAME)
    if asset is None:
        return None  # no release, or none with a manifest: an older publish
    try:
        resp = http_get(asset["url"], headers=headers_for(asset["url"], tok, accept="application/octet-stream"))
        resp.raise_for_status()
        manifest = resp.json()
    except (requests.RequestException, ValueError) as exc:
        raise ManifestUnavailable(f"could not read {MANIFEST_NAME} from the release: {exc}") from exc
    if not isinstance(manifest, dict) or not isinstance(manifest.get("assets"), dict):
        raise ManifestUnavailable(f"{MANIFEST_NAME} has no 'assets' section")
    return manifest
