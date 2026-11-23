"""
upload-zenodo — Upload scientific data bundles to a Zenodo deposit.

Configuration is loaded from scripts/.env (see .env.example).
CLI flags override .env values.

Prerequisites:
  1. Run `poetry run bundle-assets` to create dist/*.tar.gz
  2. Set ZENODO_TOKEN in scripts/.env (or export it as a shell variable)

Usage:
    poetry run upload-zenodo --new                   # create new deposit
    poetry run upload-zenodo --deposit-id 1234567    # update existing draft

.env keys used:
    ZENODO_TOKEN         (required, sensitive — never commit)
    ZENODO_DEPOSIT_ID    default deposit ID for --deposit-id
    ZENODO_USE_SANDBOX   "1" to use sandbox.zenodo.org
    GITHUB_OWNER, GITHUB_REPO  used in deposit metadata
    ZENODO_TITLE, ZENODO_AFFILIATION, ZENODO_LICENSE
"""

from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path

import hashlib

import click
import requests
from rich.rule import Rule
from tqdm import tqdm

from .assets import asset_options, resolve
from .common import DIST_DIR, MANIFEST_NAME, PROJECT_ROOT, Config, console, put_file, run_uploads, upload_plan

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _git_tag() -> str:
    try:
        result = subprocess.run(
            ["git", "describe", "--tags", "--abbrev=0"],
            capture_output=True, text=True, cwd=PROJECT_ROOT,
        )
        return result.stdout.strip() or "1.0.0"
    except Exception:
        return "1.0.0"


def _md5_of(path: Path) -> str:
    digest = hashlib.md5(usedforsecurity=False)
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1_048_576), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _remote_checksums(api_base: str, deposit_id: str, token: str) -> dict[str, str]:
    """File name -> MD5 of the files already in the deposit (Zenodo reports MD5 only)."""
    try:
        resp = requests.get(
            f"{api_base}/deposit/depositions/{deposit_id}/files",
            headers={"Authorization": f"Bearer {token}"},
            timeout=30,
        )
        resp.raise_for_status()
        return {f["filename"]: f.get("checksum", "") for f in resp.json()}
    except (requests.RequestException, ValueError, KeyError):
        return {}  # cannot tell: upload everything rather than skip wrongly


def _upload_file(bucket_url: str, filepath: Path, token: str,
                 remote: dict[str, str] | None = None, show_bar: bool = True) -> bool:
    size = filepath.stat().st_size
    if remote and remote.get(filepath.name) and remote[filepath.name] == _md5_of(filepath):
        console.print(f"  [dim][unchanged][/] {filepath.name} — same MD5 as in the deposit, not uploaded")
        return True
    console.print(f"  Uploading [cyan]{filepath.name}[/]  ({size / 1_048_576:.1f} MB) ...")

    response = put_file(
        f"{bucket_url}/{filepath.name}",
        filepath,
        {"Authorization": f"Bearer {token}"},
        show_bar=show_bar,
    )

    if response.ok:
        checksum = response.json().get("checksum", "")
        console.print(f"  [green][ok][/]   {filepath.name}  (md5:{checksum})")
        return True

    console.print(
        f"  [bold red][fail][/] {filepath.name}: "
        f"HTTP {response.status_code} — {response.text}"
    )
    return False


def _build_metadata(cfg: Config) -> dict:
    return {
        "metadata": {
            "title": os.environ.get(
                "ZENODO_TITLE",
                "Akilimo Cassava Recommendations — Soil and Yield Data",
            ),
            "upload_type": "dataset",
            "description": (
                "Soil NPK and water-limited yield (LINTUL) datasets used by the "
                "Akilimo cassava fertilizer recommendation API. "
                "Covers Nigeria, Tanzania, Rwanda, Ghana, and Burundi."
            ),
            "creators": [
                {
                    "name": "Akilimo Project",
                    "affiliation": os.environ.get(
                        "ZENODO_AFFILIATION",
                        "IITA / International Institute of Tropical Agriculture",
                    ),
                }
            ],
            "version": _git_tag(),
            "license": os.environ.get("ZENODO_LICENSE", "cc-by-4.0"),
            "keywords": [
                "cassava", "soil data", "yield model", "Africa",
                "QUEFTS", "LINTUL", "fertilizer recommendation",
            ],
            "related_identifiers": [
                {
                    "identifier": cfg.gh_repo_url,
                    "relation": "isSupplementTo",
                    "scheme": "url",
                }
            ],
        }
    }


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

@click.command()
@asset_options
@click.option("--force", is_flag=True, help="Upload every file, even when the deposit already holds the same MD5.")
@click.option("--jobs", "-j", type=click.IntRange(1, 8), default=3, show_default=True,
              help="Files uploaded at the same time.")
@click.option("--new", "create_new", is_flag=True, help="Create a new Zenodo deposit.")
@click.option(
    "--deposit-id",
    default=lambda: os.environ.get("ZENODO_DEPOSIT_ID", ""),
    show_default="env ZENODO_DEPOSIT_ID",
    help="Upload to an existing deposit draft.",
)
@click.option(
    "--sandbox",
    is_flag=True,
    default=lambda: os.environ.get("ZENODO_USE_SANDBOX", "0") == "1",
    help="Use sandbox.zenodo.org instead of zenodo.org.",
)
def main(create_new: bool, deposit_id: str, sandbox: bool, force: bool, jobs: int,
         only: tuple[str, ...], exclude: tuple[str, ...]) -> None:
    """Upload the data bundles to a Zenodo deposit."""

    token = os.environ.get("ZENODO_TOKEN", "")
    if not token:
        console.print(
            "[red]ZENODO_TOKEN is not set.[/]\n"
            "Add it to [bold]scripts/.env[/]:\n"
            "  ZENODO_TOKEN=your-personal-access-token\n\n"
            "Generate one at: https://zenodo.org → Account → Applications → Personal access tokens\n"
            "Required scope: [bold]deposit:write[/]"
        )
        sys.exit(1)

    if not create_new and not deposit_id:
        console.print(
            "[red]Provide --new or --deposit-id <id>.[/]\n"
            "You can also set [bold]ZENODO_DEPOSIT_ID[/] in scripts/.env."
        )
        sys.exit(1)

    cfg = Config(sandbox=sandbox)
    api_base = f"{cfg.zenodo_base}/api"
    headers_json = {"Authorization": f"Bearer {token}", "Content-Type": "application/json"}
    headers_auth = {"Authorization": f"Bearer {token}"}

    console.print(Rule("[bold cyan]Akilimo Zenodo Uploader[/]"))
    console.print(f"  Zenodo host: {cfg.zenodo_base}")

    # -----------------------------------------------------------------------
    # Step 1 — create or reuse deposit
    # -----------------------------------------------------------------------
    console.print(Rule("Preparing deposit"))

    if create_new:
        metadata = _build_metadata(cfg)
        resp = requests.post(
            f"{api_base}/deposit/depositions",
            headers=headers_json,
            json=metadata,
            timeout=30,
        )
        if not resp.ok:
            console.print(f"[red]Failed to create deposit:[/] {resp.status_code} — {resp.text}")
            sys.exit(1)

        deposit_id = str(resp.json()["id"])
        console.print(f"  [green][ok][/]   Created deposit ID: [bold]{deposit_id}[/]")
        console.print(f"         Draft URL: {cfg.zenodo_base}/deposit/{deposit_id}")
    else:
        console.print(f"  Using existing deposit ID: [bold]{deposit_id}[/]")

    resp = requests.get(
        f"{api_base}/deposit/depositions/{deposit_id}",
        headers=headers_auth,
        timeout=30,
    )
    if not resp.ok:
        console.print(
            f"[red]Could not fetch deposit {deposit_id}:[/] "
            f"{resp.status_code} — {resp.text}"
        )
        sys.exit(1)

    bucket_url = resp.json().get("links", {}).get("bucket")
    if not bucket_url:
        console.print(
            "[red]No bucket URL found.[/] "
            "The deposit may already be published (published records cannot be modified)."
        )
        sys.exit(1)

    console.print(f"  Bucket: {bucket_url}")

    # -----------------------------------------------------------------------
    # Step 2 — upload files
    # -----------------------------------------------------------------------
    console.print(Rule("Uploading files"))

    # What the deposit already holds, so unchanged files are not sent again
    remote = None if force else _remote_checksums(api_base, deposit_id, token)

    failed: list[str] = []
    to_send: list[Path] = []
    for filename, optional in upload_plan(resolve(only, exclude)):
        filepath = DIST_DIR / filename
        if not filepath.exists():
            console.print(
                f"  [yellow][skip][/] {filename} not found in dist/. "
                "Run [bold]poetry run bundle-assets[/] first."
            )
            if not optional:
                failed.append(filename)
            continue
        to_send.append(filepath)

    show_bar = jobs <= 1 or len(to_send) <= 1
    failed += run_uploads(to_send, lambda fp: _upload_file(bucket_url, fp, token, remote, show_bar), jobs)

    # The manifest goes last, and only when every archive it lists is there: a manifest that
    # names archives the deposit does not have would make setup-data fail
    manifest = DIST_DIR / MANIFEST_NAME
    if failed:
        console.print(f"  [yellow][skip][/] {MANIFEST_NAME} not uploaded because some archives failed")
    elif manifest.exists():
        if not _upload_file(bucket_url, manifest, token, remote):
            failed.append(MANIFEST_NAME)
    else:
        console.print(
            f"  [yellow][skip][/] {MANIFEST_NAME} not found in dist/ — "
            "setup-data will have no checksums or file list. Run [bold]poetry run bundle-assets[/]."
        )

    # -----------------------------------------------------------------------
    # Step 3 — next steps
    # -----------------------------------------------------------------------
    console.print(Rule())

    if failed:
        console.print(f"[red]{len(failed)} upload(s) failed:[/] {', '.join(failed)}")
        sys.exit(1)

    console.print(
        "[green bold]Upload complete.[/]\n\n"
        f"  1. Review your deposit: [cyan]{cfg.zenodo_base}/deposit/{deposit_id}[/]\n"
        "  2. Adjust metadata (authors, description) in the Zenodo UI if needed.\n"
        "  3. Click [bold]Publish[/] to make it public and get a DOI.\n\n"
        "  4. Once published, add the record ID to [bold]scripts/.env[/]:\n"
        f"       ZENODO_RECORD_ID={deposit_id}"
    )
