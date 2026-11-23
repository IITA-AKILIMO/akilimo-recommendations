"""
upload-github — Publish the data bundles as assets of a GitHub release.

The release (tag GITHUB_DATA_TAG, default data-latest) is created when missing, as a
pre-release that is not marked "latest". Every archive in dist/ and manifest.json is an
asset. An asset whose SHA-256 equals the local file is left alone; a changed one is
deleted and uploaded again. Files over 2 GiB cannot be assets (the bundler splits at 50 MB).

Prerequisites:
  1. Run `poetry run bundle-assets` to create dist/*.tar.gz and dist/manifest.json
  2. A token that may write releases: GITHUB_TOKEN or GH_TOKEN in the environment or
     scripts/.env, or a logged-in `gh` (gh auth login)

Usage:
    poetry run upload-github
    poetry run upload-github --tag data-2026-11-23     # a release of its own
    poetry run upload-github --only soil --exclude spam
    poetry run upload-github --force                   # upload everything again
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

import click
import requests
from rich.rule import Rule

from . import github_release as gh
from .assets import asset_options, resolve
from .common import (
    DIST_DIR,
    MANIFEST_NAME,
    UploadLedger,
    console,
    put_file,
    run_uploads,
    upload_plan,
)

MAX_ASSET_BYTES = 2 * 1024**3


def _upload_file(slug: str, tag: str, release: dict, tok: str, filepath: Path, force: bool,
                 show_bar: bool, ledger: UploadLedger) -> bool:
    target = f"github:{slug}:{tag}"
    size = filepath.stat().st_size

    if size > MAX_ASSET_BYTES:
        console.print(f"  [bold red][fail][/] {filepath.name}: {size / 1024**3:.1f} GiB is over GitHub's 2 GiB asset limit")
        return False

    if not force and ledger.is_done(target, filepath):
        console.print(f"  [dim][done earlier][/] {filepath.name} — uploaded by an earlier run, skipped")
        return True

    existing = gh.asset_named(release, filepath.name)
    if existing and not force and gh.remote_sha256(existing) == ledger.sha256(filepath):
        console.print(f"  [dim][unchanged][/] {filepath.name} — same SHA-256 as on GitHub, not uploaded")
        ledger.record(target, filepath)
        return True

    if existing:  # an asset name is unique per release: replace means delete, then upload
        url = existing["url"]
        resp = requests.delete(url, headers=gh.headers_for(url, tok), timeout=30)
        if resp.status_code not in (204, 404):
            console.print(f"  [bold red][fail][/] {filepath.name}: could not remove the old asset, "
                          f"HTTP {resp.status_code} — {resp.text[:200]}" + gh.hint_for(resp.status_code, gh.WRITE_HINT))
            return False

    console.print(f"  Uploading [cyan]{filepath.name}[/]  ({size / 1_048_576:.1f} MB) ...")
    url = gh.upload_url(release, filepath.name)
    response = put_file(
        url,
        filepath,
        {**gh.headers_for(url, tok), "Content-Type": "application/octet-stream"},
        show_bar=show_bar,
        method="POST",
    )
    if response.status_code in (200, 201):
        console.print(f"  [green][ok][/]   {filepath.name} ({'updated' if existing else 'created'})")
        ledger.record(target, filepath)
        return True

    console.print(f"  [bold red][fail][/] {filepath.name}: HTTP {response.status_code} — {response.text[:300]}"
                  + gh.hint_for(response.status_code, gh.WRITE_HINT))
    return False


@click.command()
@asset_options
@click.option("--tag", default=lambda: gh.tag_name(), show_default="env GITHUB_DATA_TAG, else data-latest",
              help="Release tag to publish to.")
@click.option("--force", is_flag=True, help="Upload every file, even when GitHub already holds the same SHA-256.")
@click.option("--jobs", "-j", type=click.IntRange(1, 8), default=3, show_default=True,
              help="Files uploaded at the same time.")
def main(tag: str, force: bool, jobs: int, only: tuple[str, ...], exclude: tuple[str, ...]) -> None:
    """Upload the data bundles to a GitHub release."""

    tok = gh.token()
    if not tok:
        console.print(
            "[red]No GitHub token.[/] Set GITHUB_TOKEN (or GH_TOKEN) in the environment or scripts/.env,\n"
            "or log in with the GitHub CLI: gh auth login"
        )
        sys.exit(1)

    os.environ["GITHUB_DATA_TAG"] = tag  # one source of truth for the helpers
    slug = gh.repo_slug()
    console.print(Rule("[bold cyan]Akilimo GitHub Release Uploader[/]"))
    console.print(f"  Repository : {slug}")
    console.print(f"  Release    : {tag}")

    release = gh.ensure_release(slug, tag, tok)
    if release is None:
        sys.exit(1)

    ledger = UploadLedger()
    target = f"github:{slug}:{tag}"
    if force:
        ledger.forget(target)

    console.print(Rule("Uploading files"))
    failed: list[str] = []
    to_send: list[Path] = []
    for filename, optional in upload_plan(resolve(only, exclude)):
        filepath = DIST_DIR / filename
        if not filepath.exists():
            console.print(f"  [yellow][skip][/] {filename} not found in dist/. "
                          "Run [bold]poetry run bundle-assets[/] first.")
            if not optional:
                failed.append(filename)
            continue
        to_send.append(filepath)

    show_bar = jobs <= 1 or len(to_send) <= 1
    failed += run_uploads(
        to_send, lambda fp: _upload_file(slug, tag, release, tok, fp, force, show_bar, ledger), jobs
    )

    # The manifest goes last, and only when every archive is there
    manifest = DIST_DIR / MANIFEST_NAME
    if failed:
        console.print(f"  [yellow][skip][/] {MANIFEST_NAME} not uploaded because some archives failed")
    elif manifest.exists():
        if not _upload_file(slug, tag, release, tok, manifest, force, True, ledger):
            failed.append(MANIFEST_NAME)
    else:
        console.print(f"  [yellow][skip][/] {MANIFEST_NAME} not found in dist/ — setup-data will have no "
                      "checksums or file list. Run [bold]poetry run bundle-assets[/].")

    console.print(Rule())
    if failed:
        console.print(f"[red]{len(failed)} upload(s) failed:[/] {', '.join(failed)}")
        sys.exit(1)

    console.print(
        "[green bold]Upload complete.[/]\n\n"
        f"  Release : https://github.com/{slug}/releases/tag/{tag}\n"
        "  Download: poetry run setup-data --source github   (or DATA_SOURCE=github)\n"
        "  A private repository needs GITHUB_TOKEN (a token that can read contents) wherever you download."
    )
