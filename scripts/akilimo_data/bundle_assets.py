"""
bundle-assets — Bundle data directories into tarballs ready for upload.

Creates one archive per asset in dist/ (project root). Which assets exist is
configured in scripts/assets.json (see akilimo_data/assets.py).

Configuration is loaded from scripts/.env (see .env.example).

Usage:
    poetry run bundle-assets
    poetry run bundle-assets --only soil --only yield
    poetry run bundle-assets --exclude dssat
    poetry run bundle-assets --list-assets
"""

from __future__ import annotations

import sys
import tarfile

import click
from rich.rule import Rule

from .assets import Asset, asset_options, resolve
from .common import DIST_DIR, PROJECT_ROOT, console, is_bundle_excluded


def create_bundle(asset: Asset) -> bool:
    source_dir = asset.path
    output_path = DIST_DIR / asset.archive

    if not source_dir.exists():
        console.print(
            f"  [yellow][skip][/] {asset.archive} — "
            f"{source_dir.relative_to(PROJECT_ROOT)} not found"
        )
        return False

    file_count = sum(1 for f in source_dir.rglob("*") if f.is_file())
    if file_count == 0:
        console.print(
            f"  [yellow][skip][/] {asset.archive} — "
            f"{source_dir.relative_to(PROJECT_ROOT)} is empty"
        )
        return False

    console.print(
        f"  Bundling [cyan]{source_dir.relative_to(PROJECT_ROOT)}[/]"
        f" ({file_count} files) → [cyan]{asset.archive}[/] ..."
    )
    DIST_DIR.mkdir(parents=True, exist_ok=True)

    with tarfile.open(output_path, "w:gz") as tar:
        # Leave out git-tracked files and the runtime price database
        tar.add(source_dir, arcname=asset.dir,
                filter=lambda info: None if is_bundle_excluded(info.name) else info)

    size_mb = output_path.stat().st_size / 1_048_576
    console.print(
        f"  [green][ok][/]   {output_path.relative_to(PROJECT_ROOT)}  ({size_mb:.1f} MB)"
    )
    return True


@click.command()
@asset_options
def main(only: tuple[str, ...], exclude: tuple[str, ...]) -> None:
    """Bundle data directories into tarballs ready for upload."""

    targets = resolve(only, exclude)
    failed: list[Asset] = []

    console.print(Rule("[bold cyan]Akilimo Asset Bundler[/]"))
    console.print(f"  Output directory: {DIST_DIR}")
    console.print(f"  Assets          : {', '.join(a.name for a in targets) or '(none)'}")

    for asset in targets:
        console.print(Rule(asset.archive, style="dim"))
        if not create_bundle(asset):
            failed.append(asset)

    console.print(Rule())

    if failed:
        console.print(f"[yellow]{len(failed)} bundle(s) skipped:[/] {', '.join(a.name for a in failed)}")

    produced = [a.archive for a in targets if a not in failed]
    if not produced:
        console.print("[red]No bundles were created.[/]")
        sys.exit(1)

    # An asset that is not optional and could not be bundled is a broken publish
    missing = [a.name for a in failed if not a.optional]
    if missing:
        console.print(f"[red]Required asset(s) could not be bundled:[/] {', '.join(missing)}")
        sys.exit(1)

    console.print(
        "\n[bold]Next steps:[/]\n\n"
        f"  Upload: {', '.join(produced)}\n\n"
        "  poetry run upload-osf        (or poetry run upload-zenodo --new)\n"
        "  Both take the same --only / --exclude options."
    )
