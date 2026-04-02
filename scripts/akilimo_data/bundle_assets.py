"""
bundle-assets — Bundle data directories into tarballs ready for upload.

Creates archives in dist/ (project root) — all uploaded to Zenodo:

    dist/data-input.tar.gz    → data/input/ (CSV tables)
    dist/soil-data.tar.gz     → data/soil/ (soil RDS)
    dist/yield-data.tar.gz    → data/yield/ (yield RDS + NetCDF)

Configuration is loaded from scripts/.env (see .env.example).

Usage:
    poetry run bundle-assets
    poetry run bundle-assets --only soil --only yield
"""

from __future__ import annotations

import sys
import tarfile

import click
from rich.rule import Rule

from .common import DIST_DIR, PROJECT_ROOT, Config, console

BUNDLES: dict[str, str] = {
    "input": "data/input",
    "soil":  "data/soil",
    "yield": "data/yield",
}

BUNDLE_OUTPUT: dict[str, str] = {
    "input": "data-input.tar.gz",
    "soil":  "soil-data.tar.gz",
    "yield": "yield-data.tar.gz",
}

DESTINATIONS: dict[str, str] = {
    "input": "Zenodo",
    "soil":  "Zenodo",
    "yield": "Zenodo",
}


def create_bundle(name: str) -> bool:
    source_dir = PROJECT_ROOT / BUNDLES[name]
    output_path = DIST_DIR / BUNDLE_OUTPUT[name]

    if not source_dir.exists():
        console.print(
            f"  [yellow][skip][/] {BUNDLE_OUTPUT[name]} — "
            f"{source_dir.relative_to(PROJECT_ROOT)} not found"
        )
        return False

    file_count = sum(1 for f in source_dir.rglob("*") if f.is_file())
    if file_count == 0:
        console.print(
            f"  [yellow][skip][/] {BUNDLE_OUTPUT[name]} — "
            f"{source_dir.relative_to(PROJECT_ROOT)} is empty"
        )
        return False

    console.print(
        f"  Bundling [cyan]{source_dir.relative_to(PROJECT_ROOT)}[/]"
        f" ({file_count} files) → [cyan]{BUNDLE_OUTPUT[name]}[/] ..."
    )
    DIST_DIR.mkdir(parents=True, exist_ok=True)

    with tarfile.open(output_path, "w:gz") as tar:
        tar.add(source_dir, arcname=BUNDLES[name])

    size_mb = output_path.stat().st_size / 1_048_576
    console.print(
        f"  [green][ok][/]   {output_path.relative_to(PROJECT_ROOT)}  ({size_mb:.1f} MB)"
    )
    return True


@click.command()
@click.option(
    "--only",
    "groups",
    multiple=True,
    type=click.Choice(list(BUNDLES.keys())),
    help="Bundle only specific group(s). Repeatable.",
)
def main(groups: tuple[str, ...]) -> None:
    """Bundle data directories into tarballs ready for Zenodo upload."""

    cfg = Config()
    targets = list(groups) if groups else list(BUNDLES.keys())
    failed: list[str] = []

    console.print(Rule("[bold cyan]Akilimo Asset Bundler[/]"))
    console.print(f"  Output directory: {DIST_DIR}")

    for name in targets:
        console.print(Rule(f"{BUNDLE_OUTPUT[name]}  →  {DESTINATIONS[name]}", style="dim"))
        if not create_bundle(name):
            failed.append(name)

    console.print(Rule())

    if failed:
        console.print(f"[yellow]{len(failed)} bundle(s) skipped:[/] {', '.join(failed)}")

    produced = [
        DIST_DIR / BUNDLE_OUTPUT[n]
        for n in targets
        if n not in failed and (DIST_DIR / BUNDLE_OUTPUT[n]).exists()
    ]
    if not produced:
        console.print("[red]No bundles were created.[/]")
        sys.exit(1)

    zenodo_bundles = [BUNDLE_OUTPUT[n] for n in targets if n not in failed]

    if zenodo_bundles:
        console.print(
            f"\n[bold]Next steps:[/]\n\n"
            f"  Upload to Zenodo: {', '.join(zenodo_bundles)}\n\n"
            "  poetry run upload-zenodo --new\n"
            "  (or poetry run upload-zenodo --deposit-id <id> to update a draft)"
        )
