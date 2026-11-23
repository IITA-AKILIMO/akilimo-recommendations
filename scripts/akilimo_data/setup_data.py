"""
setup-data — Download all runtime data files from Zenodo or OSF.

All asset bundles are hosted on a single deposit/project and downloaded from
there. Which ones is configured in scripts/assets.json; narrow a run with
--only / --exclude, or for good with ASSETS / ASSETS_EXCLUDE.

Configuration is loaded from scripts/.env (see .env.example).
CLI flags override .env values.

Usage:
    poetry run setup-data                              # auto-detect source
    poetry run setup-data --source zenodo
    poetry run setup-data --source osf --osf-node-id abc12
    poetry run setup-data --zenodo-id 1234567
    poetry run setup-data --sandbox
    poetry run setup-data --only yield --only soil
    poetry run setup-data --exclude dssat
    poetry run setup-data --list-assets

.env keys used:
    ZENODO_RECORD_ID, ZENODO_USE_SANDBOX, OSF_NODE_ID, ASSETS, ASSETS_EXCLUDE, OSF_TOKEN
"""

from __future__ import annotations

import os
import sys

import click
from rich.rule import Rule

from .assets import asset_options, resolve
from .common import (
    PROJECT_ROOT,
    Config,
    console,
    fetch_manifest,
    fetch_osf_tarball,
    fetch_zenodo_tarball,
)

def _detect_source(zenodo_id: str, osf_node_id: str) -> str:
    """Return 'zenodo' or 'osf' based on which IDs are configured."""
    if osf_node_id:
        return "osf"
    if zenodo_id:
        return "zenodo"
    return "osf"  # will fail gracefully with a clear message


@click.command()
@click.option(
    "--source",
    type=click.Choice(["zenodo", "osf"]),
    default=None,
    show_default="auto-detect from .env",
    help="Download source. Auto-detected from configured IDs if omitted.",
)
@click.option(
    "--zenodo-id",
    default=lambda: os.environ.get("ZENODO_RECORD_ID", ""),
    show_default="env ZENODO_RECORD_ID",
    help="Zenodo record ID.",
)
@click.option(
    "--sandbox",
    is_flag=True,
    default=lambda: os.environ.get("ZENODO_USE_SANDBOX", "0") == "1",
    help="Use sandbox.zenodo.org (for testing).",
)
@click.option(
    "--osf-node-id",
    default=lambda: os.environ.get("OSF_NODE_ID", ""),
    show_default="env OSF_NODE_ID",
    help="OSF project node ID (5-character code, e.g. abc12).",
)
@click.option(
    "--no-manifest",
    is_flag=True,
    help="Ignore the published manifest: no checksum check, extract everything in each bundle.",
)
@asset_options
def main(source: str | None, zenodo_id: str, sandbox: bool, osf_node_id: str,
         no_manifest: bool, only: tuple[str, ...], exclude: tuple[str, ...]) -> None:
    """Download Akilimo runtime data from Zenodo or OSF."""

    cfg = Config(zenodo_id=zenodo_id, sandbox=sandbox, osf_node_id=osf_node_id)

    if source is None:
        source = _detect_source(zenodo_id, osf_node_id)

    # What was published (archive names, checksums, the files in each bundle). Its asset names
    # are known to this run even when this machine has none of the data yet.
    manifest = None if no_manifest else fetch_manifest(source, cfg)
    published = manifest["assets"] if manifest else None
    assets = resolve(only, exclude, extra=published or ())

    console.print(Rule("[bold cyan]Akilimo Data Setup[/]"))
    console.print(f"  Project root : {PROJECT_ROOT}")
    console.print(f"  Source       : [bold]{source}[/]")
    console.print(f"  Assets       : {', '.join(a.name for a in assets) or '(none)'}")

    if source == "zenodo":
        console.print(
            "  Zenodo record: "
            + (f"{cfg.zenodo_id}  ({cfg.zenodo_base})" if cfg.zenodo_id else "[yellow]not set[/]")
        )
        if not cfg.zenodo_id:
            console.print(
                "\n  [yellow]Warning:[/] ZENODO_RECORD_ID is not set — all downloads will be skipped.\n"
                "  Add it to [bold]scripts/.env[/] or pass [bold]--zenodo-id[/]."
            )
    else:
        console.print(
            "  OSF node     : "
            + (f"[cyan]{cfg.osf_node_id}[/]  (https://osf.io/{cfg.osf_node_id})" if cfg.osf_node_id else "[yellow]not set[/]")
        )
        if not cfg.osf_node_id:
            console.print(
                "\n  [yellow]Warning:[/] OSF_NODE_ID is not set — all downloads will be skipped.\n"
                "  Add it to [bold]scripts/.env[/] or pass [bold]--osf-node-id[/]."
            )

    # 1. Directories
    console.print(Rule("Creating directories"))
    for d in [a.dir for a in assets] + ["temp"]:
        (PROJECT_ROOT / d).mkdir(parents=True, exist_ok=True)
        console.print(f"  [dim]{d}[/]")

    # 2. The published manifest
    if manifest:
        console.print(
            f"  Manifest     : {len(published)} asset(s), generated {manifest.get('generated_at', '?')}"
            + (f", commit {manifest['git_commit'][:8]}" if manifest.get("git_commit") else "")
        )
    elif not no_manifest:
        console.print("  [dim]Manifest     : none published, using the asset registry only[/]")

    # 3. Download bundles
    console.print(Rule(f"{source.capitalize()} — downloading {len(assets)} bundle(s)"))
    fetch = fetch_zenodo_tarball if source == "zenodo" else fetch_osf_tarball
    optional_missed: list[str] = []
    for asset in assets:
        before = len(cfg.failed)
        entry = published.get(asset.name) if published is not None else None
        if published is not None and entry is None:
            console.print(f"  [yellow][skip][/] {asset.archive} — not in the published manifest")
            cfg.failed.append(asset.archive)
        else:
            # What was published wins: the archive's real name, its checksum, and the files to extract
            fetch(
                entry["archive"] if entry else asset.archive,
                cfg,
                sha256=entry.get("sha256") if entry else None,
                files={f["path"] for f in entry["files"]} if entry and entry.get("files") else None,
            )
        # An optional asset that is not published (yet) is a warning, not a failed setup
        if asset.optional and len(cfg.failed) > before:
            del cfg.failed[before:]
            optional_missed.append(asset.name)
    if optional_missed:
        console.print(f"  [yellow]Optional asset(s) not available, skipped:[/] {', '.join(optional_missed)}")

    # 4. Summary
    console.print(Rule())
    if not cfg.failed:
        if optional_missed:
            console.print(
                "[green bold]All required assets downloaded and extracted.[/] "
                f"Skipped optional: {', '.join(optional_missed)}"
            )
        else:
            console.print("[green bold]All requested assets downloaded and extracted successfully.[/]")
    else:
        console.print(f"[red bold]{len(cfg.failed)} asset(s) failed:[/]")
        for item in cfg.failed:
            console.print(f"  [red]•[/] {item}")
        if source == "zenodo":
            console.print(
                "\n[yellow]Common causes:[/]\n"
                f"  • Zenodo record ID [cyan]{cfg.zenodo_id or '(not set)'}[/] is wrong or deposit not published\n"
                "  • No internet connection\n\n"
                "Re-run after fixing — already-extracted files are preserved."
            )
        else:
            console.print(
                "\n[yellow]Common causes:[/]\n"
                f"  • OSF node ID [cyan]{cfg.osf_node_id or '(not set)'}[/] is wrong or project is private\n"
                "  • File not yet uploaded to the OSF project\n"
                "  • No internet connection\n\n"
                "Re-run after fixing — already-extracted files are preserved."
            )
        sys.exit(1)
