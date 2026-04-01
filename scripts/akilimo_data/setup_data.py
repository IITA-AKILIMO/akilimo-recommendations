"""
setup-data — Download all runtime data files from Zenodo or OSF.

All asset bundles (CSV tables, soil data, yield data) are hosted on a
single deposit/project and downloaded from there.

Configuration is loaded from scripts/.env (see .env.example).
CLI flags override .env values.

Usage:
    poetry run setup-data                              # auto-detect source
    poetry run setup-data --source zenodo
    poetry run setup-data --source osf --osf-node-id abc12
    poetry run setup-data --zenodo-id 1234567
    poetry run setup-data --sandbox

.env keys used:
    ZENODO_RECORD_ID, ZENODO_USE_SANDBOX, OSF_NODE_ID
"""

from __future__ import annotations

import os
import sys

import click
from rich.rule import Rule

from .common import (
    PROJECT_ROOT,
    Config,
    console,
    fetch_osf_tarball,
    fetch_zenodo_tarball,
)

REQUIRED_DIRS = [
    "data/input",
    "data/soil",
    "data/yield",
    "temp",
]

ALL_BUNDLES = [
    "data-input.tar.gz",
    "soil-data.tar.gz",
    "yield-data.tar.gz",
]


def _detect_source(zenodo_id: str, osf_node_id: str) -> str:
    """Return 'zenodo' or 'osf' based on which IDs are configured."""
    if zenodo_id:
        return "zenodo"
    if osf_node_id:
        return "osf"
    return "zenodo"  # will fail gracefully with a clear message


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
def main(source: str | None, zenodo_id: str, sandbox: bool, osf_node_id: str) -> None:
    """Download all Akilimo runtime data from Zenodo or OSF."""

    cfg = Config(zenodo_id=zenodo_id, sandbox=sandbox, osf_node_id=osf_node_id)

    if source is None:
        source = _detect_source(zenodo_id, osf_node_id)

    console.print(Rule("[bold cyan]Akilimo Data Setup[/]"))
    console.print(f"  Project root : {PROJECT_ROOT}")
    console.print(f"  Source       : [bold]{source}[/]")

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
    for d in REQUIRED_DIRS:
        (PROJECT_ROOT / d).mkdir(parents=True, exist_ok=True)
        console.print(f"  [dim]{d}[/]")

    # 2. Download bundles
    console.print(Rule(f"{source.capitalize()} — downloading all bundles"))
    fetch = fetch_zenodo_tarball if source == "zenodo" else fetch_osf_tarball
    for bundle in ALL_BUNDLES:
        fetch(bundle, cfg)

    # 3. Summary
    console.print(Rule())
    if not cfg.failed:
        console.print("[green bold]All assets downloaded and extracted successfully.[/]")
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
