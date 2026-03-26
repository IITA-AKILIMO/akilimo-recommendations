"""
setup-data — Download all runtime data files from Zenodo.

All four asset bundles (images, CSV tables, soil data, yield data) are
hosted on a single Zenodo deposit and downloaded from there.

Configuration is loaded from scripts/.env (see .env.example).
CLI flags override .env values.

Usage:
    poetry run setup-data
    poetry run setup-data --zenodo-id 1234567
    poetry run setup-data --sandbox

.env keys used:
    ZENODO_RECORD_ID, ZENODO_USE_SANDBOX
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
    fetch_zenodo_tarball,
)

REQUIRED_DIRS = [
    "data/input",
    "data/soil",
    "data/yield",
    "temp",
    "net/cash",
    "net/green",
    "net/blue",
    "net/yellow",
    "net/purple",
    "net/royal",
    "net/orange",
    "net/red",
    "net/redMG",
    "net/grey",
]

ALL_BUNDLES = [
    "net-assets.tar.gz",
    "data-input.tar.gz",
    "soil-data.tar.gz",
    "yield-data.tar.gz",
]


@click.command()
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
def main(zenodo_id: str, sandbox: bool) -> None:
    """Download all Akilimo runtime data from Zenodo."""

    cfg = Config(zenodo_id=zenodo_id, sandbox=sandbox)

    console.print(Rule("[bold cyan]Akilimo Data Setup[/]"))
    console.print(f"  Project root  : {PROJECT_ROOT}")
    console.print(
        "  Zenodo record : "
        + (f"{cfg.zenodo_id}  ({cfg.zenodo_base})" if cfg.zenodo_id else "[yellow]not set[/]")
    )

    if not cfg.zenodo_id:
        console.print(
            "\n  [yellow]Warning:[/] ZENODO_RECORD_ID is not set — all downloads will be skipped.\n"
            "  Add it to [bold]scripts/.env[/] or pass [bold]--zenodo-id[/]."
        )

    # 1. Directories
    console.print(Rule("Creating directories"))
    for d in REQUIRED_DIRS:
        (PROJECT_ROOT / d).mkdir(parents=True, exist_ok=True)
        console.print(f"  [dim]{d}[/]")

    # 2. All bundles from Zenodo
    console.print(Rule("Zenodo — downloading all bundles"))
    for bundle in ALL_BUNDLES:
        fetch_zenodo_tarball(bundle, cfg)

    # 3. Summary
    console.print(Rule())
    if not cfg.failed:
        console.print("[green bold]All assets downloaded and extracted successfully.[/]")
    else:
        console.print(f"[red bold]{len(cfg.failed)} asset(s) failed:[/]")
        for item in cfg.failed:
            console.print(f"  [red]•[/] {item}")
        console.print(
            "\n[yellow]Common causes:[/]\n"
            f"  • Zenodo record ID [cyan]{cfg.zenodo_id or '(not set)'}[/] is wrong or deposit not published\n"
            "  • No internet connection\n\n"
            "Re-run after fixing — already-extracted files are preserved."
        )
        sys.exit(1)
