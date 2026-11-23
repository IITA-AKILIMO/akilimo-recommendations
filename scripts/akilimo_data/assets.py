"""
Which data assets exist and which of them a run should handle.

Everything in the data folder is bundled: each folder directly under the configured
root (data/) is one asset with one archive, found by looking at the disk. What is NOT
bundled is the exclusion list in scripts/assets.json (see that file's _comment). The
names to ask for on a machine without the data (setup-data) come from the required and
optional lists there and from the published manifest.

Every command (bundle-assets, setup-data, upload-osf, upload-zenodo) picks its assets
the same way, highest priority first:

    --only NAME          (repeatable, or comma separated)   exactly these
    ASSETS=a,b           (env or scripts/.env)               exactly these
    otherwise            every known asset

    --exclude NAME       removed from the selection afterwards
    ASSETS_EXCLUDE=a,b   (env or scripts/.env)               removed as well

`--list-assets` prints the assets and the current selection.
"""

from __future__ import annotations

import os
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable

import click
from rich.table import Table

from .common import PROJECT_ROOT, console, is_excluded_name, load_asset_config


@dataclass(frozen=True)
class Asset:
    name: str
    dir: str
    archive: str
    optional: bool = False

    @property
    def path(self) -> Path:
        return PROJECT_ROOT / self.dir


def discover() -> list[str]:
    """Folders directly under the data root that are not excluded, sorted."""
    root = PROJECT_ROOT / load_asset_config()["root"]
    if not root.is_dir():
        return []
    return sorted(p.name for p in root.iterdir() if p.is_dir() and not is_excluded_name(p.name))


def registry(extra: Iterable[str] = ()) -> dict[str, Asset]:
    """
    Every known asset: the required and optional names from the config, the folders on
    disk, and *extra* names (for example those in a published manifest).
    """
    cfg = load_asset_config()
    required = set(cfg["required"])
    names = [*cfg["required"], *cfg["optional"], *discover(), *extra]

    assets: dict[str, Asset] = {}
    for name in names:
        if name in assets or is_excluded_name(name):
            continue
        assets[name] = Asset(
            name=name,
            dir=f"{cfg['root']}/{name}",
            archive=cfg["archives"].get(name, f"{name}-data.tar.gz"),
            optional=name not in required,
        )
    return dict(sorted(assets.items()))


def _names(text: str) -> list[str]:
    return [n for n in re.split(r"[,\s]+", text.strip()) if n]


def resolve(only: tuple[str, ...] = (), exclude: tuple[str, ...] = (),
            extra: Iterable[str] = (), downloading: bool = False) -> list[Asset]:
    """
    The assets one run handles (see the module doc for the precedence). When *downloading*,
    the on-demand assets of the config are left out of the default selection; naming one
    with --only or ASSETS still selects it.
    """
    known = registry(extra)

    wanted = [n for item in only for n in _names(item)] or _names(os.environ.get("ASSETS", ""))
    dropped = [n for item in exclude for n in _names(item)] + _names(os.environ.get("ASSETS_EXCLUDE", ""))

    unknown = sorted({n for n in wanted + dropped if n not in known})
    if unknown:
        raise click.BadParameter(
            f"unknown asset(s): {', '.join(unknown)}. Known: {', '.join(known)}"
        )

    if wanted:
        chosen = wanted
    else:
        on_demand = set(load_asset_config()["on_demand"]) if downloading else set()
        chosen = [n for n in known if n not in on_demand]
    return [known[n] for n in chosen if n not in dropped]


def from_manifest(entries: dict, only: tuple[str, ...] = (), exclude: tuple[str, ...] = ()) -> list[Asset]:
    """
    The assets to download when a manifest is published: exactly the ones it lists. The config
    lists (required, optional, discovered folders) neither add to this nor drop from it; only
    what the user narrows (--only, --exclude, ASSETS, ASSETS_EXCLUDE) and the config's on_demand
    assets (not fetched unless named) change the default. An asset named with --only that the
    manifest does not list is still returned when the config knows it, so setup-data can say it
    is not published (and skip it when it is optional); a name nobody knows is an error.
    """
    known = registry()
    wanted = [n for item in only for n in _names(item)] or _names(os.environ.get("ASSETS", ""))
    dropped = [n for item in exclude for n in _names(item)] + _names(os.environ.get("ASSETS_EXCLUDE", ""))

    unknown = sorted({n for n in wanted + dropped if n not in entries and n not in known})
    if unknown:
        raise click.BadParameter(
            f"unknown asset(s): {', '.join(unknown)}. Published: {', '.join(sorted(entries))}"
        )

    cfg = load_asset_config()

    def build(name: str) -> Asset:
        entry = entries.get(name)
        if entry is None:  # named, known to the config, not published
            return known[name]
        archives = [p["archive"] for p in entry.get("parts", [])] or [entry.get("archive") or f"{name}-data.tar.gz"]
        return Asset(
            name=name,
            dir=entry.get("dir") or f"{cfg['root']}/{name}",
            archive=archives[0],
            optional=bool(entry.get("optional", name not in cfg["required"])),
        )

    if wanted:
        chosen = wanted
    else:
        chosen = [n for n in sorted(entries) if n not in set(cfg["on_demand"])]
    return [build(n) for n in chosen if n not in dropped]


def _print_list(ctx: click.Context, _param: click.Parameter, value: bool) -> None:
    if not value or ctx.resilient_parsing:
        return
    selected = {a.name for a in resolve(downloading=True)}
    table = Table(title="Data assets")
    for col in ("name", "folder", "archive", "optional", "on disk", "downloaded by default"):
        table.add_column(col)
    for a in registry().values():
        table.add_row(a.name, a.dir, a.archive, "yes" if a.optional else "no",
                      "yes" if a.path.is_dir() else "no", "yes" if a.name in selected else "no")
    console.print(table)
    ctx.exit()


def asset_options(func):
    """Adds --only, --exclude and --list-assets; the command receives only/exclude tuples."""
    for option in (
        click.option("--list-assets", is_flag=True, is_eager=True, expose_value=False,
                     callback=_print_list, help="Show the known assets and exit."),
        click.option("--exclude", "exclude", multiple=True,
                     help="Leave this asset out. Repeatable, or comma separated. Also env ASSETS_EXCLUDE."),
        click.option("--only", "only", multiple=True,
                     help="Handle only this asset. Repeatable, or comma separated. Also env ASSETS."),
    ):
        func = option(func)
    return func
