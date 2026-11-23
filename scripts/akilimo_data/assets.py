"""
Which data assets exist and which of them a run should handle.

The registry is scripts/assets.json (plus any file named in ASSETS_FILE), so adding an
asset is a config change. Every command (bundle-assets, setup-data, upload-osf,
upload-zenodo) picks its assets the same way, highest priority first:

    --only NAME          (repeatable, or comma separated)   exactly these
    ASSETS=a,b           (env or scripts/.env)               exactly these
    otherwise            every asset with "default": true

    --exclude NAME       removed from the selection afterwards
    ASSETS_EXCLUDE=a,b   (env or scripts/.env)               removed as well

`--list-assets` prints the registry and the current selection.
"""

from __future__ import annotations

import json
import os
import re
from dataclasses import dataclass
from pathlib import Path

import click
from rich.table import Table

from .common import PROJECT_ROOT, SCRIPTS_DIR, console

REGISTRY_FILE: Path = SCRIPTS_DIR / "assets.json"


@dataclass(frozen=True)
class Asset:
    name: str
    dir: str
    archive: str
    description: str = ""
    optional: bool = False
    default: bool = True

    @property
    def path(self) -> Path:
        return PROJECT_ROOT / self.dir


def _read(path: Path) -> dict[str, Asset]:
    try:
        raw = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise click.ClickException(f"Cannot read asset registry {path}: {exc}") from exc

    found: dict[str, Asset] = {}
    for name, spec in (raw.get("bundles") or {}).items():
        if not isinstance(spec, dict) or not spec.get("dir") or not spec.get("archive"):
            raise click.ClickException(f"{path}: asset '{name}' needs both 'dir' and 'archive'")
        folder = Path(spec["dir"])
        if folder.is_absolute() or ".." in folder.parts:
            raise click.ClickException(f"{path}: asset '{name}' dir must be a folder inside the project")
        if "/" in spec["archive"] or "\\" in spec["archive"]:
            raise click.ClickException(f"{path}: asset '{name}' archive must be a plain file name")
        found[name] = Asset(
            name=name,
            dir=spec["dir"].replace("\\", "/").strip("/"),
            archive=spec["archive"],
            description=spec.get("description", ""),
            optional=bool(spec.get("optional", False)),
            default=bool(spec.get("default", True)),
        )
    return found


def registry() -> dict[str, Asset]:
    """All known assets: scripts/assets.json, then ASSETS_FILE entries (which win on a clash)."""
    assets = _read(REGISTRY_FILE)
    extra = os.environ.get("ASSETS_FILE", "").strip()
    if extra:
        assets.update(_read(Path(extra).expanduser()))
    return assets


def _names(text: str) -> list[str]:
    return [n for n in re.split(r"[,\s]+", text.strip()) if n]


def resolve(only: tuple[str, ...] = (), exclude: tuple[str, ...] = ()) -> list[Asset]:
    """The assets one run handles (see the module doc for the precedence)."""
    known = registry()

    wanted = [n for item in only for n in _names(item)] or _names(os.environ.get("ASSETS", ""))
    dropped = [n for item in exclude for n in _names(item)] + _names(os.environ.get("ASSETS_EXCLUDE", ""))

    unknown = sorted({n for n in wanted + dropped if n not in known})
    if unknown:
        raise click.BadParameter(
            f"unknown asset(s): {', '.join(unknown)}. Known: {', '.join(known)}"
        )

    chosen = wanted or [n for n, a in known.items() if a.default]
    return [known[n] for n in chosen if n not in dropped]


def _print_list(ctx: click.Context, _param: click.Parameter, value: bool) -> None:
    if not value or ctx.resilient_parsing:
        return
    known = registry()
    selected = {a.name for a in resolve()}
    table = Table(title="Data assets")
    for col in ("name", "folder", "archive", "optional", "selected", "description"):
        table.add_column(col)
    for a in known.values():
        table.add_row(a.name, a.dir, a.archive, "yes" if a.optional else "no",
                      "yes" if a.name in selected else "no", a.description)
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
