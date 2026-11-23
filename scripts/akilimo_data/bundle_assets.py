"""
bundle-assets — Bundle data directories into tarballs ready for upload.

Creates one archive per asset in dist/ (project root) and a dist/manifest.json that
lists, for every bundled asset, its archive, SHA-256 and the exact files inside.
Upload the manifest with the bundles: setup-data reads it to verify each download and
to extract only the files it lists. Which assets exist is configured in
scripts/assets.json (see akilimo_data/assets.py).

Configuration is loaded from scripts/.env (see .env.example).

Usage:
    poetry run bundle-assets                    # the default assets
    poetry run bundle-assets --interactive      # choose what to bundle, one asset at a time
    poetry run bundle-assets --only soil --only yield
    poetry run bundle-assets --exclude dssat
    poetry run bundle-assets --list-assets
"""

from __future__ import annotations

import gzip
import json
import subprocess
import sys
import tarfile
from datetime import datetime, timezone

import click
from rich.prompt import Confirm
from rich.rule import Rule
from rich.table import Table

from .assets import Asset, asset_options, registry, resolve
from .common import (
    DIST_DIR,
    MANIFEST_NAME,
    MANIFEST_SCHEMA,
    PROJECT_ROOT,
    console,
    is_bundle_excluded,
    sha256_of,
)


def scan(asset: Asset) -> list[dict]:
    """The files an asset's bundle holds, as {path, size}: what bundling would include."""
    if not asset.path.is_dir():
        return []
    files = []
    for f in sorted(asset.path.rglob("*")):
        if not f.is_file():
            continue
        rel = f.relative_to(PROJECT_ROOT).as_posix()
        if not is_bundle_excluded(rel):
            files.append({"path": rel, "size": f.stat().st_size})
    return files


def _mb(n: int) -> str:
    return f"{n / 1_048_576:.1f} MB"


def create_bundle(asset: Asset) -> dict | None:
    """Write the asset's archive; returns its manifest entry, or None when skipped."""
    source_dir = asset.path
    output_path = DIST_DIR / asset.archive

    if not source_dir.exists():
        console.print(
            f"  [yellow][skip][/] {asset.archive} — "
            f"{source_dir.relative_to(PROJECT_ROOT)} not found"
        )
        return None

    files = scan(asset)
    if not files:
        console.print(
            f"  [yellow][skip][/] {asset.archive} — "
            f"{source_dir.relative_to(PROJECT_ROOT)} has nothing to bundle"
        )
        return None

    console.print(
        f"  Bundling [cyan]{source_dir.relative_to(PROJECT_ROOT)}[/]"
        f" ({len(files)} files) → [cyan]{asset.archive}[/] ..."
    )
    DIST_DIR.mkdir(parents=True, exist_ok=True)

    # Reproducible archive: same files in, same bytes out (sorted members, no timestamps,
    # owners or machine-specific modes, gzip header without a time). The uploads compare
    # hashes to skip files that did not change, which only works if a rebuild of unchanged
    # data gives the same hash.
    def _normalise(info: tarfile.TarInfo) -> tarfile.TarInfo | None:
        # Leave out git-tracked files and the runtime price database
        if is_bundle_excluded(info.name):
            return None
        info.mtime = 0
        info.uid = info.gid = 0
        info.uname = info.gname = ""
        info.mode = 0o755 if info.isdir() else 0o644
        return info

    with open(output_path, "wb") as raw:
        with gzip.GzipFile(filename="", mode="wb", fileobj=raw, mtime=0, compresslevel=9) as gz:
            with tarfile.open(fileobj=gz, mode="w", format=tarfile.GNU_FORMAT) as tar:
                tar.add(source_dir, arcname=asset.dir, filter=_normalise)

    size = output_path.stat().st_size
    console.print(f"  [green][ok][/]   {output_path.relative_to(PROJECT_ROOT)}  ({_mb(size)})")
    return {
        "archive": asset.archive,
        "dir": asset.dir,
        "description": asset.description,
        "optional": asset.optional,
        "sha256": sha256_of(output_path),
        "size_bytes": size,
        "file_count": len(files),
        "files": files,
    }


def _git_commit() -> str | None:
    try:
        out = subprocess.run(["git", "rev-parse", "HEAD"], cwd=PROJECT_ROOT, capture_output=True,
                             text=True, check=True, timeout=10)
        return out.stdout.strip() or None
    except (OSError, subprocess.SubprocessError):
        return None


def write_manifest(entries: dict[str, dict]) -> None:
    """
    dist/manifest.json. Entries from this run replace the ones of the same asset; assets
    bundled earlier (and still having their archive) are kept, so a partial run does not
    drop them from what setup-data can see.
    """
    path = DIST_DIR / MANIFEST_NAME
    assets: dict[str, dict] = {}
    if path.exists():
        try:
            old = json.loads(path.read_text(encoding="utf-8")).get("assets", {})
            assets = {n: e for n, e in old.items() if (DIST_DIR / e.get("archive", "")).exists()}
        except (OSError, ValueError, AttributeError):
            assets = {}
    assets.update(entries)
    assets = dict(sorted(assets.items()))

    # Nothing changed: keep the file byte for byte (same time and commit), so its hash stays
    # the same and the upload of the manifest is skipped too
    if path.exists():
        try:
            previous = json.loads(path.read_text(encoding="utf-8"))
            if previous.get("schema") == MANIFEST_SCHEMA and previous.get("assets") == assets:
                console.print(f"  [green][ok][/]   {path.relative_to(PROJECT_ROOT)}  (unchanged)")
                return
        except (OSError, ValueError):
            pass

    manifest = {
        "schema": MANIFEST_SCHEMA,
        "generated_at": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        "git_commit": _git_commit(),
        "assets": assets,
    }
    path.write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
    console.print(f"  [green][ok][/]   {path.relative_to(PROJECT_ROOT)}  ({len(assets)} asset(s))")


def choose_interactively(preselected: list[Asset]) -> list[Asset]:
    """Show what each asset would bundle and ask, one asset at a time."""
    known = registry()
    preset = {a.name for a in preselected}

    table = Table(title="What can be bundled")
    for col in ("asset", "folder", "files", "size", "optional"):
        table.add_column(col)
    scans = {}
    for a in known.values():
        files = scan(a)
        scans[a.name] = files
        table.add_row(a.name, a.dir, str(len(files)) if files else "[dim]none[/]",
                      _mb(sum(f["size"] for f in files)) if files else "-", "yes" if a.optional else "no")
    console.print(table)

    chosen = []
    for a in known.values():
        if not scans[a.name]:
            console.print(f"  [dim]{a.name}: nothing to bundle in {a.dir}, skipped[/]")
            continue
        if Confirm.ask(f"  Bundle [cyan]{a.name}[/] ({len(scans[a.name])} files)?", default=a.name in preset):
            chosen.append(a)
    return chosen


@click.command()
@click.option("--interactive", "-i", is_flag=True,
              help="Choose the assets to bundle by answering a question per asset.")
@asset_options
def main(interactive: bool, only: tuple[str, ...], exclude: tuple[str, ...]) -> None:
    """Bundle data directories into tarballs, plus a manifest, ready for upload."""

    targets = resolve(only, exclude)
    if interactive:
        if not sys.stdin.isatty():
            raise click.UsageError("--interactive needs a terminal; use --only / --exclude instead")
        targets = choose_interactively(targets)
        if not targets:
            console.print("[yellow]Nothing chosen.[/]")
            sys.exit(0)

    failed: list[Asset] = []
    entries: dict[str, dict] = {}

    console.print(Rule("[bold cyan]Akilimo Asset Bundler[/]"))
    console.print(f"  Output directory: {DIST_DIR}")
    console.print(f"  Assets          : {', '.join(a.name for a in targets) or '(none)'}")

    for asset in targets:
        console.print(Rule(asset.archive, style="dim"))
        entry = create_bundle(asset)
        if entry is None:
            failed.append(asset)
        else:
            entries[asset.name] = entry

    console.print(Rule())

    if failed:
        console.print(f"[yellow]{len(failed)} bundle(s) skipped:[/] {', '.join(a.name for a in failed)}")

    if not entries:
        console.print("[red]No bundles were created.[/]")
        sys.exit(1)

    write_manifest(entries)

    # An asset that is not optional and could not be bundled is a broken publish
    missing = [a.name for a in failed if not a.optional]
    if missing:
        console.print(f"[red]Required asset(s) could not be bundled:[/] {', '.join(missing)}")
        sys.exit(1)

    console.print(
        "\n[bold]Next steps:[/]\n\n"
        f"  Upload: {', '.join(e['archive'] for e in entries.values())} and {MANIFEST_NAME}\n\n"
        "  poetry run upload-osf        (or poetry run upload-zenodo --new)\n"
        "  Both upload the manifest too and take the same --only / --exclude options."
    )
