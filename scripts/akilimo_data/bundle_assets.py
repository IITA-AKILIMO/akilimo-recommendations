"""
bundle-assets — Bundle data directories into tarballs ready for upload.

Creates archives in dist/ (project root) and a dist/manifest.json that lists, for every
bundled asset, its archives (parts), their SHA-256 and the exact files inside. Upload the
manifest with the archives: setup-data reads it to verify each download and to extract
only the files it lists. Which assets exist is decided by the data folder and scripts/
assets.json (see akilimo_data/assets.py).

An asset becomes one archive, except that a file larger than "split_over_mb" gets an
archive of its own, so changing one big file re-uploads that file and not the whole asset.
Archives are reproducible (same files in, same bytes out), so unchanged data keeps its
hash and the uploads skip it. Files of an already-compressed type are stored, not
recompressed.

Configuration is loaded from scripts/.env (see .env.example).

Usage:
    poetry run bundle-assets                    # every asset
    poetry run bundle-assets --interactive      # choose what to bundle, one asset at a time
    poetry run bundle-assets --only soil --only yield
    poetry run bundle-assets --exclude dssat
    poetry run bundle-assets --ignore 'yield/*.bak' --ignore 'soil/old'
    poetry run bundle-assets --no-human-sizes   # byte counts only in manifest.json
    poetry run bundle-assets --list-assets
"""

from __future__ import annotations

import gzip
import json
import re
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
    add_ignore_patterns,
    console,
    human_size,
    is_bundle_excluded,
    load_asset_config,
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


def _stem(asset: Asset) -> str:
    return asset.archive.removesuffix(".tar.gz")


def plan_parts(asset: Asset, files: list[dict]) -> list[tuple[str, list[dict]]]:
    """
    [(archive name, files)]: the small files together under the asset's own archive name, every
    file over "split_over_mb" in an archive of its own (<stem>--<path below the asset>.tar.gz).
    """
    limit = load_asset_config()["split_over_mb"] * 1_048_576
    small = [f for f in files if f["size"] <= limit]
    big = [f for f in files if f["size"] > limit]

    parts: list[tuple[str, list[dict]]] = []
    if small or not big:
        parts.append((asset.archive, small))
    for f in big:
        below = f["path"].removeprefix(asset.dir + "/")
        slug = re.sub(r"[^A-Za-z0-9._-]+", "_", below)
        parts.append((f"{_stem(asset)}--{slug}.tar.gz", [f]))
    return parts


def _level_for(files: list[dict]) -> int:
    """gzip level for an archive: 0 (stored) when every file is of an already-compressed type."""
    cfg = load_asset_config()
    if files and all(f["path"].lower().endswith(cfg["already_compressed"]) for f in files):
        return 0
    return max(0, min(9, cfg["compress_level"]))


def write_archive(archive: str, files: list[dict]) -> dict:
    """Write one reproducible archive into dist/ and return its manifest entry."""
    output_path = DIST_DIR / archive
    level = _level_for(files)

    # Reproducible: sorted members, no timestamps, owners or machine-specific modes, gzip header
    # without a time. The uploads compare hashes to skip files that did not change, which only
    # works if a rebuild of unchanged data gives the same hash.
    def _normalise(info: tarfile.TarInfo) -> tarfile.TarInfo | None:
        info.mtime = 0
        info.uid = info.gid = 0
        info.uname = info.gname = ""
        info.mode = 0o755 if info.isdir() else 0o644
        return info

    with open(output_path, "wb") as raw:
        with gzip.GzipFile(filename="", mode="wb", fileobj=raw, mtime=0, compresslevel=level) as gz:
            with tarfile.open(fileobj=gz, mode="w", format=tarfile.GNU_FORMAT) as tar:
                for f in sorted(files, key=lambda x: x["path"]):
                    tar.add(PROJECT_ROOT / f["path"], arcname=f["path"], recursive=False, filter=_normalise)

    size = output_path.stat().st_size
    how = "stored" if level == 0 else f"gzip {level}"
    console.print(
        f"  [green][ok][/]   {output_path.relative_to(PROJECT_ROOT)}  "
        f"({_mb(size)}, {len(files)} file(s), {how})"
    )
    return {
        "archive": archive,
        "sha256": sha256_of(output_path),
        "size_bytes": size,
        "file_count": len(files),
        "files": files,
    }


def create_bundle(asset: Asset) -> dict | None:
    """Write the asset's archives; returns its manifest entry, or None when skipped."""
    if not asset.path.exists():
        console.print(f"  [yellow][skip][/] {asset.archive} — {asset.dir} not found")
        return None

    files = scan(asset)
    if not files:
        console.print(f"  [yellow][skip][/] {asset.archive} — {asset.dir} has nothing to bundle")
        return None

    DIST_DIR.mkdir(parents=True, exist_ok=True)

    # Archives of an earlier run that this plan no longer produces (a file that is no longer
    # big, or removed) must not linger and get uploaded
    planned = plan_parts(asset, files)
    keep = {name for name, _ in planned}
    for old in [*DIST_DIR.glob(f"{_stem(asset)}.tar.gz"), *DIST_DIR.glob(f"{_stem(asset)}--*.tar.gz")]:
        if old.name not in keep:
            old.unlink()

    console.print(
        f"  Bundling [cyan]{asset.dir}[/] ({len(files)} files, "
        f"{_mb(sum(f['size'] for f in files))}) → {len(planned)} archive(s) ..."
    )
    parts = [write_archive(name, part_files) for name, part_files in planned]
    return {
        "dir": asset.dir,
        "optional": asset.optional,
        "file_count": len(files),
        "size_bytes": sum(p["size_bytes"] for p in parts),
        "parts": parts,
    }


def add_human_sizes(entry: dict) -> dict:
    """
    Put a text size ("1.4 MiB") next to every byte count of a manifest entry: the asset, each
    archive and each file. The byte counts stay; the text is for people reading the manifest.
    """
    entry["size_human"] = human_size(entry["size_bytes"])
    for part in entry["parts"]:
        part["size_human"] = human_size(part["size_bytes"])
        for f in part["files"]:
            f["size_human"] = human_size(f["size"])
    return entry


def _git_commit() -> str | None:
    try:
        out = subprocess.run(["git", "rev-parse", "HEAD"], cwd=PROJECT_ROOT, capture_output=True,
                             text=True, check=True, timeout=10)
        return out.stdout.strip() or None
    except (OSError, subprocess.SubprocessError):
        return None


def archives_of(entry: dict) -> list[str]:
    """Archive names of a manifest entry (schema 2 parts, or schema 1's single archive)."""
    parts = entry.get("parts")
    if parts:
        return [p["archive"] for p in parts]
    return [entry["archive"]] if entry.get("archive") else []


def write_manifest(entries: dict[str, dict]) -> None:
    """
    dist/manifest.json. Entries from this run replace the ones of the same asset; assets
    bundled earlier (and still having all their archives) are kept, so a partial run does not
    drop them from what setup-data can see.
    """
    path = DIST_DIR / MANIFEST_NAME
    assets: dict[str, dict] = {}
    if path.exists():
        try:
            old = json.loads(path.read_text(encoding="utf-8")).get("assets", {})
            assets = {n: e for n, e in old.items()
                      if e.get("parts") and all((DIST_DIR / a).exists() for a in archives_of(e))}
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
    for col in ("asset", "folder", "files", "size", "archives", "optional"):
        table.add_column(col)
    scans = {}
    for a in known.values():
        files = scan(a)
        scans[a.name] = files
        table.add_row(a.name, a.dir, str(len(files)) if files else "[dim]none[/]",
                      _mb(sum(f["size"] for f in files)) if files else "-",
                      str(len(plan_parts(a, files))) if files else "-", "yes" if a.optional else "no")
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
@click.option("--human-sizes/--no-human-sizes", "human", default=None,
              help="Also write sizes as text (1.4 MiB) in manifest.json. Default: the human_sizes setting "
                   "in assets.json (on).")
@click.option("--ignore", "ignore", multiple=True, metavar="PATTERN",
              help="Leave matching files or folders out of the archives, for this run. Repeatable. "
                   "Relative to data/, e.g. --ignore 'yield/*.bak' --ignore 'soil/old' --ignore '*.tmp'. "
                   "Also env ASSETS_IGNORE (comma separated, always applied).")
@asset_options
def main(interactive: bool, human: bool | None, ignore: tuple[str, ...],
         only: tuple[str, ...], exclude: tuple[str, ...]) -> None:
    """Bundle data directories into tarballs, plus a manifest, ready for upload."""

    add_ignore_patterns(ignore)
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
    want_human = load_asset_config()["human_sizes"] if human is None else human

    console.print(Rule("[bold cyan]Akilimo Asset Bundler[/]"))
    console.print(f"  Output directory: {DIST_DIR}")
    console.print(f"  Assets          : {', '.join(a.name for a in targets) or '(none)'}")
    if ignore:
        console.print(f"  Ignoring        : {', '.join(ignore)}")

    for asset in targets:
        console.print(Rule(asset.name, style="dim"))
        entry = create_bundle(asset)
        if entry is None:
            failed.append(asset)
        else:
            entries[asset.name] = add_human_sizes(entry) if want_human else entry

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

    archives = [name for e in entries.values() for name in archives_of(e)]
    console.print(
        "\n[bold]Next steps:[/]\n\n"
        f"  Upload {len(archives)} archive(s) and {MANIFEST_NAME}:\n"
        "  poetry run upload-osf        (or poetry run upload-zenodo --new)\n"
        "  Both upload the manifest last, skip files whose hash the host already holds, and\n"
        "  take the same --only / --exclude options (plus --jobs and --force)."
    )
