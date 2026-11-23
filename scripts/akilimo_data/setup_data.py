"""
setup-data — Download all runtime data files from Zenodo or OSF.

All asset bundles are hosted on a single deposit/project and downloaded from
there. Which ones is configured in scripts/assets.json; narrow a run with
--only / --exclude, or for good with ASSETS / ASSETS_EXCLUDE.

Configuration is loaded from scripts/.env (see .env.example).
CLI flags override .env values.

Usage:
    poetry run setup-data                              # the default source: github
    poetry run setup-data --source zenodo
    poetry run setup-data --source osf --osf-node-id abc12
    poetry run setup-data --source osf                # or zenodo; DATA_SOURCE sets it for good
    poetry run setup-data --zenodo-id 1234567
    poetry run setup-data --sandbox
    poetry run setup-data --only yield --only soil
    poetry run setup-data --exclude dssat
    poetry run setup-data --list-assets

.env keys used:
    DATA_SOURCE, ZENODO_RECORD_ID, ZENODO_USE_SANDBOX, OSF_NODE_ID, OSF_TOKEN, ASSETS, ASSETS_EXCLUDE,
    GITHUB_DATA_REPO, GITHUB_DATA_TAG, GITHUB_TOKEN
"""

from __future__ import annotations

import os
import sys

import click
from rich.rule import Rule

from .assets import asset_options, from_manifest, resolve
from . import github_release
from .common import (
    PROJECT_ROOT,
    Config,
    load_asset_config,
    console,
    ManifestUnavailable,
    fetch_manifest,
    fetch_osf_tarball,
    fetch_zenodo_tarball,
)

def _detect_source(zenodo_id: str, osf_node_id: str) -> str:
    """
    The source when neither --source nor DATA_SOURCE says: "default_source" of scripts/assets.json
    (github unless changed). "auto" there restores the old guess from the IDs: OSF, then Zenodo.
    """
    configured = load_asset_config()["default_source"]
    if configured in ("github", "osf", "zenodo"):
        return configured
    if osf_node_id:
        return "osf"
    if zenodo_id:
        return "zenodo"
    return "osf"  # will fail gracefully with a clear message


@click.command()
@click.option(
    "--source",
    type=click.Choice(["zenodo", "osf", "github"]),
    default=None,
    envvar="DATA_SOURCE",
    show_default="env DATA_SOURCE, else default_source in scripts/assets.json (github)",
    help="Download source: github (a release of the data repository, see upload-github), osf or zenodo. "
         "Default: default_source in scripts/assets.json, which is github.",
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
    """Download Akilimo runtime data from a GitHub release, OSF or Zenodo."""

    cfg = Config(zenodo_id=zenodo_id, sandbox=sandbox, osf_node_id=osf_node_id)

    if source is None:
        source = _detect_source(zenodo_id, osf_node_id)

    # What was published (archive names, checksums, the files in each bundle). Its asset names
    # are known to this run even when this machine has none of the data yet.
    try:
        manifest = None if no_manifest else fetch_manifest(source, cfg)
    except ManifestUnavailable as exc:
        # Not "there is no manifest": the host failed. Guessing the archives from the registry
        # would leave an asset that is split into several archives incomplete.
        console.print(f"[red bold]Cannot read the published manifest:[/] {exc}.\n"
                      "  Not guessing which archives belong together. Try again later, or pass "
                      "[bold]--no-manifest[/] to download the main archive of each asset only.")
        sys.exit(1)
    published = manifest["assets"] if manifest else None
    # With a manifest, what it lists is what gets downloaded (config lists neither add nor drop);
    # without one, the config and the asset registry decide
    assets = (from_manifest(published, only, exclude) if published is not None
              else resolve(only, exclude, downloading=True))

    console.print(Rule("[bold cyan]Akilimo Data Setup[/]"))
    console.print(f"  Project root : {PROJECT_ROOT}")
    console.print(f"  Source       : [bold]{source}[/]")
    console.print(f"  Assets       : {', '.join(a.name for a in assets) or '(none)'}"
                  + ("  [dim](from the manifest)[/]" if published is not None else ""))

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
    elif source == "github":
        console.print(
            f"  GitHub release: {github_release.repo_slug(cfg)} @ {github_release.tag_name()}"
            + ("" if github_release.token() else "  [dim](no token: only a public repository can be read)[/]")
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
    fetch = {"zenodo": fetch_zenodo_tarball, "github": github_release.fetch_github_tarball}.get(source, fetch_osf_tarball)
    optional_missed: list[str] = []
    for asset in assets:
        before = len(cfg.failed)
        entry = published.get(asset.name) if published is not None else None
        if published is not None and entry is None:
            console.print(f"  [yellow][skip][/] {asset.archive} — not in the published manifest")
            cfg.failed.append(asset.archive)
        else:
            # What was published wins: each archive's real name, its checksum, and the files to
            # extract. An asset is one archive, or several (big files have their own)
            parts = (entry.get("parts") or [entry]) if entry else [{"archive": asset.archive}]
            for part in parts:
                fetch(
                    part["archive"],
                    cfg,
                    sha256=part.get("sha256"),
                    files={f["path"] for f in part["files"]} if part.get("files") else None,
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
        if source == "github":
            console.print(
                "\n[yellow]Common causes:[/]\n"
                f"  • Release [cyan]{github_release.tag_name()}[/] does not exist in {github_release.repo_slug(cfg)} yet (run upload-github)\n"
                "  • The repository is private and GITHUB_TOKEN is not set (or cannot read contents)\n"
                "  • No internet connection\n\n"
                "Re-run after fixing — already-extracted files are preserved."
            )
        elif source == "zenodo":
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
