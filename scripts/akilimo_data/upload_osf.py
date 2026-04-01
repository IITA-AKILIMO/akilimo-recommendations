"""
upload-osf — Upload scientific data bundles to an OSF project.

Uses the OSF Waterbutler API to upload files to an OSF project's osfstorage.
Existing files are updated in-place; new files are created automatically.

Configuration is loaded from scripts/.env (see .env.example).
CLI flags override .env values.

Prerequisites:
  1. Run `poetry run bundle-assets` to create dist/*.tar.gz
  2. Create an OSF project at https://osf.io and note the node ID (5-char code)
  3. Set OSF_TOKEN in scripts/.env (generate at https://osf.io/settings/tokens)
     Required scope: osf.full_write  (or osf.full_read for private project access)

Usage:
    poetry run upload-osf --node-id abc12

.env keys used:
    OSF_TOKEN     (required, sensitive — never commit)
    OSF_NODE_ID   default node ID for --node-id
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

import click
import requests
from rich.rule import Rule
from tqdm import tqdm

from .common import DIST_DIR, Config, ProgressReader, console

OSF_BUNDLES = [
    "data-input.tar.gz",
    "soil-data.tar.gz",
    "yield-data.tar.gz",
]

_WB_BASE = "https://files.osf.io/v1/resources"
_API_BASE = "https://api.osf.io/v2"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _list_files(node_id: str, token: str) -> list[dict]:
    """Return the file metadata list from a node's osfstorage."""
    url = f"{_API_BASE}/nodes/{node_id}/files/osfstorage/"
    resp = requests.get(
        url,
        headers={"Authorization": f"Bearer {token}"},
        timeout=30,
    )
    resp.raise_for_status()
    return resp.json().get("data", [])


def _upload_file(node_id: str, filepath: Path, token: str, existing: list[dict]) -> bool:
    """Upload *filepath* to an OSF node, overwriting any existing file of the same name."""
    size = filepath.stat().st_size
    console.print(f"  Uploading [cyan]{filepath.name}[/]  ({size / 1_048_576:.1f} MB) ...")

    # Determine URL: update existing file or create new one.
    match = next(
        (f for f in existing if f.get("attributes", {}).get("name") == filepath.name),
        None,
    )
    if match:
        # Waterbutler upload link for the existing file (overwrites content).
        upload_url = match.get("links", {}).get("upload", "")
        if not upload_url:
            file_id = match["id"].split("/")[-1]
            upload_url = f"{_WB_BASE}/{node_id}/providers/osfstorage/{file_id}"
    else:
        # New file — use the ?name= query parameter to set the filename.
        upload_url = f"{_WB_BASE}/{node_id}/providers/osfstorage/?name={filepath.name}"

    with (
        open(filepath, "rb") as fh,
        tqdm(total=size, unit="B", unit_scale=True, desc=f"  {filepath.name}", leave=False) as bar,
    ):
        response = requests.put(
            upload_url,
            data=ProgressReader(fh, bar),
            headers={
                "Authorization": f"Bearer {token}",
                "Content-Type": "application/octet-stream",
                "Content-Length": str(size),
            },
            timeout=600,
        )

    if response.status_code in (200, 201):
        name = (
            response.json()
            .get("data", {})
            .get("attributes", {})
            .get("name", filepath.name)
        )
        action = "updated" if match else "created"
        console.print(f"  [green][ok][/]   {name} ({action})")
        return True

    console.print(
        f"  [bold red][fail][/] {filepath.name}: "
        f"HTTP {response.status_code} — {response.text[:300]}"
    )
    return False


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

@click.command()
@click.option(
    "--node-id",
    default=lambda: os.environ.get("OSF_NODE_ID", ""),
    show_default="env OSF_NODE_ID",
    help="OSF project node ID (5-character code shown in the project URL).",
)
def main(node_id: str) -> None:
    """Upload soil-data and yield-data bundles to an OSF project."""

    token = os.environ.get("OSF_TOKEN", "")
    if not token:
        console.print(
            "[red]OSF_TOKEN is not set.[/]\n"
            "Add it to [bold]scripts/.env[/]:\n"
            "  OSF_TOKEN=your-personal-access-token\n\n"
            "Generate one at: https://osf.io/settings/tokens\n"
            "Required scope: [bold]osf.full_write[/]"
        )
        sys.exit(1)

    if not node_id:
        console.print(
            "[red]OSF node ID is not set.[/]\n"
            "Pass [bold]--node-id <id>[/] or add [bold]OSF_NODE_ID[/] to scripts/.env.\n"
            "The node ID is the 5-character code in your OSF project URL:\n"
            "  https://osf.io/[bold]abc12[/]/"
        )
        sys.exit(1)

    cfg = Config(osf_node_id=node_id)

    console.print(Rule("[bold cyan]Akilimo OSF Uploader[/]"))
    console.print(f"  OSF project: https://osf.io/{node_id}/")

    # -----------------------------------------------------------------------
    # Step 1 — verify node and list existing files
    # -----------------------------------------------------------------------
    console.print(Rule("Checking OSF project"))
    try:
        existing = _list_files(node_id, token)
        console.print(
            f"  [green][ok][/]   Node accessible — "
            f"{len(existing)} file(s) currently in osfstorage"
        )
    except requests.HTTPError as exc:
        status = exc.response.status_code if exc.response is not None else "?"
        console.print(
            f"[red]Cannot access OSF node {node_id}:[/] HTTP {status}\n"
            "  • Check that OSF_NODE_ID is correct\n"
            "  • Verify OSF_TOKEN has osf.full_write scope"
        )
        sys.exit(1)
    except requests.RequestException as exc:
        console.print(f"[red]Network error:[/] {exc}")
        sys.exit(1)

    # -----------------------------------------------------------------------
    # Step 2 — upload bundles
    # -----------------------------------------------------------------------
    console.print(Rule("Uploading files"))

    failed: list[str] = []
    for filename in OSF_BUNDLES:
        filepath = DIST_DIR / filename
        if not filepath.exists():
            console.print(
                f"  [yellow][skip][/] {filename} not found in dist/. "
                "Run [bold]poetry run bundle-assets[/] first."
            )
            failed.append(filename)
            continue

        if not _upload_file(node_id, filepath, token, existing):
            failed.append(filename)

    # -----------------------------------------------------------------------
    # Step 3 — summary and next steps
    # -----------------------------------------------------------------------
    console.print(Rule())

    if failed:
        console.print(f"[red]{len(failed)} upload(s) failed:[/] {', '.join(failed)}")
        sys.exit(1)

    console.print(
        "[green bold]Upload complete.[/]\n\n"
        f"  1. Review your project: [cyan]https://osf.io/{node_id}/[/]\n"
        "  2. Adjust project metadata (title, description, license) in the OSF UI if needed.\n"
        "  3. Once ready, make the project public to enable unauthenticated downloads.\n\n"
        "  4. Add the node ID to [bold]scripts/.env[/] so setup-data can use it:\n"
        f"       OSF_NODE_ID={node_id}"
    )
