#!/bin/bash
# ──────────────────────────────────────────────────────────────────────────
#  dev.sh  — hot-reload development server for akilimo-recommendations
#
#  Watches R/, api.R, net/*.css and data/input/translations.csv.
#  Restarts Rscript api.R automatically whenever a watched file changes.
#
#  Prefers watchexec; falls back to entr (Linux/macOS).
#
#  Install watchexec (preferred):
#    cargo install watchexec-cli
#    brew install watchexec          (macOS)
#    https://github.com/watchexec/watchexec/releases
#
#  Install entr (fallback):
#    apt-get install entr            (Debian/Ubuntu)
#    brew install entr               (macOS)
# ──────────────────────────────────────────────────────────────────────────

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TARGET_DIR="${WORKDIR:-$SCRIPT_DIR}"

# Find Rscript
RSCRIPT_CMD=""
for cmd in Rscript /usr/bin/Rscript /usr/local/bin/Rscript /opt/R/bin/Rscript; do
    if command -v "$cmd" >/dev/null 2>&1; then
        RSCRIPT_CMD="$cmd"
        break
    fi
done

if [ -z "$RSCRIPT_CMD" ]; then
    echo "ERROR: Rscript not found" >&2
    exit 1
fi

echo "Akilimo dev server — auto-restart enabled"
echo "Rscript : $RSCRIPT_CMD"
echo "Root    : $TARGET_DIR"
echo "────────────────────────────────────────────"

# ── watchexec (preferred) ────────────────────────────────────────────────
if command -v watchexec >/dev/null 2>&1; then
    echo "Watcher : watchexec"
    echo ""
    exec watchexec \
        --watch "$TARGET_DIR/R" \
        --watch "$TARGET_DIR/api.R" \
        --watch "$TARGET_DIR/net" \
        --watch "$TARGET_DIR/data/input/translations.csv" \
        --exts R,csv,css \
        --restart \
        --debounce 800 \
        --clear \
        -- "$RSCRIPT_CMD" "$TARGET_DIR/api.R"
fi

# ── entr (fallback) ──────────────────────────────────────────────────────
if command -v entr >/dev/null 2>&1; then
    echo "Watcher : entr"
    echo ""
    # entr needs the file list on stdin; -r restarts the process on change
    find "$TARGET_DIR/R" "$TARGET_DIR/net" \
         -name "*.R" -o -name "*.css" \
    | { echo "$TARGET_DIR/api.R"; \
        echo "$TARGET_DIR/data/input/translations.csv"; \
        cat; } \
    | entr -r "$RSCRIPT_CMD" "$TARGET_DIR/api.R"
    exit $?
fi

# ── no watcher found ─────────────────────────────────────────────────────
echo ""
echo "ERROR: no file watcher found. Install one of:"
echo "  watchexec  — https://github.com/watchexec/watchexec/releases"
echo "               cargo install watchexec-cli"
echo "  entr       — apt-get install entr  /  brew install entr"
exit 1
