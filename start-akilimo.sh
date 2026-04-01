#!/bin/bash

# Try multiple Rscript locations
RSCRIPT_CMD=""
for cmd in /usr/bin/Rscript /usr/local/bin/Rscript /opt/R/bin/Rscript $(which Rscript 2>/dev/null); do
    if [ -x "$cmd" ]; then
        RSCRIPT_CMD="$cmd"
        break
    fi
done

# If still not found, assume Rscript is in the same folder as api.R
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
if [ -z "$RSCRIPT_CMD" ] && [ -x "$SCRIPT_DIR/Rscript" ]; then
    RSCRIPT_CMD="$SCRIPT_DIR/Rscript"
fi

if [ -z "$RSCRIPT_CMD" ]; then
    echo "ERROR: Rscript not found" >&2
    exit 1
fi

# Use WORKDIR env variable if set, otherwise default to script location
if [ -n "$WORKDIR" ]; then
    TARGET_DIR="$WORKDIR"
else
    TARGET_DIR="$SCRIPT_DIR"
fi

echo "Using Rscript at: $RSCRIPT_CMD"
echo "Working directory: $TARGET_DIR"

exec "$RSCRIPT_CMD" "$TARGET_DIR/api.R"