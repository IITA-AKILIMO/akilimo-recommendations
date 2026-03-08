#!/bin/bash
# Try multiple Rscript locations
RSCRIPT_CMD=""
for cmd in /usr/bin/Rscript /usr/local/bin/Rscript /opt/R/bin/Rscript $(which Rscript 2>/dev/null); do
    if [ -x "$cmd" ]; then
        RSCRIPT_CMD="$cmd"
        break
    fi
done

if [ -z "$RSCRIPT_CMD" ]; then
    echo "ERROR: Rscript not found" >&2
    exit 1
fi

echo "Using Rscript at: $RSCRIPT_CMD"
exec $RSCRIPT_CMD /home/akilimo/projects/new_akilimo/api.R
