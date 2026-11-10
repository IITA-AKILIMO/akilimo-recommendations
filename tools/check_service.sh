#!/bin/sh
# Read-only check of a deployed instance. Every path comes from
# .github/deploy-targets.json (the APP_DIR and service name the deploy itself
# uses), so nothing is typed by hand and the docs' older paths do not matter.
#
#   sh tools/check_service.sh            # beta
#   sh tools/check_service.sh production
#
# Shows whether the directory the deploy updates is the one the service runs
# from, the port from that directory's .env, what is listening on 8000/8001,
# and the service's state and last log lines. Changes nothing.

target="${1:-beta}"
root="$(cd "$(dirname "$0")/.." && pwd)"
cfg="$root/.github/deploy-targets.json"

get() {
  python3 -c 'import json, sys; print(json.load(open(sys.argv[1]))[sys.argv[2]][sys.argv[3]])' "$cfg" "$target" "$1" 2>/dev/null
}

APP_DIR="$(get app_dir)" || { echo "No target '$target' in $cfg" >&2; exit 1; }
SERVICE="$(get service_name)"

# Real path of something that may not exist (readlink -f accepts a missing last part)
real() { readlink -f "$1" 2>/dev/null || echo "$1"; }

echo "== $target ($SERVICE)"
echo "APP_DIR from deploy-targets.json : $APP_DIR -> $(real "$APP_DIR")"
echo "this checkout                    : $root -> $(real "$root")"

if command -v systemctl >/dev/null 2>&1; then
  WD="$(systemctl show -p WorkingDirectory --value "$SERVICE" 2>/dev/null)"
  if [ -z "$WD" ]; then
    echo "service WorkingDirectory         : (unit not found, or it sets no WorkingDirectory)"
    echo "CHECK: cannot compare the service's directory with APP_DIR."
  else
    echo "service WorkingDirectory         : $WD -> $(real "$WD")"
    if [ "$(real "$WD")" = "$(real "$APP_DIR")" ]; then
      echo "OK: the deploy updates the directory the service runs from."
    else
      echo "CHECK: the service's directory differs from APP_DIR; the deploy may update a different copy."
    fi
  fi
else
  echo "systemctl not available here, so the service's directory was not checked."
fi

echo
echo "API_PORT in $APP_DIR/.env:"
grep -n -E '^[[:space:]]*API_PORT=' "$APP_DIR/.env" 2>/dev/null || echo "  (none: the API uses its default, 8000)"

echo
echo "Listening on 8000 or 8001 (process names need sudo):"
{ sudo -n ss -ltnp 2>/dev/null || ss -ltn 2>/dev/null; } | grep -E ':(8000|8001)[[:space:]]' || echo "  (nothing)"

if command -v systemctl >/dev/null 2>&1; then
  echo
  systemctl status "$SERVICE" --no-pager 2>&1 | head -n 6
  echo
  echo "Last log lines:"
  { sudo -n journalctl -u "$SERVICE" -n 10 --no-pager 2>/dev/null || journalctl -u "$SERVICE" -n 10 --no-pager 2>&1; } | tail -n 10
fi
