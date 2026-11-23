#!/bin/sh
# Tests for tools/housekeeping.sh: which tasks run, that a failing task never fails the
# deploy, and the port the coverage warm-up calls. Plain sh with fake Rscript, curl,
# systemctl and ss, so it needs neither a server nor R.
#
#   sh tests/test_housekeeping.sh

root="$(cd "$(dirname "$0")/.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

fails=0
ok()   { echo "ok   - $1"; }
fail() { echo "FAIL - $1 (expected '$2', got '$3')"; fails=$((fails + 1)); }
expect() { [ "$2" = "$3" ] && ok "$1" || fail "$1" "$2" "$3"; }

mkdir -p "$tmp/bin" "$tmp/app" "$tmp/props"
PATH="$tmp/bin:$PATH"
export PATH
LOG="$tmp/calls.log"
export LOG

# Fake Rscript: logs its arguments, fails when FAIL_ON contains the argument text
cat > "$tmp/bin/Rscript" <<'EOF'
#!/bin/sh
echo "Rscript $*" >> "$LOG"
case "$FAIL_ON" in *"$*"*) exit 1 ;; esac
exit 0
EOF
# Fake curl: logs the URL, answers a FeatureCollection (or fails for BAD_CURL)
cat > "$tmp/bin/curl" <<'EOF'
#!/bin/sh
for a in "$@"; do url="$a"; done
echo "curl $url" >> "$LOG"
[ -n "$BAD_CURL" ] && exit 22
echo '{"type":"FeatureCollection","features":[]}'
EOF
# No systemctl / ss: the service port then comes from the app's .env
chmod +x "$tmp/bin/Rscript" "$tmp/bin/curl"
echo "API_PORT=8123" > "$tmp/app/.env"

run() {  # run <tasks> ; output in $tmp/out, calls in $LOG
  : > "$LOG"
  sh "$root/tools/housekeeping.sh" svc.service "$tmp/app" "$1" > "$tmp/out" 2>&1
  echo $?
}
calls() { grep -c "$1" "$LOG"; }

# all: every R task and both coverage calls; api-sync skipped without a command
rc="$(run all)"
expect "all exits 0" 0 "$rc"
expect "translations ran" 1 "$(calls 'refresh_prices.R --type translations')"
expect "default prices ran" 1 "$(calls 'refresh_prices.R --type default')"
expect "starch prices ran" 1 "$(calls 'refresh_prices.R --type starch')"
expect "coverage warmed for FR and SP on the configured port" 2 "$(calls 'http://127.0.0.1:8123/coverage?type=')"
expect "api-sync skipped when unset" 1 "$(grep -c 'skipped: api-sync' "$tmp/out")"

# a subset, in the fixed order, nothing else
rc="$(run coverage,translations)"
expect "subset exits 0" 0 "$rc"
expect "subset ran translations" 1 "$(calls 'type translations')"
expect "subset ran no prices" 0 "$(calls 'type default')"
expect "subset ran coverage" 2 "$(calls '/coverage?type=')"

# none
rc="$(run none)"
expect "none exits 0" 0 "$rc"
expect "none runs nothing" 0 "$(wc -l < "$LOG" | tr -d ' ')"

# a bad name is a usage error before anything runs
rc="$(run translations,typo)"
expect "unknown task exits 2" 2 "$rc"
expect "unknown task runs nothing" 0 "$(wc -l < "$LOG" | tr -d ' ')"

# a failing task is reported but never fails the deploy; the others still run
FAIL_ON="refresh_prices.R --type translations"; export FAIL_ON
rc="$(run all)"
expect "a failing task still exits 0" 0 "$rc"
expect "the failure is reported" 1 "$(grep -c 'WARNING: translations failed' "$tmp/out")"
expect "a warning annotation is printed" 1 "$(grep -c "::warning::Housekeeping task 'translations' failed" "$tmp/out")"
expect "later tasks still ran" 1 "$(calls 'type default')"

# strict mode turns a failure into exit 1
HOUSEKEEPING_STRICT=1; export HOUSEKEEPING_STRICT
rc="$(run translations)"
expect "strict exits 1 on a failure" 1 "$rc"
unset HOUSEKEEPING_STRICT FAIL_ON

# a failing coverage warm-up is a failure too (and does not stop the next task)
BAD_CURL=1; export BAD_CURL
rc="$(run coverage,api-sync)"
expect "bad coverage call still exits 0" 0 "$rc"
expect "bad coverage call reported" 1 "$(grep -c 'WARNING: coverage failed' "$tmp/out")"
unset BAD_CURL

# api-sync runs the command it is given, and a failing command is a warning
COVERAGE_SYNC_CMD="echo synced >> $LOG"; export COVERAGE_SYNC_CMD
rc="$(run api-sync)"
expect "api-sync exits 0" 0 "$rc"
expect "api-sync ran its command" 1 "$(calls synced)"
COVERAGE_SYNC_CMD="exit 5"; export COVERAGE_SYNC_CMD
rc="$(run api-sync)"
expect "failing api-sync still exits 0" 0 "$rc"
expect "failing api-sync reported" 1 "$(grep -c 'WARNING: api-sync failed (exit 5)' "$tmp/out")"
unset COVERAGE_SYNC_CMD

# usage
sh "$root/tools/housekeeping.sh" > /dev/null 2>&1
expect "no arguments exits 2" 2 "$?"

[ "$fails" -eq 0 ] && echo "all housekeeping tests passed" || { echo "$fails failed"; exit 1; }
