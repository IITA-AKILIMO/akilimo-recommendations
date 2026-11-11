#!/bin/sh
# Tests for tools/check_health.sh: how it finds the configured port and the
# ports the service itself listens on. Runs with plain sh and fake systemctl and
# ss commands, so it needs neither a server nor R.
#
#   sh tests/test_check_health.sh

root="$(cd "$(dirname "$0")/.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

fails=0
ok()   { echo "ok   - $1"; }
fail() { echo "FAIL - $1 (expected '$2', got '$3')"; fails=$((fails + 1)); }
expect() { [ "$2" = "$3" ] && ok "$1" || fail "$1" "$2" "$3"; }

mkdir -p "$tmp/bin" "$tmp/app"
PATH="$tmp/bin:$PATH"
export PATH

# A fake systemctl driven by files in $tmp
cat > "$tmp/bin/systemctl" <<'EOF'
#!/bin/sh
# systemctl show -p <Property> --value <service>
prop="$3"
cat "$FAKE_DIR/$prop" 2>/dev/null
EOF
chmod +x "$tmp/bin/systemctl"
FAKE_DIR="$tmp/props"; export FAKE_DIR
mkdir -p "$FAKE_DIR"

# A fake ss: prints what the test put in $tmp/ss.out
cat > "$tmp/bin/ss" <<'EOF'
#!/bin/sh
cat "$FAKE_DIR/ss.out"
EOF
chmod +x "$tmp/bin/ss"

CHECK_HEALTH_LIB=1
service="akilimo-api-beta.service"
app_dir="$tmp/app"
. "$root/tools/check_health.sh"

# --- the configured port ------------------------------------------------------

expect "defaults to 8000 with nothing configured" "8000" "$(configured_port)"

printf 'API_PORT=8001\r\nOTHER=1\r\n' > "$app_dir/.env"
expect "reads API_PORT from the app directory's .env, ignoring a carriage return" "8001" "$(configured_port)"

printf 'API_PORT="8002"\n' > "$tmp/unit.env"
printf '%s (ignore_errors=no)\n' "$tmp/unit.env" > "$FAKE_DIR/EnvironmentFiles"
expect "an EnvironmentFile in the unit wins over .env" "8002" "$(configured_port)"

printf 'FOO=bar API_PORT=8003 BAZ=1\n' > "$FAKE_DIR/Environment"
expect "Environment= in the unit wins over everything" "8003" "$(configured_port)"

# --- the ports the service itself listens on ----------------------------------

rm -f "$FAKE_DIR/Environment" "$FAKE_DIR/EnvironmentFiles"
printf '/system.slice/akilimo-api-beta.service\n' > "$FAKE_DIR/ControlGroup"
printf '4242\n' > "$FAKE_DIR/MainPID"

cat > "$FAKE_DIR/ss.out" <<'EOF'
State  Recv-Q Send-Q Local Address:Port Peer Address:Port Process
LISTEN 0      4096         0.0.0.0:8000      0.0.0.0:*    users:(("R",pid=999,fd=7))
LISTEN 0      4096         0.0.0.0:8001      0.0.0.0:*    users:(("R",pid=4242,fd=7))
LISTEN 0      128             [::]:22              [::]:*    users:(("sshd",pid=1,fd=3))
EOF

expect "finds the port its own process listens on, not production's" "8001" "$(listening_ports)"

printf '4242\n5000\n' > "$FAKE_DIR/MainPID"
expect "a service with more than one process still reports the port that one listens on" "8001" "$(listening_ports)"

cat > "$FAKE_DIR/ss.out" <<'EOF'
State  Recv-Q Send-Q Local Address:Port Peer Address:Port Process
LISTEN 0      4096         0.0.0.0:8000      0.0.0.0:*    users:(("R",pid=999,fd=7))
EOF
expect "when only another instance listens, the service has no port (so it cannot pass on that)" "" "$(listening_ports)"

if can_verify; then ok "can verify when ss and the service's process are known"; else fail "can_verify" "yes" "no"; fi

rm -f "$FAKE_DIR/MainPID" "$FAKE_DIR/ControlGroup"
if can_verify; then fail "can_verify without a process" "no" "yes"; else ok "cannot verify without the service's process"; fi

[ "$fails" -eq 0 ] && { echo "all passed"; exit 0; }
echo "$fails failed"
exit 1
