#!/bin/sh
# Waits for a deployed service to answer GET /health, on the port the service
# really listens on.
#
#   sh tools/check_health.sh <service> <app_dir> [seconds]     (default 180)
#
# The deploy used to read API_PORT from .env and call that port. That can pass for
# the wrong reason: if the service has crashed but another instance (production,
# on 8000) answers on the port beta was meant to use, the check goes green. So:
#
#   1. find the ports the service's own processes listen on (its systemd cgroup,
#      then ss), and only accept a /health answer from one of them;
#   2. if that cannot be worked out here (no ss, no systemd), fall back to the
#      configured port: Environment= or EnvironmentFile= in the unit, then the
#      app directory's .env, then 8000, and say that the port was not verified.
#
# Exits 0 when healthy, 1 when not. Prints the service status and last log lines
# on failure. Changes nothing.

# The tests source this file for its functions and set these themselves
if [ -z "$CHECK_HEALTH_LIB" ]; then
  service="$1"
  app_dir="$2"
fi
wait_s="${3:-180}"

# API_PORT=... from an env file, quotes and carriage returns removed
env_file_port() {
  [ -f "$1" ] || return 0
  grep -E '^[[:space:]]*API_PORT=' "$1" 2>/dev/null | tail -n 1 | cut -d= -f2 | tr -d "\"' \r"
}

# The port the service is configured for: the unit's own Environment=, then its
# EnvironmentFile=s, then the app directory's .env, then the API's default.
configured_port() {
  port=""

  if command -v systemctl >/dev/null 2>&1; then
    # Environment=A=1 B=2 (systemd prints them space separated)
    port="$(systemctl show -p Environment --value "$service" 2>/dev/null | tr ' ' '\n' | grep '^API_PORT=' | tail -n 1 | cut -d= -f2 | tr -d "\"'")"

    if [ -z "$port" ]; then
      # EnvironmentFiles=/path/.env (ignore_errors=no)
      for f in $(systemctl show -p EnvironmentFiles --value "$service" 2>/dev/null | sed 's/([^)]*)//g'); do
        port="$(env_file_port "${f#-}")"
        [ -n "$port" ] && break
      done
    fi
  fi

  [ -z "$port" ] && port="$(env_file_port "$app_dir/.env")"
  echo "${port:-8000}"
}

# Process ids that belong to the service: its whole cgroup (R may start workers),
# or just the main process.
service_pids() {
  command -v systemctl >/dev/null 2>&1 || return 0

  cg="$(systemctl show -p ControlGroup --value "$service" 2>/dev/null)"
  if [ -n "$cg" ] && [ -r "/sys/fs/cgroup${cg}/cgroup.procs" ]; then
    cat "/sys/fs/cgroup${cg}/cgroup.procs" 2>/dev/null
    return 0
  fi

  main="$(systemctl show -p MainPID --value "$service" 2>/dev/null)"
  [ -n "$main" ] && [ "$main" != "0" ] && echo "$main"
  return 0
}

# Is there a way to see which process listens on a port? (ss, and process names
# need sudo or the same user)
can_verify() {
  command -v ss >/dev/null 2>&1 || return 1
  [ -n "$(service_pids)" ] || return 1
  return 0
}

# Ports (one per line) that the service's own processes listen on
listening_ports() {
  pids="$(service_pids | tr '\n' ' ')"
  [ -n "$pids" ] || return 0

  { sudo -n ss -ltnp 2>/dev/null || ss -ltnp 2>/dev/null; } | while read -r line; do
    for pid in $pids; do
      case "$line" in
        *"pid=$pid,"*)
          addr="$(echo "$line" | awk '{print $4}')"
          echo "${addr##*:}"
          break
          ;;
      esac
    done
  done | sort -u
}

# Run a command for at most 10 seconds when `timeout` exists: the failure report
# must never hang the deploy it is reporting on
limited() {
  if command -v timeout >/dev/null 2>&1; then timeout 10 "$@"; else "$@"; fi
}

report_failure() {
  echo "ERROR: $service did not become healthy within ${wait_s}s"
  command -v systemctl >/dev/null 2>&1 && {
    limited systemctl status "$service" --no-pager 2>&1 | head -n 12 || true
    { limited sudo -n journalctl -u "$service" -n 50 --no-pager 2>/dev/null || limited journalctl -u "$service" -n 50 --no-pager 2>&1; } | tail -n 50 || true
  }
}

# Sourced by the tests, which only want the functions
[ -n "$CHECK_HEALTH_LIB" ] && return 0 2>/dev/null

if [ -z "$service" ] || [ -z "$app_dir" ]; then
  echo "Usage: sh tools/check_health.sh <service> <app_dir> [seconds]" >&2
  exit 2
fi

configured="$(configured_port)"
verify=false
can_verify && verify=true

if [ "$verify" = true ]; then
  echo "Waiting for $service to listen and answer /health (configured port $configured) ..."
else
  echo "Waiting for http://127.0.0.1:$configured/health (could not look up the service's own port, so it is not verified) ..."
fi

waited=0
while [ "$waited" -lt "$wait_s" ]; do
  if [ "$verify" = true ]; then
    for port in $(listening_ports); do
      if curl -fsS --connect-timeout 3 --max-time 5 "http://127.0.0.1:$port/health" >/dev/null 2>&1; then
        echo "Service healthy on port $port after ~${waited}s"
        [ "$port" != "$configured" ] && echo "NOTE: it listens on $port, but its configuration says $configured. Check API_PORT."
        exit 0
      fi
    done
  elif curl -fsS --connect-timeout 3 --max-time 5 "http://127.0.0.1:$configured/health" >/dev/null 2>&1; then
    echo "Something answered on port $configured after ~${waited}s (not confirmed to be $service)"
    exit 0
  fi

  sleep 5
  waited=$((waited + 5))
done

report_failure
exit 1
