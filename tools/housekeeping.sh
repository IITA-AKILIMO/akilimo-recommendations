#!/bin/sh
# Housekeeping after a deploy, run on the server once the service is healthy.
#
#   sh tools/housekeeping.sh <service> <app_dir> [tasks]
#
# tasks: a comma separated list, "all" (the default) or "none". The tasks:
#
#   translations  Rscript refresh_prices.R --type translations: pull the translation
#                 strings from the AKILIMO API into the local database
#   prices        Rscript refresh_prices.R --type default, then --type starch: the
#                 fertilizer, labour, cassava and starch prices
#   coverage      GET /coverage for FR and SP on the service's own port. The footprints
#                 are built on the first call and then cached for the life of the
#                 process, so this does it now and not on a farmer's first request
#   api-sync      run $COVERAGE_SYNC_CMD, the command that makes the AKILIMO API copy
#                 the engine's coverage into its database (php artisan coverage:sync).
#                 Skipped when COVERAGE_SYNC_CMD is not set; it is the one task that
#                 reaches outside this app, so its command is yours to give
#
# translations and prices need AKILIMO_API_URL in the app's .env; without it the R
# script says so and does nothing.
#
# Each task is independent and none can fail the deploy: a failing task is reported
# as a warning and the script still exits 0. Set HOUSEKEEPING_STRICT=1 to exit 1 when
# any task failed. A bad task name is a usage error (exit 2), before anything runs.

here="$(cd "$(dirname "$0")" && pwd)"
service="$1"
app_dir="$2"
tasks="${3:-all}"

ALL_TASKS="translations prices coverage api-sync"

if [ -z "$service" ] || [ -z "$app_dir" ]; then
  echo "Usage: sh tools/housekeeping.sh <service> <app_dir> [tasks]" >&2
  exit 2
fi

# Which tasks, in a fixed order
case "$tasks" in
  all)  selected="$ALL_TASKS" ;;
  none) selected="" ;;
  *)
    selected=""
    for want in $(echo "$tasks" | tr ',' ' '); do
      case " $ALL_TASKS " in
        *" $want "*) ;;
        *) echo "Unknown housekeeping task '$want'. Known: $(echo "$ALL_TASKS" | tr ' ' ',')" >&2; exit 2 ;;
      esac
    done
    for t in $ALL_TASKS; do
      case ",$tasks," in *",$t,"*) selected="$selected $t" ;; esac
    done
    ;;
esac

if [ -z "$selected" ]; then
  echo "Housekeeping: nothing to do."
  exit 0
fi

# The port-finding functions of the health check (the service's own port, then the configured one)
CHECK_HEALTH_LIB=1
. "$here/check_health.sh"

# At most $2 seconds when `timeout` exists, so a stuck task cannot hang the deploy
limit() {
  secs="$1"; shift
  if command -v timeout >/dev/null 2>&1; then timeout "$secs" "$@"; else "$@"; fi
}

service_port() {
  port=""
  if can_verify; then port="$(listening_ports | head -n 1)"; fi
  echo "${port:-$(configured_port)}"
}

task_translations() {
  cd "$app_dir" && limit 600 Rscript refresh_prices.R --type translations
}

task_prices() {
  cd "$app_dir" || return 1
  limit 900 Rscript refresh_prices.R --type default || return 1
  limit 900 Rscript refresh_prices.R --type starch
}

task_coverage() {
  port="$(service_port)"
  for type in FR SP; do
    out="$(curl -fsS --connect-timeout 5 --max-time 300 "http://127.0.0.1:$port/coverage?type=$type")" || return 1
    case "$out" in
      *FeatureCollection*) echo "  /coverage?type=$type answered on port $port" ;;
      *) echo "  /coverage?type=$type on port $port did not return a FeatureCollection" >&2; return 1 ;;
    esac
  done
}

task_api_sync() {
  if [ -z "$COVERAGE_SYNC_CMD" ]; then
    echo "  COVERAGE_SYNC_CMD is not set"
    return 3   # skipped
  fi
  limit 900 sh -c "$COVERAGE_SYNC_CMD"
}

failed=""
echo "Housekeeping for $service in $app_dir:$selected"
for task in $selected; do
  echo "== $task"
  fn="task_$(echo "$task" | tr '-' '_')"
  ( "$fn" )
  rc=$?
  case "$rc" in
    0) echo "   ok: $task" ;;
    3) echo "   skipped: $task" ;;
    *) echo "   WARNING: $task failed (exit $rc), the deploy carries on"
       echo "::warning::Housekeeping task '$task' failed (exit $rc)"
       failed="$failed $task" ;;
  esac
done

if [ -n "$failed" ]; then
  echo "Housekeeping finished with failures:$failed"
  [ "$HOUSEKEEPING_STRICT" = "1" ] && exit 1
else
  echo "Housekeeping finished."
fi
exit 0
