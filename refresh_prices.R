#!/usr/bin/env Rscript
# refresh_prices.R — Manual price refresh from the external price API
#
# Fetches current fertilizer/labour/cassava prices and starch factory prices
# from the configured PRICE_API_URL and writes them into prices.sqlite.
#
# Usage:
#   Rscript refresh_prices.R [options]
#
# Options:
#   --country  NG|TZ|RW|GH|BI   Refresh one country (default: all)
#   --type     default|starch|all  Which table to refresh (default: all)
#   --dry-run                    Validate API response without writing to DB
#
# Examples:
#   Rscript refresh_prices.R
#   Rscript refresh_prices.R --country NG --type default
#   Rscript refresh_prices.R --type starch
#   Rscript refresh_prices.R --dry-run
#
# Cron (every Monday at 02:00):
#   0 2 * * 1  cd /opt/akilimo && Rscript refresh_prices.R >> logs/price-refresh.log 2>&1

VALID_COUNTRIES <- c("NG", "TZ", "RW", "GH", "BI")
VALID_TYPES     <- c("default", "starch", "all")

# ── Parse arguments ───────────────────────────────────────────────────────────

parse_args <- function(argv) {
  out <- list(country = NULL, type = "all", dry_run = FALSE)
  i <- 1L
  while (i <= length(argv)) {
    arg <- argv[i]
    if (arg == "--dry-run") {
      out$dry_run <- TRUE
    } else if (arg == "--country") {
      i <- i + 1L
      val <- toupper(trimws(argv[i]))
      if (!val %in% VALID_COUNTRIES)
        stop("--country must be one of: ", paste(VALID_COUNTRIES, collapse = ", "))
      out$country <- val
    } else if (arg == "--type") {
      i <- i + 1L
      val <- tolower(trimws(argv[i]))
      if (!val %in% VALID_TYPES)
        stop("--type must be one of: ", paste(VALID_TYPES, collapse = ", "))
      out$type <- val
    } else {
      stop("Unknown argument: ", arg, "\nRun with no arguments for usage.")
    }
    i <- i + 1L
  }
  out
}

args <- tryCatch(
  parse_args(commandArgs(trailingOnly = TRUE)),
  error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    cat("Usage: Rscript refresh_prices.R [--country NG|TZ|RW|GH|BI] [--type default|starch|all] [--dry-run]\n")
    quit(status = 1L)
  }
)

# ── Bootstrap ─────────────────────────────────────────────────────────────────

akpath <- Sys.getenv("AKILIMO_ROOT", unset = ".")
setwd(akpath)

# Load .env so PRICE_API_URL etc. are available when running outside the server
if (requireNamespace("dotenv", quietly = TRUE)) {
  env_file <- file.path(akpath, ".env")
  if (file.exists(env_file)) dotenv::load_dot_env(env_file)
}

source(file.path(akpath, "R", "logging.R"))
source(file.path(akpath, "R", "prices_db.R"))

# ── Pre-flight checks ─────────────────────────────────────────────────────────

api_url <- Sys.getenv("PRICE_API_URL", unset = "")
if (!nzchar(api_url)) {
  cat("ERROR: PRICE_API_URL is not set.\n")
  cat("Set it in .env or as an environment variable before running this script.\n")
  quit(status = 1L)
}

cat(sprintf("Price refresh  %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")))
cat(sprintf("API URL        %s\n", api_url))
cat(sprintf("Type           %s\n", args$type))
cat(sprintf("Country        %s\n", if (is.null(args$country)) "all" else args$country))
if (args$dry_run) cat("Mode           DRY RUN — no DB writes\n")
cat(strrep("-", 60), "\n")

# ── Open DB ───────────────────────────────────────────────────────────────────

conn <- tryCatch(
  open_prices_db(),
  error = function(e) {
    cat("ERROR: could not open prices DB:", conditionMessage(e), "\n")
    quit(status = 1L)
  }
)
on.exit(DBI::dbDisconnect(conn), add = TRUE)

# ── Run refreshes ─────────────────────────────────────────────────────────────

results  <- list()
any_fail <- FALSE

run_refresh <- function(label, fn, ...) {
  cat(sprintf("%-12s ... ", label))
  r <- tryCatch(
    fn(..., dry_run = args$dry_run),
    error = function(e) list(status = "error", message = conditionMessage(e),
                             rows_upserted = 0L)
  )
  status_str <- toupper(r$status %||% "?")
  rows_str   <- if (!is.null(r$rows_upserted))
    sprintf("%d row(s)", r$rows_upserted) else ""
  msg_str    <- if (!is.null(r$message) && r$status != "ok")
    sprintf(" — %s", r$message) else ""
  cat(sprintf("%-8s %s%s\n", status_str, rows_str, msg_str))
  results[[label]] <<- r
  if (r$status != "ok") any_fail <<- TRUE
}

countries <- if (is.null(args$country)) VALID_COUNTRIES else args$country

if (args$type %in% c("default", "all")) {
  for (ctry in countries) {
    run_refresh(sprintf("default/%s", ctry), refresh_prices,
                country = ctry, source_tag = "manual")
  }
}

if (args$type %in% c("starch", "all")) {
  for (ctry in countries) {
    run_refresh(sprintf("starch/%s",  ctry), refresh_starch_prices,
                country = ctry, source_tag = "manual")
  }
}

# ── Summary ───────────────────────────────────────────────────────────────────

cat(strrep("-", 60), "\n")
total_rows <- sum(vapply(results, function(r) r$rows_upserted %||% 0L, integer(1)))
n_ok   <- sum(vapply(results, function(r) r$status == "ok",      logical(1)))
n_fail <- sum(vapply(results, function(r) r$status != "ok",      logical(1)))

if (args$dry_run) {
  cat("Dry run complete — no changes written to DB.\n")
} else {
  cat(sprintf("Done: %d succeeded, %d failed, %d row(s) upserted total.\n",
              n_ok, n_fail, total_rows))
}

quit(status = if (any_fail) 1L else 0L)
