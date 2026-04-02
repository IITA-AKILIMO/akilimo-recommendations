#!/usr/bin/env Rscript
# tests/test_prices_db.R — unit tests for R/prices_db.R
#
# Run with:
#   Rscript tests/test_prices_db.R
#
# Live API tests are skipped unless TEST_PRICE_API_URL is set.

library(tinytest)

akpath <- Sys.getenv("AKILIMO_ROOT", unset = ".")
setwd(akpath)

# Source dependencies in the same order api.R uses
source(file.path(akpath, "R", "misc.R"))
source(file.path(akpath, "R", "logging.R"))
source(file.path(akpath, "R", "prices_db.R"))

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Create a fresh in-memory (temp-file) database for each test block
new_test_db <- function() {
    db_file <- tempfile(fileext = ".sqlite")
    old <- Sys.getenv("PRICES_DB_PATH", unset = NA_character_)
    Sys.setenv(PRICES_DB_PATH = db_file)
    conn <- open_prices_db()
    list(conn = conn, db_file = db_file, old_env = old)
}

cleanup_test_db <- function(ctx) {
    DBI::dbDisconnect(ctx$conn)
    unlink(ctx$db_file)
    # Restore env var
    if (is.na(ctx$old_env)) {
        Sys.unsetenv("PRICES_DB_PATH")
    } else {
        Sys.setenv(PRICES_DB_PATH = ctx$old_env)
    }
    assign(".prices_db_conn", NULL, envir = globalenv())
    invisible(NULL)
}

# Insert a minimal set of default_prices rows directly
insert_default_prices <- function(conn, rows) {
    now <- strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    for (r in rows) {
        DBI::dbExecute(conn,
            "INSERT OR REPLACE INTO default_prices
                 (country, item, price, unit, currency, updated_at, source)
             VALUES (?, ?, ?, ?, ?, ?, ?)",
            params = list(r$country, r$item, r$price,
                          r$unit %||% "per_bag", r$currency %||% "",
                          r$updated_at %||% now, r$source %||% "test")
        )
    }
}

insert_starch_prices <- function(conn, rows) {
    now <- strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    for (r in rows) {
        DBI::dbExecute(conn,
            "INSERT OR REPLACE INTO starch_prices
                 (starch_factory, starch_factory_label, class, country, key,
                  min_starch, range_starch, price, currency, updated_at, source)
             VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
            params = list(r$starch_factory, r$starch_factory_label %||% "",
                          r$class, r$country, r$key, r$min_starch,
                          r$range_starch %||% "", r$price, r$currency %||% "",
                          r$updated_at %||% now, r$source %||% "test")
        )
    }
}

# ---------------------------------------------------------------------------
# 1. open_prices_db — creates schema and tables
# ---------------------------------------------------------------------------

ctx <- new_test_db()

expect_true(DBI::dbIsValid(ctx$conn),
    info = "open_prices_db returns a valid connection")

tables <- DBI::dbListTables(ctx$conn)
expect_true("default_prices"   %in% tables, info = "default_prices table exists")
expect_true("starch_prices"    %in% tables, info = "starch_prices table exists")
expect_true("price_refresh_log" %in% tables, info = "price_refresh_log table exists")

ver <- DBI::dbGetQuery(ctx$conn, "PRAGMA user_version;")[[1]]
expect_equal(ver, 1L, info = "schema version is 1")

cleanup_test_db(ctx)

# ---------------------------------------------------------------------------
# 2. seed_prices_db — seeds from CSV when they exist
# ---------------------------------------------------------------------------

# We only run this sub-test if the CSV files are present (they are gitignored
# but present on a developer machine that has run setup-data).
dp_csv  <- file.path(akpath, "data", "input", "Default_prices.csv")
sp_csv  <- file.path(akpath, "data", "input", "starchPrices.csv")
has_csv <- file.exists(dp_csv) && file.exists(sp_csv)

if (has_csv) {
    ctx <- new_test_db()

    dp_rows <- DBI::dbGetQuery(ctx$conn, "SELECT COUNT(*) AS n FROM default_prices")$n
    sp_rows <- DBI::dbGetQuery(ctx$conn, "SELECT COUNT(*) AS n FROM starch_prices")$n

    expect_true(dp_rows > 0, info = "default_prices seeded from CSV")
    expect_true(sp_rows > 0, info = "starch_prices seeded from CSV")

    # BU → BI normalisation
    bu_rows <- DBI::dbGetQuery(ctx$conn,
        "SELECT COUNT(*) AS n FROM default_prices WHERE country = 'BU'")$n
    expect_equal(bu_rows, 0L, info = "legacy BU normalised to BI during seed")

    cleanup_test_db(ctx)
} else {
    message("SKIP: CSV data files not present — seed tests skipped")
}

# ---------------------------------------------------------------------------
# 3. get_default_prices — returns correct columns and filters by country
# ---------------------------------------------------------------------------

ctx <- new_test_db()
insert_default_prices(ctx$conn, list(
    list(country = "NG", item = "urea",   price = 7500),
    list(country = "NG", item = "MOP",    price = 13500),
    list(country = "TZ", item = "urea",   price = 4200)
))

ng <- get_default_prices("NG")
expect_equal(nrow(ng), 2L, info = "get_default_prices returns 2 rows for NG")
expect_true(all(c("Country", "Item", "Price") %in% names(ng)),
    info = "get_default_prices columns: Country, Item, Price")
expect_true(all(ng$Country == "NG"), info = "all returned rows are for NG")

tz <- get_default_prices("TZ")
expect_equal(nrow(tz), 1L, info = "get_default_prices returns 1 row for TZ")

cleanup_test_db(ctx)

# ---------------------------------------------------------------------------
# 4. get_starch_prices — returns all rows with correct column names
# ---------------------------------------------------------------------------

ctx <- new_test_db()
insert_starch_prices(ctx$conn, list(
    list(starch_factory = "TestFactory", starch_factory_label = "Test Factory Ltd",
         class = 1L, country = "NG", key = "TestFactory1",
         min_starch = 20, range_starch = ">20", price = 19000),
    list(starch_factory = "TestFactory", starch_factory_label = "Test Factory Ltd",
         class = 2L, country = "NG", key = "TestFactory2",
         min_starch = 18, range_starch = "18-20", price = 17000)
))

sp <- get_starch_prices()
expect_equal(nrow(sp), 2L, info = "get_starch_prices returns 2 rows")
expected_cols <- c("starchFactory", "starchFactory_label", "class", "country",
                   "KEY", "minStarch", "rangeStarch", "price")
expect_true(all(expected_cols %in% names(sp)),
    info = "get_starch_prices returns expected column names")

cleanup_test_db(ctx)

# ---------------------------------------------------------------------------
# 5. prices_are_stale — TRUE when no rows; FALSE when fresh; TRUE when old
# ---------------------------------------------------------------------------

ctx <- new_test_db()

# Empty table → stale
Sys.setenv(PRICE_MAX_AGE_DAYS = "7")
expect_true(prices_are_stale("NG"), info = "prices_are_stale: TRUE when table empty")

# Insert a fresh row
insert_default_prices(ctx$conn, list(
    list(country = "NG", item = "urea", price = 7500,
         updated_at = strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
))
expect_false(prices_are_stale("NG"), info = "prices_are_stale: FALSE for fresh row")

# Insert an old row (8 days ago)
old_ts <- strftime(Sys.time() - 8 * 86400, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
DBI::dbExecute(ctx$conn,
    "UPDATE default_prices SET updated_at = ? WHERE country = 'NG'",
    params = list(old_ts)
)
expect_true(prices_are_stale("NG"), info = "prices_are_stale: TRUE when row is 8 days old")

cleanup_test_db(ctx)

# ---------------------------------------------------------------------------
# 6. starch_prices_are_stale — same logic with 30-day threshold
# ---------------------------------------------------------------------------

ctx <- new_test_db()
Sys.setenv(STARCH_PRICE_MAX_AGE_DAYS = "30")

expect_true(starch_prices_are_stale(), info = "starch_prices_are_stale: TRUE when table empty")

insert_starch_prices(ctx$conn, list(
    list(starch_factory = "F", starch_factory_label = "",
         class = 1L, country = "NG", key = "F1",
         min_starch = 20, price = 19000,
         updated_at = strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
))
expect_false(starch_prices_are_stale(), info = "starch_prices_are_stale: FALSE for fresh row")
expect_false(starch_prices_are_stale("NG"), info = "starch_prices_are_stale: FALSE for NG specifically")

old_ts <- strftime(Sys.time() - 31 * 86400, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
DBI::dbExecute(ctx$conn,
    "UPDATE starch_prices SET updated_at = ?",
    params = list(old_ts)
)
expect_true(starch_prices_are_stale(), info = "starch_prices_are_stale: TRUE when row is 31 days old")

cleanup_test_db(ctx)

# ---------------------------------------------------------------------------
# 7. refresh_prices — skipped when PRICE_API_URL not set
# ---------------------------------------------------------------------------

ctx <- new_test_db()
Sys.unsetenv("PRICE_API_URL")

r <- refresh_prices("NG")
expect_equal(r$status, "skipped", info = "refresh_prices: skipped when PRICE_API_URL unset")

cleanup_test_db(ctx)

# ---------------------------------------------------------------------------
# 8. refresh_starch_prices — skipped when PRICE_API_URL not set
# ---------------------------------------------------------------------------

ctx <- new_test_db()
Sys.unsetenv("PRICE_API_URL")

r <- refresh_starch_prices("NG")
expect_equal(r$status, "skipped",
    info = "refresh_starch_prices: skipped when PRICE_API_URL unset")

cleanup_test_db(ctx)

# ---------------------------------------------------------------------------
# 9. Live API refresh (skipped unless TEST_PRICE_API_URL is set)
# ---------------------------------------------------------------------------

live_url <- Sys.getenv("TEST_PRICE_API_URL", unset = "")
if (nzchar(live_url)) {
    message("Running live API refresh tests against: ", live_url)
    Sys.setenv(PRICE_API_URL = live_url)

    ctx <- new_test_db()

    r <- refresh_prices("NG", source_tag = "test")
    expect_equal(r$status, "ok",      info = "live refresh_prices: status ok")
    expect_true(r$rows_upserted > 0,  info = "live refresh_prices: rows upserted")

    ng <- get_default_prices("NG")
    expect_true(nrow(ng) > 0, info = "live: get_default_prices returns rows after refresh")

    r2 <- refresh_starch_prices("NG", source_tag = "test")
    expect_true(r2$status %in% c("ok", "skipped"),
        info = "live refresh_starch_prices: ok or skipped")

    cleanup_test_db(ctx)
    Sys.unsetenv("PRICE_API_URL")
} else {
    message("SKIP: TEST_PRICE_API_URL not set — live API tests skipped")
}

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

message("\ntest_prices_db.R done.")
