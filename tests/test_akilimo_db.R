#!/usr/bin/env Rscript
# tests/test_akilimo_db.R — unit tests for R/akilimo_db.R
#
# Run with:
#   Rscript tests/test_akilimo_db.R
#
# Live API tests are skipped unless TEST_AKILIMO_API_URL is set.

library(tinytest)

akpath <- Sys.getenv("AKILIMO_ROOT", unset = ".")
setwd(akpath)

# Source dependencies in the same order api.R uses
source(file.path(akpath, "R", "misc.R"))
source(file.path(akpath, "R", "logging.R"))
source(file.path(akpath, "R", "akilimo_db.R"))

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Create a fresh in-memory (temp-file) database for each test block
new_test_db <- function() {
    db_file <- tempfile(fileext = ".sqlite")
    old <- Sys.getenv("AKILIMO_DB_PATH", unset = NA_character_)
    Sys.setenv(AKILIMO_DB_PATH = db_file)
    conn <- open_akilimo_db()
    list(conn = conn, db_file = db_file, old_env = old)
}

cleanup_test_db <- function(ctx) {
    DBI::dbDisconnect(ctx$conn)
    unlink(ctx$db_file)
    # Restore env var
    if (is.na(ctx$old_env)) {
        Sys.unsetenv("AKILIMO_DB_PATH")
    } else {
        Sys.setenv(AKILIMO_DB_PATH = ctx$old_env)
    }
    assign(".akilimo_db_conn", NULL, envir = globalenv())
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
# 1. open_akilimo_db — creates schema and tables
# ---------------------------------------------------------------------------

ctx <- new_test_db()

expect_true(DBI::dbIsValid(ctx$conn),
    info = "open_akilimo_db returns a valid connection")

tables <- DBI::dbListTables(ctx$conn)
expect_true("default_prices"   %in% tables, info = "default_prices table exists")
expect_true("starch_prices"    %in% tables, info = "starch_prices table exists")
expect_true("price_refresh_log" %in% tables, info = "price_refresh_log table exists")
expect_true("translations"     %in% tables, info = "translations table exists")

ver <- DBI::dbGetQuery(ctx$conn, "PRAGMA user_version;")[[1]]
expect_equal(ver, 2L, info = "schema version is 2")

cleanup_test_db(ctx)

# ---------------------------------------------------------------------------
# 2. seed_akilimo_db — seeds from CSV when they exist
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

tr_csv <- file.path(akpath, "data", "input", "translations.csv")
if (file.exists(tr_csv)) {
    ctx <- new_test_db()

    tr_rows <- DBI::dbGetQuery(ctx$conn, "SELECT COUNT(*) AS n FROM translations")$n
    expect_true(tr_rows > 0, info = "translations seeded from CSV")

    # QUA-11: cisRatePre en must be "" (corrected at seed time)
    cis <- DBI::dbGetQuery(ctx$conn,
        "SELECT en FROM translations WHERE key = 'cisRatePre'")
    expect_equal(nrow(cis), 1L, info = "cisRatePre row exists in translations")
    expect_equal(cis$en[1], "", info = "cisRatePre en is empty string after QUA-11 correction")

    cleanup_test_db(ctx)
} else {
    message("SKIP: translations.csv not present — translation seed tests skipped")
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
# 7. refresh_prices — skipped when AKILIMO_API_URL not set
# ---------------------------------------------------------------------------

ctx <- new_test_db()
Sys.unsetenv("AKILIMO_API_URL")

r <- refresh_prices("NG")
expect_equal(r$status, "skipped", info = "refresh_prices: skipped when AKILIMO_API_URL unset")

cleanup_test_db(ctx)

# ---------------------------------------------------------------------------
# 8. refresh_starch_prices — skipped when AKILIMO_API_URL not set
# ---------------------------------------------------------------------------

ctx <- new_test_db()
Sys.unsetenv("AKILIMO_API_URL")

r <- refresh_starch_prices("NG")
expect_equal(r$status, "skipped",
    info = "refresh_starch_prices: skipped when AKILIMO_API_URL unset")

cleanup_test_db(ctx)

# ---------------------------------------------------------------------------
# 9. Live API refresh (skipped unless TEST_AKILIMO_API_URL is set)
# ---------------------------------------------------------------------------

live_url <- Sys.getenv("TEST_AKILIMO_API_URL", unset = "")
if (nzchar(live_url)) {
    message("Running live API refresh tests against: ", live_url)
    Sys.setenv(AKILIMO_API_URL = live_url)

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
    Sys.unsetenv("AKILIMO_API_URL")
} else {
    message("SKIP: TEST_AKILIMO_API_URL not set — live API tests skipped")
}

# ---------------------------------------------------------------------------
# 10. get_translations — returns correct shape
# ---------------------------------------------------------------------------

ctx <- new_test_db()
DBI::dbExecute(ctx$conn,
    "INSERT INTO translations (key, en, sw) VALUES (?, ?, ?)",
    params = list("testKey", "test english", "test swahili"))

tbl <- get_translations()
expect_true(all(c("key", "en", "sw") %in% names(tbl)),
    info = "get_translations returns key, en, sw columns")
row <- tbl[tbl$key == "testKey", ]
expect_equal(nrow(row), 1L, info = "get_translations finds inserted row")
expect_equal(row$en, "test english", info = "get_translations returns correct en value")
expect_equal(row$sw, "test swahili", info = "get_translations returns correct sw value")

cleanup_test_db(ctx)

# ---------------------------------------------------------------------------
# 11. schema migration v1 → v2
# ---------------------------------------------------------------------------

old_db <- tempfile(fileext = ".sqlite")
old_conn <- DBI::dbConnect(RSQLite::SQLite(), old_db)
DBI::dbExecute(old_conn, "PRAGMA journal_mode=WAL;")
# Build a v1 schema manually (3 tables, no translations)
for (ddl in c(
    "CREATE TABLE default_prices (country TEXT, item TEXT, price REAL, unit TEXT,
         currency TEXT, updated_at TEXT, source TEXT, PRIMARY KEY (country, item));",
    "CREATE TABLE starch_prices (starch_factory TEXT, starch_factory_label TEXT,
         class INTEGER, country TEXT, key TEXT PRIMARY KEY, min_starch REAL,
         range_starch TEXT, price REAL, currency TEXT, updated_at TEXT, source TEXT);",
    "CREATE TABLE price_refresh_log (id INTEGER PRIMARY KEY AUTOINCREMENT,
         price_type TEXT, country TEXT, source TEXT, status TEXT,
         rows_upserted INTEGER, message TEXT, ran_at TEXT);",
    "PRAGMA user_version = 1;"
)) DBI::dbExecute(old_conn, ddl)
DBI::dbDisconnect(old_conn)

old_env <- Sys.getenv("AKILIMO_DB_PATH", unset = NA_character_)
Sys.setenv(AKILIMO_DB_PATH = old_db)
conn2 <- open_akilimo_db()

ver2 <- DBI::dbGetQuery(conn2, "PRAGMA user_version;")[[1]]
expect_equal(ver2, 2L, info = "v1→v2 migration sets user_version to 2")

tabs2 <- DBI::dbListTables(conn2)
expect_true("translations" %in% tabs2, info = "translations table created by v1→v2 migration")

DBI::dbDisconnect(conn2)
unlink(old_db)
if (is.na(old_env)) Sys.unsetenv("AKILIMO_DB_PATH") else Sys.setenv(AKILIMO_DB_PATH = old_env)
assign(".akilimo_db_conn", NULL, envir = globalenv())

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

message("\ntest_akilimo_db.R done.")
