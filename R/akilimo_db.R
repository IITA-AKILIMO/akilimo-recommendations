# R/akilimo_db.R — SQLite price store
#
# Provides read/write access to the akilimo_compute.sqlite database.
# Called by get_data.R (reads) and AkilimoMain.R / refresh_prices.R (writes).
#
# Public API:
#   open_akilimo_db()                          → DBI connection
#   seed_akilimo_db(conn)                      → called once when DB is new
#   get_default_prices(country)               → data.frame(Country, Item, Price)
#   get_starch_prices()                       → data.frame matching old CSV columns
#   get_translations()                        → data.frame(key, en, sw, rw)
#   refresh_prices(country, source_tag, dry_run)
#   refresh_starch_prices(country, source_tag, dry_run)
#   prices_are_stale(country)                 → logical
#   starch_prices_are_stale(country = NULL)   → logical
#   translations_are_stale()                  → logical
#   refresh_translations(source_tag, dry_run)  endpoint: GET {AKILIMO_API_URL}/translations (paginated)

VALID_COUNTRIES <- c("NG", "TZ", "RW", "GH", "BI")
VALID_UNITS     <- c("per_bag", "per_kg", "per_tonne", "per_acre", "per_ha")

# Package-level connection handle — set by open_akilimo_db(), used by readers/writers
.akilimo_db_conn <- NULL

# Current schema version — increment whenever DDL changes
.DB_SCHEMA_VERSION <- 6L

# ---------------------------------------------------------------------------
# DDL helpers
# ---------------------------------------------------------------------------

.create_tables <- function(conn) {
    DBI::dbExecute(conn, "
        CREATE TABLE IF NOT EXISTS default_prices (
            country     TEXT    NOT NULL,
            item        TEXT    NOT NULL,
            price       REAL    NOT NULL,
            subsidised_price REAL,
            unit        TEXT    NOT NULL DEFAULT 'per_bag',
            bag_kg      REAL,
            currency    TEXT    NOT NULL DEFAULT '',
            min_price   REAL,
            max_price   REAL,
            updated_at  TEXT    NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
            source      TEXT    NOT NULL DEFAULT 'seed',
            PRIMARY KEY (country, item)
        );
    ")
    DBI::dbExecute(conn, "
        CREATE TABLE IF NOT EXISTS country_price_settings (
            country             TEXT NOT NULL PRIMARY KEY,
            default_price_basis TEXT NOT NULL DEFAULT 'open_market',
            refreshed_at        TEXT,
            api_cursor          TEXT,
            updated_at          TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now'))
        );
    ")
    DBI::dbExecute(conn, "
        CREATE TABLE IF NOT EXISTS starch_prices (
            starch_factory       TEXT    NOT NULL,
            starch_factory_label TEXT    NOT NULL DEFAULT '',
            class                INTEGER NOT NULL,
            country              TEXT    NOT NULL,
            key                  TEXT    NOT NULL,
            min_starch           REAL    NOT NULL,
            range_starch         TEXT    NOT NULL DEFAULT '',
            price                REAL    NOT NULL,
            currency             TEXT    NOT NULL DEFAULT '',
            updated_at           TEXT    NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
            source               TEXT    NOT NULL DEFAULT 'seed',
            PRIMARY KEY (key)
        );
    ")
    DBI::dbExecute(conn, "
        CREATE TABLE IF NOT EXISTS price_refresh_log (
            id            INTEGER PRIMARY KEY AUTOINCREMENT,
            price_type    TEXT    NOT NULL,
            country       TEXT,
            source        TEXT    NOT NULL,
            status        TEXT    NOT NULL,
            rows_upserted INTEGER,
            message       TEXT,
            ran_at        TEXT    NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now'))
        );
    ")
    DBI::dbExecute(conn, "
        CREATE TABLE IF NOT EXISTS translations (
            key        TEXT NOT NULL PRIMARY KEY,
            en         TEXT NOT NULL DEFAULT '',
            sw         TEXT NOT NULL DEFAULT '',
            rw         TEXT NOT NULL DEFAULT '',
            updated_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
            source     TEXT NOT NULL DEFAULT 'seed'
        );
    ")
    DBI::dbExecute(conn,
        paste0("PRAGMA user_version = ", .DB_SCHEMA_VERSION, ";"))
    invisible(NULL)
}

# ---------------------------------------------------------------------------
# Migration (run when user_version < .DB_SCHEMA_VERSION)
# ---------------------------------------------------------------------------

migrate_akilimo_db <- function(conn, current_version) {
    # Migrations are applied in order. Each entry: list(sql = character vector, seed_fn = function|NULL).
    migrations <- list(
        # version 1 — initial schema; handled by .create_tables above
        "1" = list(sql = character(0), seed_fn = NULL),
        # version 2 — add translations table (filled by sync_seed_data() after
        # the migrations, like every other table)
        "2" = list(
            sql = c("CREATE TABLE IF NOT EXISTS translations (
                         key        TEXT NOT NULL PRIMARY KEY,
                         en         TEXT NOT NULL DEFAULT '',
                         sw         TEXT NOT NULL DEFAULT '',
                         updated_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now'))
                     );"),
            seed_fn = NULL
        ),
        # version 3 — add rw (Kinyarwanda) column to translations
        "3" = list(
            sql = c("ALTER TABLE translations ADD COLUMN rw TEXT NOT NULL DEFAULT '';"),
            seed_fn = NULL
        ),
        # version 4 — track where each translation came from, so seed updates
        # never overwrite rows refreshed from the API
        "4" = list(
            sql = c("ALTER TABLE translations ADD COLUMN source TEXT NOT NULL DEFAULT 'seed';"),
            seed_fn = NULL
        ),
        # version 5 — persist price catalogue metadata and each country's
        # default price basis while keeping the legacy price columns intact.
        "5" = list(
            sql = c(
                "ALTER TABLE default_prices ADD COLUMN subsidised_price REAL;",
                "ALTER TABLE default_prices ADD COLUMN bag_kg REAL;",
                "ALTER TABLE default_prices ADD COLUMN min_price REAL;",
                "ALTER TABLE default_prices ADD COLUMN max_price REAL;",
                "CREATE TABLE IF NOT EXISTS country_price_settings (
                    country TEXT NOT NULL PRIMARY KEY,
                    default_price_basis TEXT NOT NULL DEFAULT 'open_market',
                    updated_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now'))
                );"
            ),
            seed_fn = NULL
        ),
        # version 6 — per-country sync state. refreshed_at is the last successful
        # refresh (drives staleness even when nothing changed); api_cursor is the newest
        # price-service updated_at seen, sent back verbatim as updated_since.
        "6" = list(
            sql = c(
                "ALTER TABLE country_price_settings ADD COLUMN refreshed_at TEXT;",
                "ALTER TABLE country_price_settings ADD COLUMN api_cursor TEXT;"
            ),
            seed_fn = NULL
        )
    )

    for (v in seq(current_version + 1L, .DB_SCHEMA_VERSION)) {
        key <- as.character(v)
        if (!is.null(migrations[[key]])) {
            for (sql in migrations[[key]]$sql) {
                DBI::dbExecute(conn, sql)
            }
            if (!is.null(migrations[[key]]$seed_fn)) {
                migrations[[key]]$seed_fn(conn)
            }
        }
        DBI::dbExecute(conn,
            paste0("PRAGMA user_version = ", v, ";"))
        log_write("INFO", "akilimo_db: migrated schema to version", v)
    }
    invisible(NULL)
}

# ---------------------------------------------------------------------------
# open_akilimo_db()
# ---------------------------------------------------------------------------

open_akilimo_db <- function() {
    db_path <- Sys.getenv("AKILIMO_DB_PATH", unset = "data/input/akilimo_compute.sqlite")

    # Resolve relative paths from AKILIMO_ROOT
    if (!grepl("^(/|[A-Za-z]:)", db_path)) {
        akpath <- Sys.getenv("AKILIMO_ROOT", unset = ".")
        db_path <- file.path(akpath, db_path)
    }

    is_new <- !file.exists(db_path)

    # Ensure parent directory exists
    dir.create(dirname(db_path), showWarnings = FALSE, recursive = TRUE)

    conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)

    # WAL mode — allows concurrent reads during a write
    DBI::dbExecute(conn, "PRAGMA journal_mode=WAL;")

    if (is_new) {
        log_write("INFO", "akilimo_db: new database — creating schema and seeding from CSV")
        .create_tables(conn)
    } else {
        # Check schema version and migrate if behind
        ver <- DBI::dbGetQuery(conn, "PRAGMA user_version;")[[1]]
        if (ver < .DB_SCHEMA_VERSION) {
            log_write("INFO", "akilimo_db: schema version", ver,
                      "→ migrating to", .DB_SCHEMA_VERSION)
            migrate_akilimo_db(conn, ver)
        }
    }

    # Bring the seed CSVs into the database on every start: new rows are added
    # and seed rows pick up CSV edits, but rows refreshed from the API are kept.
    # So the database survives deploys and restarts and never needs deleting.
    sync_seed_data(conn)

    # Store globally so readers/writers can use it without re-opening
    assign(".akilimo_db_conn", conn, envir = globalenv())
    log_write("INFO", "akilimo_db: opened", db_path)
    invisible(conn)
}

# ---------------------------------------------------------------------------
# .seed_translations(conn)  — internal helper; called from seed_akilimo_db()
#                             and from the v2 migration entry
# ---------------------------------------------------------------------------

.seed_translations <- function(conn) {
    akpath <- Sys.getenv("AKILIMO_ROOT", unset = ".")
    tr_path <- file.path(akpath, "data", "input", "translations.csv")
    if (!file.exists(tr_path)) {
        log_write("WARN", "akilimo_db: translations.csv not found — translations table empty")
        return(invisible(NULL))
    }
    tr_raw <- read.csv(tr_path, stringsAsFactors = FALSE,
                       strip.white = FALSE, na.strings = character(0))
    # QUA-11: cisRatePre 'en' must be "" (no unit prefix). Fix at seed time so
    # the DB never stores the bad value. The runtime guard in get_data.R is removed.
    idx <- which(tr_raw$key == "cisRatePre")
    if (length(idx) == 1 && !is.na(tr_raw$en[idx]) && nzchar(tr_raw$en[idx])) {
        log_write("WARN", "akilimo_db: seed — cisRatePre 'en' corrected from",
                  shQuote(tr_raw$en[idx]), "to \"\"")
        tr_raw$en[idx] <- ""
    }
    rows <- data.frame(
        key        = tr_raw$key,
        en         = tr_raw$en,
        sw         = if (!is.null(tr_raw$sw)) tr_raw$sw else rep("", nrow(tr_raw)),
        rw         = if (!is.null(tr_raw$rw)) tr_raw$rw else rep("", nrow(tr_raw)),
        updated_at = strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        source     = "seed",
        stringsAsFactors = FALSE
    )
    # An empty language column (e.g. rw before it is translated) reads as NA;
    # the table stores blanks as '' and tr() falls back to English
    for (col in c("en", "sw", "rw")) {
        v <- as.character(rows[[col]])
        v[is.na(v)] <- ""
        rows[[col]] <- v
    }
    .sync_seed_rows(conn, "translations", rows, key_cols = "key")
    invisible(NULL)
}


# ---------------------------------------------------------------------------
# .sync_seed_rows(conn, table, rows, key_cols)
# Adds seed rows whose key is missing, and updates rows that still come from
# the seed (source = 'seed') when the CSV value changed. Rows written by a
# refresh (any other source) are never touched. Columns the table lacks are
# dropped, so it works at any schema version.
# ---------------------------------------------------------------------------

.sync_seed_rows <- function(conn, table, rows, key_cols) {
    fields <- DBI::dbListFields(conn, table)
    rows <- rows[, intersect(names(rows), fields), drop = FALSE]
    if (nrow(rows) == 0) return(invisible(c(added = 0L, updated = 0L)))
    # Keep the first row per key, as the old plain INSERT would have failed on dupes
    rows <- rows[!duplicated(rows[, key_cols, drop = FALSE]), , drop = FALSE]

    tmp <- paste0("seed_", table)
    DBI::dbWriteTable(conn, tmp, rows, temporary = TRUE, overwrite = TRUE)
    on.exit(DBI::dbExecute(conn, sprintf("DROP TABLE IF EXISTS temp.%s;", tmp)), add = TRUE)

    cols     <- names(rows)
    col_list <- paste(cols, collapse = ", ")
    added <- DBI::dbExecute(conn, sprintf(
        "INSERT OR IGNORE INTO %s (%s) SELECT %s FROM temp.%s;", table, col_list, col_list, tmp))

    updated <- 0L
    value_cols <- setdiff(cols, c(key_cols, "updated_at", "source"))
    if ("source" %in% fields && length(value_cols) > 0) {
        match   <- paste(sprintf("s.%s = %s.%s", key_cols, table, key_cols), collapse = " AND ")
        changed <- paste(sprintf("%s.%s IS NOT s.%s", table, value_cols, value_cols), collapse = " OR ")
        set     <- paste(sprintf("%s = (SELECT s.%s FROM temp.%s s WHERE %s)",
                                 value_cols, value_cols, tmp, match), collapse = ", ")
        updated <- DBI::dbExecute(conn, sprintf(
            "UPDATE %s SET %s, updated_at = strftime('%%Y-%%m-%%dT%%H:%%M:%%SZ','now')
             WHERE source = 'seed' AND EXISTS (SELECT 1 FROM temp.%s s WHERE %s AND (%s));",
            table, set, tmp, match, changed))
    }

    if (added > 0 || updated > 0)
        log_write("INFO", sprintf("akilimo_db: %s from seed — %d added, %d updated",
                                  table, added, updated))
    invisible(c(added = as.integer(added), updated = as.integer(updated)))
}


# ---------------------------------------------------------------------------
# sync_seed_data(conn)  — run on every start (see open_akilimo_db())
# seed_akilimo_db() is the old name, kept for callers and tests.
# ---------------------------------------------------------------------------

seed_akilimo_db <- function(conn) sync_seed_data(conn)

sync_seed_data <- function(conn) {
    akpath <- Sys.getenv("AKILIMO_ROOT", unset = ".")

    # ── default_prices ───────────────────────────────────────────────────────
    dp_path <- file.path(akpath, "data", "input", "Default_prices.csv")
    if (file.exists(dp_path)) {
        dp <- read.csv(dp_path, stringsAsFactors = FALSE)
        # Legacy CSV uses "BU" for Burundi; normalise to "BI"
        dp$Country[dp$Country == "BU"] <- "BI"

        rows <- data.frame(
            country    = dp$Country,
            item       = dp$Item,
            price      = as.numeric(dp$Price),
            unit       = "per_bag",
            currency   = "",
            updated_at = strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
            source     = "seed",
            stringsAsFactors = FALSE
        )
        .sync_seed_rows(conn, "default_prices", rows, key_cols = c("country", "item"))
    } else {
        log_write("WARN", "akilimo_db: Default_prices.csv not found — default_prices table empty")
    }

    # ── starch_prices ────────────────────────────────────────────────────────
    sp_path <- file.path(akpath, "data", "input", "starchPrices.csv")
    if (file.exists(sp_path)) {
        sp <- read.csv(sp_path, stringsAsFactors = FALSE)
        rows <- data.frame(
            starch_factory       = sp$starchFactory,
            starch_factory_label = sp$starchFactory_label,
            class                = as.integer(sp$class),
            country              = sp$country,
            key                  = sp$KEY,
            min_starch           = as.numeric(sp$minStarch),
            range_starch         = sp$rangeStarch,
            price                = as.numeric(sp$price),
            # Optional column; older copies of the CSV have none
            currency             = if ("currency" %in% names(sp)) ifelse(is.na(sp$currency), "", sp$currency) else "",
            updated_at           = strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
            source               = "seed",
            stringsAsFactors = FALSE
        )
        .sync_seed_rows(conn, "starch_prices", rows, key_cols = "key")
    } else {
        log_write("WARN", "akilimo_db: starchPrices.csv not found — starch_prices table empty")
    }

    # ── translations ─────────────────────────────────────────────────────────
    .seed_translations(conn)

    invisible(NULL)
}

# ---------------------------------------------------------------------------
# .assert_db_conn()
# Internal: returns the connection, stopping with a clear diagnostic if it
# is NULL (never opened) or no longer valid (OS closed the handle, SQLite
# file deleted, etc.).  Use at the top of every public reader function.
# ---------------------------------------------------------------------------

.assert_db_conn <- function() {
    conn <- .akilimo_db_conn
    if (is.null(conn))
        stop("akilimo_db: connection not open — call open_akilimo_db() first")
    if (!DBI::dbIsValid(conn))
        stop("akilimo_db: connection is no longer valid — restart the server to reconnect")
    conn
}

# ---------------------------------------------------------------------------
# get_default_prices(country)
# Returns data.frame with columns Country, Item, Price  (matching old CSV)
# ---------------------------------------------------------------------------

get_default_prices <- function(country = NULL) {
    conn <- .assert_db_conn()

    # NULL country returns all rows — matches old CSV behaviour where callers
    # (e.g. fertilizers.R) load the full table and filter themselves.
    tryCatch({
        if (is.null(country)) {
            DBI::dbGetQuery(conn,
                "SELECT country AS Country, item AS Item, price AS Price
                 FROM default_prices"
            )
        } else {
            DBI::dbGetQuery(conn,
                "SELECT country AS Country, item AS Item, price AS Price
                 FROM default_prices
                 WHERE country = ?",
                params = list(country)
            )
        }
    }, error = function(e) {
        log_write("ERROR", "akilimo_db: get_default_prices query failed:", conditionMessage(e))
        stop(conditionMessage(e))
    })
}

# ---------------------------------------------------------------------------
# get_catalogue_price(country, item, subsidised = NULL)
# Returns the price and metadata for one catalogue item. When subsidised is
# NULL, the country's stored default_price_basis selects the applicable price.
# ---------------------------------------------------------------------------

get_catalogue_price <- function(country, item, subsidised = NULL) {
    conn <- .assert_db_conn()
    row <- DBI::dbGetQuery(conn,
        "SELECT price, subsidised_price, unit, bag_kg, currency, min_price, max_price
         FROM default_prices
         WHERE country = ? AND lower(item) = lower(?)
         LIMIT 1",
        params = list(country, item)
    )
    if (nrow(row) == 0) return(NULL)

    basis <- DBI::dbGetQuery(conn,
        "SELECT default_price_basis FROM country_price_settings WHERE country = ?",
        params = list(country)
    )$default_price_basis
    use_subsidised <- if (is.null(subsidised)) {
        length(basis) == 1 && identical(basis[[1]], "subsidised")
    } else {
        isTRUE(subsidised)
    }
    if (use_subsidised && !is.na(row$subsidised_price[[1]]) && row$subsidised_price[[1]] > 0)
        row$selected_price <- row$subsidised_price
    else
        row$selected_price <- row$price
    row
}

# ---------------------------------------------------------------------------
# get_starch_prices()
# Returns data.frame with columns matching old starchPrices.csv
# ---------------------------------------------------------------------------

get_starch_prices <- function() {
    conn <- .assert_db_conn()

    tryCatch(
        DBI::dbGetQuery(conn,
            "SELECT starch_factory        AS starchFactory,
                    starch_factory_label  AS starchFactory_label,
                    class,
                    country,
                    key                   AS KEY,
                    min_starch            AS minStarch,
                    range_starch          AS rangeStarch,
                    price
             FROM starch_prices
             ORDER BY country, starch_factory, class"
        ),
        error = function(e) {
            log_write("ERROR", "akilimo_db: get_starch_prices query failed:", conditionMessage(e))
            stop(conditionMessage(e))
        }
    )
}

# ---------------------------------------------------------------------------
# get_translations()
# Returns data.frame with columns key, en, sw  (same shape as translations.csv)
# ---------------------------------------------------------------------------

get_translations <- function() {
    conn <- .assert_db_conn()
    tryCatch(
        DBI::dbGetQuery(conn, "SELECT key, en, sw, rw FROM translations ORDER BY key"),
        error = function(e) {
            log_write("ERROR", "akilimo_db: get_translations query failed:", conditionMessage(e))
            stop(conditionMessage(e))
        }
    )
}


# ---------------------------------------------------------------------------
# Internal: write a row to price_refresh_log
# ---------------------------------------------------------------------------

.log_refresh <- function(conn, price_type, country, source_tag,
                          status, rows_upserted = 0L, message = NULL) {
    DBI::dbExecute(conn,
        "INSERT INTO price_refresh_log
             (price_type, country, source, status, rows_upserted, message)
         VALUES (?, ?, ?, ?, ?, ?)",
        params = list(
            price_type,
            country %||% NA_character_,
            source_tag,
            status,
            as.integer(rows_upserted),
            message %||% NA_character_
        )
    )
    invisible(NULL)
}

# ---------------------------------------------------------------------------
# refresh_prices(country, source_tag, dry_run)
# ---------------------------------------------------------------------------

refresh_prices <- function(country = NULL,
                           source_tag = "api",
                           dry_run    = FALSE) {
    conn    <- .akilimo_db_conn
    api_url <- sub("/+$", "", Sys.getenv("AKILIMO_API_URL", unset = ""))

    if (!nzchar(api_url)) {
        log_write("DEBUG", "akilimo_db: AKILIMO_API_URL not set — skipping default price refresh")
        return(list(status = "skipped", rows_upserted = 0L, message = "AKILIMO_API_URL not set"))
    }

    countries <- if (is.null(country)) VALID_COUNTRIES else country

    total_rows <- 0L
    any_fail   <- FALSE
    fail_msgs  <- character(0)

    for (ctry in countries) {
        result <- .refresh_default_one(conn, api_url, ctry, source_tag, dry_run)
        total_rows <- total_rows + result$rows_upserted
        if (result$status != "ok") {
            any_fail  <- TRUE
            fail_msgs <- c(fail_msgs, result$message %||% ctry)
        }

        if (!dry_run) {
            .log_refresh(conn, "default", ctry, source_tag,
                         result$status, result$rows_upserted, result$message)
        }

        # Invalidate in-memory cache so next get_data() call re-queries the DB
        if (result$status == "ok" && exists(".data_cache", envir = globalenv())) {
            cache <- get(".data_cache", envir = globalenv())
            if (exists("default_prices", envir = cache, inherits = FALSE))
                rm("default_prices", envir = cache)
        }
    }

    list(
        status        = if (any_fail) "error" else "ok",
        rows_upserted = total_rows,
        message       = if (any_fail) paste(fail_msgs, collapse = "; ") else NULL
    )
}

.refresh_default_one <- function(conn, api_url, country, source_tag, dry_run) {
    token    <- Sys.getenv("AKILIMO_API_TOKEN", unset = "")
    now_str  <- strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    valid    <- list()
    page_num <- 1L
    # The cursor is the price service's own timestamp, so updated_since compares like with like
    last_sync <- DBI::dbGetQuery(conn,
        "SELECT api_cursor FROM country_price_settings WHERE country = ?",
        params = list(country)
    )$api_cursor
    last_sync <- if (length(last_sync) == 1) last_sync[[1]] else NA_character_
    updated_since <- if (!is.na(last_sync) && nzchar(last_sync)) paste0("&updated_since=", utils::URLencode(last_sync, reserved = TRUE)) else ""
    page_url <- paste0(api_url, "/prices?country=", country, updated_since, "&page=1")
    price_basis <- NULL
    api_cursor  <- NA_character_

    repeat {
        log_write("DEBUG", sprintf("akilimo_db: fetching default prices %s page %d", country, page_num))

        fetch_err <- NULL
        resp <- tryCatch({
            raw <- httr::GET(page_url,
                if (nzchar(token)) httr::add_headers(Authorization = paste("Bearer", token)),
                httr::timeout(10)
            )
            httr::stop_for_status(raw, task = paste("GET", page_url))
            httr::content(raw, as = "parsed", type = "application/json")
        }, error = function(e) {
            fetch_err <<- conditionMessage(e)
            log_write("WARN", "akilimo_db: HTTP error fetching default prices for", country, ":", fetch_err)
            NULL
        })

        if (is.null(resp)) {
            if (page_num == 1L)
                return(list(status = "error", rows_upserted = 0L,
                            message = paste0("error fetching prices for ", country, ": ", fetch_err)))
            log_write("WARN", sprintf("akilimo_db: network error on prices %s page %d — using %d rows so far",
                                      country, page_num, length(valid)))
            break
        }

        if (inherits(resp, "list") && isTRUE(resp$status %in% c("error", "skipped")))
            return(resp)

        records <- resp$data
        if (is.null(records)) {
            if (page_num == 1L)
                return(list(status = "error", rows_upserted = 0L,
                            message = paste("prices API response missing 'data' field for", country)))
            log_write("WARN", sprintf("akilimo_db: missing 'data' on prices %s page %d — stopping early",
                                      country, page_num))
            break
        }

        for (row in records) {
            ctry  <- row$country %||% ""
            item  <- row$item    %||% ""
            price <- suppressWarnings(as.numeric(row$price))
            unit  <- row$unit    %||% "per_bag"

            if (!ctry  %in% VALID_COUNTRIES) { log_write("WARN", "akilimo_db: invalid country", ctry,  "— skipping row"); next }
            if (!nzchar(item))               { log_write("WARN", "akilimo_db: empty item — skipping row"); next }
            if (is.na(price) || price <= 0)  { log_write("WARN", "akilimo_db: invalid price for", item, "— skipping row"); next }
            if (!unit %in% VALID_UNITS)      { log_write("WARN", "akilimo_db: invalid unit", unit, "for", item, "— skipping row"); next }

            valid[[length(valid) + 1L]] <- list(
                country    = ctry,
                item       = item,
                price      = price,
                subsidised_price = suppressWarnings(as.numeric(row$subsidised_price %||% NA_real_)),
                unit       = unit,
                bag_kg     = suppressWarnings(as.numeric(row$bag_kg %||% NA_real_)),
                currency   = row$currency %||% "",
                min_price  = suppressWarnings(as.numeric(row$min_price %||% NA_real_)),
                max_price  = suppressWarnings(as.numeric(row$max_price %||% NA_real_)),
                # Local sync time, in the format prices_are_stale() parses
                updated_at = now_str,
                source     = source_tag
            )
            row_ts <- row$updated_at %||% ""
            if (nzchar(row_ts) && (is.na(api_cursor) || row_ts > api_cursor)) api_cursor <- row_ts
        }

        if (!is.null(resp$default_price_basis)) price_basis <- resp$default_price_basis

        log_write("DEBUG", sprintf("akilimo_db: prices %s page %d — %d record(s), total so far: %d",
                                   country, page_num, length(records), length(valid)))

        next_url <- resp$links[["next"]] %||% ""
        if (!nzchar(next_url)) break
        page_url <- next_url
        page_num <- page_num + 1L
    }

    if (length(valid) == 0 && is.na(last_sync)) {
        msg <- paste("no valid rows returned for", country)
        log_write("WARN", "akilimo_db:", msg)
        return(list(status = "error", rows_upserted = 0L, message = msg))
    }

    if (dry_run) {
        log_write("INFO", sprintf("akilimo_db [dry-run] default/%s: %d valid row(s)", country, length(valid)))
        return(list(status = "ok", rows_upserted = length(valid), message = "dry-run"))
    }

    tryCatch({
        for (v in valid) {
            DBI::dbExecute(conn,
                "INSERT OR REPLACE INTO default_prices
                     (country, item, price, subsidised_price, unit, bag_kg, currency,
                      min_price, max_price, updated_at, source)
                 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
                params = list(v$country, v$item, v$price, v$subsidised_price, v$unit,
                              v$bag_kg, v$currency, v$min_price, v$max_price,
                              v$updated_at, v$source)
            )
        }
        basis <- if (!is.null(price_basis) && price_basis %in% c("open_market", "subsidised")) price_basis else NA_character_
        DBI::dbExecute(conn,
            "INSERT OR IGNORE INTO country_price_settings (country) VALUES (?)",
            params = list(country)
        )
        # A refresh that brought nothing new still counts: it resets staleness
        DBI::dbExecute(conn,
            "UPDATE country_price_settings
                SET default_price_basis = COALESCE(?, default_price_basis),
                    api_cursor          = COALESCE(?, api_cursor),
                    refreshed_at        = ?,
                    updated_at          = ?
              WHERE country = ?",
            params = list(basis, api_cursor, now_str, now_str, country)
        )
        list(status = "ok", rows_upserted = length(valid), message = NULL)
    }, error = function(e) {
        log_write("WARN", "akilimo_db: DB write error for default/", country, ":", conditionMessage(e))
        list(status = "error", rows_upserted = 0L, message = conditionMessage(e))
    })
}

# ---------------------------------------------------------------------------
# refresh_starch_prices(country, source_tag, dry_run)
# ---------------------------------------------------------------------------

refresh_starch_prices <- function(country    = NULL,
                                  source_tag = "api",
                                  dry_run    = FALSE) {
    conn    <- .akilimo_db_conn
    api_url <- sub("/+$", "", Sys.getenv("AKILIMO_API_URL", unset = ""))

    if (!nzchar(api_url)) {
        log_write("DEBUG", "akilimo_db: AKILIMO_API_URL not set — skipping starch price refresh")
        return(list(status = "skipped", rows_upserted = 0L, message = "AKILIMO_API_URL not set"))
    }

    countries  <- if (is.null(country)) VALID_COUNTRIES else country
    total_rows <- 0L
    any_fail   <- FALSE
    fail_msgs  <- character(0)

    for (ctry in countries) {
        result <- .refresh_starch_one(conn, api_url, ctry, source_tag, dry_run)
        total_rows <- total_rows + result$rows_upserted
        # "skipped" = no starch factories listed for this country; not a failure
        if (result$status == "error") {
            any_fail  <- TRUE
            fail_msgs <- c(fail_msgs, result$message %||% ctry)
        }

        if (!dry_run) {
            .log_refresh(conn, "starch", ctry, source_tag,
                         result$status, result$rows_upserted, result$message)
        }

        if (result$status == "ok" && exists(".data_cache", envir = globalenv())) {
            cache <- get(".data_cache", envir = globalenv())
            if (exists("starch_prices", envir = cache, inherits = FALSE))
                rm("starch_prices", envir = cache)
        }
    }

    list(
        status        = if (any_fail) "error" else "ok",
        rows_upserted = total_rows,
        message       = if (any_fail) paste(fail_msgs, collapse = "; ") else NULL
    )
}

.refresh_starch_one <- function(conn, api_url, country, source_tag, dry_run) {
    token     <- Sys.getenv("AKILIMO_API_TOKEN", unset = "")
    now_str   <- strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    valid     <- list()
    seen_keys <- character(0)
    page_num  <- 1L
    page_url  <- paste0(api_url, "/starch-prices?country=", country, "&page=1")

    repeat {
        log_write("DEBUG", sprintf("akilimo_db: fetching starch prices %s page %d", country, page_num))

        fetch_err <- NULL
        resp <- tryCatch({
            raw <- httr::GET(page_url,
                if (nzchar(token)) httr::add_headers(Authorization = paste("Bearer", token)),
                httr::timeout(10)
            )
            httr::stop_for_status(raw, task = paste("GET", page_url))
            httr::content(raw, as = "parsed", type = "application/json")
        }, error = function(e) {
            fetch_err <<- conditionMessage(e)
            log_write("WARN", "akilimo_db: HTTP error fetching starch prices for", country, ":", fetch_err)
            NULL
        })

        if (is.null(resp)) {
            if (page_num == 1L)
                return(list(status = "error", rows_upserted = 0L,
                            message = paste0("error fetching starch prices for ", country, ": ", fetch_err)))
            log_write("WARN", sprintf("akilimo_db: network error on starch prices %s page %d — using %d rows so far",
                                      country, page_num, length(valid)))
            break
        }

        if (inherits(resp, "list") && isTRUE(resp$status %in% c("error", "skipped")))
            return(resp)

        records <- resp$data
        if (is.null(records)) {
            if (page_num == 1L)
                return(list(status = "error", rows_upserted = 0L,
                            message = paste("starch prices API response missing 'data' field for", country)))
            log_write("WARN", sprintf("akilimo_db: missing 'data' on starch prices %s page %d — stopping early",
                                      country, page_num))
            break
        }

        for (row in records) {
            factory   <- row$starch_factory %||% ""
            key       <- row$key            %||% ""
            cls       <- suppressWarnings(as.integer(row$class))
            ctry      <- row$country        %||% ""
            minstarch <- suppressWarnings(as.numeric(row$min_starch))
            price     <- suppressWarnings(as.numeric(row$price))

            if (!ctry %in% VALID_COUNTRIES)        { log_write("WARN", "akilimo_db: invalid country", ctry, "— skipping starch row"); next }
            if (!nzchar(factory))                  { log_write("WARN", "akilimo_db: empty starch_factory — skipping row"); next }
            if (!nzchar(key))                      { log_write("WARN", "akilimo_db: empty key — skipping starch row"); next }
            if (key %in% seen_keys)                { log_write("WARN", "akilimo_db: duplicate key", key, "— skipping row"); next }
            if (is.na(cls) || cls < 1L)            { log_write("WARN", "akilimo_db: invalid class for", key, "— skipping row"); next }
            if (is.na(minstarch) || minstarch < 0) { log_write("WARN", "akilimo_db: invalid min_starch for", key, "— skipping row"); next }
            if (is.na(price) || price <= 0)        { log_write("WARN", "akilimo_db: invalid price for", key, "— skipping row"); next }

            seen_keys <- c(seen_keys, key)
            valid[[length(valid) + 1L]] <- list(
                starch_factory       = factory,
                starch_factory_label = row$starch_factory_label %||% "",
                class                = cls,
                country              = ctry,
                key                  = key,
                min_starch           = minstarch,
                range_starch         = row$range_starch %||% "",
                price                = price,
                currency             = row$currency %||% "",
                updated_at           = now_str,
                source               = source_tag
            )
        }

        log_write("DEBUG", sprintf("akilimo_db: starch prices %s page %d — %d record(s), total so far: %d",
                                   country, page_num, length(records), length(valid)))

        next_url <- resp$links[["next"]] %||% ""
        if (!nzchar(next_url)) break
        page_url <- next_url
        page_num <- page_num + 1L
    }

    if (length(valid) == 0) {
        msg <- paste("no valid rows returned for starch/", country, "— preserving existing data")
        log_write("WARN", "akilimo_db:", msg)
        return(list(status = "skipped", rows_upserted = 0L, message = msg))
    }

    if (dry_run) {
        log_write("INFO", sprintf("akilimo_db [dry-run] starch/%s: %d valid row(s)", country, length(valid)))
        return(list(status = "ok", rows_upserted = length(valid), message = "dry-run"))
    }

    tryCatch({
        DBI::dbWithTransaction(conn, {
            DBI::dbExecute(conn,
                "DELETE FROM starch_prices WHERE country = ?",
                params = list(country)
            )
            for (v in valid) {
                DBI::dbExecute(conn,
                    "INSERT INTO starch_prices
                         (starch_factory, starch_factory_label, class, country, key,
                          min_starch, range_starch, price, currency, updated_at, source)
                     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
                    params = list(v$starch_factory, v$starch_factory_label, v$class,
                                  v$country, v$key, v$min_starch, v$range_starch,
                                  v$price, v$currency, v$updated_at, v$source)
                )
            }
        })
        list(status = "ok", rows_upserted = length(valid), message = NULL)
    }, error = function(e) {
        log_write("WARN", "akilimo_db: transaction error for starch/", country, ":", conditionMessage(e))
        list(status = "error", rows_upserted = 0L, message = conditionMessage(e))
    })
}

# ---------------------------------------------------------------------------
# prices_are_stale(country)
# ---------------------------------------------------------------------------

prices_are_stale <- function(country) {
    conn <- .akilimo_db_conn
    if (is.null(conn)) return(FALSE)

    max_age_days <- suppressWarnings(
        as.numeric(Sys.getenv("PRICE_MAX_AGE_DAYS", unset = "7"))
    )
    if (is.na(max_age_days) || max_age_days <= 0) max_age_days <- 7

    # After a catalogue sync, age is the last successful refresh: incremental syncs
    # only touch changed rows, so the oldest row would otherwise stay "stale" forever
    refreshed <- DBI::dbGetQuery(conn,
        "SELECT refreshed_at FROM country_price_settings WHERE country = ?",
        params = list(country)
    )$refreshed_at
    if (length(refreshed) == 1 && !is.na(refreshed[[1]])) {
        at <- as.POSIXct(refreshed[[1]], format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        if (!is.na(at)) return(difftime(Sys.time(), at, units = "days") > max_age_days)
    }

    row <- DBI::dbGetQuery(conn,
        "SELECT MIN(updated_at) AS oldest FROM default_prices WHERE country = ?",
        params = list(country)
    )

    if (nrow(row) == 0 || is.na(row$oldest[1])) return(TRUE)

    oldest <- tryCatch(
        as.POSIXct(row$oldest[1], format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        error = function(e) NULL
    )
    if (is.null(oldest) || is.na(oldest)) return(TRUE)

    difftime(Sys.time(), oldest, units = "days") > max_age_days
}

# ---------------------------------------------------------------------------
# starch_prices_are_stale(country = NULL)
# ---------------------------------------------------------------------------

starch_prices_are_stale <- function(country = NULL) {
    conn <- .akilimo_db_conn
    if (is.null(conn)) return(FALSE)

    max_age_days <- suppressWarnings(
        as.numeric(Sys.getenv("STARCH_PRICE_MAX_AGE_DAYS", unset = "30"))
    )
    if (is.na(max_age_days) || max_age_days <= 0) max_age_days <- 30

    if (is.null(country)) {
        row <- DBI::dbGetQuery(conn,
            "SELECT MIN(updated_at) AS oldest FROM starch_prices"
        )
    } else {
        row <- DBI::dbGetQuery(conn,
            "SELECT MIN(updated_at) AS oldest FROM starch_prices WHERE country = ?",
            params = list(country)
        )
    }

    if (nrow(row) == 0 || is.na(row$oldest[1])) return(TRUE)

    oldest <- tryCatch(
        as.POSIXct(row$oldest[1], format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        error = function(e) NULL
    )
    if (is.null(oldest) || is.na(oldest)) return(TRUE)

    difftime(Sys.time(), oldest, units = "days") > max_age_days
}

# ---------------------------------------------------------------------------
# translations_are_stale()
# ---------------------------------------------------------------------------

translations_are_stale <- function() {
    conn <- .akilimo_db_conn
    if (is.null(conn)) return(FALSE)

    max_age_days <- suppressWarnings(
        as.numeric(Sys.getenv("TRANSLATIONS_MAX_AGE_DAYS", unset = "30"))
    )
    if (is.na(max_age_days) || max_age_days <= 0) max_age_days <- 30

    row <- DBI::dbGetQuery(conn,
        "SELECT MIN(updated_at) AS oldest FROM translations"
    )

    if (nrow(row) == 0 || is.na(row$oldest[1])) return(TRUE)

    oldest <- tryCatch(
        as.POSIXct(row$oldest[1], format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        error = function(e) NULL
    )
    if (is.null(oldest) || is.na(oldest)) return(TRUE)

    difftime(Sys.time(), oldest, units = "days") > max_age_days
}

# ---------------------------------------------------------------------------
# refresh_translations(source_tag, dry_run)
# Fetches all translation keys from TRANSLATIONS_API_URL and upserts them.
# Expected API response: JSON array of {key, en, sw} objects.
# ---------------------------------------------------------------------------

refresh_translations <- function(source_tag = "api",
                                 dry_run    = FALSE) {
    conn    <- .akilimo_db_conn
    api_url <- sub("/+$", "", Sys.getenv("AKILIMO_API_URL", unset = ""))

    if (!nzchar(api_url)) {
        log_write("DEBUG", "akilimo_db: AKILIMO_API_URL not set — skipping translation refresh")
        return(list(status = "skipped", rows_upserted = 0L, message = "AKILIMO_API_URL not set"))
    }

    result <- .refresh_translations_inner(conn, api_url, source_tag, dry_run)

    if (!dry_run) {
        .log_refresh(conn, "translations", NULL, source_tag,
                     result$status, result$rows_upserted, result$message)
    }

    # Invalidate in-memory cache so next tr() call re-queries the DB
    if (result$status == "ok" && exists(".data_cache", envir = globalenv())) {
        cache <- get(".data_cache", envir = globalenv())
        if (exists("TRNS", envir = cache, inherits = FALSE))
            rm("TRNS", envir = cache)
    }

    result
}

# Walk all pages of the Laravel paginated translations endpoint and collect
# every record.  Returns the same status/rows_upserted/message list as the
# other .refresh_*_inner helpers.
#
# Only two fields are consumed from each response:
#   resp$data          — the array of translation records for this page
#   resp$links[["next"]] — absolute URL of the next page; NULL on the last page
# Everything in resp$meta is informational and ignored by this function.
#
# Duplicate keys: last record wins (keyed named list, matches CSV row-order semantics).
# Partial sync: only rows returned by the API are upserted; existing DB rows
#   for keys absent from the remote feed are left untouched.
.refresh_translations_inner <- function(conn, api_url, source_tag, dry_run) {
    base_url <- paste0(api_url, "/translations")
    token    <- Sys.getenv("AKILIMO_API_TOKEN", unset = "")

    now_str  <- strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    # Named list keyed by translation key — last record wins on duplicates
    valid    <- list()
    page_num <- 1L
    page_url <- paste0(base_url, "?page=1")

    repeat {
        log_write("DEBUG", sprintf("akilimo_db: fetching translations page %d", page_num))

        fetch_err <- NULL
        resp <- tryCatch({
            raw <- httr::GET(page_url,
                if (nzchar(token)) httr::add_headers(Authorization = paste("Bearer", token)),
                httr::timeout(15)
            )
            httr::stop_for_status(raw, task = paste("GET", page_url))
            httr::content(raw, as = "parsed", type = "application/json")
        }, error = function(e) {
            fetch_err <<- conditionMessage(e)
            log_write("WARN", sprintf(
                "akilimo_db: HTTP error fetching translations page %d: %s",
                page_num, fetch_err))
            NULL
        })

        # Network or HTTP failure
        if (is.null(resp)) {
            if (page_num == 1L) {
                return(list(status = "error", rows_upserted = 0L,
                            message = paste("error on first translations page:", fetch_err)))
            }
            log_write("WARN", sprintf(
                "akilimo_db: network error on translations page %d — using %d rows fetched so far",
                page_num, length(valid)))
            break
        }

        # Early-return error sentinel from the API
        if (inherits(resp, "list") && isTRUE(resp$status %in% c("error", "skipped")))
            return(resp)

        # Extract records from resp$data; fail clearly if the envelope is wrong
        records <- resp$data
        if (is.null(records)) {
            if (page_num == 1L)
                return(list(status = "error", rows_upserted = 0L,
                            message = "translations API response missing 'data' field"))
            log_write("WARN", sprintf(
                "akilimo_db: missing 'data' on page %d — stopping early with %d rows",
                page_num, length(valid)))
            break
        }

        for (row in records) {
            key <- trimws(row$key %||% "")
            en  <- row$en %||% ""
            sw  <- row$sw %||% ""
            rw  <- row$rw %||% ""

            if (!nzchar(key)) {
                log_write("WARN", "akilimo_db: empty translation key — skipping row")
                next
            }
            if (!is.character(en)) {
                log_write("WARN", "akilimo_db: non-string 'en' for key", key, "— skipping row")
                next
            }

            valid[[key]] <- list(
                key        = key,
                en         = en,
                sw         = if (is.character(sw)) sw else "",
                rw         = if (is.character(rw)) rw else "",
                updated_at = now_str
            )
        }

        log_write("DEBUG", sprintf(
            "akilimo_db: translations page %d — %d record(s), total so far: %d",
            page_num, length(records), length(valid)))

        # Advance to next page using the server-provided cursor (links.next)
        next_url <- resp$links[["next"]] %||% ""
        if (!nzchar(next_url)) break
        page_url <- next_url
        page_num <- page_num + 1L
    }

    if (length(valid) == 0) {
        msg <- "no valid rows returned from translations API"
        log_write("WARN", "akilimo_db:", msg)
        return(list(status = "error", rows_upserted = 0L, message = msg))
    }

    if (dry_run) {
        log_write("INFO", sprintf("akilimo_db [dry-run] translations: %d valid row(s) across %d page(s)",
                                  length(valid), page_num))
        return(list(status = "ok", rows_upserted = length(valid), message = "dry-run"))
    }

    tryCatch({
        for (v in valid) {
            DBI::dbExecute(conn,
                "INSERT OR REPLACE INTO translations (key, en, sw, rw, updated_at, source)
                 VALUES (?, ?, ?, ?, ?, ?)",
                params = list(v$key, v$en, v$sw, v$rw, v$updated_at, source_tag)
            )
        }
        log_write("INFO", sprintf("akilimo_db: refreshed translations (%d rows, %d page(s))",
                                  length(valid), page_num))
        list(status = "ok", rows_upserted = length(valid), message = NULL)
    }, error = function(e) {
        log_write("WARN", "akilimo_db: DB write error for translations:", conditionMessage(e))
        list(status = "error", rows_upserted = 0L, message = conditionMessage(e))
    })
}
