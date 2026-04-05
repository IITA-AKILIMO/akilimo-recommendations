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
#   get_translations()                        → data.frame(key, en, sw)
#   refresh_prices(country, source_tag, dry_run)
#   refresh_starch_prices(country, source_tag, dry_run)
#   prices_are_stale(country)                 → logical
#   starch_prices_are_stale(country = NULL)   → logical
#   translations_are_stale()                  → logical
#   refresh_translations(source_tag, dry_run)  endpoint: GET {AKILIMO_API_URL}/translations

VALID_COUNTRIES <- c("NG", "TZ", "RW", "GH", "BI")
VALID_UNITS     <- c("per_bag", "per_kg", "per_tonne", "per_acre", "per_ha")

# Package-level connection handle — set by open_akilimo_db(), used by readers/writers
.akilimo_db_conn <- NULL

# Current schema version — increment whenever DDL changes
.DB_SCHEMA_VERSION <- 2L

# ---------------------------------------------------------------------------
# DDL helpers
# ---------------------------------------------------------------------------

.create_tables <- function(conn) {
    DBI::dbExecute(conn, "
        CREATE TABLE IF NOT EXISTS default_prices (
            country     TEXT    NOT NULL,
            item        TEXT    NOT NULL,
            price       REAL    NOT NULL,
            unit        TEXT    NOT NULL DEFAULT 'per_bag',
            currency    TEXT    NOT NULL DEFAULT '',
            updated_at  TEXT    NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
            source      TEXT    NOT NULL DEFAULT 'seed',
            PRIMARY KEY (country, item)
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
            updated_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now'))
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
        # version 2 — add translations table; seed from CSV
        "2" = list(
            sql = c("CREATE TABLE IF NOT EXISTS translations (
                         key        TEXT NOT NULL PRIMARY KEY,
                         en         TEXT NOT NULL DEFAULT '',
                         sw         TEXT NOT NULL DEFAULT '',
                         updated_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now'))
                     );"),
            seed_fn = .seed_translations
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
        seed_akilimo_db(conn)
    } else {
        # Check schema version and migrate if behind
        ver <- DBI::dbGetQuery(conn, "PRAGMA user_version;")[[1]]
        if (ver < .DB_SCHEMA_VERSION) {
            log_write("INFO", "akilimo_db: schema version", ver,
                      "→ migrating to", .DB_SCHEMA_VERSION)
            migrate_akilimo_db(conn, ver)
        }
    }

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
        sw         = tr_raw$sw,
        updated_at = strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        stringsAsFactors = FALSE
    )
    DBI::dbWriteTable(conn, "translations", rows, append = TRUE)
    log_write("INFO", sprintf("akilimo_db: seeded translations (%d rows)", nrow(rows)))
    invisible(NULL)
}


# ---------------------------------------------------------------------------
# seed_akilimo_db(conn)  — run once on first startup
# ---------------------------------------------------------------------------

seed_akilimo_db <- function(conn) {
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
        DBI::dbWriteTable(conn, "default_prices", rows, append = TRUE)
        log_write("INFO", sprintf("akilimo_db: seeded default_prices (%d rows)", nrow(rows)))
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
            currency             = "",
            updated_at           = strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
            source               = "seed",
            stringsAsFactors = FALSE
        )
        DBI::dbWriteTable(conn, "starch_prices", rows, append = TRUE)
        log_write("INFO", sprintf("akilimo_db: seeded starch_prices (%d rows)", nrow(rows)))
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
        DBI::dbGetQuery(conn, "SELECT key, en, sw FROM translations ORDER BY key"),
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
    api_url <- Sys.getenv("AKILIMO_API_URL", unset = "")

    if (!nzchar(api_url)) {
        log_write("DEBUG", "akilimo_db: AKILIMO_API_URL not set — skipping default price refresh")
        return(list(status = "skipped", rows_upserted = 0L, message = "AKILIMO_API_URL not set"))
    }

    countries <- if (is.null(country)) VALID_COUNTRIES else country

    total_rows <- 0L
    any_fail   <- FALSE

    for (ctry in countries) {
        result <- .refresh_default_one(conn, api_url, ctry, source_tag, dry_run)
        total_rows <- total_rows + result$rows_upserted
        if (result$status != "ok") any_fail <- TRUE

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
        message       = if (any_fail) "one or more countries failed" else NULL
    )
}

.refresh_default_one <- function(conn, api_url, country, source_tag, dry_run) {
    url <- paste0(api_url, "/prices?country=", country)
    token <- Sys.getenv("AKILIMO_API_TOKEN", unset = "")

    resp <- tryCatch({
        raw <- httr::GET(url,
            if (nzchar(token)) httr::add_headers(Authorization = paste("Bearer", token)),
            httr::timeout(10)
        )
        httr::content(raw, as = "parsed", type = "application/json")
    }, error = function(e) {
        log_write("WARN", "akilimo_db: HTTP error fetching default prices for", country, ":", conditionMessage(e))
        return(list(status = "error", rows_upserted = 0L, message = conditionMessage(e)))
    })

    if (inherits(resp, "list") && !is.null(resp$status)) return(resp)

    # Validate and coerce rows
    now_str <- strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    valid   <- list()

    for (row in resp) {
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
            unit       = unit,
            currency   = row$currency %||% "",
            updated_at = now_str,
            source     = source_tag
        )
    }

    if (length(valid) == 0) {
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
                     (country, item, price, unit, currency, updated_at, source)
                 VALUES (?, ?, ?, ?, ?, ?, ?)",
                params = list(v$country, v$item, v$price, v$unit,
                              v$currency, v$updated_at, v$source)
            )
        }
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
    api_url <- Sys.getenv("AKILIMO_API_URL", unset = "")

    if (!nzchar(api_url)) {
        log_write("DEBUG", "akilimo_db: AKILIMO_API_URL not set — skipping starch price refresh")
        return(list(status = "skipped", rows_upserted = 0L, message = "AKILIMO_API_URL not set"))
    }

    countries  <- if (is.null(country)) VALID_COUNTRIES else country
    total_rows <- 0L
    any_fail   <- FALSE

    for (ctry in countries) {
        result <- .refresh_starch_one(conn, api_url, ctry, source_tag, dry_run)
        total_rows <- total_rows + result$rows_upserted
        if (result$status != "ok") any_fail <- TRUE

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
        message       = if (any_fail) "one or more countries failed" else NULL
    )
}

.refresh_starch_one <- function(conn, api_url, country, source_tag, dry_run) {
    url   <- paste0(api_url, "/starch-prices?country=", country)
    token <- Sys.getenv("AKILIMO_API_TOKEN", unset = "")

    resp <- tryCatch({
        raw <- httr::GET(url,
            if (nzchar(token)) httr::add_headers(Authorization = paste("Bearer", token)),
            httr::timeout(10)
        )
        httr::content(raw, as = "parsed", type = "application/json")
    }, error = function(e) {
        log_write("WARN", "akilimo_db: HTTP error fetching starch prices for", country, ":", conditionMessage(e))
        return(list(status = "error", rows_upserted = 0L, message = conditionMessage(e)))
    })

    if (inherits(resp, "list") && !is.null(resp$status)) return(resp)

    now_str <- strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    valid   <- list()
    seen_keys <- character(0)

    for (row in resp) {
        factory  <- row$starch_factory        %||% ""
        key      <- row$key                   %||% ""
        cls      <- suppressWarnings(as.integer(row$class))
        ctry     <- row$country               %||% ""
        minstarch <- suppressWarnings(as.numeric(row$min_starch))
        price    <- suppressWarnings(as.numeric(row$price))

        if (!ctry %in% VALID_COUNTRIES)       { log_write("WARN", "akilimo_db: invalid country", ctry, "— skipping starch row"); next }
        if (!nzchar(factory))                 { log_write("WARN", "akilimo_db: empty starch_factory — skipping row"); next }
        if (!nzchar(key))                     { log_write("WARN", "akilimo_db: empty key — skipping starch row"); next }
        if (key %in% seen_keys)               { log_write("WARN", "akilimo_db: duplicate key", key, "— skipping row"); next }
        if (is.na(cls)  || cls  < 1L)         { log_write("WARN", "akilimo_db: invalid class for", key, "— skipping row"); next }
        if (is.na(minstarch) || minstarch < 0){ log_write("WARN", "akilimo_db: invalid min_starch for", key, "— skipping row"); next }
        if (is.na(price) || price <= 0)       { log_write("WARN", "akilimo_db: invalid price for", key, "— skipping row"); next }

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
    api_url <- Sys.getenv("AKILIMO_API_URL", unset = "")

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

.refresh_translations_inner <- function(conn, api_url, source_tag, dry_run) {
    url   <- paste0(api_url, "/translations")
    token <- Sys.getenv("AKILIMO_API_TOKEN", unset = "")

    resp <- tryCatch({
        raw <- httr::GET(url,
            if (nzchar(token)) httr::add_headers(Authorization = paste("Bearer", token)),
            httr::timeout(10)
        )
        httr::content(raw, as = "parsed", type = "application/json")
    }, error = function(e) {
        log_write("WARN", "akilimo_db: HTTP error fetching translations:", conditionMessage(e))
        return(list(status = "error", rows_upserted = 0L, message = conditionMessage(e)))
    })

    if (inherits(resp, "list") && !is.null(resp$status)) return(resp)

    now_str <- strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    valid   <- list()

    for (row in resp) {
        key <- trimws(row$key %||% "")
        en  <- row$en %||% ""
        sw  <- row$sw %||% ""

        if (!nzchar(key)) {
            log_write("WARN", "akilimo_db: empty translation key — skipping row")
            next
        }
        if (!is.character(en) || !is.character(sw)) {
            log_write("WARN", "akilimo_db: non-string value for key", key, "— skipping row")
            next
        }

        valid[[length(valid) + 1L]] <- list(
            key        = key,
            en         = en,
            sw         = sw,
            updated_at = now_str,
            source     = source_tag
        )
    }

    if (length(valid) == 0) {
        msg <- "no valid rows returned from translations API"
        log_write("WARN", "akilimo_db:", msg)
        return(list(status = "error", rows_upserted = 0L, message = msg))
    }

    if (dry_run) {
        log_write("INFO", sprintf("akilimo_db [dry-run] translations: %d valid row(s)", length(valid)))
        return(list(status = "ok", rows_upserted = length(valid), message = "dry-run"))
    }

    tryCatch({
        for (v in valid) {
            DBI::dbExecute(conn,
                "INSERT OR REPLACE INTO translations (key, en, sw, updated_at)
                 VALUES (?, ?, ?, ?)",
                params = list(v$key, v$en, v$sw, v$updated_at)
            )
        }
        log_write("INFO", sprintf("akilimo_db: refreshed translations (%d rows)", length(valid)))
        list(status = "ok", rows_upserted = length(valid), message = NULL)
    }, error = function(e) {
        log_write("WARN", "akilimo_db: DB write error for translations:", conditionMessage(e))
        list(status = "error", rows_upserted = 0L, message = conditionMessage(e))
    })
}
