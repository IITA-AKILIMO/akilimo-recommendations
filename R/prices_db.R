# R/prices_db.R — SQLite price store
#
# Provides read/write access to the prices.sqlite database.
# Called by get_data.R (reads) and AkilimoMain.R / refresh_prices.R (writes).
#
# Public API:
#   open_prices_db()                          → DBI connection
#   seed_prices_db(conn)                      → called once when DB is new
#   get_default_prices(country)               → data.frame(Country, Item, Price)
#   get_starch_prices()                       → data.frame matching old CSV columns
#   refresh_prices(country, source_tag, dry_run)
#   refresh_starch_prices(country, source_tag, dry_run)
#   prices_are_stale(country)                 → logical
#   starch_prices_are_stale(country = NULL)   → logical

VALID_COUNTRIES <- c("NG", "TZ", "RW", "GH", "BI")
VALID_UNITS     <- c("per_bag", "per_kg", "per_tonne", "per_acre", "per_ha")

# Package-level connection handle — set by open_prices_db(), used by readers/writers
.prices_db_conn <- NULL

# Current schema version — increment whenever DDL changes
.DB_SCHEMA_VERSION <- 1L

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
    DBI::dbExecute(conn,
        paste0("PRAGMA user_version = ", .DB_SCHEMA_VERSION, ";"))
    invisible(NULL)
}

# ---------------------------------------------------------------------------
# Migration (run when user_version < .DB_SCHEMA_VERSION)
# ---------------------------------------------------------------------------

migrate_prices_db <- function(conn, current_version) {
    # Migrations are applied in order. Each entry: list(sql = character vector).
    migrations <- list(
        # version 1 — initial schema; handled by .create_tables above
        "1" = list(sql = character(0))
    )

    for (v in seq(current_version + 1L, .DB_SCHEMA_VERSION)) {
        key <- as.character(v)
        if (!is.null(migrations[[key]])) {
            for (sql in migrations[[key]]$sql) {
                DBI::dbExecute(conn, sql)
            }
        }
        DBI::dbExecute(conn,
            paste0("PRAGMA user_version = ", v, ";"))
        log_write("INFO", "prices_db: migrated schema to version", v)
    }
    invisible(NULL)
}

# ---------------------------------------------------------------------------
# open_prices_db()
# ---------------------------------------------------------------------------

open_prices_db <- function() {
    db_path <- Sys.getenv("PRICES_DB_PATH", unset = "data/input/prices.sqlite")

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
        log_write("INFO", "prices_db: new database — creating schema and seeding from CSV")
        .create_tables(conn)
        seed_prices_db(conn)
    } else {
        # Check schema version and migrate if behind
        ver <- DBI::dbGetQuery(conn, "PRAGMA user_version;")[[1]]
        if (ver < .DB_SCHEMA_VERSION) {
            log_write("INFO", "prices_db: schema version", ver,
                      "→ migrating to", .DB_SCHEMA_VERSION)
            migrate_prices_db(conn, ver)
        }
    }

    # Store globally so readers/writers can use it without re-opening
    assign(".prices_db_conn", conn, envir = globalenv())
    log_write("INFO", "prices_db: opened", db_path)
    invisible(conn)
}

# ---------------------------------------------------------------------------
# seed_prices_db(conn)  — run once on first startup
# ---------------------------------------------------------------------------

seed_prices_db <- function(conn) {
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
        log_write("INFO", sprintf("prices_db: seeded default_prices (%d rows)", nrow(rows)))
    } else {
        log_write("WARN", "prices_db: Default_prices.csv not found — default_prices table empty")
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
        log_write("INFO", sprintf("prices_db: seeded starch_prices (%d rows)", nrow(rows)))
    } else {
        log_write("WARN", "prices_db: starchPrices.csv not found — starch_prices table empty")
    }

    invisible(NULL)
}

# ---------------------------------------------------------------------------
# get_default_prices(country)
# Returns data.frame with columns Country, Item, Price  (matching old CSV)
# ---------------------------------------------------------------------------

get_default_prices <- function(country) {
    conn <- .prices_db_conn
    if (is.null(conn)) stop("prices_db: connection not open — call open_prices_db() first")

    rows <- DBI::dbGetQuery(conn,
        "SELECT country AS Country, item AS Item, price AS Price
         FROM default_prices
         WHERE country = ?",
        params = list(country)
    )
    rows
}

# ---------------------------------------------------------------------------
# get_starch_prices()
# Returns data.frame with columns matching old starchPrices.csv
# ---------------------------------------------------------------------------

get_starch_prices <- function() {
    conn <- .prices_db_conn
    if (is.null(conn)) stop("prices_db: connection not open — call open_prices_db() first")

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
    conn    <- .prices_db_conn
    api_url <- Sys.getenv("PRICE_API_URL", unset = "")

    if (!nzchar(api_url)) {
        log_write("DEBUG", "prices_db: PRICE_API_URL not set — skipping default price refresh")
        return(list(status = "skipped", rows_upserted = 0L, message = "PRICE_API_URL not set"))
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
    token <- Sys.getenv("PRICE_API_TOKEN", unset = "")

    resp <- tryCatch({
        raw <- httr::GET(url,
            if (nzchar(token)) httr::add_headers(Authorization = paste("Bearer", token)),
            httr::timeout(10)
        )
        httr::content(raw, as = "parsed", type = "application/json")
    }, error = function(e) {
        log_write("WARN", "prices_db: HTTP error fetching default prices for", country, ":", conditionMessage(e))
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

        if (!ctry  %in% VALID_COUNTRIES) { log_write("WARN", "prices_db: invalid country", ctry,  "— skipping row"); next }
        if (!nzchar(item))               { log_write("WARN", "prices_db: empty item — skipping row"); next }
        if (is.na(price) || price <= 0)  { log_write("WARN", "prices_db: invalid price for", item, "— skipping row"); next }
        if (!unit %in% VALID_UNITS)      { log_write("WARN", "prices_db: invalid unit", unit, "for", item, "— skipping row"); next }

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
        log_write("WARN", "prices_db:", msg)
        return(list(status = "error", rows_upserted = 0L, message = msg))
    }

    if (dry_run) {
        log_write("INFO", sprintf("prices_db [dry-run] default/%s: %d valid row(s)", country, length(valid)))
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
        log_write("WARN", "prices_db: DB write error for default/", country, ":", conditionMessage(e))
        list(status = "error", rows_upserted = 0L, message = conditionMessage(e))
    })
}

# ---------------------------------------------------------------------------
# refresh_starch_prices(country, source_tag, dry_run)
# ---------------------------------------------------------------------------

refresh_starch_prices <- function(country    = NULL,
                                  source_tag = "api",
                                  dry_run    = FALSE) {
    conn    <- .prices_db_conn
    api_url <- Sys.getenv("PRICE_API_URL", unset = "")

    if (!nzchar(api_url)) {
        log_write("DEBUG", "prices_db: PRICE_API_URL not set — skipping starch price refresh")
        return(list(status = "skipped", rows_upserted = 0L, message = "PRICE_API_URL not set"))
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
    token <- Sys.getenv("PRICE_API_TOKEN", unset = "")

    resp <- tryCatch({
        raw <- httr::GET(url,
            if (nzchar(token)) httr::add_headers(Authorization = paste("Bearer", token)),
            httr::timeout(10)
        )
        httr::content(raw, as = "parsed", type = "application/json")
    }, error = function(e) {
        log_write("WARN", "prices_db: HTTP error fetching starch prices for", country, ":", conditionMessage(e))
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

        if (!ctry %in% VALID_COUNTRIES)       { log_write("WARN", "prices_db: invalid country", ctry, "— skipping starch row"); next }
        if (!nzchar(factory))                 { log_write("WARN", "prices_db: empty starch_factory — skipping row"); next }
        if (!nzchar(key))                     { log_write("WARN", "prices_db: empty key — skipping starch row"); next }
        if (key %in% seen_keys)               { log_write("WARN", "prices_db: duplicate key", key, "— skipping row"); next }
        if (is.na(cls)  || cls  < 1L)         { log_write("WARN", "prices_db: invalid class for", key, "— skipping row"); next }
        if (is.na(minstarch) || minstarch < 0){ log_write("WARN", "prices_db: invalid min_starch for", key, "— skipping row"); next }
        if (is.na(price) || price <= 0)       { log_write("WARN", "prices_db: invalid price for", key, "— skipping row"); next }

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
        log_write("WARN", "prices_db:", msg)
        return(list(status = "skipped", rows_upserted = 0L, message = msg))
    }

    if (dry_run) {
        log_write("INFO", sprintf("prices_db [dry-run] starch/%s: %d valid row(s)", country, length(valid)))
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
        log_write("WARN", "prices_db: transaction error for starch/", country, ":", conditionMessage(e))
        list(status = "error", rows_upserted = 0L, message = conditionMessage(e))
    })
}

# ---------------------------------------------------------------------------
# prices_are_stale(country)
# ---------------------------------------------------------------------------

prices_are_stale <- function(country) {
    conn <- .prices_db_conn
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
    conn <- .prices_db_conn
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
