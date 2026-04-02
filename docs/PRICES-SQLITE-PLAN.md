# Price Data: CSV → SQLite + API Refresh

**Status:** Proposed  
**Affects:** `R/get_data.R`, `R/fertilizers.R`, `R/AkilimoMain.R`, `api.R`, `install_packages.R`, `docs/SETUP.md`

---

## 1. Problem Statement

`data/input/Default_prices.csv` and `data/input/starchPrices.csv` are static
files bundled with the data archive. Updating them requires a new OSF/Zenodo
release, a re-download, and a server restart. Cassava and fertilizer prices
change frequently — weekly in some markets — so the defaults are often stale
by the time they reach production.

### Current flow (prices)

```
OSF/Zenodo archive
  └─ data/input/Default_prices.csv   ← read once per process, cached in .data_cache
  └─ data/input/starchPrices.csv     ← read once per process, cached in .data_cache
```

### Target flow

```
data/input/prices.sqlite             ← single file, ships with the image as seed data
       │
       ├─ populated from CSV on first run (no migration required)
       ├─ queried per request (RSQLite, WAL mode, read-fast)
       └─ updated via API refresh (upsert, atomic, auditable)
                │
         PRICE_API_URL env var ──→ external price service (CGIAR, future)
                │
         manual: POST /admin/refresh-prices
         automatic: if prices older than PRICE_MAX_AGE_DAYS
```

---

## 2. Database Schema

Single file: `data/input/prices.sqlite`

### Table: `default_prices`

Replaces `Default_prices.csv`. Covers fertilizer bag prices, labour costs,
cassava prices, and secondary crop prices (maize, sweet potato).

```sql
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
```

| Column | Purpose |
|--------|---------|
| `country` | ISO country code — NG, TZ, RW, GH, BI |
| `item` | Price key matching fertilizer type names and labour/crop keys |
| `price` | Numeric price value (in local currency) |
| `unit` | `per_bag`, `per_kg`, `per_tonne`, `per_acre`, `per_ha` |
| `currency` | Local currency code (NGN, TZS, RWF, GHS, BIF) |
| `updated_at` | ISO-8601 UTC timestamp of last update |
| `source` | `seed` (CSV), `api` (external refresh), `manual` (admin endpoint) |

### Table: `starch_prices`

Replaces `starchPrices.csv`. Starch factory cassava purchase prices, tiered by
starch content.

```sql
CREATE TABLE IF NOT EXISTS starch_prices (
    starch_factory       TEXT    NOT NULL,
    starch_factory_label TEXT    NOT NULL DEFAULT '',
    class                INTEGER NOT NULL,
    country              TEXT    NOT NULL,
    key                  TEXT    NOT NULL,
    min_starch           REAL    NOT NULL,
    range_starch         TEXT    NOT NULL DEFAULT '',
    price                REAL    NOT NULL,
    updated_at           TEXT    NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
    source               TEXT    NOT NULL DEFAULT 'seed',
    PRIMARY KEY (key)
);
```

### Table: `price_refresh_log`

Audit trail for all API refresh operations.

```sql
CREATE TABLE IF NOT EXISTS price_refresh_log (
    id          INTEGER PRIMARY KEY AUTOINCREMENT,
    country     TEXT,
    source      TEXT    NOT NULL,
    status      TEXT    NOT NULL,   -- 'ok' | 'error' | 'skipped'
    rows_upserted INTEGER,
    message     TEXT,
    ran_at      TEXT    NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now'))
);
```

### Schema version

```sql
PRAGMA user_version = 1;
```

Increment on any schema change. The R loader checks this on startup and runs
migration SQL if the version is behind.

---

## 3. R Implementation

### 3.1 New file: `R/prices_db.R`

Sourced by `api.R` after `get_data.R`. Contains all DB interaction.

#### `open_prices_db()`

Opens (or creates) the SQLite connection. Called once at startup; connection
stored in `.prices_db_conn` package-level variable.

```
open_prices_db()
  ├─ db path: Sys.getenv("PRICES_DB_PATH", unset = "data/input/prices.sqlite")
  ├─ if file does not exist → call seed_prices_db()
  ├─ check PRAGMA user_version → run migrations if needed
  ├─ set WAL journal mode (PRAGMA journal_mode=WAL)
  └─ return DBI connection
```

#### `seed_prices_db(conn)`

Runs once when the DB file is absent. Reads the existing CSVs and inserts all
rows with `source = 'seed'`. After seeding, the CSVs are no longer needed at
runtime (but kept for reference).

```
seed_prices_db(conn)
  ├─ read Default_prices.csv  → INSERT INTO default_prices
  └─ read starchPrices.csv    → INSERT INTO starch_prices
```

#### `get_default_prices(country)`

Replaces the `cached_read("default_prices", ...)` call in `get_input_data()`.

```
get_default_prices(country)
  └─ SELECT country, item, price FROM default_prices WHERE country = ?
  └─ returns data.frame with columns: Country, Item, Price
     (same column names as the old CSV — no changes needed in fertilizers.R)
```

#### `get_starch_prices()`

Replaces `cached_read("starch_prices", ...)`.

```
get_starch_prices()
  └─ SELECT * FROM starch_prices
  └─ returns data.frame with same column names as old starchPrices.csv
```

#### `refresh_prices(country = NULL, source_tag = "api")`

Fetches fresh prices from `PRICE_API_URL` and upserts into the DB.

```
refresh_prices(country, source_tag)
  ├─ check PRICE_API_URL is set — return early with log entry if not
  ├─ build request URL: {PRICE_API_URL}/prices?country={country}
  ├─ GET with httr, timeout 10s
  ├─ validate response:
  │     must be JSON array of {country, item, price, unit, currency}
  │     each price must be numeric and > 0
  │     country must be one of the 5 supported codes
  ├─ upsert into default_prices (INSERT OR REPLACE)
  │     set updated_at = now(), source = source_tag
  ├─ invalidate in-memory cache for this country
  ├─ write to price_refresh_log
  └─ return list(status, rows_upserted, country)
```

#### `prices_are_stale(country)`

Used by the auto-refresh hook.

```
prices_are_stale(country)
  ├─ SELECT MIN(updated_at) FROM default_prices WHERE country = ?
  ├─ compare to Sys.time() - PRICE_MAX_AGE_DAYS * 86400
  └─ return TRUE if oldest row is older than threshold, FALSE otherwise
```

#### `migrate_prices_db(conn, current_version)`

Applies schema migrations when `PRAGMA user_version` is behind the code
version. Each migration is a named list entry: version → SQL vector.

### 3.2 Changes to `R/get_data.R`

Replace the two CSV-based `cached_read` blocks:

```r
# Before
} else if (x == "default_prices") {
    cached_read("default_prices", function() {
        out <- read.csv(data_path("input/Default_prices.csv"))
        out$Country[out$Country == "BU"] <- "BI"
        out
    })
} else if (x == "starch_prices") {
    cached_read("starch_prices", function() read.csv(data_path("input/starchPrices.csv")))
}

# After
} else if (x == "default_prices") {
    get_default_prices(country)      # country passed through from get_data()
} else if (x == "starch_prices") {
    get_starch_prices()
}
```

`get_data()` signature gains an optional `country` parameter (already present
for soil lookups — just needs threading to the `input_keys` branch).

### 3.3 Changes to `R/AkilimoMain.R`

Add a stale-price check at the start of `run_akilimo()`, after `parse_request()`:

```r
# Auto-refresh prices if API is configured and data is stale
if (nzchar(Sys.getenv("PRICE_API_URL"))) {
    if (prices_are_stale(params$country)) {
        tryCatch(
            refresh_prices(params$country),
            error = function(e)
                log_write("WARN", "Auto price refresh failed:", conditionMessage(e))
        )
    }
}
```

This runs synchronously before dispatch. Failure is logged as WARN but never
blocks the recommendation.

### 3.4 Changes to `api.R`

Add an admin endpoint for manual refresh:

```r
pr$handle(
    method  = "POST",
    path    = "/admin/refresh-prices",
    handler = function(req, res) {
        body    <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) list())
        country <- body[["country"]]   # NULL = refresh all countries
        result  <- tryCatch(
            refresh_prices(country, source_tag = "manual"),
            error = function(e) list(status = "error", message = conditionMessage(e))
        )
        result
    }
)
```

A shared secret can be added via `ADMIN_TOKEN` env var if the endpoint needs
to be protected (not in scope for v1).

---

## 4. Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `PRICES_DB_PATH` | `data/input/prices.sqlite` | Path to the SQLite file |
| `PRICE_API_URL` | *(unset)* | Base URL of the external price service. If unset, auto-refresh is disabled |
| `PRICE_MAX_AGE_DAYS` | `7` | Days before prices are considered stale and a refresh is attempted |
| `PRICE_API_TOKEN` | *(unset)* | Bearer token for the price API (sent as `Authorization: Bearer …`) |

Add all four to `.env.example`.

---

## 5. API Contract (external price service)

The external service must expose:

```
GET {PRICE_API_URL}/prices?country={NG|TZ|RW|GH|BI}
Authorization: Bearer {PRICE_API_TOKEN}
```

Response — JSON array:

```json
[
  {
    "country":  "NG",
    "item":     "urea",
    "price":    8500,
    "unit":     "per_bag",
    "currency": "NGN"
  },
  ...
]
```

Validation rules enforced by `refresh_prices()`:

- `country` must be one of the 5 supported codes
- `item` must be a non-empty string
- `price` must be numeric and > 0
- `unit` must be one of: `per_bag`, `per_kg`, `per_tonne`, `per_acre`, `per_ha`
- Any row failing validation is skipped (logged at WARN); the rest are upserted

If the response contains zero valid rows, no DB write occurs and the log entry
is marked `status = 'skipped'`.

---

## 6. Migration Path (zero downtime)

1. `seed_prices_db()` runs automatically the first time the server starts after
   deployment — no manual step required.
2. The old CSV files are left in place and continue to work as the seed source.
   They can be removed in a later release once the DB is confirmed stable.
3. If `prices.sqlite` is deleted, the next server start re-seeds from CSV
   automatically.
4. Schema migrations run automatically on startup via `migrate_prices_db()`.
   No manual `ALTER TABLE` commands needed by ops.

---

## 7. Dependencies

| Package | Already required? | Purpose |
|---------|--------------------|---------|
| `RSQLite` | No — add to `install_packages.R` | SQLite driver |
| `DBI` | No — add to `install_packages.R` | DB interface (pulled in by RSQLite) |
| `httr` | Yes | HTTP calls in `refresh_prices()` |
| `jsonlite` | Yes | Parse API response |

---

## 8. Files Changed

| File | Change |
|------|--------|
| `R/prices_db.R` | **New** — all DB logic |
| `R/get_data.R` | Replace two `cached_read` blocks; thread `country` to input branch |
| `R/AkilimoMain.R` | Add stale-price auto-refresh after `parse_request()` |
| `api.R` | Source `prices_db.R`; add `/admin/refresh-prices` endpoint |
| `install_packages.R` | Add `RSQLite`, `DBI` |
| `.env.example` | Add four new env vars |
| `docs/SETUP.md` | Document SQLite file, new env vars, admin endpoint |

`data/input/Default_prices.csv` and `data/input/starchPrices.csv` are **not
deleted** — kept as seed source and offline reference.

---

## 9. Out of Scope (this plan)

- Authentication / rate-limiting on `/admin/refresh-prices`
- Price history / time-series storage (only latest value per country+item kept)
- Migrating `translations.csv` or `dry_matter.csv` to SQLite (static, infrequent changes)
- The external price service itself — contract defined in §5, implementation is external

---

## 10. Testing

| Test | Location |
|------|----------|
| DB seeds correctly from CSV | `tests/test_prices_db.R` |
| `get_default_prices()` returns same shape as old CSV loader | `tests/test_prices_db.R` |
| `refresh_prices()` upserts correctly with mock response | `tests/test_prices_db.R` |
| `refresh_prices()` skips invalid rows, upserts valid ones | `tests/test_prices_db.R` |
| `prices_are_stale()` returns TRUE/FALSE correctly | `tests/test_prices_db.R` |
| Auto-refresh skipped when `PRICE_API_URL` not set | `tests/test_prices_db.R` |
| `/admin/refresh-prices` endpoint returns correct JSON | `tests/test_api.R` |
