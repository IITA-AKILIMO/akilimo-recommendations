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
starch content. Unlike `default_prices`, rows are keyed by factory + tier
(`key`), and the factory list itself can change (new factories, closures,
contract renegotiations).

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
    currency             TEXT    NOT NULL DEFAULT '',
    updated_at           TEXT    NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now')),
    source               TEXT    NOT NULL DEFAULT 'seed',
    PRIMARY KEY (key)
);
```

| Column | Purpose |
|--------|---------|
| `starch_factory` | Internal factory identifier — matches `nameSF` in API requests |
| `starch_factory_label` | Human-readable factory name shown in the UI |
| `class` | Tier number (1 = highest starch content, N = lowest) |
| `country` | ISO country code |
| `key` | Composite natural key: `{starch_factory}{class}` (e.g. `MatnaStarch1`) |
| `min_starch` | Minimum starch percentage for this tier |
| `range_starch` | Display range string (e.g. `22-24`) |
| `price` | Factory purchase price in local currency per tonne of fresh roots |
| `currency` | Local currency code |
| `updated_at` | ISO-8601 UTC timestamp of last update |
| `source` | `seed`, `api`, or `manual` |

> **Why separate from `default_prices`?** The starch-price data model is a
> factory × tier matrix, not a flat country × item list. New factories can
> appear, old ones can close, and tier thresholds (`min_starch`) can shift
> independently of prices. Keeping the tables separate makes each refresh
> operation atomic and independently auditable.

### Table: `price_refresh_log`

Audit trail for all API refresh operations — covers both `default_prices` and
`starch_prices` refreshes.

```sql
CREATE TABLE IF NOT EXISTS price_refresh_log (
    id            INTEGER PRIMARY KEY AUTOINCREMENT,
    price_type    TEXT    NOT NULL,   -- 'default' | 'starch'
    country       TEXT,               -- NULL when starch refresh covers all countries
    source        TEXT    NOT NULL,   -- 'api' | 'manual'
    status        TEXT    NOT NULL,   -- 'ok' | 'error' | 'skipped'
    rows_upserted INTEGER,
    message       TEXT,
    ran_at        TEXT    NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ','now'))
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

Replaces `cached_read("starch_prices", ...)`. Returns all factories/tiers;
callers filter by `starchFactory == nameSF` as before.

```
get_starch_prices()
  └─ SELECT starch_factory, starch_factory_label, class, country,
            key, min_starch, range_starch, price
     FROM starch_prices
     ORDER BY country, starch_factory, class
  └─ returns data.frame with columns matching old starchPrices.csv column names:
       starchFactory, starchFactory_label, class, country, KEY,
       minStarch, rangeStarch, price
     (column aliases applied in SQL so callers need no changes)
```

#### `refresh_prices(country = NULL, source_tag = "api")`

Fetches fresh fertilizer / labour / cassava prices from the external service
and upserts into `default_prices`.

```
refresh_prices(country, source_tag)
  ├─ check PRICE_API_URL is set — return early (log 'skipped') if not
  ├─ if country is NULL, loop over all 5 supported countries
  ├─ for each country:
  │     GET {PRICE_API_URL}/prices?country={country}
  │         Authorization: Bearer {PRICE_API_TOKEN}   (if set)
  │         timeout: 10s
  │     validate each row:
  │         country ∈ {NG, TZ, RW, GH, BI}
  │         item    non-empty string
  │         price   numeric > 0
  │         unit    ∈ {per_bag, per_kg, per_tonne, per_acre, per_ha}
  │     invalid rows → skip + log WARN (valid rows still upserted)
  │     INSERT OR REPLACE INTO default_prices
  │         set updated_at = now(), source = source_tag
  │     invalidate in-memory .data_cache entry for this country
  │     write row to price_refresh_log (price_type = 'default')
  └─ return list(status, rows_upserted, countries_refreshed)
```

#### `refresh_starch_prices(country = NULL, source_tag = "api")`

Fetches starch factory price tiers from the external service and upserts into
`starch_prices`. Because a refresh may add new factories or retire old ones,
the strategy is **replace-all for the given country** rather than row-level
upsert: existing rows for that country are deleted within the same transaction
before inserting the new set.

```
refresh_starch_prices(country, source_tag)
  ├─ check PRICE_API_URL is set — return early (log 'skipped') if not
  ├─ if country is NULL, loop over all 5 supported countries
  ├─ for each country:
  │     GET {PRICE_API_URL}/starch-prices?country={country}
  │         Authorization: Bearer {PRICE_API_TOKEN}   (if set)
  │         timeout: 10s
  │     validate each row:
  │         country  ∈ {NG, TZ, RW, GH, BI}
  │         starch_factory   non-empty string
  │         key      non-empty string, unique within the response
  │         class    positive integer
  │         min_starch  numeric ≥ 0
  │         price    numeric > 0
  │     if zero valid rows → abort transaction, log 'skipped' (preserve existing data)
  │     BEGIN TRANSACTION
  │         DELETE FROM starch_prices WHERE country = ?
  │         INSERT rows with updated_at = now(), source = source_tag
  │     COMMIT
  │     invalidate .data_cache entry "starch_prices"
  │     write row to price_refresh_log (price_type = 'starch')
  └─ return list(status, rows_upserted, countries_refreshed)
```

> **Why delete-then-insert for starch prices?** Factory closures and tier
> restructuring must be reflected accurately — a factory with 8 tiers that
> drops to 6 tiers would leave stale rows if we only upserted. The
> transaction guarantees callers never see a half-updated factory list.

#### `prices_are_stale(country)`

Checks whether `default_prices` for a given country need refreshing.

```
prices_are_stale(country)
  ├─ SELECT MIN(updated_at) FROM default_prices WHERE country = ?
  ├─ compare to Sys.time() - PRICE_MAX_AGE_DAYS * 86400
  └─ return TRUE if oldest row is older than threshold, FALSE otherwise
```

#### `starch_prices_are_stale(country = NULL)`

Checks whether starch factory prices need refreshing. Because starch contracts
are renegotiated less frequently (typically annually), a separate staleness
threshold is used: `STARCH_PRICE_MAX_AGE_DAYS` (default: 30).

```
starch_prices_are_stale(country)
  ├─ if country is NULL: SELECT MIN(updated_at) FROM starch_prices
  │  else:               SELECT MIN(updated_at) FROM starch_prices WHERE country = ?
  ├─ compare to Sys.time() - STARCH_PRICE_MAX_AGE_DAYS * 86400
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

Add stale-price checks at the start of `run_akilimo()`, after `parse_request()`.
Both checks run only when `PRICE_API_URL` is configured. Starch prices are only
checked when the request includes a starch factory sale (`params$saleSF == TRUE`),
since they are irrelevant otherwise.

```r
if (nzchar(Sys.getenv("PRICE_API_URL"))) {
    # Default prices (fertilizer / labour / cassava)
    if (prices_are_stale(params$country)) {
        tryCatch(
            refresh_prices(params$country),
            error = function(e)
                log_write("WARN", "Auto price refresh failed:", conditionMessage(e))
        )
    }
    # Starch factory prices — only when selling to a starch factory
    if (isTRUE(params$saleSF) && starch_prices_are_stale(params$country)) {
        tryCatch(
            refresh_starch_prices(params$country),
            error = function(e)
                log_write("WARN", "Auto starch price refresh failed:", conditionMessage(e))
        )
    }
}
```

Both refreshes run synchronously before dispatch. Failure is logged as WARN but
never blocks the recommendation.

### 3.4 Changes to `api.R`

Add an admin endpoint that handles both price types via a `type` field in the
request body:

```r
pr$handle(
    method  = "POST",
    path    = "/admin/refresh-prices",
    handler = function(req, res) {
        body    <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) list())
        country <- body[["country"]]   # NULL = all countries
        type    <- tolower(trimws(body[["type"]] %||% "all"))
        #   type = "default" → fertilizer / labour / cassava prices only
        #   type = "starch"  → starch factory prices only
        #   type = "all"     → both (default)

        results <- list()

        if (type %in% c("default", "all")) {
            results$default <- tryCatch(
                refresh_prices(country, source_tag = "manual"),
                error = function(e) list(status = "error", message = conditionMessage(e))
            )
        }
        if (type %in% c("starch", "all")) {
            results$starch <- tryCatch(
                refresh_starch_prices(country, source_tag = "manual"),
                error = function(e) list(status = "error", message = conditionMessage(e))
            )
        }

        results
    }
)
```

A shared secret can be added via `ADMIN_TOKEN` env var if the endpoint needs
to be protected (not in scope for v1).

Example calls:
```bash
# Refresh both price types for Nigeria
curl -X POST http://localhost:8000/admin/refresh-prices \
  -H "Content-Type: application/json" \
  -d '{"country": "NG", "type": "all"}'

# Refresh only starch prices across all countries
curl -X POST http://localhost:8000/admin/refresh-prices \
  -H "Content-Type: application/json" \
  -d '{"type": "starch"}'
```

---

## 4. Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `PRICES_DB_PATH` | `data/input/prices.sqlite` | Path to the SQLite file |
| `PRICE_API_URL` | *(unset)* | Base URL of the external price service. If unset, all auto-refresh is disabled |
| `PRICE_API_TOKEN` | *(unset)* | Bearer token sent as `Authorization: Bearer …` (both endpoints) |
| `PRICE_MAX_AGE_DAYS` | `7` | Days before fertilizer/labour/cassava prices are considered stale |
| `STARCH_PRICE_MAX_AGE_DAYS` | `30` | Days before starch factory prices are considered stale |

Add all five to `.env.example`.

---

## 5. API Contract (external price service)

Both endpoints share the same base URL (`PRICE_API_URL`) and token
(`PRICE_API_TOKEN`).

### 5.1 Fertilizer / Labour / Cassava prices

```
GET {PRICE_API_URL}/prices?country={NG|TZ|RW|GH|BI}
Authorization: Bearer {PRICE_API_TOKEN}
```

Response — JSON array:

```json
[
  { "country": "NG", "item": "urea",   "price": 8500,  "unit": "per_bag", "currency": "NGN" },
  { "country": "NG", "item": "cassUP", "price": 14000, "unit": "per_tonne", "currency": "NGN" },
  { "country": "NG", "item": "manual_ploughing", "price": 18000, "unit": "per_acre", "currency": "NGN" }
]
```

Validation rules enforced by `refresh_prices()`:

- `country` must be one of the 5 supported codes
- `item` must be a non-empty string matching a known price key
- `price` must be numeric and > 0
- `unit` must be one of: `per_bag`, `per_kg`, `per_tonne`, `per_acre`, `per_ha`
- Any row failing validation is skipped (logged at WARN); the rest are upserted
- Zero valid rows → no DB write, log entry marked `status = 'skipped'`

### 5.2 Starch factory prices

```
GET {PRICE_API_URL}/starch-prices?country={NG|TZ|RW|GH|BI}
Authorization: Bearer {PRICE_API_TOKEN}
```

Response — JSON array of all tiers for all factories in the requested country.
The full factory list for the country must be returned — partial responses are
rejected to prevent accidental factory deletion.

```json
[
  {
    "starch_factory":       "MatnaStarch",
    "starch_factory_label": "Matna Starch Ltd.",
    "class":                1,
    "country":              "NG",
    "key":                  "MatnaStarch1",
    "min_starch":           24,
    "range_starch":         ">24",
    "price":                17000,
    "currency":             "NGN"
  },
  {
    "starch_factory":       "MatnaStarch",
    "starch_factory_label": "Matna Starch Ltd.",
    "class":                2,
    "country":              "NG",
    "key":                  "MatnaStarch2",
    "min_starch":           22,
    "range_starch":         "22-23",
    "price":                16000,
    "currency":             "NGN"
  }
]
```

Validation rules enforced by `refresh_starch_prices()`:

- `country` must be one of the 5 supported codes
- `starch_factory` and `key` must be non-empty strings
- `key` values must be unique within the response
- `class` must be a positive integer
- `min_starch` must be numeric ≥ 0
- `price` must be numeric > 0
- Any row failing validation → entire country refresh aborted (log `status = 'error'`), existing data preserved
- Zero valid rows → abort, log `status = 'skipped'`

> **Why abort on any invalid row?** The delete-then-insert strategy means a
> partial write would leave the factory list incomplete. Aborting preserves
> the previous complete dataset and forces the API provider to fix the response.

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

All price DB tests live in `tests/test_prices_db.R`. Each test uses an
in-memory or temp-file SQLite DB — never the production file.

### Default prices

| Test | What is checked |
|------|----------------|
| DB seeds from CSV on first run | `default_prices` row count matches CSV |
| `get_default_prices("NG")` shape | Same columns as old CSV (`Country`, `Item`, `Price`) |
| `get_default_prices("BI")` country alias | `BU` rows in CSV are returned as `BI` |
| `refresh_prices()` upserts new rows | Row count increases; `source = 'api'` |
| `refresh_prices()` updates existing price | Existing row price changes; `updated_at` advances |
| `refresh_prices()` skips invalid rows | Invalid row absent; valid rows present |
| `prices_are_stale()` returns FALSE for fresh data | `updated_at = now()` |
| `prices_are_stale()` returns TRUE for old data | `updated_at` set 30 days ago |
| Auto-refresh skipped when `PRICE_API_URL` unset | No DB write, no error |
| Refresh log entry written on success | `price_refresh_log` has 1 row, `status = 'ok'` |
| Refresh log entry written on API error | `price_refresh_log` has 1 row, `status = 'error'` |

### Starch prices

| Test | What is checked |
|------|----------------|
| DB seeds from starchPrices.csv | `starch_prices` row count matches CSV |
| `get_starch_prices()` column names | Match old CSV: `starchFactory`, `KEY`, `minStarch`, etc. |
| `refresh_starch_prices()` replaces country rows | Old rows gone; new rows present; `source = 'api'` |
| `refresh_starch_prices()` adds new factory | Factory present after refresh |
| `refresh_starch_prices()` removes retired factory | Factory absent after refresh |
| `refresh_starch_prices()` aborts on invalid row | DB unchanged; log `status = 'error'` |
| `refresh_starch_prices()` aborts on zero valid rows | DB unchanged; log `status = 'skipped'` |
| `starch_prices_are_stale()` returns FALSE for fresh data | `updated_at = now()` |
| `starch_prices_are_stale()` returns TRUE for old data | `updated_at` set 60 days ago |
| Other country unaffected by single-country refresh | TZ rows intact after NG refresh |

### Admin endpoint

| Test | Location |
|------|----------|
| `POST /admin/refresh-prices {"type":"default"}` refreshes only default | `tests/test_api.R` |
| `POST /admin/refresh-prices {"type":"starch"}` refreshes only starch | `tests/test_api.R` |
| `POST /admin/refresh-prices {"type":"all"}` refreshes both | `tests/test_api.R` |
| Unknown `type` value returns error | `tests/test_api.R` |
