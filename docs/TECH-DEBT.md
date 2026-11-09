# Technical Debt & Open Issues

**Last reviewed:** 2026-11-08

This file tracks only open and deferred items. Resolved items are removed — see git log for history.

---

## Action List

### MEDIUM — fix in next maintenance window

- [ ] **PERF-3** `process-SP.R:150–190` — row-by-row QUEFTS + getRFY loops (~256 iterations per SP request); vectorise when LOG-12 is unblocked

---

## Deferred (known, scoped out — do not fix until explicitly planned)

| ID | File | Issue | Blocker |
|----|------|-------|---------|
| API-4 | `AkilimoMain.R` | `dispatch_recommendations` uses `else if` — only the first active flag processed per request | Large orchestration refactor; must audit all downstream callers |
| LOG-12 | `process-SP.R` | QUEFTS row loop — scalar max/min internally blocks vectorisation | Major rewrite of `quefts.R`; blocks PERF-3 |
| LOG-18 | `optimize_fert.R` | `run_Optim_NG2` hardcoded `country = "NG"` for dry→fresh yield conversion | TZ dry-matter data not yet validated; DEFERRED comment in source |
| MNT-2 | `process-FR.R` | Functions with 11–20+ positional parameters | Large refactor, regression risk |
| MNT-3 | `AkilimoMain.R` | No semantic versioning — `AKI_VERSION` is a hardcoded date string | Requires team decision on versioning scheme |
| PERF-4 | `AkilimoMain.R` | `setup_temp_dir` global temp deletion — concurrent requests can delete a live request's temp dir. *Partially improved (2026-04-04): NA mtime guard, `force=TRUE`, millisecond timestamp, `dir.create` failure check. Race window narrowed but not eliminated.* | Per-request scoped cleanup or explicit "done" signal needed |
| MNT-6 | `misc.R` | `getRDY` defined but has no call sites in active code | Kept as inverse of `getRFY`; underlying bug (LOG-16) already fixed |

## Blocked on data or expert input (no code fix possible yet)

| ID | Area | Gap | What unblocks it |
|----|------|-----|------------------|
| DATA-1 | SP | **RW done (2026-11-09):** `tools/build_sp_nc.R` builds `RW_WLY_LINTUL_2020SP.nc` from `Rwanda_WLY_LINTUL.RDS`; RW is enabled in `REC_TYPE_COUNTRIES$SP` and covered by fixtures/tests. Burundi still has no planting-window file and `SoilData_4Country.RDS` has no BI points, so BI requests get a clear 400 and `GET /coverage?type=SP` is empty for BI. | Build `BI_WLY_LINTUL_2020SP.nc` after the owner chooses the BI soil source (`SoilData_4Country.RDS` or `BI_FCY*_soilNPK.RDS`) |
| DATA-2 | Translations | The `rw` (Kinyarwanda) column of `data/input/translations.csv` is empty; `lang: "rw"` falls back to English key by key. | A Kinyarwanda translator filling in the column |
| DATA-3 | Prices | The price service has no currency on any default price and no starch prices for BI, RW or GH. (Its Burundi default prices, stored as `BU`, were fixed to `BI` in new-akilimo-api `3681ce2`.) | Real currency and starch-price data from IITA, entered in new-akilimo-api |
| AGR-1 | `process-PP.R` | PP returns only the tillage recommendation. Candidate additions (from a former TODO): a caution that flat planting may not suit water-logged, weedy, very clayey or dry-season-harvest fields; that harrowing costs/benefits are not modelled; the reason behind the recommendation (cost saving vs revenue gain); that a farmer may prefer a cheaper option with a smaller gain; warnings about unrealistic input prices. | Agronomist-approved wording (en/sw/rw) for each message |
| AGR-2 | `process-IC.R` | `getCISrecommendations()` now computes every revenue and cost for the whole field (cassava revenue used to stay per hectare while sweet potato and fertilizer cost were per field; 1 ha results are unchanged). The coefficients themselves (0.7 sweet potato yield ratio, 0.8 / 0.6 intercrop shares, 40 % / 20 % fertilizer gains) were flagged "check with Pieter". | Agronomist review of the coefficients |

---

## Architecture Notes

### SQLite price database (`data/input/akilimo_compute.sqlite`)

Managed by `R/akilimo_db.R`. Tables:

| Table | Replaces | Key |
|-------|----------|-----|
| `default_prices` | `Default_prices.csv` | `(country, item)` |
| `starch_prices` | `starchPrices.csv` | `key` (`{factory}{class}`) |
| `translations` | `translations.csv` | `key` |
| `price_refresh_log` | — | `id` (autoincrement) |

Schema version tracked via `PRAGMA user_version` (current: 4; v4 adds
`translations.source`). Migrations run automatically at startup via
`migrate_akilimo_db()`. The CSV files in `data/input/` are the seed source: on
every start `sync_seed_data()` adds rows missing from the database and updates
rows whose `source` is still `seed`, but never touches rows refreshed from the
API. So the database is kept across deploys and restarts (it used to be deleted
on every deploy and dev start, losing refreshed prices).

Refresh endpoints (all share `AKILIMO_API_URL` base URL):

| Data | Endpoint | Staleness env var | Default |
|------|----------|-------------------|---------|
| Default prices | `GET /default-prices?country=XX` | `PRICE_MAX_AGE_DAYS` | 7 days |
| Starch prices | `GET /starch-prices?country=XX` | `STARCH_PRICE_MAX_AGE_DAYS` | 30 days |
| Translations | `GET /translations` | `TRANSLATIONS_MAX_AGE_DAYS` | 30 days |

Manual refresh: `Rscript refresh_prices.R [--country XX] [--type default|starch|translations|all] [--dry-run]`

Auto-refresh: runs before each request when `AKILIMO_API_URL` is set and data is
stale. Failure is logged as WARN and never blocks the recommendation.

### PDF pipeline (WeasyPrint)

Implemented in `R/html_helpers.R`, `R/pdf_builders.R`, `R/email.R`.
Renderer: [WeasyPrint](https://weasyprint.org) (`pip install weasyprint`) — pure
Python, no browser or Java required.

Call chain per recommendation type:
```
generate_pdfs()  (email.R)
  └─ build_{fr|ic|pp|sp}_pdf()  (pdf_builders.R)
        └─ html_open() + html_personal_info() + html_*() fragment helpers
        └─ render_pdf(html, path)
              └─ writeLines(html, temp/render_tmp.html)
              └─ system2("weasyprint", ...) → PDF file
```

All assets (banner, bag PNGs, chart PNGs, map PNG) are embedded as base64 data
URIs. PDF failures are isolated per type — one failure never suppresses the
others or the JSON recommendation response.

CSS constraints (WeasyPrint ≥ 60): `display: grid`, `@page`, `break-before:
page` all supported. Avoid `position: fixed`, `filter`, `backdrop-filter`,
`clip-path`.

Map resolution order: Mapbox Static API (`MAPBOX_TOKEN`) → generic HTTP
(`MAP_API_URL`) → offline coordinate card (no network).

Debugging a failed PDF:
```bash
weasyprint temp/<request-id>/render_tmp.html /tmp/debug.pdf
```

The HTML temp file is preserved on failure and its path is logged at ERROR level.
