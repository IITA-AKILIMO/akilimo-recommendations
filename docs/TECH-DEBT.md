# Technical Debt & Open Issues

**Last reviewed:** 2026-04-05

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

Schema version tracked via `PRAGMA user_version` (current: 2). Migrations run
automatically at startup via `migrate_akilimo_db()`. The original CSV files are
kept in `data/input/` as the seed source and offline reference.

Refresh endpoints (all share `AKILIMO_API_URL` base URL):

| Data | Endpoint | Staleness env var | Default |
|------|----------|-------------------|---------|
| Default prices | `GET /prices?country=XX` | `PRICE_MAX_AGE_DAYS` | 7 days |
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
