# Technical Debt & Open Issues

**Last reviewed:** 2026-04-04

This file tracks all confirmed open issues and deferred technical debt.
Resolved items are removed; see git log for history.

---

## Action List

Check off each item after the fix is committed. Grouped by priority; tackle
HIGH items before MEDIUM before LOW. Deferred items are listed separately and
must not be worked on until explicitly planned.

### HIGH — fix before next production release

- [x] **NEW-PP-1** `process-PP.R:31` — yield-class threshold `12.5` vs soil-data break `15`; misclassifies medium-yield farms *(fixed c56785b)*
- [x] **NEW-IC-1** `process-IC.R:182` — `maizeUW` divided without zero/NA guard; produces silent NaN *(fixed fb58bf2)*
- [x] **NEW-SP-1** `process-SP.R:154`, `optimize_fert.R` — QUEFTS calls inside loops with no `tryCatch()`; bad soil data crashes or silently corrupts recommendations *(fixed 6f34483)*
- [x] **NEW-NA-1** `AkilimoMain.R` (multiple) — `as.numeric(from_json(...))` produces silent NA on non-numeric input; add `safe_numeric()` helper *(fixed 8dd917f)*

### MEDIUM — fix in next maintenance window

- [ ] **PERF-3** `process-SP.R:150–190` — row-by-row QUEFTS + getRFY loops (~256 iterations per SP request); vectorise when LOG-12 is unblocked
- [ ] **NEW-DB-1** `akilimo_db.R:268,294,317` — DB functions only check for NULL connection, not for stale/dropped connections; add `tryCatch` + reconnect
- [ ] **NEW-FERT-1** `fertilizers.R:82–89` — custom fertilizer merge failure logged as WARN only; recommendation silently uses default data; escalate to ERROR
- [ ] **NEW-PARSE-1** `AkilimoMain.R:95–98` — `as.Date()` can return NA for edge-case strings even after regex check; assert non-NA immediately after conversion
- [ ] **QUA-3** Multiple — magic numbers without named constants: `12.5`/`15` (PP), `7.64` (IC/pdf_builders), `seq(235,455,7)`, `34:65`, QUEFTS physiology constants
- [ ] **NEW-SP-2** `process-SP.R:189` — yield scaling formula undocumented; no guard for FCY outside `[1.5, 13.5]`; add comment + `warning()` for out-of-range input
- [ ] **QUA-6** `AkilimoMain.R:72`, `api.R` — version string hardcoded in two independent places; centralise (defer full semantic versioning to MNT-3)
- [ ] **QUA-8** `process-SP.R`, `process-PP.R`, `process-FR.R` — large commented-out code blocks; delete (git history preserves them)
- [ ] **ERR-7** `misc.R:139–162` — `getWMrecommendations` dead code with an AND/OR logic bug; remove until weed-management is scoped

### LOW — clean up when convenient

- [ ] **NEW-RESP-1** `AkilimoMain.R`, `api.R` — error and success response envelopes have different shapes; normalise to `{ status, version, data: {...} }`
- [ ] **NEW-SEC-1** `html_helpers.R:255–268` — `mask_email()` retains full domain, leaking org affiliation; mask domain or drop email from PDF
- [ ] **NEW-CASH-1** `html_helpers.R:472–480` — cash-stack ratio produces NaN/Inf when both amounts are zero; handle as special case
- [ ] **NEW-COUNTRY-1** `AkilimoMain.R` — country code validation is case-sensitive; normalise with `toupper(trimws(...))` before the check
- [ ] **NEW-UTIL-1** `html_helpers.R`, `markdown.R` — `%||%` defined in multiple files with subtly different semantics; consolidate into `misc.R`
- [ ] **NEW-OPT-1** `optimize_fert.R` — dead `country` parameter in `run_Optim_NG2()`; remove from signature and call sites in `process-FR.R`

---

## Open Issues — actionable, not yet scheduled

### CRITICAL / Blocking

_(None currently — all blocking issues from the original review are resolved.)_

---

### HIGH

**NEW-PP-1 — Yield-class threshold mismatch in PP (`process-PP.R:31`)**

```r
yd <- yd[yd$YL == ifelse(FCY < 12.5, "low", "high"),]
```

`get_soil_data()` uses break points `c(-Inf, 7.5, 15, 22.5, 30, Inf)` to define
yield classes. The PP processor independently applies a threshold of `12.5`, not
`15`. FCY values in the 12.5–15 range are classified "low" in PP but "high" by
the soil-data loader, producing inconsistent land-management recommendations for
medium-yield farms. The values `12.5` and `15` both appear without any shared
constant.

Fix: align both thresholds to `15` (matching the soil-data breaks), or extract a
named constant and use it in both places.

---

**NEW-IC-1 — `maizeUW` used without zero/NA guard (`process-IC.R:182`)**

```r
cobUP <- ifelse(maizePD == "fresh_cob", maizeUP, maizeUP / maizeUW / 7.64)
```

`maizeUW` defaults to NA when absent from the request body. Division by NA
produces NaN; division by zero (if `maizeUW = 0` ever slips through) is undefined.
Neither case is caught before it propagates into the recommendation text.

Fix: validate `maizeUW > 0` before this line and return `bad_request()` if not.

---

**NEW-SP-1 — QUEFTS calls unguarded in optimisation loops (`process-SP.R:154`, `optimize_fert.R`)**

`QUEFTS()` is called inside loops and `optim()` callbacks with no `tryCatch()`.
If soil data contains invalid values (NA, negative, or extreme outliers), QUEFTS
can produce NaN/Inf silently, causing the optimiser to converge on a spurious
minimum and return a financially nonsensical recommendation, or crash entirely.

Fix: wrap each `QUEFTS()` call site in `tryCatch()`. Validate that soilN, soilP,
soilK are positive and finite before dispatch.

---

**NEW-NA-1 — Silent NA propagation from `as.numeric()` coercion (`AkilimoMain.R`, multiple)**

Lines like:
```r
cassUW <- as.numeric(from_json("cassUW", body, default_value = 1000))
maizeUP <- as.numeric(from_json("maizeUP", body, default_value = 0))
```
produce NA without error when the JSON field contains a non-numeric string (e.g.
`"cassUW": "invalid"`). The NA propagates silently through subsequent arithmetic
and is only caught, if at all, much later with an unhelpful error.

Fix: add a `safe_numeric(x, field_name)` helper that calls `as.numeric()`,
checks `is.na()`, and returns `bad_request()` on failure. Apply it to all numeric
`from_json` calls.

---

### MEDIUM

**PERF-3 — `getSPrecommendations` row loops not vectorised (`process-SP.R` ~lines 150–190)**

Two sequential `for` loops call `QUEFTS()` and `getRFY()` per row. A 2-month
planting × 2-month harvest window yields ~256 iterations per SP request.
Vectorising `QUEFTS` would be the most impactful single performance improvement
in the codebase. Blocked by LOG-12 (QUEFTS uses scalar max/min internally —
deferred until quefts.R can be rewritten).

---

**NEW-DB-1 — DB connection validity not checked after initial open (`akilimo_db.R:268, 294, 317`)**

`get_default_prices()`, `get_starch_prices()`, and `get_translations()` check for
a NULL connection but do not detect a stale or invalidated connection (e.g., if
the SQLite file is deleted or the file handle is closed by the OS after a long
idle period). The resulting error message from DBI is cryptic.

Fix: wrap all `dbGetQuery` calls in `tryCatch` and include a reconnection attempt
on connection-related errors, or log a clear diagnostic message.

---

**NEW-FERT-1 — Custom fertilizer merge failure silent (`fertilizers.R:82–89`)**

When the `rbind()` of custom and default fertilizers fails, the error is caught
and logged as a warning, but the recommendation proceeds with only the default
fertilizers. The user receives a result that silently ignores their custom inputs.

Fix: log at ERROR (not WARN). Include a note in the response that custom
fertilizer data was rejected, so the caller can investigate.

---

**NEW-PARSE-1 — NA from `as.Date()` not caught before dispatch (`AkilimoMain.R:95–98`)**

`validate_request()` checks date string format with a regex, but does not guard
against `as.Date()` returning NA for edge-case strings (e.g. `"0000-00-00"`).
The NA propagates into processors that call `strftime(PD, ...)` and fail with a
confusing error.

Fix: after `as.Date()`, assert `!is.na(PD) && !is.na(HD)` and return
`bad_request()` on failure.

---

**QUA-3 — Magic numbers with no named constants**

Notable instances:
- `process-SP.R:189` — `(13.5 - 1.5) / 2.5`, `1.5 * 2.5` (yield scaling formula; also see NEW-SP-2 below)
- `process-SP.R:147` — `seq(235, 455, 7)`, `34:65` (harvest age window in days/weeks)
- `process-IC.R:182`, `pdf_builders.R:297` — `7.64` (maize cobs→grain kg conversion; used in two places)
- `optimize_fert.R`, `quefts.R` — various QUEFTS crop-physiology constants

---

**NEW-SP-2 — Yield scaling formula undocumented and edge-case unclear (`process-SP.R:189`)**

```r
ds$RY <- (ds$RFWY - ds$RFCY) / (13.5 - 1.5) / 2.5 * (FCY - 1.5 * 2.5) + ds$RFCY
```

When FCY = 3.75 (`1.5 * 2.5`), the multiplier is zero and `RY == RFCY` regardless
of yield potential — the formula silently ignores the water-limited gain. No
comment explains the domain (`1.5 ≤ FCY ≤ 13.5`) or the derivation.
Out-of-range FCY values produce results outside the interpolation range with no
warning.

Fix: add a comment explaining the formula. Add a `stopifnot` or `warning()` for
FCY outside the expected range.

---

**QUA-6 — `aki_version` hardcoded date string appears in two places**

`AkilimoMain.R:72` and `api.R:/health` handler both hardcode version strings
independently. Updating one without the other causes version skew. Tied to MNT-3.

---

**QUA-8 — Large commented-out code blocks**

- `process-SP.R` lines ~127–141, ~157–159, ~171–180 (coordinate rounding, WLY load, debug)
- `process-PP.R` lines ~41–74
- `process-FR.R` — various smaller blocks

Git history preserves all of this; delete the blocks.

---

**ERR-7 / DEBT-1 — `getWMrecommendations` dead code (`misc.R:139–162`)**

Not reachable from any active request path. Also contains a logic bug:
the spray-threshold condition uses `&` (AND) where `|` (OR) is almost certainly
intended, making the spray recommendation unreachable in normal conditions.
Remove the function or gate it behind a feature flag when the weed-management
work is scoped.

---

### LOW

**NEW-RESP-1 — Inconsistent error vs success response envelope**

`bad_request()` produces `{ status: "400 ...", data: { message: "..." } }`.
The success envelope in `build_response()` puts recommendation fields at the top
level alongside `status` and `version`. Clients cannot parse both shapes with the
same code path.

Fix: normalise to `{ status, version, data: {...} }` for both success and error,
putting `message` inside `data` for errors.

---

**NEW-SEC-1 — Email address masking reveals full domain (`html_helpers.R:255–268`)**

`mask_email("user@cgiar.org")` → `"u***r@cgiar.org"`. The full domain is retained,
leaking organisational affiliation. For single-character local parts the mask is
trivially reversible.

Fix: mask part of the domain too (e.g., keep TLD only), or suppress the email
field from PDFs entirely and rely on the delivery log.

---

**NEW-CASH-1 — Cash-stack ratio silently produces NaN when amounts are zero (`html_helpers.R:472–480`)**

When both `sum_total` and `netRevenue` are zero (e.g., a request with no
fertilizer cost and zero expected revenue), ratio calculations produce `Inf` or
`NaN` before the clamp to 1–10. The clamp hides the problem; the user sees a
misleading cash-stack image.

Fix: detect the zero-amount case and render a "no cost / no gain" message instead
of a cash-stack.

---

**NEW-COUNTRY-1 — Country code comparison is case-sensitive without normalisation**

`validate_request()` compares `country` against `c("NG","TZ","RW","GH","BI")`
exactly. A request with `"ng"` or `"Ng"` is rejected with no hint that
normalisation would fix it.

Fix: `country <- toupper(trimws(country))` before validation, or add a note in
the error message.

---

**NEW-UTIL-1 — `%||%` defined in multiple source files**

The null-coalescing operator is defined in at least `html_helpers.R` and
`markdown.R`. The exact semantics (NULL-only vs. NULL+NA+length-0) differ
slightly between definitions. Define it once in `misc.R` (already the shared
utilities file) and remove the duplicates.

---

**NEW-OPT-1 — Dead `country` parameter in `run_Optim_NG2()` (`optimize_fert.R`)**

The function accepts `country` but the DEFERRED comment explicitly documents that
it is unused (hardcoded "NG" for all callers). The parameter is dead and
misleading.

Fix: remove the parameter from the function signature and all call sites in
`process-FR.R`.

---

## Deferred (known, scoped out — do not fix until explicitly planned)

| ID | File | Issue | Blocker |
|----|------|-------|---------|
| API-4 | `AkilimoMain.R` | `dispatch_recommendations` uses `else if` — only the first active flag processed per request | Large orchestration refactor; must audit all downstream callers |
| LOG-12 | `process-SP.R` | QUEFTS row loop — scalar max/min internally blocks vectorisation | Major rewrite of `quefts.R`; blocks PERF-3 |
| LOG-18 | `optimize_fert.R` | `run_Optim_NG2` hardcoded `country = "NG"` at three call sites | TZ dry-matter data not yet validated; DEFERRED comment in source |
| MNT-2 | `process-FR.R` | Functions with 11–20+ positional parameters | Large refactor, regression risk |
| MNT-3 | `AkilimoMain.R` | No semantic versioning — `aki_version` is a hardcoded date string | Requires team decision on versioning scheme; also affects QUA-6 |
| PERF-4 | `AkilimoMain.R` | `setup_temp_dir` global temp deletion — concurrent requests can delete a live request's temp dir | Per-request scoped cleanup needed; subdirectories already created |
| MNT-6 | `misc.R` | `getRDY` defined but has no call sites in active code | Kept as inverse of `getRFY`; underlying bug (LOG-16) already fixed |
| DEBT-1 | `misc.R` | `getWMrecommendations` reserved dead code (also see ERR-7 above) | Remove when weed-management feature is scoped |

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
