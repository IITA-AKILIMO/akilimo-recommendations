# Akilimo Recommendations API — Code Review

**Review date:** 2026-03-27 (compacted 2026-04-03)

**Reviewer:** Claude Code (automated)

**Scope:** `api.R`, `R/AkilimoMain.R`, `R/process-FR.R`, `R/process-IC.R`, `R/process-PP.R`, `R/process-SP.R`, `R/get_data.R`, `R/quefts.R`, `R/optimize_fert.R`, `R/fertilizers.R`, `R/markdown.R`, `R/misc.R`

---

## 1. Architecture Overview

### Request Flow

```
POST /compute (api.R)
    → run_akilimo(json)                          [AkilimoMain.R]
        → validate_request(body)                 country/lat/lon/area/flags/FCY/dates
        → parse_request(body)                    normalise all fields → named list params
        → dispatch_recommendations(params, body)
              p$FR  → get_fertilizers2() → process_FR()
              p$IC  → process_IC_NG() [NG] | process_IC_TZ() [TZ] | bad_request() [RW/GH/BI]
              p$PP  → process_PP()
              p$SPP|p$SPH → process_SP()
        → generate_pdfs() → build_*_pdf() → WeasyPrint CLI → PDF files
        → sendEmailReport(user, PDFs, lang)      if requested
        → build_response(result, version)        wrap in {status, version, ...}
```

### Key Modules

| File | Role |
|------|------|
| `api.R` | Plumber entry; explicit dependency-ordered source list |
| `AkilimoMain.R` | Orchestrator: validate, parse, dispatch, respond |
| `get_data.R` | Data loader with `cached_read` in-memory cache for RDS and CSV files |
| `misc.R` | `tr(key, lang)` translation helper; `get_currency()`; `getRFY()`/`getRDY()` |
| `quefts.R` | Pure QUEFTS crop-growth model; no I/O |
| `optimize_fert.R` | Cost-benefit fertilizer optimiser via `optim()`; calls `quefts.R` |
| `fertilizers.R` | Parses fertilizer fields from JSON body; merges with hardcoded NPK table |
| `markdown.R` | Shared helpers: `FERT_COLOUR`, `FERT_LABEL`, `calc_fertilizer_recom()` |
| `html_helpers.R` | HTML fragment builders for WeasyPrint; `.PDF_LABELS` label lookup |
| `pdf_builders.R` | `build_fr/ic/pp/sp_pdf()` — one builder per recommendation type |
| `sms_email.R` | Email dispatch (smtp/mailtrap/mailgun); SMS |
| `process-*.R` | One per recommendation type: data → model → text → PDF |

### Architectural Strengths

- `cached_read` covers static CSVs, soil RDS files (all countries), and WLY yield RDS files.
- `safe_filename_part` applied consistently in PDF filename construction.
- Credentials read exclusively from environment variables.
- `validate_request` centralised before any business logic; covers FCY range and date format.
- `tr(key, lang, ...)` validates `lang` against CSV column names and warns+falls back to `en`.
- Source order is explicit and dependency-ordered in `api.R`.
- PDF pipeline is WeasyPrint (Python CLI); no browser or Java dependency.
- All four recommendation types fully localised via `tr()` and `.PDF_LABELS`.

---

## 2. Resolved Issues

All CRITICAL and most HIGH issues are fixed. Newly resolved since original review:

| ID | Resolved in | Notes |
|----|-------------|-------|
| LOG-1 | early | `getRFY` now uses `country` param |
| LOG-2 | early | `getSPrecText` returns `rec` |
| LOG-3 | early | `getPPrecommendations` uses `which(ds$CP)[1]` |
| LOG-13/TRANS-1/ERR-9 | early | `tr("werec", lang)` — key corrected |
| LOG-14 | early | `ds[1,]$dTC` — correct column |
| LOG-15 | early | `getSPrecText` inverted branch fixed |
| LOG-19 | early | `maizeUP` defaulted to 0, guarded with `is.na` |
| SEC-1..4 | early | Path validation, filename sanitisation, traceback suppressed |
| SEC-5 | early | `is.na()` guards on all zero-checks |
| ERR-2..6,8 | early | NetCDF leak, rbind swallow, IC null, date format |
| ERR-10 | later | `getSPrecText` — `which(ds$CP)[1]` throughout |
| ERR-11 | later | `build_response` — `%\|\|% ""` NULL guard on `recommendation` |
| QUA-1,2,10,13 | early | `rec_type`, `FR_MarkdownText`, Swahili via `tr()`, `rcost` sign fix |
| QUA-11/TRANS-2 | later | `cisRatePre` en value corrected to `""` |
| QUA-12,15 | n/a | Functions deleted with Rmd pipeline removal |
| QUA-16 | later | `sphdpd` key added; factual error corrected |
| PERF-1,2 | early | `cached_read` on soil RDS for RW/GH/BI |
| PERF-5 | later | `get_yield_data("WLY_365")` wrapped in `cached_read` |
| PERF-6 | later | `get_soil_data("soil_NPK-4")` wrapped in `cached_read` |
| TRANS-3 | later | `tr()` validates `lang`; warns and falls back to `en` |
| TRANS-4 | later | `rectext` row removed (contained raw R code) |
| TRANS-5 | later | `rw` column removed (entirely NA; not reachable) |
| TRANS-6 | early | `inc` key leading space added |
| MNT-1 | early | `api.R` explicit source order |
| MNT-4 | later | `getICrecText` now takes `lang`/`country`; uses `tr()` throughout |
| API-1,3 | early | `bad_request()` prefixing; FCY/date validation |

## Deferred (scoped, not yet fixed)

| ID | Issue |
|----|-------|
| LOG-12 | `getSPrecommendations` QUEFTS row loop — scalar max/min in quefts.R blocks vectorisation; requires major rewrite |
| LOG-18 | `run_Optim_NG2` hardcoded `country = "NG"` — TZ dry-matter data not yet validated; comment added |
| PERF-4 | `setup_temp_dir` global temp deletion — concurrent request corruption risk |
| API-4 | `dispatch_recommendations` uses `else if` — only first active flag processed |
| MNT-2 | Functions with 11–20+ positional parameters (`process-FR.R`) |
| MNT-3 | No semantic versioning / changelog |
| MNT-6 | `getRDY` defined but uncalled — kept as inverse of `getRFY`; bug fixed (LOG-16) |
| DEBT-1 | `getWMrecommendations` reserved dead code |

---

## 3. Security

No open security issues.

---

## 4. Logic and Correctness

All logic issues resolved or formally deferred. See section 2 deferred table for LOG-12 and LOG-18.

---

## 5. Error Handling

### MEDIUM

**ERR-7 — `getWMrecommendations` is deferred dead code loaded on every startup (misc.R)**

Accurate comment in source. No fix needed until the weed-management feature is scoped, but it adds parse overhead on every server start.

---

## 6. Code Quality

### MEDIUM

**QUA-3 — Magic numbers with no named constants**

Notable instances:
- `process-SP.R`: `(13.5 - 1.5) / 2.5`, `1.5 * 2.5` (yield scaling), `seq(235, 455, 7)`, `34:65` (harvest age window)
- `optimize_fert.R`, `quefts.R`: `7.64` (cob→kg conversion factor)

**QUA-4 — Fertilizer NPK content table hardcoded in R source (fertilizers.R ~lines 42–47)**

Should be in `data/input/` as a CSV.

**QUA-5 — `dNRmin` profitability threshold duplicated in four places**

`process-IC.R` (×2), `process-FR.R`, `process-PP.R`. Should be a named constant in `misc.R`.

**QUA-6 — `aki_version` hardcoded date string (AkilimoMain.R)**

`aki_version <- "20251222"`. Tracked under MNT-3 (deferred).

**QUA-8 — Large commented-out code blocks**

`process-SP.R`, `process-PP.R`, `process-FR.R`. Should be removed; git history preserves them.

**QUA-14 — `get_costLMO` mixes tab and space indentation (AkilimoMain.R ~lines 357–444)**

Not a runtime issue; makes diffs harder to read.

### LOW

**QUA-9 — `get_data` requires all arguments even when not needed (get_data.R)**

`get_data(x, country, FCY, lon, lat)` — callers that don't need `FCY`/`lon`/`lat` must still pass placeholders. Should default to `NULL`.

**QUA-17 — Debug `message()` in `getSPrecommendations` hot path (process-SP.R ~line 216)**

`message("this situation needs to be avoided")` fires on every request where the current-practice combination is absent from merged yield data. Replace with a structured `warning()` with diagnostic coordinates, or remove.

---

## 7. Performance

### HIGH

**PERF-3 — `getSPrecommendations` row loops not vectorised (process-SP.R ~lines 150–190)**

Two sequential `for` loops call `QUEFTS()` and `getRFY()` per row. A 2-month planting × 2-month harvest window yields ~256 iterations per SP request. Vectorising `QUEFTS` would be the most impactful fix.

### MEDIUM

**PERF-4 — `setup_temp_dir` race condition (AkilimoMain.R)** — Deferred. See section 2.

---

## 8. API Design

### HIGH

**API-5 — Translation key errors and model crashes return identical HTTP 500 shape**

A misspelled `tr()` key is indistinguishable from a model computation crash in the HTTP 500 response. A startup check that validates all `tr(key)` call sites against `translations.csv` would catch this class of error before production deployment.

### MEDIUM

**API-4 — `dispatch_recommendations` only processes first active flag** — Deferred. See section 2.

**API-6 — `validate_request` does not check `PD_window`/`HD_window` (AkilimoMain.R)**

Both values feed into `seq((-4 * PD_window), (4 * PD_window), by = 2)`. A negative or non-integer value silently produces an unexpectedly large or reversed scheduling grid.

---

## 9. Translation

All translation issues resolved. See section 2.

---

## 10. Maintainability

### MEDIUM

**MNT-3 — No semantic versioning** — Deferred. See section 2.

**MNT-5 — `fertilizers.R` type-name reformatting is undocumented (~line 106)**

`NPK201010` → `NPK20_10_10` transformation is silent; expected input/output format not documented.

**MNT-6 — `getRDY` defined but never called from active code (misc.R)**

`getRDY` converts fresh→dry yield but has no call sites in the active codebase (only `getRFY` is used). Either remove it or mark as deferred like `getWMrecommendations`.

---

## 11. Open Issues Summary

| Category        | Severity | Open IDs |
|-----------------|----------|----------|
| Logic           | —        | All resolved or deferred (LOG-12, LOG-18 in deferred table) |
| Error Handling  | MEDIUM   | ERR-7 |
| Code Quality    | MEDIUM   | QUA-3, QUA-4, QUA-5, QUA-6, QUA-8, QUA-14 |
| Code Quality    | LOW      | QUA-9, QUA-17 |
| Performance     | HIGH     | PERF-3 |
| Performance     | MEDIUM   | PERF-4 (deferred) |
| API Design      | HIGH     | API-5 |
| API Design      | MEDIUM   | API-4 (deferred), API-6 |
| Maintainability | MEDIUM   | MNT-3 (deferred), MNT-5 |

**No blocking issues remain.** All CRITICAL and HIGH security/logic/error issues are resolved.
