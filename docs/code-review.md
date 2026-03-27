# Akilimo Recommendations API — Code Review

**Review date:** 2026-03-27
**Reviewer:** Claude Code (automated)
**Scope:** `api.R`, `R/AkilimoMain.R`, `R/process-FR.R`, `R/process-IC.R`, `R/process-PP.R`, `R/process-SP.R`, `R/get_data.R`, `R/quefts.R`, `R/optimize_fert.R`, `R/fertilizers.R`, `R/markdown.R`, `R/misc.R`

---

## 1. Architecture Overview

### Request Flow

```
HTTP POST /compute
        |
        v
   api.R (Plumber handler)
        |  tryCatch outer — catches unhandled errors → HTTP 500
        |
        v
   run_akilimo(json)                      [AkilimoMain.R]
        |
        +-- jsonlite::fromJSON(json)       parse raw body string
        +-- validate_request(body)         country/lat/lon/area/flags/FCY/dates check
        +-- parse_request(body)            normalise all fields → named list p
        |        |
        |        +-- get_cassUPUW()        resolve cassava price/weight
        |        +-- from_json()           field extractor helper
        |
        +-- dispatch_recommendations(p, body)
                 |
                 +-- p$FR  → get_fertilizers2() → process_FR()
                 |                                     |
                 |                                     +-- getFRrecommendations()
                 |                                     |       +-- get_data("WLY_365")
                 |                                     |       +-- get_data("soil_NPK")
                 |                                     |       +-- QUEFTS()
                 |                                     |       +-- run_Optim_NG2()
                 |                                     |               +-- optim_NR() [objective fn]
                 |                                     +-- getFRrecText()
                 |                                     +-- FR_MarkdownText()
                 |                                     +-- fertilizerAdviseTable()
                 |
                 +-- p$IC  → get_fertilizers2() → process_IC_NG()  [NG]
                 |                              → bad_request()     [RW/GH/BI]
                 |                              → process_IC_TZ()  [TZ]
                 |                                     +-- getICrecommendations() / getCISrecommendations()
                 |                                     +-- getICrecText()  / getCISrecText()
                 |                                     +-- IC_MarkdownText() / CIS_MarkdownText()
                 |
                 +-- p$PP  → process_PP()
                 |                +-- getPPrecommendations()
                 |                +-- getPPrecText()
                 |                +-- PP_MarkdownText()
                 |
                 +-- p$SPP|p$SPH → process_SP()
                                      +-- getSPrecommendations()
                                      |       +-- get_data("soil_NPK-4")
                                      |       +-- get_data("WLY_15M_ncdf")
                                      |       +-- QUEFTS() [loop over rows]
                                      +-- getSPrecText()
                                      +-- SP_MarkdownText()
        |
        v
   build_response(result, version)        wrap in {status, version, ...}
        |
        v
   HTTP 200 JSON response
```

### Module Interactions

- **`api.R`** — minimal entry point; sources R files in explicit dependency order.
- **`AkilimoMain.R`** — orchestrator: validates, parses, dispatches, wraps. Also owns helpers `from_json`, `get_user`, `get_cassUPUW`, `get_costLMO`, `bad_request`, `build_response`.
- **`get_data.R`** — single dispatcher with in-memory cache for static CSVs and RDS soil files; routes to typed loaders.
- **`misc.R`** — small helpers: `get_currency`, `getRFY`, `getRDY`, `tr()` translation function, and deferred `getWMrecommendations`.
- **`quefts.R`** — pure crop-growth model; no I/O; composed of nested closures.
- **`optimize_fert.R`** — calls `optim()` with `quefts.R` as the objective. Still hardcodes `country = "NG"` for dry→fresh conversion in its local cache.
- **`fertilizers.R`** — parses fertilizer fields from the JSON body; merges with a hardcoded NPK content table.
- **`process-*.R`** — one file per recommendation type; each calls data loaders, model functions, and markdown writers.
- **`markdown.R`** — writes CSV artefacts to `./temp/` consumed by Rmd templates.

### Architectural Strengths

- `cached_read` in `get_data.R` now covers static CSVs **and** soil RDS files for all countries.
- `safe_filename_part` in `markdown.R` is applied consistently in all four markdown writers.
- Credentials are read exclusively from environment variables (no hardcoded secrets found).
- Validation is centralised in `validate_request` before any business logic runs, and now covers FCY range and date format.
- `from_json` provides a uniform default-value mechanism for optional fields.
- Source order is now explicit and dependency-ordered in `api.R`.
- `tr(key, lang, ...)` provides a clean, named-token translation interface.

---

## 2. Status of Previously Identified Issues

### Resolved (all CRITICAL and most HIGH issues fixed)

| ID     | Status      | Notes |
|--------|-------------|-------|
| LOG-1  | RESOLVED    | `getRFY` now uses the `country` parameter; `fd$country == country` |
| LOG-2  | RESOLVED    | `getSPrecText` now ends with `rec` as the last expression, returning its value |
| LOG-3  | RESOLVED    | `getPPrecommendations` now uses `which(ds$CP)[1]` with a fallback to lowest-NR row |
| ERR-1  | RESOLVED    | Consequence of LOG-2; resolved together |
| ERR-5  | RESOLVED    | `ds[cni,]$method_ridging` corrected to `ds[1,]$method_ridging` |
| SEC-4  | RESOLVED    | Traceback now logged server-side only; HTTP 500 body contains only `message` |
| QUA-1  | RESOLVED    | `process_IC_NG` now returns `rec_type = "IC"` |
| SEC-1  | RESOLVED    | `get_soil_data` now validates country before path interpolation |
| SEC-2  | RESOLVED    | `sendEmailReport` now uses `safe_filename_part(user$PhoneNr)` |
| SEC-3  | RESOLVED    | Rate-limiting delegated to NGINX (documented in checklist) |
| LOG-4  | RESOLVED    | Guard added: `if (!is.numeric(cassUW) || cassUW <= 0) cassUW <- 1000` |
| LOG-5  | RESOLVED    | Changed to `if (!is.na(maizeUW) && maizeUW == 0) maizeUW <- NA` |
| LOG-7  | RESOLVED    | Price defaults applied before `tuberUP` computation in TZ IC block |
| ERR-2  | RESOLVED    | `on.exit(ncdf4::nc_close(nc), add = TRUE)` added |
| ERR-3  | RESOLVED    | `try(rbind(...))` replaced with `tryCatch` that warns and falls back |
| ERR-4  | RESOLVED    | IC for RW/GH/BI now returns an explicit `bad_request()` response |
| ERR-6  | RESOLVED    | Subsumed by API-3 fix (date format validation in `validate_request`) |
| ERR-8  | RESOLVED    | `dd_ply` removed |
| QUA-2  | RESOLVED    | `FR_MarkdownText` and `fertilizerAdviseTable` calls restored in `process_FR` |
| API-1  | RESOLVED    | ERR-4 fix ensures all 400 paths emit a correctly prefixed status string |
| API-3  | RESOLVED    | `validate_request` now checks FCY range and PD/HD date format |
| MNT-1  | RESOLVED    | `api.R` now sources an explicit, ordered list of files |
| PERF-1 | RESOLVED    | `cached_read` applied to RDS soil files for RW/GH/BI |
| PERF-2 | RESOLVED    | `predicted_soil_properties` RDS wrapped in `cached_read` |
| QUA-10 | RESOLVED    | Swahili strings moved into TRNS CSV; `getCISrecText` and `getSPrecText` now use `tr()` |

### Deferred (tracked, not yet fixed)

| ID     | Status      | Notes |
|--------|-------------|-------|
| PERF-4 | DEFERRED    | `setup_temp_dir` global temp-file deletion; concurrent request corruption risk |
| API-4  | DEFERRED    | `dispatch_recommendations` processes only first active flag |
| MNT-2  | DEFERRED    | Functions with 11–20+ positional parameters |

### Still Open (not fixed, not deferred)

See section 3–8 below, which carry forward unfixed issues and adds newly found ones.

---

## 3. Security

### CRITICAL

_None._

### HIGH

**SEC-5 — `get_costLMO` zero-check compares potentially-NA values without `is.na` guard (AkilimoMain.R lines 370–378)** ✅ Fixed

~~The `from_json` calls for cost fields default to `NA`. Immediately after, lines like `if (cost_manual_ploughing == 0) cost_manual_ploughing <- NA` compare `NA == 0`, which produces `NA` (not `FALSE`), causing the `if` to raise a warning and skip the assignment.~~

All ten zero-checks on lines 370–379 now use `!is.na(x) && x == 0` guards.

### MEDIUM

_None new._

---

## 4. Logic and Correctness

### CRITICAL

**LOG-13 — `tr("werec_", lang)` references a missing translation key — all PP recommendations involving a change will crash (process-PP.R lines 124, 126, 131)**

The key `"werec_"` (with a trailing underscore) is called in `getPPrecText` on lines 124, 126, and 131. The translations CSV (`data/input/translations.csv`) contains only `"werec"` (no underscore). `tr()` will call `stop(sprintf("Missing translation key 'werec_'"))`, which propagates through `getPPrecText` → `process_PP` → `run_akilimo` → returns an HTTP 500 error for every PP request where the recommendation differs from the current practice (i.e., almost all PP requests).

This is a regression introduced by the translation refactor.

**Fix:** Add `werec_` to `translations.csv` (it appears to be the same text as `werec` but may warrant a trailing space or different phrasing for the PP context), or change the three call sites to use `tr("werec", lang)`:

```r
# translations.csv — add row:
werec_,We recommend ,Tunapendekeza ,

# OR change the call sites in process-PP.R lines 124, 126, 131 from:
tr("werec_", lang)
# to:
tr("werec", lang)
```

**LOG-14 — `ds[1,]$cost` references a non-existent column in `getPPrecText` — crashes on all recommendation-with-cost-change PP requests (process-PP.R line 137)**

The data frame returned by `getPPrecommendations` has columns `TC`, `GR`, `NR`, `dTC`, `dNR`, `cost_ploughing`, `cost_ridging`, `cost_weeding`. There is no column named `cost`. Line 137 reads:

```r
paste0(tr("changcost", lang), tr("this", lang), tr("decr", lang), tr("incr", lang), ds[1,]$cost, tr("costb", lang), tr("rtprod", lang), "\n")
```

`ds[1,]$cost` returns `NULL`, which `paste0` silently converts to `""`. The resulting recommendation text will say "This will decrease/increase cost by " with no figure, then "Root production and net revenue will not change" — which contradicts the fact that there is a recommendation with a changed cost. It also means the actual dTC value is never shown.

**Fix:** Replace `ds[1,]$cost` with the correct column name. Based on context (this is the cost change for the recommended tillage), the intended value is `ds[1,]$dTC`:

```r
paste0(..., ds[1,]$dTC, ...)
```

**LOG-15 — `getSPrecText` inverts the "both dates changed" branch — emits "no change" text when both PD and HD differ (process-SP.R lines 76–79)**

Inside the `else` branch (entered when `ds[1,]$CP` is FALSE, meaning the top recommendation differs from the current practice), the code at line 76 checks:

```r
if ((ds[1,]$PD != ds[ds$CP,]$PD) & (ds[1,]$HD != ds[ds$CP,]$HD)) {
    rec <- paste0(tr("recrev", ...), tr("hvsdate", ...), tr("nochange", lang))
}
```

When both dates differ — the normal case for a productive recommendation — this emits the identical "your revenue will be highest at your proposed planting ... We do not recommend any changes" text that is correctly emitted at line 18–21 only when the current practice IS optimal. The branch should instead build `rec` from `recP`, `recH`, and `recR` (the objects already computed on lines 31–73), as the `else` branch at line 80 does.

The condition at line 76 appears to be a copy-paste of the wrong logic. The entire `if` at lines 76–79 should be deleted; the `else` at line 80 is already the correct fallback:

```r
# Delete lines 76-79. Correct remaining code:
rec <- paste0(recP, recH, recR)
```

### HIGH

**LOG-16 — `getRDY` accepts an integer day-of-year but `process-SP.R` passes a `Date` object (misc.R lines 55–63)**

`getRDY` is defined with the guard `if (HD > 366) HD <- HD - 366` — this only makes sense if `HD` is a numeric day-of-year. However the function is called from `process-SP.R` with `ds$HD` which is a `Date` object. Comparing a `Date` to `366` will succeed in R (it compares the underlying integer), but the subtraction `HD - 366` returns another `Date` shifted back 366 days, not a valid day-of-year. `getRDY` is currently called only in `getSPrecommendations` but only under the `saleSF` branch (starch factory). The mismatch should be documented or `getRDY` should use `as.numeric(strftime(HD, "%j"))` as `getRFY` already does.

**LOG-17 — `get_yield_data("WLY_365")` reads RDS files fresh on every request with no caching (get_data.R lines 159–180)**

The `get_yield_data` function is called for every FR, IC, and SP request and reads large country-specific RDS files (`Nigeria_WLY_LINTUL_2020.RDS`, `Tanzania_WLY_LINTUL_2020.RDS`, etc.) with plain `readRDS()` — no `cached_read` wrapper. These are the same static assets that soil RDS files now correctly cache. Under production load with sequential requests, every FR request re-reads the full Nigeria WLY RDS from disk.

**Fix:** Wrap each `readRDS` in `get_yield_data` with `cached_read`:

```r
if (country == "NG") {
    w <- cached_read("WLY_365_NG", function() readRDS(data_path("yield/Nigeria_WLY_LINTUL_2020.RDS")))
} else if (country == "TZ") {
    w <- cached_read("WLY_365_TZ", function() readRDS(data_path("yield/Tanzania_WLY_LINTUL_2020.RDS")))
} # ... etc.
```

**LOG-18 — `optimize_fert.R` still hardcodes `country = "NG"` for dry→fresh conversion in three places (optimize_fert.R lines 29–30, 35, 63)**

Although `getRFY` was fixed (LOG-1) to use the `country` parameter, `run_Optim_NG2` still passes `country = "NG"` explicitly at lines 29, 30, 35, and 63. The comment on line 29 says "TZ model is extremely high" — this is the reason the hardcoding persists, but it is not documented as intentional technical debt. For FR requests from TZ, RW, GH, or BI, the dry-to-fresh conversion factor used in the optimiser objective will be wrong, inflating or deflating the estimated net revenue and therefore producing a suboptimal fertilizer recommendation.

This was acknowledged in LOG-1 of the previous review as a data quality issue (TZ dry-matter data not validated), but the `run_Optim_NG2` function is **not** Nigeria-only — it is called for all countries via `process_FR`. The discrepancy between the fixed `getRFY` (now country-aware) and the optimiser (still hardcoded to NG) means FR recommendations for non-NG countries use consistent country-specific values everywhere EXCEPT inside the optimiser, which is where it matters most.

**Fix in the short term:** Add a comment block above `run_Optim_NG2` clearly stating the known limitation and which countries are affected. **Fix in the long term:** Pass validated `country`-specific dry-matter data into the optimiser or use the corrected `getRFY` directly.

### MEDIUM

**LOG-6 — Redundant `p$country == "TZ"` check inside TZ-only branch (AkilimoMain.R lines 183–186)**

Inside the `else if (p$country == "TZ")` block, line 183 checks `if (is.na(sweetPotatoUP) || sweetPotatoUP == 0)`. The `sweetPotatoUP` default check is fine. However the surrounding block also contains a second `if (!is.na(sweetPotatoUW) && sweetPotatoUW == 0)` guard at line 180 which now correctly uses `is.na()` — this is a minor inconsistency with the earlier code style but not a bug.

The structural redundancy noted in the original review (a TZ country check inside a TZ branch) is still present but harmless.

**LOG-9 — `from_json` double-null check is redundant (AkilimoMain.R lines 296–304)**

Still present: the inner `if (!is.null(value))` is always true when the outer `if (!is.null(body[[field_name]]))` is true. Reduces readability.

**LOG-10 — `NRabove18Cost` hardcodes column subset that will break if column names change (process-FR.R lines 111, 118)**

Still present: `subset(ds, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))` hardcodes the column list.

**LOG-11 — Maize output always reported in cobs even when `maizePD == "grain"` (process-IC.R lines 131–151)**

Still present. The `KNOWN BUG` comment was converted to a clearer NOTE but the bug is not fixed. `dMP` is divided by 7.64 to convert cobs to kg, but the unit "cobs" is used in the `grain` branch's text.

**LOG-12 — `getSPrecommendations` runs QUEFTS in an O(n) row loop (process-SP.R lines 155–159)**

Still present. The per-row QUEFTS loop iterates over every planting/harvest combination within the scheduling window. No vectorisation.

---

## 5. Error Handling

### CRITICAL

**ERR-9 — `tr("werec_", lang)` will throw a fatal error stopping all PP responses (process-PP.R — see LOG-13)**

This is both a logic error and an unhandled exception. Every PP request where the recommendation differs from the current practice will produce an HTTP 500 from `tr()`'s `stop()` call. Because this is caught by the outer `tryCatch` in `api.R`, the client receives:

```json
{"status": "error", "data": {"message": "Missing translation key 'werec_'"}}
```

with HTTP 500, rather than a recommendation.

### HIGH

**ERR-10 — `getSPrecText` line 30: `ds[ds$CP == TRUE,]$PD` may return a vector of length > 1 (process-SP.R lines 30, 40, 49, 76)**

`ds$CP` is set at `getSPrecommendations` line 219 as `(ds$rPWnr == 0) & (ds$rHWnr == 0)` — at most one row should be TRUE by construction. However after the `merge(ds, yld)` at line 179, and given that `ds` is an `expand.grid` result, there can be duplicate `(rPWnr=0, rHWnr=0)` rows if `yld` has multiple rows with the same `(plw, haw)` combination (which can happen if `WLY$WLY[k]` produces the same values). When `ds[ds$CP == TRUE,]` is multi-row, `ds[ds$CP == TRUE,]$PD` is a vector, and `ds[1,]$PD != ds[ds$CP == TRUE,]$PD` produces a vector comparison, causing `paste0(tr(...), ...)` to produce a vector of strings rather than a single string. The returned `rec` would then be a character vector, not a scalar.

The fix from LOG-3 (using `cp_idx[1]`) was applied in `getPPrecommendations` but not in `getSPrecText`. Consistent defensive indexing is needed:

```r
cp_row <- ds[which(ds$CP)[1], ]  # always a single row
# Replace all ds[ds$CP == TRUE,] and ds[ds$CP,] with cp_row
```

**ERR-11 — `build_response` calls `gsub` on `result$recommendation` without checking for `NULL` (AkilimoMain.R line 268)**

```r
result$recommendation <- jsonlite::unbox(gsub("[ ]+", " ", result$recommendation))
```

If any processor returns a list where `recommendation` is `NULL` (e.g., because `recText` was not set before the function returned), `gsub(pattern, "", NULL)` returns `character(0)` and `jsonlite::unbox(character(0))` throws an error. Currently `process_SP` guarantees `recText` is always set, but this is fragile. A guard is needed:

```r
result$recommendation <- jsonlite::unbox(gsub("[ ]+", " ", result$recommendation %||% ""))
```

### MEDIUM

**ERR-7 — `getWMrecommendations` is deferred dead code (misc.R lines 66–98)**

Still present. The comment is now accurate ("DEFERRED technical debt"). No fix needed until the feature is scoped, but it loads into every server process.

---

## 6. Code Quality

### HIGH

**QUA-11 — `cisRatePre` / `cisRatePost` translation keys produce semantically wrong output (process-IC.R line 376, translations.csv lines 81–82)**

`getCISrecText` builds fertilizer rate lines as:

```r
paste0(tr("cisRatePre", lang), round(fs$rate), tr("cisRatePost", lang), fs$type, collapse = "\n")
```

The translation table has:
- `cisRatePre` English: `"kg"` — Swahili: `"kilo "`
- `cisRatePost` English: `" kg of "` — Swahili: `" ya "`

For English this produces: `"kg123 kg of Urea"` — the English `cisRatePre` value of `"kg"` is a stray token that appears before the rate number, giving nonsensical output. The intent of `cisRatePost` seems to be the connector between rate and fertilizer type, and `cisRatePre` should likely be empty for English. Either the English `cisRatePre` value should be `""`, or the construction order should be `rate, cisRatePost, type` without `cisRatePre`.

Compare the FR text which uses `tr("kgof", lang)` (English: `" kg of "`) cleanly after the rate number.

**QUA-12 — `PPSP_MarkdownText` still writes to the working directory root (markdown.R line 312)**

Still unfixed from LOG-8. `write.csv(MarkDownTextD, "PP_MarkDownText.csv", ...)` writes to `.` not `./temp/`. `PPSP_MarkdownText` is not called from any active code path (both `PP_MarkdownText` and `SP_MarkdownText` are used instead), but the function's file path inconsistency remains a maintenance hazard.

### MEDIUM

**QUA-3 — Magic numbers throughout optimization and model code**

Still present. See original issue. Notable additions from new code:
- `process-SP.R` line 193: `(13.5 - 1.5) / 2.5` and `1.5 * 2.5` are still unexplained yield scaling constants.
- `process-SP.R` line 152: `seq(235, 455, 7)` and `34:65` for the harvest age window are unexplained.

**QUA-4 — Fertilizer NPK content table hardcoded in R source (fertilizers.R lines 42–47)**

Still present. The lookup table should be in `data/input/`.

**QUA-5 — Duplicated `dNRmin` profitability threshold logic**

Still present in `process-IC.R` line 55, `process-IC.R` line 315, `process-FR.R` line 106, `process-PP.R` line 89. Should be a shared helper in `misc.R`.

**QUA-6 — `aki_version` hardcoded date string (AkilimoMain.R line 276)**

Still present: `aki_version <- "20251222"`.

**QUA-8 — Large commented-out code blocks**

Still present in `process-SP.R` (lines 132–136, 161–164), `process-PP.R` (lines 41–74), `process-FR.R` (lines 162–165, 198–199).

**QUA-13 — `getPPrecText` mixes `tr()` keys with raw concatenation producing confusing output (process-PP.R lines 136–137)**

The `rcost` branch constructs:

```r
paste0(tr("changcost", lang), tr("this", lang), tr("decr", lang), tr("incr", lang), ds[1,]$dTC, tr("costb", lang), tr("rtprod", lang), "\n")
```

This concatenates `changcost` ("This will not change cost...") with `this` ("This will ") with `decr` ("decrease") with `incr` ("increase") with the cost value. A single sentence cannot both decrease AND increase. Looking at the translation keys, `changcost` is a standalone sentence. The intent appears to be: emit either `decr` OR `incr` depending on the sign of `dTC`, then the cost value. The current code always emits both "decrease" and "increase" concatenated. The fix requires a conditional:

```r
cost_direction <- if (ds[1,]$dTC < 0) tr("decr", lang) else tr("incr", lang)
rcost <- paste0(tr("this", lang), cost_direction, tr("costb", lang),
                formatC(abs(ds[1,]$dTC), format = "f", big.mark = ",", digits = 0),
                tr("rtprod", lang), "\n")
```

**QUA-14 — `get_costLMO` has inconsistent indentation (AkilimoMain.R lines 357–444)**

The function mixes tab and space indentation throughout. While not a runtime issue, it makes diffs and review harder.

### LOW

**QUA-9 — `get_data` signature requires all arguments even when not needed (get_data.R line 193)**

Still present. `get_data <- function(x, country, FCY, lon, lat)` — missing arguments cause `R CMD check` warnings. Should use `country = NULL, FCY = NULL, lon = NULL, lat = NULL` default values.

**QUA-15 — `IC_MarkdownText` always calls `message()` in the hot path (markdown.R line 208)**

```r
message(paste("Processing IC_MarkdownText with risk attitude", riskAtt))
```

This is a debug trace statement that writes to stderr on every IC request. It should be removed or gated behind a verbose flag.

---

## 7. Performance

### HIGH

**PERF-3 — `getSPrecommendations` row loops not vectorised (process-SP.R lines 155–159, 185–190)**

Still present. Two sequential `for` loops iterate over all scheduling combinations calling `QUEFTS()` and `getRFY()` one row at a time. For a 2-month planting × 2-month harvest window this is 256 iterations per request.

**PERF-5 — `get_yield_data("WLY_365")` reads large RDS files on every request (get_data.R lines 159–180)**

New finding, see LOG-17. Nigeria WLY LINTUL RDS files are not cached despite the `cached_read` pattern being available and already applied to soil data. This is the most frequently called data load in the FR hot path.

### MEDIUM

**PERF-4 — `setup_temp_dir` race condition under concurrent load (AkilimoMain.R lines 50–53)**

Still deferred. Global temp file deletion can corrupt concurrent requests.

---

## 8. API Design

### HIGH

**API-5 — HTTP status mapping only covers 400; 500 errors from `tr()` key failures return `status = "error"` body with no `rec_type` (api.R)**

When `tr("werec_", lang)` throws (see LOG-13/ERR-9), the outer `tryCatch` catches it and returns HTTP 500 with `status = "error"`. This is correct behaviour for the transport layer. However it means a mis-keyed translation — a developer error — is indistinguishable from a model computation crash. Adding a test that validates all `tr(key, ...)` calls against the translation CSV at startup would catch this class of error before it reaches production.

### MEDIUM

**API-4 — `dispatch_recommendations` uses `else if` — only first active flag processed**

Still deferred. `FR + PP` in one request will silently process only FR.

**API-6 — `validate_request` does not check that `PD_window` and `HD_window` are non-negative integers (AkilimoMain.R)**

`PD_window` and `HD_window` are passed to `getSPrecommendations` and used in `seq((-4 * PD_window), (4 * PD_window), by = 2)`. A negative window or a non-integer (e.g., `1.5`) will not error but will produce an unexpectedly large or reversed sequence, silently inflating the scheduling grid to hundreds of rows.

---

## 9. Translation System

### CRITICAL

**TRANS-1 — Missing key `werec_` causes runtime `stop()` on all PP change recommendations (process-PP.R lines 124, 126, 131 — see LOG-13)**

### HIGH

**TRANS-2 — `cisRatePre` English value is `"kg"` where it should be `""` (translations.csv line 81 — see QUA-11)**

### MEDIUM

**TRANS-3 — `tr()` does not validate that `lang` is a known column (misc.R lines 5–18)**

`tr()` uses `row[[lang]]` with no check that `lang` is a valid column name in the TRNS table. If `lang = "fr"` is passed (not in the table), `row[["fr"]]` returns `NULL`, `val` becomes `character(0)`, and the function falls back to English. This silent fallback is correct behaviour but `lang` values are normalised in `parse_request` to `c("en", "sw")` — so the only risk is if `tr()` is called directly from a test or future code path with an unvalidated language code.

**TRANS-4 — `rectext` key in translations.csv contains raw R `paste0(...)` code, not translatable text (translations.csv line 2)**

The `rectext` key contains literal R code fragments in both English and Swahili columns. This key is no longer used in any active code path (all text is now built via individual keys), but its presence is confusing — a translator editing the CSV might attempt to update these fields, not realising they are not rendered.

**TRANS-5 — `rw` column in translations.csv is empty for all keys (translations.csv)**

The CSV has a third column `rw` (Kinyarwanda) that is blank for every row. Requests with `lang = "rw"` would fall back to English via `tr()`'s fallback logic — but `parse_request` normalises `lang` to `c("en", "sw")`, so `"rw"` is currently coerced to `"en"` before it reaches `tr()`. The column exists but has no path to be used. Either populate it or remove it to avoid confusion.

---

## 10. Maintainability

### HIGH

**MNT-4 — Translation system now uses `tr()` but `getICrecText` (NG path) still hardcodes English (process-IC.R lines 121–178)**

`getICrecText` is the NG IC recommendation text generator. All text is hardcoded English strings with `paste0`. The `lang` parameter is not passed to this function — `process_IC_NG` calls `getICrecText(res, maizePD)` with no `lang`. This means Nigerian intercropping recommendations are always returned in English regardless of the `lang` field in the request. All other text generators now use `tr()`.

### MEDIUM

**MNT-3 — No semantic versioning or changelog**

Still present: `aki_version <- "20251222"` (AkilimoMain.R line 276).

**MNT-5 — `fertilizers.R` type-name reformatting is undocumented**

Still present: `NPK201010` → `NPK20_10_10` transformation at line 106 is silent and the expected input/output format is not documented.

**MNT-6 — `getRDY` is defined but never called from any active code path (misc.R lines 55–63)**

`getRDY` converts fresh-weight yield to dry-weight. It is defined but no call to it appears anywhere in the active codebase (only `getRFY` is used). If it is truly unused it should be removed; if it is intended for a future use it should be marked with a deferred comment like `getWMrecommendations`.

---

## 11. Summary Table

| Category        | Severity  | Count | Status        |
|-----------------|-----------|-------|---------------|
| Security        | HIGH      | 1     | Should fix    |
| Logic           | CRITICAL  | 3     | Must fix      |
| Logic           | HIGH      | 3     | Should fix    |
| Logic           | MEDIUM    | 5     | Recommended   |
| Error Handling  | CRITICAL  | 1     | Must fix      |
| Error Handling  | HIGH      | 2     | Should fix    |
| Error Handling  | MEDIUM    | 1     | Recommended   |
| Code Quality    | HIGH      | 2     | Should fix    |
| Code Quality    | MEDIUM    | 7     | Recommended   |
| Code Quality    | LOW       | 3     | Nice to have  |
| Performance     | HIGH      | 2     | Should fix    |
| Performance     | MEDIUM    | 1     | Deferred      |
| API Design      | HIGH      | 1     | Should fix    |
| API Design      | MEDIUM    | 2     | Recommended   |
| Translation     | CRITICAL  | 1     | Must fix      |
| Translation     | HIGH      | 1     | Should fix    |
| Translation     | MEDIUM    | 3     | Recommended   |
| Maintainability | HIGH      | 1     | Should fix    |
| Maintainability | MEDIUM    | 3     | Recommended   |

---

## 12. Must-Fix Before Next Production Push

| ID      | File              | Issue                                                                  |
|---------|-------------------|------------------------------------------------------------------------|
| LOG-13  | process-PP.R      | `tr("werec_", lang)` — key missing from translations.csv; all PP recommendations with change will return HTTP 500 |
| LOG-14  | process-PP.R      | `ds[1,]$cost` — column does not exist; cost figure silently omitted from all PP change recommendation text |
| LOG-15  | process-SP.R      | `getSPrecText` emits "no change" text when both PD and HD differ — the opposite of the correct behaviour |
| ERR-9   | process-PP.R      | Consequence of LOG-13: every PP request with a change recommendation crashes |
| TRANS-1 | process-PP.R      | Same as LOG-13 from translation system perspective |
