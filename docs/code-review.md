# Akilimo Recommendations API — Code Review

**Review date:** 2026-03-26
**Reviewer:** Claude Code (automated)
**Scope:** `api.R`, `R/AkilimoMain.R`, `R/process-FR.R`, `R/process-IC.R`, `R/process-PP.R`, `R/process-SP.R`, `R/get_data.R`, `R/quefts.R`, `R/optimize_fert.R`, `R/fertilizers.R`, `R/markdown.R`, `R/sms_email.R`, `R/misc.R`

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
        +-- validate_request(body)         country/lat/lon/area/flags check
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
                 |
                 +-- p$IC  → get_fertilizers2() → process_IC_NG()  [NG]
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

- **`api.R`** — minimal entry point; sets working directory and sources all R files blindly via `list.files`.
- **`AkilimoMain.R`** — orchestrator: validates, parses, dispatches, wraps. Also owns helper utilities `from_json`, `get_user`, `get_cassUPUW`, `get_costLMO`.
- **`get_data.R`** — single dispatcher with in-memory cache for static CSVs; routes to typed loaders for soil and yield data.
- **`quefts.R`** — pure crop-growth model; no I/O; composed of nested closures. Relatively well-isolated.
- **`optimize_fert.R`** — calls `optim()` with `quefts.R` as the objective. Hardcodes `country = "NG"` for dry→fresh conversion.
- **`fertilizers.R`** — parses fertilizer fields from the JSON body; merges with a hardcoded NPK content table.
- **`process-*.R`** — one file per recommendation type; each calls data loaders, model functions, and markdown writers.
- **`markdown.R`** — writes CSV artefacts to `./temp/` consumed by Rmd templates; also contains `fertilizerAdviseTable` and `safe_filename_part`.
- **`sms_email.R`** — reads SMTP / Plivo credentials from environment variables; generates PDFs via `webshot::rmdshot`.
- **`misc.R`** — small helpers: `get_currency`, `getRFY`, `getRDY`, `getWMrecommendations`, and a dead utility `dd_ply`.

### Architectural Strengths

- The `cached_read` pattern in `get_data.R` prevents re-reading static CSVs on every request.
- `safe_filename_part` in `markdown.R` sanitises user-supplied phone numbers before building file paths — a correct, targeted defence.
- Credentials are read exclusively from environment variables (no hardcoded secrets found).
- Validation is centralised in `validate_request` before any business logic runs.
- `from_json` provides a uniform default-value mechanism for optional fields.

---

## 2. Security

### CRITICAL

**SEC-1 — Path traversal in `get_soil_data` (get_data.R line 80)**

For countries that are not NG or TZ, the country code is interpolated directly into a file path using `data_path(paste0("soil/", country, "_FCY", Yclass, "_soilNPK.RDS"))`. The `country` value has been validated to one of five known strings by `validate_request`, so in the current call chain this is safe. However, nothing inside `get_soil_data` re-validates the country argument. If `get_soil_data` is ever called from a test, a script, or a future code path that bypasses `validate_request`, a caller could inject `../../../etc/passwd` as `country`. The fix is to add an assertion at the top of the function.

```r
# get_data.R — add at the top of get_soil_data()
VALID_COUNTRIES <- c("NG", "TZ", "RW", "GH", "BI")
stopifnot(x %in% c("soil_NPK", "soil_NPK-4", "predicted_soil_properties"))
if (!is.null(country) && !country %in% VALID_COUNTRIES)
    stop(paste("Invalid country for soil data lookup:", country))
```

**SEC-2 — Phone number used in a file path before sanitisation (markdown.R lines 182–183, 244–245, 283–284, 310–311)**

`safe_filename_part` is correctly defined in `markdown.R` and is used consistently in `FR_MarkdownText`, `IC_MarkdownText`, `CIS_MarkdownText`, and `SP_MarkdownText`. However, `PPSP_MarkdownText` (line 312) writes `"PP_MarkDownText.csv"` to the **working directory root**, not `./temp/`, using no user-supplied value — this is a minor path inconsistency rather than a security hole, but worth fixing.

Additionally, the `sendEmailReport` function in `sms_email.R` (lines 48–49) constructs PDF filenames directly from `user$PhoneNr` without going through `safe_filename_part`:

```r
# sms_email.R line 48 — UNSAFE
fname <- add_pdf(paste0('./temp/fertilizer_advice_', user$PhoneNr, ".pdf"))
```

Although `PhoneNr` originates from the JSON body and could contain path characters (`..`, `/`), the actual risk is low because `webshot::rmdshot` would likely fail before writing outside `./temp/`. Nevertheless it should be sanitised for consistency:

```r
fname <- add_pdf(file.path("temp", paste0("fertilizer_advice_", safe_filename_part(user$PhoneNr), ".pdf")))
```

### HIGH

**SEC-3 — No rate-limiting or request-size cap (api.R)**

The Plumber server binds to `0.0.0.0:8000` with no authentication, no rate-limiting, and no body-size limit. An adversary on the same network (or if port 8000 is inadvertently exposed) can POST arbitrarily large bodies. `jsonlite::fromJSON` will attempt to parse the entire string. A large payload can exhaust server memory. This should be handled at the NGINX reverse-proxy level with `client_max_body_size` and connection-rate limits, or in the Plumber filter chain.

**SEC-4 — Traceback exposed in HTTP 500 responses (api.R lines 36–37)**

The error handler returns the full R traceback to the caller:

```r
trace = jsonlite::unbox(paste(capture.output(traceback()), collapse = "\n"))
```

This leaks internal file paths, function names, and stack frames to an external client. The traceback should be logged server-side only.

```r
# api.R error handler — replace trace in response with a generic message
message("TRACE: ", paste(capture.output(traceback()), collapse = "\n"))
list(
    status = jsonlite::unbox("error"),
    data   = list(
        request_token = token,
        message       = jsonlite::unbox(e$message)
        # trace omitted from public response
    )
)
```

---

## 3. Logic and Correctness

### CRITICAL

**LOG-1 — `country` ignored in `getRFY` and `run_Optim_NG2` — hardcoded to `"NG"` (misc.R line 37, optimize_fert.R lines 29–30, 63, 35)**

`getRFY` accepts a `country` parameter but ignores it: the dry-matter table is always filtered to `fd$country == "NG"`. The function is also called from `optimize_fert.R` with `country = "NG"` explicitly passed, and from `process-SP.R` (lines 217–218) with a comment acknowledging the hardcoding. This means Tanzania, Rwanda, Ghana, and Burundi requests all use Nigerian dry-matter content values, producing incorrect fresh-weight yield estimates for those countries.

```r
# misc.R line 37 — BUG: country parameter is never used
DC <- merge(data.frame(dayNr = d), fd[fd$country == "NG",], sort = FALSE)$DMCont
#                                                ^^^^
# Should be: fd[fd$country == country,]
# (Requires validating that the dry_matter CSV contains data for all countries,
#  or a fallback for countries without data.)
```

**LOG-2 — `getSPrecText` does not return a value in all code paths (process-SP.R lines 13–123)**

The function constructs `rec` in the `else` branch but **never calls `return(rec)` or places `rec` as the last expression**. The final lines in the `else` branch assign `rec` on line 107 or 111 but the function body ends without an explicit or implicit return. R will return `NULL` invisibly because the last evaluated expression inside the outer `if (is.null(ds))` block is the `} else { ... }` construct, and the last line of that `else` is a `}`. The `rec` variable will be silently lost. The caller in `process_SP` assigns `recText <- getSPrecText(...)` and then writes it to CSV — it would write `NULL`.

```r
# process-SP.R — the else block should end with an explicit return
    } # closes else block for ds[1,]$CP
    rec   # <-- this line is missing; rec is computed but never returned
  }       # closes outer else (ds not NULL)
}         # closes getSPrecText
```

**LOG-3 — `ds$CP` may be zero-length or multi-row causing `ds$TC[ds$CP]` to crash or produce wrong results (process-PP.R lines 76–79)**

The code assumes `ds$CP` contains exactly one `TRUE`. If the farmer's current method appears in the generated scenario grid more than once (e.g., due to `expand.grid` producing duplicate combinations after `na.omit`), `ds$TC[ds$CP]` will be a vector of length > 1 and vector recycling silently produces wrong dTC/dRP values. If `ds$CP` is all-`FALSE` (no row matches the current practice), these lines produce `numeric(0)` and subsequent arithmetic silently returns `NA`-filled columns that propagate into `getPPrecText`.

A guard is needed:

```r
cp_rows <- which(ds$CP)
if (length(cp_rows) != 1) stop("Expected exactly one current-practice row; got ", length(cp_rows))
```

### HIGH

**LOG-4 — Division by zero in `parse_request` when `cassUW == 0` after defaults (AkilimoMain.R line 86–87)**

`cass_denominator <- cassUW * conversion_factor / 1000`. If `cassUW` ends up as 0 (and `get_cassUPUW` can return the original input unchanged if `saleSF` is FALSE, `cassUP != 0`, and no default branch is entered), division at line 87 produces `Inf` for `rootUP`. This propagates silently into the optimization and recommendation logic.

**LOG-5 — `maizeUW == 0` check before `NA` assignment (AkilimoMain.R line 130)**

`if (maizeUW == 0) maizeUW <- NA` — if `maizeUW` is `NA` (the default), this comparison raises a warning and produces `NA`, leaving `maizeUW` as `NA`. The check should be `if (!is.na(maizeUW) && maizeUW == 0) maizeUW <- NA`.

**LOG-6 — `sweetPotatoUP == 0` default check uses `country == "TZ"` twice (AkilimoMain.R lines 163–166)**

The condition at line 163 checks `p$country == "TZ"` again, but this entire block is already inside the `else if (p$country == "TZ")` branch at line 149. The redundant check is harmless but indicates copy-paste from a prior multi-country block — the logic inside would never execute for a non-TZ country here, but the intent is obscured.

**LOG-7 — `tuberUP` computed before zero-guard for `sweetPotatoUW` (AkilimoMain.R line 161)**

`tuberUP <- sweetPotatoUP / sweetPotatoUW / conversion_factor3 * 1000` is evaluated on line 161, but the guard `if (sweetPotatoUW == 0) sweetPotatoUW <- 1000` appears later on line 154 as the default... actually line 154 has `if (sweetPotatoUW == 0) sweetPotatoUW <- 1000` — but that is only reached after `from_json` returns at line 153, and `tuberUP` is computed at line 161 *after* the guard. Reading the actual source again: lines 151–154 set defaults then line 161 computes `tuberUP`. The guard executes before the division — this is fine. **Retract LOG-7.** However, if `sweetPotatoUP == 0` (which falls through to the defaults block on lines 163–166), `tuberUP` was already computed using the uncorrected zero value of `sweetPotatoUP` on line 161, yielding `tuberUP = 0`. The defaults for `sweetPotatoUP` are applied too late to affect `tuberUP`.

**LOG-8 — `PPSP_MarkdownText` writes to working directory root without `temp/` prefix (markdown.R line 312)**

```r
write.csv(MarkDownTextD, "PP_MarkDownText.csv", row.names = FALSE)
```

This writes to the process working directory, not `./temp/`. The function is not currently called from any active code path (it appears to be superseded by `PP_MarkdownText`), but the inconsistency is a maintenance hazard.

### MEDIUM

**LOG-9 — `from_json` double-null check is redundant (AkilimoMain.R lines 270–277)**

The inner `if (!is.null(value))` is always true when the outer `if (!is.null(body[[field_name]]))` is true. The extra check adds no protection and obscures intent.

**LOG-10 — `NRabove18Cost` uses hardcoded field subset that will fail if column names change (process-FR.R lines 121, 128)**

Both `subset(ds, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))` calls hardcode the column list. If `run_Optim_NG2` adds or renames a column, these subsets will silently drop or error on the new column.

**LOG-11 — `getICrecText` always shows output in cobs even when `maizePD == "grain"` (process-IC.R lines 168–176)**

A `# KNOWN BUG` comment in the file confirms this. The function divides `dMP` by 7.64 to get kilograms but then emits the unit as `cobs` if `maizePD == "grain"`. The comment inside the function should become an open issue.

**LOG-12 — `getSPrecommendations` row-loop for QUEFTS is O(n) synchronous calls (process-SP.R lines 184–188)**

The loop `for (k in 1:nrow(WLY)) { WLY$Current_Yield[k] <- QUEFTS(...) }` and the subsequent `for (k in 1:nrow(ds)) { ds$RFCY[k] <- getRFY(...) }` iterate over all scheduling combinations one at a time. For wide windows this can be hundreds of iterations, all inside the HTTP request handler with no async escape.

---

## 4. Error Handling

### CRITICAL

**ERR-1 — `getSPrecText` called but result is `NULL` because no value is returned (process-SP.R, see LOG-2)**

`recText` in `process_SP` will be `NULL` when `getSPrecommendations` returns a valid data frame. `build_response` calls `gsub` on `result$recommendation`, which will fail with `argument "x" is missing` or silently return character(0).

### HIGH

**ERR-2 — NetCDF file opened but not closed on all error paths (get_data.R lines 128–143)**

```r
nc <- ncdf4::nc_open(f)
off <- which(nc$dim$cell$vals == cell)
if (length(off) != 1) {
    ncdf4::nc_close(nc)           # closed here
    warning(...)
    return(NULL)
}
x <- ncdf4::ncvar_get(...)        # if this throws, nc stays open
ncdf4::nc_close(nc)
```

If `ncvar_get` throws an error the NetCDF file handle leaks. Use `on.exit`:

```r
nc <- ncdf4::nc_open(f)
on.exit(ncdf4::nc_close(nc), add = TRUE)
```

**ERR-3 — `try(rbind(fd, d_new))` in `fertilizers.R` silently swallows errors (fertilizers.R line 84)**

If `rbind` fails (mismatched column types between the standard and user-defined fertilizer data frames), the returned `try-error` object is assigned to `fd`. Subsequent `rowSums(is.na(fd))` would fail with a confusing error. Use `tryCatch` with an explicit error message, or validate `d_new` columns before the merge.

**ERR-4 — `dispatch_recommendations` returns `NULL` silently for unhandled IC countries (AkilimoMain.R lines 121–177)**

If `p$IC` is `TRUE` and `p$country` is neither `"NG"` nor `"TZ"` (e.g., `"GH"`, `"RW"`, `"BI"`), neither `process_IC_NG` nor `process_IC_TZ` is called and the function falls through, returning `NULL`. `build_response(NULL, ...)` then returns a 200 OK with `status = "400 - bad request"` message (per line 240 of AkilimoMain.R). The correct response is to raise a `bad_request` error before dispatch, or add a default `else` branch.

**ERR-5 — `getPPrecText` uses `ds[cni,]$method_ridging` instead of `ds[1,]$method_ridging` (process-PP.R line 116)**

```r
ifelse(ds[1,]$method_ridging == "N/A", tr$no[cni], ds[cni,]$method_ridging)
#                                                       ^^^^
# Should be ds[1,]$method_ridging
```

For `cni == 1` (NG) or `cni == 3` (RW) this accesses the second or third row of `ds`, not the recommended row. This is a copy-paste bug: `ds[1,]` is consistently used for ploughing but `ds[cni,]` is accidentally used for ridging on this one line.

### MEDIUM

**ERR-6 — `validate_request` does not guard against `NA` dates from malformed PD/HD strings (AkilimoMain.R line 57–58)**

`as.Date(from_json("PD", body, default_value = 0), format = "%Y-%m-%d")` returns `NA` silently for any string that does not match the format (e.g., `"2024/01/15"`). A `NA` date propagates into `difftime` in `process_SP` (line 279: `(HD - PD) <= 30`) and produces `NA` for the comparison, taking the wrong branch.

**ERR-7 — `getWMrecommendations` in `misc.R` is unreachable dead code (misc.R lines 67–91)**

The function is defined but never called from any file in scope. It should either be integrated or removed.

**ERR-8 — `dd_ply` in `misc.R` is explicitly marked "not used anymore" but remains (misc.R lines 10–21)**

Dead code; the comment says it is no longer used. Should be removed to reduce confusion.

---

## 5. Code Quality

### HIGH

**QUA-1 — Inconsistent response structure across processors**

- `process_FR` returns `c(rec_type="FR", recommendation=recText, response)` (a named vector + list mix)
- `process_IC_NG` returns `c(list(type="IC", ...), res)` — note `type` not `rec_type`
- `process_IC_TZ` returns `c(list(rec_type="IC", ...), res)` — different key from NG
- `process_PP` returns `list(rec_type="PP", recommendation=recText, data=res)`
- `process_SP` returns `list(rec_type="SP", recommendation=recText, data=res)`

The key `type` vs `rec_type` discrepancy between `process_IC_NG` and all other processors will cause clients to find `rec_type` missing in NG IC responses. `build_response` in AkilimoMain.R accesses `result$rec_type` on line 242, which will be `NULL` for NG IC requests.

### HIGH

**QUA-2 — `FR_MarkdownText` is commented out in `process_FR` (process-FR.R lines 276–283)**

The `FR_MarkdownText` call is entirely commented out. This means the FR processor never writes its Rmd data file, so any email/PDF generation for FR will either use stale data from a previous request or fail silently if no prior file exists. The comment says nothing about why it was disabled.

### MEDIUM

**QUA-3 — Magic numbers throughout optimization and model code**

Examples:
- `quefts.R` line 30–31: `yd$RY <- c(rep(10, 4), 20, 25, 15, 22)` — unexplained yield values in `getPPrecommendations`
- `process-SP.R` line 222: `ds$RY <- (ds$RFWY - ds$RFCY) / (13.5 - 1.5) / 2.5 * (FCY - 1.5 * 2.5) + ds$RFCY` — constants 13.5, 1.5, 2.5 are unexplained
- `optimize_fert.R` line 40: `ndeps = rep(1, length(initial))` — finite-difference step size chosen without comment
- `process-IC.R` line 137: `cobUP <- ifelse(maizePD == "fresh_cob", maizeUP, maizeUP / maizeUW / 7.64)` — `7.64` (cobs per kg) is unexplained

**QUA-4 — Fertilizer NPK content table is hardcoded in R source (fertilizers.R lines 42–47)**

Adding a new fertilizer product requires editing R source code and redeploying. The table should be in a CSV under `data/input/` and loaded via `get_data` / `cached_read`.

**QUA-5 — Duplicated `dNRmin` profitability threshold logic**

The expression `dTC * ifelse(riskAtt == 0, 1.8, ifelse(riskAtt == 1, 1, 0.2))` appears identically in:
- `process-IC.R` line 55
- `process-IC.R` line 320
- `process-FR.R` line 116 (using `ds$NR < ds$TC * dNRmin` variant)
- `process-PP.R` line 83

This should be a shared helper function in `misc.R`.

**QUA-6 — `aki_version` hardcoded string in `run_akilimo` (AkilimoMain.R line 249)**

```r
aki_version <- "20251222"
```

This date-coded string will become stale without any automated reminder. It should be sourced from a `VERSION` file or a package `DESCRIPTION` field.

**QUA-7 — Inconsistent use of `./temp/` absolute vs relative prefix**

Most file writes use `"./temp/..."` but `PPSP_MarkdownText` writes `"PP_MarkDownText.csv"` to the working directory. When `setup_temp_dir()` is called it only creates and clears `temp/`. Any file written to `.` is never cleaned up between requests, accumulating stale data.

**QUA-8 — Commented-out code blocks throughout processors**

Large blocks of commented-out alternatives exist in multiple files:
- `process-FR.R` lines 172–175 (commented `#hd`, `#hw`, `#haw` assignments)
- `process-SP.R` lines 161–165, 210–213 (commented `latr`/`lonr` rounding blocks with an explicit note "--- note the error")
- `process-SP.R` lines 316–323 (commented return alternatives)
- `process-PP.R` lines 41–74 (large commented block including the old `ds$CP` logic)
- `AkilimoMain.R` line 94 (commented `#rdd <- subset(...)`)

These should be removed or replaced with `# TODO` comments that reference issues.

### LOW

**QUA-9 — `get_data` signature requires all arguments even when not needed (get_data.R line 188)**

`get_data <- function(x, country, FCY, lon, lat)` — callers like `get_data("TRNS")` and `get_data("starch_prices")` work only because R allows missing arguments when they are not accessed. Calls to `get_data("soil_NPK-4", lon=lon, lat=lat)` omit `country` and `FCY` using positional-argument skipping. The signature should use default values (`country = NULL, FCY = NULL, lon = NULL, lat = NULL`) to make the intent explicit and prevent `R CMD check` warnings.

**QUA-10 — Swahili hardcoded strings mixed with translation table (process-IC.R, process-SP.R)**

Some Swahili strings come from the `TRNS` translation CSV (`get_data("TRNS")`), but others are hardcoded literals directly in R source (e.g., `process-IC.R` lines 300–312, `process-SP.R` lines 276–277). Maintaining translations in two places is fragile.

---

## 6. Performance

### HIGH

**PERF-1 — RDS soil files read fresh on every request for non-NG/TZ countries (get_data.R lines 80–97)**

For countries `"RW"`, `"GH"`, and `"BI"`, `get_soil_data` calls `readRDS(f)` on every request with no caching. The `cached_read` pattern exists for static CSVs but is not applied here. A large RDS file read on every request adds significant latency, especially under concurrent load.

```r
# get_data.R — apply cached_read to the RDS path
get_soil_data <- function(x, country, FCY, lon, lat) {
    ...
    } else {
        soil <- cached_read(paste0("soil_", country, "_FCY", Yclass), function() readRDS(f))
        ...
    }
}
```

Note: this is safe only if the RDS files are immutable at runtime. For this application they are static data assets, so caching is appropriate.

**PERF-2 — `get_soil_data("predicted_soil_properties")` loads the entire RDS into memory every call (get_data.R lines 99–108)**

`readRDS(data_path("soil/predicted_soil_properties.rds"))` is not cached, even though `predicted_soil_properties` is accessed on every NG/TZ FR or IC request (via `get_soil_data("soil_NPK", ...)`). This is likely the largest single file read in the hot path.

**PERF-3 — `getSPrecommendations` runs QUEFTS in a row loop (process-SP.R lines 184–188)**

As noted in LOG-12, the loop prevents vectorisation. QUEFTS is a pure function of its inputs and could be vectorised by refactoring `do_quefts` to operate on a matrix of supply rows. This would eliminate `O(n)` function call overhead for the scheduling window scan.

**PERF-4 — `setup_temp_dir` deletes all temp files on every request (AkilimoMain.R lines 36–40)**

```r
old_files <- list.files("temp", full.names = TRUE)
if (length(old_files) > 0) suppressWarnings(file.remove(old_files))
```

Under concurrent load, request A can delete the temp files written by the concurrent request B mid-computation. There is no per-request isolation for temp files. Each request should write to a unique subdirectory under `temp/` (e.g., using `tempfile(tmpdir = "temp")`).

---

## 7. API Design

### HIGH

**API-1 — HTTP status code mapping is fragile string-prefix matching (api.R lines 22–23)**

```r
if (!is.null(status_str) && grepl("^400", status_str)) res$status <- 400L
```

Only 400 errors are mapped. A `bad_request` from a missing SP window or unhandled IC country returns `status = "400 - bad request"` in the JSON body but HTTP 200 in the header, because the caller in `build_response` never sets `res$status`. Any caller checking HTTP status will treat all non-validation errors as success.

Processors that return `NULL` (unhandled IC country, out-of-domain SP) should explicitly produce a 400 or 422 response with a correct HTTP status code.

**API-2 — No `Content-Type` header is explicitly set**

Plumber defaults to JSON, but the response structure is built manually. If a future code path returns a non-list (e.g., a plain string), the content type will be wrong. Explicitly declare `res$setHeader("Content-Type", "application/json")` in the handler.

**API-3 — `validate_request` does not validate `FCY`, `CMP`, date formats, or numeric bounds on financial fields**

`FCY` (farmer current yield) is required by the model but passes through `parse_request` with no range check. A value of `FCY = -999` or `FCY = 0` will silently enter `QUEFTS` and produce nonsensical output. Similarly, `cassUP`, `maxInv`, fertilizer costs, and `CMP` have no upper-bound validation.

**API-4 — Only one recommendation type can be processed per request**

`dispatch_recommendations` uses `else if` chaining, so `FR + IC` cannot both be computed in one request even if both flags are `TRUE`. The `sendEmailReport` function signature accepts both `FR` and `IC` flags suggesting the original design supported combined requests, but the dispatcher silently picks only the first `TRUE` flag. This should either be documented or the dispatcher should iterate over all active flags.

---

## 8. Maintainability

### HIGH

**MNT-1 — `api.R` sources every `*.R` file in `R/` unconditionally and alphabetically (api.R line 9)**

```r
for (f in list.files(srcdir, pattern="\\.R$")) source(file.path(srcdir, f))
```

This means:
1. Load order is alphabetical, not dependency-ordered. If `AkilimoMain.R` calls `get_currency` (defined in `misc.R`), this works today only because `A` < `m`. Adding any file that starts with a letter before `A` and calls a later-defined function will break silently.
2. Any `.R` scratch file or test helper accidentally placed in `R/` will be loaded into production.
3. There is no explicit dependency declaration — it is impossible to tell which modules depend on which.

**MNT-2 — `process_FR` signature has 14 positional parameters (process-FR.R line 265)**

`getFRrecommendations` has 11 parameters; `process_FR` has 14. Several downstream functions (e.g., `process_IC_NG`) have 20+. Functions with more than ~6 parameters should accept a named list to improve call-site readability and reduce the risk of positional argument errors. The `parse_request` list `p` already exists and could be passed directly.

**MNT-3 — `aki_version` is the only version identifier and lives inside a function (AkilimoMain.R line 249)**

There is no `DESCRIPTION` file or `NEWS.md` that tracks what changed in each version. The version string `"20251222"` provides a date but no semantic meaning. If the model parameters change (e.g., a new country, updated NPK coefficients), there is no mechanism to communicate this to API consumers.

### MEDIUM

**MNT-4 — Translation system partially implemented**

`getFRrecText` and `getPPrecText` use the `TRNS` CSV via `get_data("TRNS")`. `getICrecText` (NG) hardcodes English. `getCISrecText` (TZ) hardcodes Swahili. `getSPrecText` mixes the translation table with hardcoded English and placeholder Kinyarwanda strings (`"kinyarwanda, " ...`). This patchwork makes adding a new language or correcting a translation error error-prone.

**MNT-5 — `fertilizers.R` line 99 reformats type names silently**

```r
fd$type[i] <- paste0(substr(fd$type[i], 1, 5), "_", substr(fd$type[i], 6, 7), "_", substr(fd$type[i], 8, 9))
```

This transforms `"NPK201010"` to `"NPK20_10_10"`. This renaming happens after the NPK content merge, so the internal `type` names used in `FERT_COLOUR` and `FERT_LABEL` in `markdown.R` must match these reformatted names. The transformation is undocumented and the expected format is not obvious from the function inputs.

---

## Summary Table

| Category        | Severity  | Count | Status         |
|-----------------|-----------|-------|----------------|
| Security        | CRITICAL  | 1     | Must fix       |
| Security        | HIGH      | 3     | Should fix     |
| Logic           | CRITICAL  | 3     | Must fix       |
| Logic           | HIGH      | 4     | Should fix     |
| Logic           | MEDIUM    | 5     | Recommended    |
| Error Handling  | CRITICAL  | 1     | Must fix       |
| Error Handling  | HIGH      | 4     | Should fix     |
| Error Handling  | MEDIUM    | 3     | Recommended    |
| Code Quality    | HIGH      | 2     | Should fix     |
| Code Quality    | MEDIUM    | 8     | Recommended    |
| Code Quality    | LOW       | 2     | Nice to have   |
| Performance     | HIGH      | 4     | Should fix     |
| API Design      | HIGH      | 4     | Should fix     |
| Maintainability | HIGH      | 3     | Should fix     |
| Maintainability | MEDIUM    | 2     | Recommended    |

### Must-fix before next production push

| ID     | File              | Issue                                                       |
|--------|-------------------|-------------------------------------------------------------|
| LOG-1  | misc.R            | `country` parameter ignored in `getRFY` — always uses NG   |
| LOG-2  | process-SP.R      | `getSPrecText` does not return its value                    |
| LOG-3  | process-PP.R      | `ds$CP` multi-row / zero-row crash in dTC/dRP computation  |
| ERR-1  | process-SP.R      | `recText` is `NULL` because LOG-2 is unfixed               |
| ERR-5  | process-PP.R      | `ds[cni,]` should be `ds[1,]` for ridging method text      |
| SEC-4  | api.R             | Full traceback returned to callers in HTTP 500 response     |
| QUA-1  | process-IC.R      | `type` key instead of `rec_type` in NG IC response         |
