# Code Review — Akilimo Recommendation Engine

Static analysis of the R codebase. Issues are grouped by category and ordered by severity.

> **Security issues (section 1) resolved** in commit fixing `api.R`, `R/sms_email.R`, and `R/markdown.R`.

---

## 1. Security ✓ Resolved

### 1.1 Credentials stored in an RDS file — CRITICAL ✓ Fixed

**File:** `R/sms_email.R` lines 18, 90–98

`passwords.rds` is loaded at runtime and its contents (Plivo AUTH_ID, SMTP username/password, host) are used directly. If that file is ever committed or leaked, all credentials are exposed.

**Fix:** Load secrets from environment variables or a secrets manager. Remove `passwords.rds` from the project entirely.

```r
# Before
creds   <- readRDS("passwords.rds")
AUTH_ID <- creds$auth_id

# After
AUTH_ID <- Sys.getenv("PLIVO_AUTH_ID")
```

---

### 1.2 Hostname-based path configuration — HIGH ✓ Fixed

**File:** `api.R` lines 3–8

```r
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
    akpath <- "C:/github/omilika/akilimo-recommendations"
} else {
    akpath <- "."
}
```

This hard-codes a developer's machine name and a Windows absolute path. The fallback `"."` is fragile and will silently break if the working directory is wrong in any deployment.

**Fix:** Use an environment variable with a documented default.

```r
akpath <- Sys.getenv("AKILIMO_ROOT", unset = ".")
```

---

### 1.3 Path injection via user-supplied phone number — HIGH ✓ Fixed

**File:** `R/markdown.R` lines 237–238, 330–331, 398–399

```r
filename <- paste("./temp/personalized_info", user$PhoneNr, sep = "_")
filename <- paste0(filename, ".csv")
```

If `PhoneNr` contains `../` or other special characters a caller could write files outside `./temp/`. Sanitise before use.

```r
safe_phone <- gsub("[^0-9+]", "", user$PhoneNr)
filename   <- file.path("temp", paste0("personalized_info_", safe_phone, ".csv"))
```

---

## 2. Logic Errors ✓ Resolved

### 2.1 `substr()` arguments reversed — HIGH ✓ Fixed

**File:** `R/sms_email.R` line 26

```r
# Wrong — first arg should be the string
txt <- paste0(substr(1, 1588, txt), " [truncated]")

# Correct
txt <- paste0(substr(txt, 1, 1588), " [truncated]")
```

As written, this will either error or silently return the wrong value on every SMS send.

---

### 2.2 Undefined variable `listofPDFs` — HIGH ✓ Fixed

**File:** `R/sms_email.R` line 89

The condition references `listofPDFs`, but the variable in scope is `PDFs`. This branch will always error at runtime.

---

### 2.3 Scalar vs. vector comparison in soil-prep logic — HIGH ✓ Fixed

**File:** `process-PP.R` lines 53–54

```r
ds$CP <- ifelse(ds$ploughing, ploughing & ds$method_ploughing == method_ploughing, !ploughing) &
         ifelse(ds$ridging,   ridging   & ds$method_ridging   == method_ridging,   !ridging)
```

`ds$ploughing` is a column (logical vector); `ploughing` is a scalar flag. The `ifelse` test and the body use different types, which produces unexpected filtering results.

---

### 2.4 Ambiguous `SP` duplication in key vector — MEDIUM ✓ Fixed

**File:** `R/AkilimoMain.R` line 49

```r
selected_key <- unique(c("FR", "PP", "IC", "SP", "SP")[c(FR, PP, IC, SPP, SPH)])
```

`"SP"` appears at positions 4 and 5, meaning both `SPP` and `SPH` map to the same `"SP"` key. This is intentional but completely non-obvious. A named vector makes the intent clear:

```r
key_map      <- c(FR = "FR", PP = "PP", IC = "IC", SPP = "SP", SPH = "SP")
flags        <- c(FR = FR,   PP = PP,   IC = IC,   SPP = SPP,  SPH = SPH)
selected_key <- unique(key_map[flags])
```

---

## 3. Error Handling ✓ Resolved

### 3.1 `request_token` potentially undefined in error handler — HIGH ✓ Fixed

**File:** `api.R` lines 26–35

```r
tryCatch({
    run_akilimo(req$postBody)
}, error = function(e) {
    request_token = jsonlite::unbox(request_token),   # may be undefined
    ...
})
```

If `run_akilimo` throws before `request_token` is assigned, the error handler itself errors, returning an opaque 500 instead of the intended error payload.

---

### 3.2 Silent `NULL` returns from data loading — MEDIUM ✓ Fixed

**File:** `R/get_data.R` lines 29, 81, 193–195

Several branches return `NULL` (or an empty data frame) with no log message when a NetCDF cell lookup or soil RDS lookup fails. Callers receive `NULL` and fail later with a cryptic error far from the root cause.

**Fix:** Log the failure at the point it occurs.

```r
if (length(off) != 1) {
    warning(sprintf("Cell %s not found in NetCDF for country %s", cell, country))
    return(NULL)
}
```

---

### 3.3 Silent fallback for unknown area units — MEDIUM ✓ Fixed

**File:** `R/AkilimoMain.R` lines 78–79

```r
area_conversion_factor <- unit_factors[[areaUnits]]
if (is.null(area_conversion_factor)) area_conversion_factor <- 10000
# comment in code: "seems rather risky!!"
```

An unrecognised unit silently defaults to hectares (10000 m²). The comment acknowledges this is wrong but it was never fixed. The API should reject unknown units with a 400 error.

---

## 4. Code Duplication ✓ Resolved (4.1, 4.2)

### 4.1 Country × product price table as nested `if` blocks — HIGH ✓ Fixed

**File:** `R/AkilimoMain.R` lines 272–296

20+ hardcoded price assignments across 5 country blocks:

```r
if (country == "NG") {
    if (cassPD == "roots") { cassUP <- 12000; cassUW <- 1000 }
    if (cassPD == "chips") { cassUP <- 36000; cassUW <- 1000 }
    ...
} else if (country == "TZ") { ... }
```

This should be a lookup table — either a named list in R or an entry in `data/input/`.

```r
price_table <- list(
    NG = list(roots = 12000, chips = 36000, flour = 38400, gari = 42000),
    TZ = list(roots = 180000, ...),
    ...
)
cassUP <- price_table[[country]][[cassPD]]
```

---

### 4.2 Tillage cost blocks duplicated per country — HIGH ✓ Fixed

**File:** `R/AkilimoMain.R` lines 357–406

Identical cost structures (ploughing, harrowing, ridging) are written out separately for NG and TZ with different numbers. Same fix as 4.1 — use a lookup table. The magic number `2.47105` (acre → m² conversion) should be a named constant.

---

### 4.3 `markdown.R` report-building logic repeated 3× — MEDIUM ✓ Fixed

**File:** `R/markdown.R` — `FR_MarkdownText()`, `IC_MarkdownText()`, `CIS_MarkdownText()`

All three functions share the same pattern:
- Build a user-info data frame
- Format currency values
- Write a personalised CSV
- Loop over fertilizer options

Extract the shared structure into a helper and let each function supply only what differs.

---

### 4.4 Fertilizer → colour mapping as `if-else` chain — MEDIUM ✓ Fixed

**File:** `R/markdown.R` lines 43–71

```r
if      (dat[, 1] == "Urea")       { fertColCode <- "green" }
else if (dat[, 1] == "NPK15_15_15") { fertColCode <- "blue"  }
...
```

Replace with a named vector:

```r
fert_colours <- c(Urea = "green", NPK15_15_15 = "blue", ...)
fertColCode  <- fert_colours[[dat[, 1]]]
```

---

## 5. Performance ✓ Resolved (5.1, 5.3)

### 5.1 Data files read on every request — MEDIUM ✓ Fixed

**File:** `R/get_data.R`, `R/AkilimoFunctions_5D.R`

CSV and RDS files are read with `read.csv()` / `readRDS()` inside functions that are called on every API request. For a high-traffic API these should be loaded once at startup and cached.

```r
# In api.R, at startup:
.cache <- list(
    translations = read.csv("data/input/translations_TEST.csv"),
    starch_prices = read.csv("data/input/starchPrices.csv"),
    ...
)
```

---

### 5.2 Intermediate CSV files used as in-process data transfer — MEDIUM

**File:** `R/markdown.R` lines 34–96

The loop writes `datall1.csv … datall6.csv` to disk then reads them back within the same request. Passing the data as in-memory R objects (a list or data frame) eliminates the disk round-trip entirely.

---

### 5.3 Temp directory never cleaned up — LOW ✓ Fixed

**File:** `R/AkilimoMain.R` line 5; `R/markdown.R`

`./temp/` is created on startup but never pruned. On a long-running server this accumulates per-request CSV files indefinitely.

---

## 6. API Design

### 6.1 No input validation on `/compute` — HIGH

**File:** `api.R` lines 21–37

The endpoint accepts any JSON body and passes it directly to `run_akilimo()`. Missing or malformed fields produce cryptic internal errors rather than a clear 400 response.

Minimum validation to add:
- Required fields present (`country`, `lat`, `lon`, `area`, at least one flag)
- `country` is one of NG | TZ | RW | GH | BI
- Coordinates are numeric and within plausible bounds
- Area is positive

---

### 6.2 Wrong HTTP status for JSON parse failure — MEDIUM

**File:** `api.R` line 8

A malformed JSON body returns HTTP 404. The correct code is **400 Bad Request**.

---

### 6.3 No rate limiting or authentication — LOW

The `/compute` endpoint is publicly accessible with no throttling. For production, consider adding an API key header check inside the Plumber filter and rate-limiting at the reverse proxy (NGINX) level.

---

## 7. Dead Code and TODOs

### 7.1 Unresolved TODOs — MEDIUM

| File | Lines | Note |
|------|-------|------|
| `process-IC.R` | 168–173 | 5 open TODOs; one marked URGENT: maize output reported in cobs even when user selected grain |
| `process-IC.R` | 400–404 | 4 more TODOs for CIS recommendations |
| `process-SP.R` | 27–32 | Future enhancements never started |
| `process-SP.R` | 217 | Comment: TZ function gives "very strange values" — not fixed |
| `process-FR.R` | 27–61 | Missing information in recommendation text |

---

### 7.2 Commented-out code — LOW

Large blocks of commented code exist in `R/AkilimoMain.R` (lines 18–19, 45–47, 89), `R/get_data.R` (lines 149–155), `R/sms_email.R` (lines 36–37), and `R/markdown.R` (lines 82–92, 136). These should either be restored or deleted — git history preserves them if needed.

---

## 8. Maintainability

### 8.1 `run_akilimo()` is 234 lines long — MEDIUM

**File:** `R/AkilimoMain.R`

The function handles parameter extraction (~40 parameters), unit conversion, price defaults, country-specific cost lookup, dispatch to four recommendation engines, and response assembly. Suggested split:

- `parse_request(json)` → validated parameter list
- `apply_country_defaults(params)` → fills in missing prices/costs
- `dispatch_recommendations(params)` → calls process-*.R files
- `build_response(results)` → assembles JSON

### 8.2 `get_data()` branches across 88 lines — MEDIUM

**File:** `R/get_data.R`

Eight different data-loading branches in one function. Splitting by data type (yield, soil, input tables) improves testability.

---

## Summary

| Category | Severity | Count |
|----------|----------|-------|
| Security | Critical | 1 |
| Security / Logic / API | High | 9 |
| Duplication / Error handling / Performance | Medium | 14 |
| Dead code / Style | Low | 4 |

### Suggested priority order

1. Fix `substr()` bug and undefined `listofPDFs` in `sms_email.R` — these are silent runtime failures.
2. Replace hostname check in `api.R` with an environment variable.
3. Sanitise phone number before using it in a file path.
4. Add a required-fields check at the top of `/compute`.
5. Move credentials out of `passwords.rds` into environment variables.
6. Replace country × product price `if-else` trees with lookup tables.
7. Cache data files at startup instead of reading on every request.
8. Clean up or resolve all TODO comments.
