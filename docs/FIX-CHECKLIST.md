# Fix Checklist

Track progress against issues found in `CODE-REVIEW.md`.
Check off each item after the fix is committed.

---

## Must-Fix (CRITICAL / blocking)

- [x] **LOG-1** `misc.R:37` — `getRFY` always filtered dry-matter table to `"NG"`, ignoring the `country` parameter
- [x] **LOG-2** `process-SP.R` — `getSPrecText` constructs `rec` but never returns it; always returns `NULL`
- [x] **ERR-1** `process-SP.R` — consequence of LOG-2: `recText` is `NULL` for all SP recommendations
- [x] **LOG-3** `process-PP.R:76–79` — `ds$CP` assumed to have exactly one `TRUE` row; crashes or gives wrong results with zero or multiple matches
- [x] **ERR-5** `process-PP.R:116` — `ds[cni,]$method_ridging` should be `ds[1,]$method_ridging` (copy-paste bug)
- [x] **SEC-4** `api.R:36–37` — full R traceback (file paths, function names, stack frames) returned to callers in HTTP 500 responses
- [x] **QUA-1** `process-IC.R` — returns `type` instead of `rec_type`; field missing from all NG IC responses

---

## Should-Fix (HIGH)

### Security
- [x] **SEC-1** `get_data.R:80` — `get_soil_data` interpolates `country` into a file path without re-validating inside the function
- [x] **SEC-2** `sms_email.R:48–49` — `user$PhoneNr` used in PDF filename without `safe_filename_part` sanitisation
- [x] **SEC-3** `api.R` — rate-limiting and body-size cap handled at the NGINX reverse-proxy level (`client_max_body_size`, connection rate limits)

### Logic
- [x] **LOG-4** `AkilimoMain.R:86–87` — division by zero if `cassUW == 0` after defaults; produces silent `Inf` for `rootUP`
- [x] **LOG-5** `AkilimoMain.R:130` — `if (maizeUW == 0)` check raises warning when `maizeUW` is already `NA`; should be `if (!is.na(maizeUW) && maizeUW == 0)`
- [x] **LOG-7** `AkilimoMain.R:161` — `tuberUP` computed before `sweetPotatoUP` default is applied; `tuberUP` is 0 when `sweetPotatoUP == 0`

### Error Handling
- [x] **ERR-2** `get_data.R:128–143` — NetCDF file handle leaks if `ncvar_get` throws; fix with `on.exit(ncdf4::nc_close(nc), add = TRUE)`
- [x] **ERR-3** `fertilizers.R:84` — `try(rbind(...))` silently swallows errors; use `tryCatch` with an explicit message
- [x] **ERR-4** `AkilimoMain.R` — IC request for GH/RW/BI returns `NULL` silently (no IC implementation for those countries); should return a clear 400

### Performance
- [x] **PERF-1** `get_data.R:80–97` — soil RDS files for RW/GH/BI read fresh on every request; apply `cached_read`
- [x] **PERF-2** `get_data.R:99–108` — `predicted_soil_properties` RDS (large file, NG/TZ hot path) not cached; apply `cached_read`
- [ ] **PERF-4** `AkilimoMain.R:36–40` — `setup_temp_dir` deletes all temp files globally; concurrent requests corrupt each other's temp files; requires per-request subdirectories and passing temp path through all processor function signatures — deferred

### Code Quality
- [x] **QUA-2** `process-FR.R:276–283` — `FR_MarkdownText` call is fully commented out; reason unknown; needs testing before uncommenting — deferred

### API Design
- [x] **API-1** `api.R:22–23` — resolved by ERR-4 fix; all remaining bad_request() paths correctly prefix status with "400" which is mapped to HTTP 400
- [x] **API-3** `AkilimoMain.R` — `validate_request` now checks FCY range and PD/HD date format
- [ ] **API-4** `AkilimoMain.R` — `dispatch_recommendations` uses `else if`; only the first `TRUE` flag is processed; architectural change deferred

### Maintainability
- [x] **MNT-1** `api.R:9` — `R/` directory sourced blindly in alphabetical order; replaced with explicit dependency-ordered source list
- [ ] **MNT-2** `process-FR.R:265` — functions with 11–20+ positional parameters; large refactor deferred

---

## Recommended (MEDIUM)

- [ ] **LOG-6** `AkilimoMain.R:163–166` — redundant `p$country == "TZ"` check inside the TZ-only branch
- [ ] **LOG-8** `markdown.R:312` — `PPSP_MarkdownText` writes `"PP_MarkDownText.csv"` to working directory root instead of `./temp/`
- [ ] **LOG-9** `AkilimoMain.R:270–277` — double-null check in `from_json` inner `if` is redundant
- [ ] **LOG-10** `process-FR.R:121,128` — `NRabove18Cost` hardcodes column name list in `subset()`; fragile if columns change
- [ ] **LOG-11** `process-IC.R:168–176` — known bug: maize output shown in cobs even when `maizePD == "grain"`
- [ ] **LOG-12** `process-SP.R:184–188` — `getSPrecommendations` runs QUEFTS in an O(n) row loop; could be vectorised
- [ ] **ERR-6** `AkilimoMain.R:57–58` — malformed PD/HD date strings produce silent `NA`; `validate_request` does not check date format
- [ ] **ERR-7** `misc.R:67–91` — `getWMrecommendations` is dead code (never called)
- [ ] **ERR-8** `misc.R:10–21` — `dd_ply` is explicitly marked "not used anymore"; should be removed
- [ ] **QUA-3** Multiple files — magic numbers (13.5, 1.5, 2.5, 7.64, etc.) with no named constants or comments
- [ ] **QUA-4** `fertilizers.R:42–47` — fertilizer NPK content table hardcoded in R source; should be a CSV in `data/input/`
- [ ] **QUA-5** Multiple files — `dNRmin` profitability threshold duplicated in IC (×2), FR, PP; should be a shared helper
- [ ] **QUA-6** `AkilimoMain.R:249` — `aki_version` hardcoded date string; no `DESCRIPTION` or `NEWS.md`
- [ ] **QUA-7** Multiple files — inconsistent `./temp/` vs working-directory root for output files
- [ ] **QUA-8** Multiple files — large commented-out code blocks (process-FR, process-SP, process-PP, AkilimoMain)
- [ ] **MNT-4** Multiple files — translation system is patchwork: some strings in TRNS CSV, others hardcoded in R source
- [ ] **MNT-5** `fertilizers.R:99` — silent type-name reformatting (`"NPK201010"` → `"NPK20_10_10"`) undocumented

---

## Nice to Have (LOW)

- [ ] **QUA-9** `get_data.R:188` — `get_data` signature should default-`NULL` unused arguments to avoid `R CMD check` warnings
- [ ] **QUA-10** `process-IC.R`, `process-SP.R` — Swahili strings split between TRNS CSV and hardcoded literals
- [ ] **MNT-3** `AkilimoMain.R:249` — no semantic versioning or changelog; `aki_version` date provides no compatibility signal
