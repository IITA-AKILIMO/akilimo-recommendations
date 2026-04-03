# Fix Checklist

Track progress against issues found in `CODE-REVIEW.md`.
Check off each item after the fix is committed.

---

## Must-Fix (CRITICAL / blocking)

### All resolved
- [x] **LOG-1** `misc.R:36` — `getRFY` always filtered dry-matter table to `"NG"`, ignoring the `country` parameter
- [x] **LOG-2** `process-SP.R` — `getSPrecText` constructs `rec` but never returns it; always returns `NULL`
- [x] **ERR-1** `process-SP.R` — consequence of LOG-2: `recText` is `NULL` for all SP recommendations
- [x] **LOG-3** `process-PP.R:76–81` — `ds$CP` assumed to have exactly one `TRUE` row; crashes or gives wrong results with zero or multiple matches
- [x] **ERR-5** `process-PP.R:117` — `ds[cni,]$method_ridging` should be `ds[1,]$method_ridging` (copy-paste bug)
- [x] **SEC-4** `api.R:82–86` — full R traceback (file paths, function names, stack frames) returned to callers in HTTP 500 responses
- [x] **QUA-1** `process-IC.R` — returns `type` instead of `rec_type`; field missing from all NG IC responses
- [x] **LOG-13 / ERR-9 / TRANS-1** `process-PP.R:124,126,131` — `tr("werec_", lang)` references a key that does not exist in `translations.csv`; all PP requests where the recommendation differs from current practice return HTTP 500
- [x] **LOG-14** `process-PP.R:140` — `ds[1,]$cost` references a non-existent column (should be `ds[1,]$dTC`); cost figure is silently omitted from all PP change recommendation text
- [x] **LOG-15** `process-SP.R:77` — `getSPrecText` emits "no change" text when both PD and HD differ; should emit `paste0(recP, recH, recR)` instead
- [x] **LOG-19** `AkilimoMain.R:162` — `maizeUP` parsed without `default_value`; string `"NA"` operand causes HTTP 500 for all NG IC grain requests where field is absent
- [x] **TRANS-6** `translations.csv` — `inc` key missing leading space; SP yield-increase text read `"We expectan increase..."`

---

## Should-Fix (HIGH)

### All resolved
- [x] **SEC-1** `get_data.R:69–71` — `get_soil_data` interpolates `country` into a file path without re-validating inside the function
- [x] **SEC-2** `sms_email.R` — `user$PhoneNr` used in PDF filename without `safe_filename_part` sanitisation
- [x] **SEC-3** `api.R` — rate-limiting and body-size cap handled at the NGINX reverse-proxy level
- [x] **SEC-5** `AkilimoMain.R:429–438` — `if (cost_X == 0)` comparisons on potentially-NA values; should use `!is.na(x) && x == 0`
- [x] **LOG-4** `AkilimoMain.R:109–112` — division by zero if `cassUW == 0` after defaults
- [x] **LOG-5** `AkilimoMain.R:160` — `if (maizeUW == 0)` check raises warning when `maizeUW` is already `NA`
- [x] **LOG-7** `AkilimoMain.R:203` — `tuberUP` computed before `sweetPotatoUP` default is applied
- [x] **ERR-2** `get_data.R:137–149` — NetCDF file handle leaks if `ncvar_get` throws
- [x] **ERR-3** `fertilizers.R:88–95` — `try(rbind(...))` silently swallows errors
- [x] **ERR-4** `AkilimoMain.R` — IC request for GH/RW/BI returns `NULL` silently
- [x] **PERF-1** `get_data.R:82–107` — soil RDS files for RW/GH/BI not cached
- [x] **PERF-2** `get_data.R:110–120` — `predicted_soil_properties` RDS not cached
- [x] **QUA-2** `process-FR.R` — `FR_MarkdownText` call was commented out
- [x] **QUA-13** `process-PP.R:136–145` — `rcost` block concatenates both `tr("decr")` and `tr("incr")` unconditionally; fixed with sign-conditional branch
- [x] **API-1** `api.R` — resolved by ERR-4 fix
- [x] **API-3** `AkilimoMain.R` — `validate_request` now checks FCY range and PD/HD date format
- [x] **MNT-1** `api.R:27–33` — `R/` directory sourced blindly in alphabetical order

### Open — should fix
- [x] **LOG-17 / PERF-5** `get_data.R:166–198` — `get_yield_data("WLY_365")` reads large RDS files fresh on every request; apply `cached_read`
- [x] **PERF-6** `get_data.R:73–79` — `get_soil_data("soil_NPK-4")` reads RDS fresh on every SP request with no `cached_read` wrapper
- [ ] **LOG-18** `optimize_fert.R:29–30,35,63` — `country = "NG"` still hardcoded in `run_Optim_NG2`; FR recommendations for non-NG countries use wrong dry-matter conversion in the optimiser; document as technical debt or fix
- [x] **ERR-10** `process-SP.R:30,40,49` — `ds[ds$CP == TRUE,]` can return multiple rows if `yld` has duplicate `(plw, haw)`; index with `which(ds$CP)[1]` throughout `getSPrecText`
- [x] **ERR-11** `AkilimoMain.R:278` — `gsub(...)` on `result$recommendation` has no NULL guard; add `%||% ""`
- [x] **QUA-11** `process-IC.R:346`, `translations.csv:81` — `cisRatePre` English value is `"kg"` where it should be `""`; produces `"kg123 kg of Urea"` output — fixed: local CSV already has `en=""` (correct); added self-healing guard in `get_data.R` so a stale bundle cannot regress the output
- [x] **TRANS-2** `translations.csv:81` — same as QUA-11
- [ ] **MNT-4** `process-IC.R:116–178` — `getICrecText` (NG IC) has no `lang` parameter and hardcodes English; does not use `tr()`

---

## Recommended (MEDIUM)

- [ ] **LOG-6** `AkilimoMain.R:185` — redundant `p$country == "TZ"` check inside TZ-only branch; could be a plain `else`
- [ ] **LOG-9** `AkilimoMain.R:355–363` — double-null check in `from_json` inner `if` is redundant
- [ ] **LOG-10** `process-FR.R:111,118` — `NRabove18Cost` hardcodes column name list in `subset()`
- [ ] **LOG-11** `process-IC.R:131–151` — known bug: maize output shown in cobs even when `maizePD == "grain"`
- [ ] **LOG-12** `process-SP.R:150–154` — `getSPrecommendations` runs QUEFTS in an O(n) row loop
- [ ] **LOG-16** `misc.R:55–63` — `getRDY` guard `if (HD > 366)` assumes integer day-of-year; called with `Date` objects from `process-SP.R`
- [x] **ERR-6** `AkilimoMain.R:80–81` — malformed PD/HD date strings; fixed by API-3
- [x] **ERR-8** `misc.R` — `dd_ply` removed
- [ ] **QUA-3** Multiple files — magic numbers (13.5, 1.5, 2.5, 7.64, seq(235,455,7), 34:65) with no named constants
- [ ] **QUA-4** `fertilizers.R:46–51` — fertilizer NPK content table hardcoded in R source
- [ ] **QUA-5** Multiple files — `dNRmin` profitability threshold duplicated in IC (×2), FR, PP
- [ ] **QUA-6** `AkilimoMain.R:286` — `aki_version` hardcoded date string
- [ ] **QUA-8** Multiple files — large commented-out code blocks
- [x] **QUA-12** `markdown.R` — `PPSP_MarkdownText` writes to wrong dir — resolved: function deleted (Rmd pipeline removed)
- [ ] **QUA-14** `AkilimoMain.R:408–504` — `get_costLMO` has inconsistent tab/space indentation
- [ ] **API-6** `AkilimoMain.R:7–47` — `validate_request` does not check that `PD_window`/`HD_window` are non-negative integers
- [x] **TRANS-3** `misc.R:5–22` — `tr()` does not validate that `lang` is a known column name; now derives allowed langs from CSV column names and warns+falls back to `en` for unknown values
- [x] **TRANS-4** `translations.csv:2` — `rectext` key contained raw R code fragments; row removed (key was never called from active code)
- [x] **TRANS-5** `translations.csv` — `rw` (Kinyarwanda) column was entirely `NA`; column removed (only `en` and `sw` are accepted by the API)
- [ ] **MNT-5** `fertilizers.R:103–110` — silent type-name reformatting undocumented
- [ ] **MNT-6** `misc.R:55–63` — `getRDY` is defined but never called from any active code path

---

## Nice to Have (LOW)

- [ ] **QUA-9** `get_data.R:205` — `get_data` signature should default-`NULL` unused arguments
- [x] **QUA-15** `markdown.R` — debug `message()` in `IC_MarkdownText` hot path — resolved: function deleted (Rmd pipeline removed)
- [ ] **QUA-16** `process-SP.R:242` — hardcoded English string with copy-paste factual error ("after planting date" should be "after planting date" → harvest); replace with `tr("sphdpd", lang)`
- [ ] **QUA-17** `process-SP.R:217` — debug `message()` call in `getSPrecommendations` hot path; replace with `warning()` or remove

---

## Deferred Technical Debt (do not fix until scoped)

- [ ] **MNT-3** `AkilimoMain.R:286` — no semantic versioning or changelog; requires process decision before implementation
- [ ] **DEBT-1** `misc.R:75–98` — `getWMrecommendations` reserved for a future weed-management feature
- [ ] **PERF-4** `AkilimoMain.R:50–63` — `setup_temp_dir` deletes all temp files globally; concurrent requests corrupt each other's temp files; requires per-request subdirectories
- [ ] **API-4** `AkilimoMain.R:138–273` — `dispatch_recommendations` uses `else if`; only the first `TRUE` flag is processed
- [ ] **MNT-2** `process-FR.R:255` — functions with 11–20+ positional parameters; large refactor required
