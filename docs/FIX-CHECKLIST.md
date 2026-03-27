# Fix Checklist

Track progress against issues found in `CODE-REVIEW.md`.
Check off each item after the fix is committed.

---

## Must-Fix (CRITICAL / blocking)

### Previously identified — all resolved
- [x] **LOG-1** `misc.R:37` — `getRFY` always filtered dry-matter table to `"NG"`, ignoring the `country` parameter
- [x] **LOG-2** `process-SP.R` — `getSPrecText` constructs `rec` but never returns it; always returns `NULL`
- [x] **ERR-1** `process-SP.R` — consequence of LOG-2: `recText` is `NULL` for all SP recommendations
- [x] **LOG-3** `process-PP.R:76–79` — `ds$CP` assumed to have exactly one `TRUE` row; crashes or gives wrong results with zero or multiple matches
- [x] **ERR-5** `process-PP.R:116` — `ds[cni,]$method_ridging` should be `ds[1,]$method_ridging` (copy-paste bug)
- [x] **SEC-4** `api.R:36–37` — full R traceback (file paths, function names, stack frames) returned to callers in HTTP 500 responses
- [x] **QUA-1** `process-IC.R` — returns `type` instead of `rec_type`; field missing from all NG IC responses

### New — must fix before next push
- [ ] **LOG-13 / ERR-9 / TRANS-1** `process-PP.R:124,126,131` — `tr("werec_", lang)` references a key that does not exist in `translations.csv`; all PP requests where the recommendation differs from current practice return HTTP 500
- [ ] **LOG-14** `process-PP.R:137` — `ds[1,]$cost` references a non-existent column (should be `ds[1,]$dTC`); cost figure is silently omitted from all PP change recommendation text
- [ ] **LOG-15** `process-SP.R:76–79` — `getSPrecText` emits "no change" text when both PD and HD differ; should emit `paste0(recP, recH, recR)` instead; delete the `if` block at lines 76–79

---

## Should-Fix (HIGH)

### Previously identified — all resolved
- [x] **SEC-1** `get_data.R:80` — `get_soil_data` interpolates `country` into a file path without re-validating inside the function
- [x] **SEC-2** `sms_email.R:48–49` — `user$PhoneNr` used in PDF filename without `safe_filename_part` sanitisation
- [x] **SEC-3** `api.R` — rate-limiting and body-size cap handled at the NGINX reverse-proxy level
- [x] **LOG-4** `AkilimoMain.R:86–87` — division by zero if `cassUW == 0` after defaults
- [x] **LOG-5** `AkilimoMain.R:130` — `if (maizeUW == 0)` check raises warning when `maizeUW` is already `NA`
- [x] **LOG-7** `AkilimoMain.R:161` — `tuberUP` computed before `sweetPotatoUP` default is applied
- [x] **ERR-2** `get_data.R:128–143` — NetCDF file handle leaks if `ncvar_get` throws
- [x] **ERR-3** `fertilizers.R:84` — `try(rbind(...))` silently swallows errors
- [x] **ERR-4** `AkilimoMain.R` — IC request for GH/RW/BI returns `NULL` silently
- [x] **PERF-1** `get_data.R:80–97` — soil RDS files for RW/GH/BI not cached
- [x] **PERF-2** `get_data.R:99–108` — `predicted_soil_properties` RDS not cached
- [x] **QUA-2** `process-FR.R:276–283` — `FR_MarkdownText` call was commented out
- [x] **API-1** `api.R:22–23` — resolved by ERR-4 fix
- [x] **API-3** `AkilimoMain.R` — `validate_request` now checks FCY range and PD/HD date format
- [x] **MNT-1** `api.R:9` — `R/` directory sourced blindly in alphabetical order

### New — should fix
- [x] **SEC-5** `AkilimoMain.R:370–378` — `if (cost_X == 0)` comparisons on potentially-NA values; should use `!is.na(x) && x == 0`
- [ ] **LOG-17 / PERF-5** `get_data.R:159–180` — `get_yield_data("WLY_365")` reads large RDS files fresh on every request; apply `cached_read`
- [ ] **LOG-18** `optimize_fert.R:29–30,35,63` — `country = "NG"` still hardcoded in `run_Optim_NG2`; FR recommendations for non-NG countries use wrong dry-matter conversion in the optimiser; document as technical debt or fix
- [ ] **ERR-10** `process-SP.R:30,40,49,76` — `ds[ds$CP == TRUE,]` can return multiple rows if `yld` has duplicate `(plw, haw)`; index with `which(ds$CP)[1]` throughout `getSPrecText`
- [ ] **ERR-11** `AkilimoMain.R:268` — `gsub(...)` on `result$recommendation` has no NULL guard; add `%||% ""`
- [ ] **QUA-11** `process-IC.R:376`, `translations.csv:81` — `cisRatePre` English value is `"kg"` where it should be `""`; produces `"kg123 kg of Urea"` output
- [ ] **QUA-13** `process-PP.R:136–137` — `rcost` block concatenates both `tr("decr")` and `tr("incr")` unconditionally; add a conditional on sign of `ds[1,]$dTC`
- [ ] **TRANS-2** `translations.csv:81` — same as QUA-11
- [ ] **MNT-4** `process-IC.R:116–178` — `getICrecText` (NG IC) has no `lang` parameter and hardcodes English; does not use `tr()`

---

## Recommended (MEDIUM)

- [ ] **LOG-6** `AkilimoMain.R:183–186` — redundant `p$country == "TZ"` check inside TZ-only branch
- [ ] **LOG-9** `AkilimoMain.R:296–304` — double-null check in `from_json` inner `if` is redundant
- [ ] **LOG-10** `process-FR.R:111,118` — `NRabove18Cost` hardcodes column name list in `subset()`
- [ ] **LOG-11** `process-IC.R:131–151` — known bug: maize output shown in cobs even when `maizePD == "grain"`
- [ ] **LOG-12** `process-SP.R:155–159` — `getSPrecommendations` runs QUEFTS in an O(n) row loop
- [ ] **LOG-16** `misc.R:55–63` — `getRDY` guard `if (HD > 366)` assumes integer day-of-year; called with `Date` objects from `process-SP.R`
- [x] **ERR-6** `AkilimoMain.R:57–58` — malformed PD/HD date strings; fixed by API-3
- [x] **ERR-8** `misc.R:10–21` — `dd_ply` removed
- [ ] **QUA-3** Multiple files — magic numbers (13.5, 1.5, 2.5, 7.64, seq(235,455,7), 34:65) with no named constants
- [ ] **QUA-4** `fertilizers.R:42–47` — fertilizer NPK content table hardcoded in R source
- [ ] **QUA-5** Multiple files — `dNRmin` profitability threshold duplicated in IC (×2), FR, PP
- [ ] **QUA-6** `AkilimoMain.R:276` — `aki_version` hardcoded date string
- [ ] **QUA-8** Multiple files — large commented-out code blocks
- [ ] **QUA-12** `markdown.R:312` — `PPSP_MarkdownText` writes `"PP_MarkDownText.csv"` to working directory root
- [ ] **QUA-14** `AkilimoMain.R:357–444` — `get_costLMO` has inconsistent tab/space indentation
- [ ] **API-6** `AkilimoMain.R` — `validate_request` does not check that `PD_window`/`HD_window` are non-negative integers
- [ ] **TRANS-3** `misc.R:5–18` — `tr()` does not validate that `lang` is a known column name
- [ ] **TRANS-4** `translations.csv:2` — `rectext` key contains raw R code fragments, not human-readable text
- [ ] **TRANS-5** `translations.csv` — `rw` (Kinyarwanda) column is entirely blank; either populate or remove
- [ ] **MNT-3** `AkilimoMain.R:276` — no semantic versioning or changelog
- [ ] **MNT-5** `fertilizers.R:99–106` — silent type-name reformatting undocumented
- [ ] **MNT-6** `misc.R:55–63` — `getRDY` is defined but never called from any active code path

---

## Nice to Have (LOW)

- [ ] **QUA-9** `get_data.R:193` — `get_data` signature should default-`NULL` unused arguments
- [ ] **QUA-15** `markdown.R:208` — debug `message()` call in `IC_MarkdownText` hot path

---

## Deferred Technical Debt (do not fix until scoped)

- [ ] **DEBT-1** `misc.R` — `getWMrecommendations` reserved for a future weed-management feature
- [ ] **PERF-4** `AkilimoMain.R:50–53` — `setup_temp_dir` deletes all temp files globally; concurrent requests corrupt each other's temp files; requires per-request subdirectories
- [ ] **API-4** `AkilimoMain.R` — `dispatch_recommendations` uses `else if`; only the first `TRUE` flag is processed
- [ ] **MNT-2** `process-FR.R:265` — functions with 11–20+ positional parameters; large refactor required
