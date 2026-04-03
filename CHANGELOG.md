# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [1.8.4] - 2026-04-03

### Fixed
- **API-5:** Exclude comment lines from translation key scan; reword docstring that contained `tr("key")` literally, which caused a false-positive missing-key report at startup

---

## [1.8.3] - 2026-04-03

### Fixed
- **API-5:** Fix `check_translation_keys` — swap `sub()` order and add `\b` word boundary to prevent false matches on `str(`, `attr(`, and similar function names

---

## [1.8.2] - 2026-04-03

### Added
- **API-5:** Startup translation key validator — scans all `.R` source files for `tr("key")` literals at startup and logs ERROR for any key absent from `translations.csv`
- `min_nr_multiplier(riskAtt)` shared helper in `misc.R`; replaced four duplicate threshold expressions across FR, IC (×2), and PP processors

### Fixed
- **API-6:** `validate_request` now rejects non-numeric, negative, or non-integer `PD_window`/`HD_window` values
- **i18n:** All remaining hardcoded English strings replaced with `tr()` / `html_label()` calls across all PDF builders
- **i18n:** IC recommendation text (`getICrecText`) now accepts `lang`/`country` and uses `tr()` throughout
- **i18n:** Coordinate card labels and email subject/body localised via `.PDF_LABELS`
- **LOG-6:** Replace redundant `else if (country == "TZ")` with plain `else` in IC dispatch
- **LOG-9:** Remove redundant inner null check in `from_json`
- **LOG-10:** Extract column list to variable in `NRabove18Cost`; replace `subset()` with `[`-indexing
- **LOG-11:** Remove stale KNOWN BUG comment — grain/cob text was already corrected
- **LOG-12:** Document DEFERRED status of QUEFTS row loop; scalar `max()`/`min()` blocks vectorisation
- **LOG-16/MNT-6:** Fix `getRDY` day-of-year conversion to use `strftime("%j")` matching `getRFY`; mark as DEFERRED (no active callers)
- **LOG-18:** Document DEFERRED technical debt for all three hardcoded `country = "NG"` sites in `run_Optim_NG2`
- **MNT-5:** Document urea capitalisation and NPK underscore reformatting in `fertilizers.R`
- **QUA-4:** Move fertilizer NPK content table from hardcoded R to `data/input/fertilizer_npk.csv`; load via `cached_read`
- **QUA-5:** Extract `dNRmin` multiplier to `min_nr_multiplier()`; replace four duplicates
- **QUA-9:** Default `lon`/`lat` to `NULL` in `get_data()` signature
- **QUA-14:** Normalise `AkilimoMain.R` to consistent 2-space indentation
- **QUA-17:** Replace debug `message()` with structured `warning()` in `getSPrecommendations`
- **TRANS-3:** `tr()` now validates `lang` against CSV column names and falls back to `en` with a warning
- **TRANS-4:** Remove `rectext` key containing raw R code fragments
- **TRANS-5:** Remove `rw` (Kinyarwanda) column — was entirely `NA`
- **cisRatePre:** Guard against stale bundle regression producing `"kg123 kg of Urea"` output
- `getSPrecommendations`: Guard against NULL recommendation and duplicate CP rows

### Performance
- Cache `WLY_365` and `soil_NPK-4` RDS files on first load via `cached_read`

---

## [1.8.1] - 2026-04-02

### Documentation
- Document systemd `PATH` trap for WeasyPrint and fix service file example
- Expand WeasyPrint system-wide install instructions
- Add April 2026 release notes

---

## [1.8.0] - 2026-04-02

### Changed
- Update systemd service file example

---

## [1.7.0] - 2026-04-02

### Added
- Update package dependencies to use `openjdk-21-jre-headless`
- WeasyPrint startup check — server fails fast if WeasyPrint is not installed or not on PATH

### Fixed
- Wrap `system2` WeasyPrint call in `tryCatch`; verify installation at startup rather than on first request

---

## [1.6.4] - 2026-04-02

### Fixed
- Surface WeasyPrint stderr in error log output for easier diagnosis of PDF failures

---

## [1.6.3] - 2026-04-02

### Fixed
- Fix parse error in ragg/ggplot2 device detection block

---

## [1.6.2] - 2026-04-02

### Fixed
- Correct ragg/ggplot2 device setup to work across all installed versions

---

## [1.6.1] - 2026-04-02

### Fixed
- Remove `set_default_device` call — not available before ggplot2 3.5, causing startup failures

---

## [1.6.0] - 2026-04-02

### Changed
- Switch to `ragg` for headless ggplot2 PNG rendering; replaces Cairo bitmap device

---

## [1.5.1] - 2026-04-02

### Fixed
- Use Cairo bitmap device on headless Linux servers where no display is available

---

## [1.5.0] - 2026-04-02

### Added
- Mapbox Static Images API as preferred map provider in PDF location card

### Fixed
- Restore PP cost-benefit chart to match reference sample layout
- Replace network-dependent interactive map with offline coordinate card fallback
- `get_default_prices(NULL)` now returns all rows instead of erroring

---

## [1.4.1] - 2026-04-02

### Changed
- Mark shell scripts as executable

---

## [1.4.0] - 2026-04-02

### Added
- SQLite price store (`R/prices_db.R`) — replaces CSV-based price lookup with a persistent, auto-refreshable database
- `open_prices_db()` called at startup; auto-refresh hook in `run_akilimo()`
- `refresh_prices.R` CLI script for manual price refreshes
- `DBI` and `RSQLite` package dependencies
- Price database environment variables in `.env.example`

### Changed
- `get_data.R` price lookups now route through SQLite instead of CSV
- Remove dead webshot/knitr references from `api.R` and `install_packages.R`
- Replace pandoc/wkhtmltopdf with WeasyPrint system library requirements in setup docs

### Tests
- Add `tests/test_prices_db.R`

---

## [1.3.2] - 2026-04-02

### Changed
- Improve WeasyPrint detection and handling in `install_packages.R`

---

## [1.3.1] - 2026-04-02

### Changed
- Simplify WeasyPrint detection logic in `install_packages.R`

---

## [1.3.0] - 2026-04-02

### Added
- **WeasyPrint PDF pipeline** — replaces Rmd/pagedown/Chrome; no browser or Java dependency required
- `R/html_helpers.R` — HTML fragment builders and `.PDF_LABELS` label lookup for all four recommendation types
- `R/pdf_builders.R` — `build_fr/ic/pp/sp_pdf()` one builder per type
- `net/akilimo_print.css` — print stylesheet for WeasyPrint
- Hot-reload development server scripts (`dev.sh` / `dev.bat`)
- OSF.io as default data hosting option alongside Zenodo
- PDF always generated per request, decoupled from email flag
- Meaningful temp directory names; email guarded on PDF availability
- Multi-provider email support (smtp/mailtrap/mailgun)

### Fixed
- `generate_pdfs` `<<-` scoping bug
- Privacy: mask phone number and email address in generated PDFs; remove plain phone number from PDF filenames
- Thousand separators applied to all numeric values in PDFs and recommendation text
- PP current-practice matching (`CP` logic) and recommendation wording
- Per-request temp file isolation to prevent cross-request contamination

### Changed
- Processors return structured data; remove markdown/CSV output
- Unify Plumber server entry point; configure host/port via `API_HOST`/`API_PORT` env vars

### Tests
- WeasyPrint smoke test and email delivery test suite

---

## [1.2.0] - 2026-04-01

### Added
- `GET /health` endpoint for liveness checks

---

## [1.1.0] - 2026-04-01

### Added
- Structured logging system (`R/logging.R`) with configurable `LOG_LEVEL`
- Named `{token}` substitution in `tr()` — eliminates `paste0` fragmentation in recommendation text
- `lang` decoupled from `country` — Swahili responses work for any supported country
- Automated bump-and-tag CI workflow
- Zenodo data setup CI workflow

### Fixed
- **Critical:** `getSPrecText` never returned its value — all SP recommendations were `NULL`
- **Critical:** `getPPrecommendations` `ds$CP` crash on zero or multiple matching rows
- **Critical:** `getRFY` always used Nigerian dry-matter table, ignoring `country` parameter
- **Critical:** `tr("werec_")` referenced a non-existent key — all PP change recommendations returned HTTP 500
- **Critical:** PP cost column referenced as `ds[1,]$cost` instead of `ds[1,]$dTC`
- **Critical:** `getSPrecText` emitted wrong text branch when both PD and HD differed
- **Critical:** `maizeUP` parsed without default; string `"NA"` caused HTTP 500 for NG IC grain requests
- **Critical:** `inc` translation key missing leading space — produced `"We expectan increase..."`
- `getPPrecText` used `ds[cni,]` instead of `ds[1,]` for ridging method lookup
- `process_IC_NG` returned `type` instead of `rec_type` — field missing from all NG IC responses
- Remove R traceback from HTTP 500 response body (security)
- Path validation and filename sanitisation for phone numbers in PDF paths
- `is.na()` guards on all zero-checks for cost comparisons
- Division by zero when `cassUW == 0`
- `getICrecText` IC null country branch now calls `bad_request()` instead of silently returning `NULL`
- NetCDF file handle leak on `ncvar_get` error
- `try(rbind(...))` in `fertilizers.R` replaced with `tryCatch` and warning
- Accept mixed-case `areaUnits` and `UREA` fertilizer keys
- Explicit source order in `api.R` instead of alphabetical glob
- Input validation: FCY range, PD/HD date format, NULL `areaUnits` guard
- Soil RDS files for RW/GH/BI and `predicted_soil_properties` now cached via `cached_read`

---

## [1.0.0] - 2025-12-16

### Added
- Initial stable release of the Akilimo Recommendations REST API
- Plumber-based HTTP server (`api.R`) — `POST /compute` endpoint
- Four recommendation types: Fertilizer (FR), Intercropping (IC), Post-Planting tillage (PP), Scheduled Planting (SP)
- QUEFTS crop growth model (`R/quefts.R`)
- Cost-benefit fertilizer optimiser (`R/optimize_fert.R`) via `optim()`
- `tr(key, lang, ...)` translation helper with English fallback (`data/input/translations.csv`)
- `cached_read` in-memory caching for RDS and CSV data files
- Zenodo-hosted data assets with Python setup scripts (`scripts/`)
- Systemd service file for production deployment
- GitHub Actions deployment workflow (SSH to production)
- Support for Nigeria (NG), Tanzania (TZ), Rwanda (RW), Ghana (GH), Burundi (BI)
- Test suite with 3 000+ fixtures (`tests/test_full.R`, `tests/test_small.R`, `tests/test_api.R`)

[1.8.4]: https://github.com/IITA-AKILIMO/akilimo-recommendations/compare/1.8.3...1.8.4
[1.8.3]: https://github.com/IITA-AKILIMO/akilimo-recommendations/compare/1.8.2...1.8.3
[1.8.2]: https://github.com/IITA-AKILIMO/akilimo-recommendations/compare/1.8.1...1.8.2
[1.8.1]: https://github.com/IITA-AKILIMO/akilimo-recommendations/compare/1.8.0...1.8.1
[1.8.0]: https://github.com/IITA-AKILIMO/akilimo-recommendations/compare/1.7.0...1.8.0
[1.7.0]: https://github.com/IITA-AKILIMO/akilimo-recommendations/compare/1.6.4...1.7.0
[1.6.4]: https://github.com/IITA-AKILIMO/akilimo-recommendations/compare/1.6.3...1.6.4
[1.6.3]: https://github.com/IITA-AKILIMO/akilimo-recommendations/compare/1.6.2...1.6.3
[1.6.2]: https://github.com/IITA-AKILIMO/akilimo-recommendations/compare/1.6.1...1.6.2
[1.6.1]: https://github.com/IITA-AKILIMO/akilimo-recommendations/compare/1.6.0...1.6.1
[1.6.0]: https://github.com/IITA-AKILIMO/akilimo-recommendations/compare/1.5.1...1.6.0
[1.5.1]: https://github.com/IITA-AKILIMO/akilimo-recommendations/compare/1.5.0...1.5.1
[1.5.0]: https://github.com/IITA-AKILIMO/akilimo-recommendations/compare/1.4.1...1.5.0
[1.4.1]: https://github.com/IITA-AKILIMO/akilimo-recommendations/compare/1.4.0...1.4.1
[1.4.0]: https://github.com/IITA-AKILIMO/akilimo-recommendations/compare/1.3.2...1.4.0
[1.3.2]: https://github.com/IITA-AKILIMO/akilimo-recommendations/compare/1.3.1...1.3.2
[1.3.1]: https://github.com/IITA-AKILIMO/akilimo-recommendations/compare/1.3.0...1.3.1
[1.3.0]: https://github.com/IITA-AKILIMO/akilimo-recommendations/compare/1.2.0...1.3.0
[1.2.0]: https://github.com/IITA-AKILIMO/akilimo-recommendations/compare/1.1.0...1.2.0
[1.1.0]: https://github.com/IITA-AKILIMO/akilimo-recommendations/compare/1.0.0...1.1.0
[1.0.0]: https://github.com/IITA-AKILIMO/akilimo-recommendations/releases/tag/1.0.0
