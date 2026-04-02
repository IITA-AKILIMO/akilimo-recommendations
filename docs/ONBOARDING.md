# Technical Onboarding Guide

This guide is for **developers** and **data scientists** joining the Akilimo project. It assumes you have already completed the environment setup described in [SETUP.md](SETUP.md).

---

## Table of Contents

1. [Domain Context](#domain-context)
2. [Repository Layout](#repository-layout)
3. [Request Lifecycle](#request-lifecycle)
4. [Core Modules In Depth](#core-modules-in-depth)
5. [Data Layer](#data-layer)
6. [Translation System](#translation-system)
7. [Testing](#testing)
8. [Development Workflow](#development-workflow)
9. [Data Science: Models and Methods](#data-science-models-and-methods)
10. [Extending the Engine](#extending-the-engine)
11. [Common Gotchas](#common-gotchas)

---

## Domain Context

Akilimo provides agronomic decision support for cassava farmers in Sub-Saharan Africa. The engine answers four types of questions:

| Recommendation | What it answers |
|----------------|-----------------|
| **FR** — Fertilizer Recommendation | Which fertilizers, at what rates, maximise net revenue given local prices and soil conditions? |
| **IC** — Intercropping | Is it profitable to intercrop cassava with maize (NG) or sweet potato (TZ), and what management practice is optimal? |
| **PP** — Post-Planting | What tillage and ridging approach (manual vs tractor) minimises cost while preserving yield? |
| **SP** — Schedule Planting | What is the optimal planting or harvest date window given local seasonal yield variation and cassava price curves? |

All recommendations are site-specific: they use the farmer's GPS coordinates to look up local soil NPK supply and water-limited yield potential from spatial datasets.

**Key domain terms:**

| Term | Meaning |
|------|---------|
| WLY | Water-Limited Yield — maximum attainable yield given local rainfall, from LINTUL simulation model |
| FCY | Farmer's Current Yield — self-reported baseline yield (t/ha fresh weight) |
| DCY | Dry Current Yield — FCY converted to dry matter for QUEFTS |
| NPK supply | Soil native nitrogen, phosphorus, potassium available to the crop (kg/ha) |
| HI | Harvest Index — ratio of root dry weight to total plant dry weight (default 0.52 for cassava) |
| RFY | Root Fresh Yield — dry matter yield converted to fresh weight using a dry matter content curve |
| NR | Net Revenue — total revenue minus total input cost |

---

## Repository Layout

```
akilimo-recommendations/
├── api.R                   # Plumber server entry point
├── install_packages.R      # One-shot R package installer
├── setup.sh                # Automated Linux setup script
├── R/
│   ├── AkilimoMain.R       # Orchestrator: validation, parsing, dispatch
│   ├── quefts.R            # QUEFTS crop growth model
│   ├── optimize_fert.R     # Cost-benefit fertilizer optimisation
│   ├── get_data.R          # Data access layer (soil, yield, CSVs)
│   ├── fertilizers.R       # Fertilizer type/price/NPK parsing
│   ├── misc.R              # tr(), get_currency(), getRFY(), getRDY()
│   ├── markdown.R          # Shared helpers: FERT_COLOUR/LABEL, calc_fertilizer_recom()
│   ├── html_helpers.R      # HTML fragment builders for WeasyPrint PDFs
│   ├── pdf_builders.R      # build_fr/ic/pp/sp_pdf() — one per recommendation type
│   ├── sms_email.R         # Email (smtp/mailtrap/mailgun) and SMS dispatch
│   ├── process-FR.R        # Fertilizer Recommendation processor
│   ├── process-IC.R        # Intercropping processor
│   ├── process-PP.R        # Post-Planting processor
│   └── process-SP.R        # Schedule Planting processor
├── data/                   # Runtime data (gitignored — downloaded from OSF or Zenodo)
│   ├── input/              # CSV lookup tables (prices, translations, dry-matter curve)
│   ├── soil/               # Soil NPK RDS files, one per country
│   └── yield/              # NetCDF yield rasters ({COUNTRY}_WLY_LINTUL_2020SP.nc)
├── net/                    # Recommendation images (gitignored — from OSF or Zenodo)
├── tests/
│   ├── test_full.R         # Full regression suite (3203 cases)
│   ├── test_api.R          # Live API integration tests
│   ├── test_small.R        # Quick smoke test subset
│   └── input/              # Test JSON payloads (in_{N}_{COUNTRY}_{TYPE}_{params}.json)
├── scripts/                # Python data management scripts (Poetry)
│   ├── setup_data.py       # Downloads and extracts bundles from OSF or Zenodo
│   ├── bundle_assets.py    # Packs data dirs into tar.gz for upload
│   ├── upload_osf.py       # Uploads bundles to OSF
│   └── upload_zenodo.py    # Uploads bundles to Zenodo
├── systemd/                # systemd service template
├── docs/                   # Documentation
└── .github/workflows/      # CI/CD (deploy-production.yml, claude-code-review.yml)
```

Directories to ignore: `old/` (archived) and `R/preprocess/` (offline data prep, not sourced by the API).

---

## Request Lifecycle

A request goes through six stages:

### Stage 1 — HTTP entry (`api.R`)

Plumber exposes a single `POST /compute` endpoint. `api.R` sources all `R/*.R` files, sets `akpath` (working directory, derived from hostname), and registers the route.

### Stage 2 — Validation (`validate_request`, `AkilimoMain.R:7`)

Checks required fields, value ranges, date formats, and that at least one recommendation flag is `TRUE`. Returns a 400 error immediately on the first failure — no partial processing.

### Stage 3 — Parsing (`parse_request`, `AkilimoMain.R:57`)

Extracts and normalises every field from the raw JSON body using `from_json()`, which handles missing fields with typed defaults. Area is converted to hectares. Fertilizer entries are assembled into a data frame by `get_fertilizers()` (`R/fertilizers.R`).

### Stage 4 — Processor dispatch (`run_akilimo`, `AkilimoMain.R`)

Flags are evaluated in priority order: FR → IC → PP → SP. The matching processor file (`process-*.R`) is called with the parsed parameter list.

### Stage 5 — Processor execution (`R/process-*.R`)

Each processor follows the same pattern:

```
1. get_soil_data(lat, lon, country)   → soil NPK supply (kg/ha)
2. get_yield_data(lat, lon, country)  → WLY from NetCDF raster
3. QUEFTS(QID, rec)                   → yield at candidate NPK rates
4. run_Optim_*(...)                   → find NPK rate that maximises NR
5. tr(key, lang, ...)                 → build recommendation text
6. return list(rec_type, recommendation, data, fertilizers, ...)
```

Processors no longer write CSV files to the temp dir. All data needed for PDF generation is returned in the result list.

### Stage 6 — PDF generation (`R/sms_email.R`, `R/pdf_builders.R`)

`generate_pdfs()` calls the appropriate `build_*_pdf()` for each active flag. Each builder:

1. Assembles HTML using fragment helpers from `html_helpers.R`
2. Fetches a map PNG via Mapbox Static API → generic HTTP → offline coordinate card fallback
3. Renders any charts via `ggplot2::ggsave()`
4. Calls `render_pdf(html, out_path)` which invokes WeasyPrint via `system2()`

All images are base64-embedded in the HTML so WeasyPrint requires no network access. Per-request isolation: every HTML file, map PNG, chart PNG, and output PDF is written to a dedicated temp subdirectory named `temp/{YYYYMMDD_HHMMSS}_{COUNTRY}_{TYPE}_{rand4}/`. Temp dirs older than 1 hour are cleaned up on each new request.

Individual PDF failures are caught and logged; the recommendation JSON is always returned regardless of PDF or email outcome.

### Stage 7 — Response assembly

The processor result is wrapped by `build_response()` with `status = "success"`, `version`, and `rec_type`, then Plumber serialises it to JSON.

---

## Core Modules In Depth

### `R/AkilimoMain.R`

Central orchestrator. Key functions:

| Function | Purpose |
|----------|---------|
| `validate_request(body)` | Input validation; returns error string or `NULL` |
| `parse_request(body)` | Extracts all fields with defaults; returns named list `params` |
| `setup_temp_dir(country, rec_type)` | Creates `temp/{date}_{country}_{type}_{rand4}/`; cleans up dirs older than 1 h |
| `dispatch_recommendations(params, body)` | Routes to the correct `process_*` function |
| `run_akilimo(postBody)` | Top-level entry called by Plumber; validates → parses → temp dir → dispatch → PDFs → response |
| `from_json(key, body, default_value)` | Safe field extractor with typed defaults |

The `params` list from `parse_request()` is passed unchanged to all processors and to `generate_pdfs()`. The temp dir is created after parsing so the directory name can include the country and recommendation type.

### `R/html_helpers.R`

HTML fragment builders shared across all PDF types.

| Symbol | Purpose |
|--------|---------|
| `.PDF_LABELS` | Named list of `c(en=..., sw=...)` pairs for all PDF UI strings |
| `html_label(key, lang)` | Looks up a label with English fallback |
| `img_base64(path, alt, class)` | Embeds a PNG as a base64 data-URI `<img>` tag |
| `img_bag(fert_type, bags)` | Returns the correct colour/count bag image for a fertilizer |
| `img_cash(ratio)` | Returns a cash-stack image (1–10 scale) |
| `html_open(title, banner_path, css_path)` | Full `<!DOCTYPE html>…<body>` with inlined CSS |
| `html_section(heading, content)` | `<div class="section">` wrapper |
| `html_two_col(left, right)` | CSS Grid two-column wrapper |
| `html_three_col(col1, col2, col3)` | CSS Grid three-column wrapper |
| `html_personal_info(...)` | "What you told us" section |
| `html_location_map(lat, lon, ...)` | Map PNG (Mapbox/HTTP/offline card) → base64 `<img>` |
| `html_fertilizer_table(...)` | Fertilizer prices + cassava price + max investment |
| `html_cost_benefit(...)` | Three-row cost/revenue/net section with cash images |
| `html_recommendation(recText, lang)` | Bold full-width recommendation paragraph |
| `html_table(df, col_names)` | Generic data frame → HTML `<table>` |

### `R/pdf_builders.R`

One builder per recommendation type plus the shared renderer.

| Function | Purpose |
|----------|---------|
| `render_pdf(html, path)` | Writes HTML to temp dir; calls `weasyprint` via `system2()` |
| `build_fr_pdf(rr, fertilizers, ...)` | FR: two-column layout with map, fertilizer rows, cost-benefit |
| `build_ic_pdf(rr, ...)` | IC (NG maize) and CIS (TZ sweet potato): similar to FR with crop-specific sections |
| `build_pp_pdf(rr, ...)` | PP: ggplot practice matrix + LMO cost table |
| `build_sp_pdf(rr, ...)` | SP: ggplot heatmap + planting/harvest window summary |

The CSS stylesheet is at `net/akilimo_print.css` (A4 landscape, CSS Grid, `@page` rules, WeasyPrint-compatible).

### `R/get_data.R`

Data access layer. All static files are cached in `.data_cache` (a private environment) after first load — repeated calls within a server process are essentially free.

| Function | What it loads |
|----------|--------------|
| `get_input_data("TRNS")` | `translations.csv` |
| `get_input_data("default_prices")` | Default cassava/maize prices per country |
| `get_input_data("dry_matter")` | Dry matter content curve (`fd2.csv`) |
| `get_soil_data(lat, lon, country)` | Soil NPK from `data/soil/{country}_soil.rds` |
| `get_yield_data(lat, lon, country)` | WLY from `data/yield/{country}_WLY_LINTUL_2020SP.nc` |

Spatial lookup uses `round5min()` to snap coordinates to the 0.05° grid and `cellFromLonLat()` to compute the raster cell index.

### `R/misc.R`

Shared utilities:

| Function | Purpose |
|----------|---------|
| `tr(key, lang, ...)` | Translation lookup with token substitution |
| `get_currency(country)` | Returns local currency symbol (e.g. `"NGN"`, `"TZS"`) |
| `getRFY(HD, RDY, country)` | Converts dry matter yield (kg/ha) to root fresh yield using `fd2.csv` dry matter content curve |
| `getRDY(HD, RFY, country)` | Inverse of `getRFY` |
| `ha_to_unit(ha, unit)` | Converts hectares to the farmer's preferred area unit |

### `R/fertilizers.R`

Builds the fertilizer data frame used by the optimiser. Each fertilizer type (urea, CAN, NPK, MOP, DAP, SSP, TSP, etc.) has:

- N/P/K content (fraction of nutrient per kg fertilizer)
- Price per kg (derived from `*CostperBag` / `*BagWt`)
- Availability flag

Fertilizers marked unavailable (`*available = FALSE`) are excluded from the optimisation. The country's default price is used when no user price is supplied.

---

## Data Layer

### Soil data

Stored as RDS files under `data/soil/`. Each country file is a data frame with columns:

```
lon, lat, N_supply, P_supply, K_supply
```

Supply values are native nutrient availability in kg/ha/season. The API looks up the nearest grid cell by snapping coordinates to 0.05° resolution.

### Yield data (NetCDF)

One NetCDF file per country: `data/yield/{COUNTRY}_WLY_LINTUL_2020SP.nc`.

The LINTUL model simulates cassava dry matter production under water-limited conditions at 0.05° resolution across a range of planting weeks. The API extracts the cell value at the farmer's coordinates and planting date.

### Input CSV files (`data/input/`)

| File | Contents |
|------|----------|
| `translations.csv` | All user-facing strings (wide: key × language) |
| `Default_prices.csv` | Cassava and intercrop default prices per country |
| `starchPrices.csv` | Starch factory price schedules |
| `fd2.csv` | Dry matter content by harvest day-of-year and country |

All CSV files are loaded once and cached. Do not write to these at runtime.

---

## Translation System

User-facing text is never hardcoded in R. All strings go through `tr(key, lang, ...)`:

```r
# Simple lookup
tr("norecom", lang)

# With token substitution
tr("recPopt", lang, date = format(ds$PD, "%d %B %Y"))

# Nested tr() for reusable atomic words
tr("recPln", lang,
   weeks     = abs(ds$rPWnr),
   direction = ifelse(ds$rPWnr < 0, tr("early", lang), tr("late", lang)))
```

To add a new string: add a row to `data/input/translations.csv` (English required; other languages fall back to English if blank). No R code changes needed unless you are replacing a `paste0()` pattern.

See [TRANSLATIONS.md](TRANSLATIONS.md) for full details including adding a new language.

---

## Testing

### Test types

| Script | What it does | Prerequisite |
|--------|-------------|--------------|
| `tests/test_small.R` | Runs 29 representative cases in-process | Data downloaded |
| `tests/test_full.R` | Runs all 3203 regression cases in-process | Data downloaded |
| `tests/test_api.R` | POSTs each test input to the live server | Server running on port 8000 |

### Running tests

```bash
# In-process (no server needed)
Rscript tests/test_small.R
Rscript tests/test_full.R

# Against live server
Rscript api.R &          # start server in background
Rscript tests/test_api.R
```

### Single request via curl

```bash
# Run an existing fixture
curl -X POST http://localhost:8000/compute \
  -H "Content-Type: application/json" \
  --data "@./tests/input/in_1_TZ_FR_starch_factory_riskAtt0.json"

# Send an ad-hoc payload
curl -X POST http://localhost:8000/compute \
  -H "Content-Type: application/json" \
  -d '{"country":"NG","lat":7.55,"lon":4.51,"area":1,"areaUnits":"ha",
       "FR":true,"IC":false,"PP":false,"SPP":false,"SPH":false,
       "PD":"2025-05-01","HD":"2026-02-01","FCY":11}'
```

### Adding test cases

Test inputs are JSON files in `tests/input/` following the naming convention:

```
in_{N}_{COUNTRY}_{TYPE}_{key_params}.json
```

- `N` — next sequential number (preserves ordering)
- `COUNTRY` — `NG`, `TZ`, `GH`, `RW`, or `BI`
- `TYPE` — `FR`, `IC`, `PP`, or `SP`
- `key_params` — short description of what makes this case distinct
  (e.g. `starch_factory_riskAtt1`, `custom_price_maxInv`, `out_of_scope_location`)

**Example:** `in_30_TZ_IC_fresh_cob_riskAtt0.json`

To register the new fixture:

1. Create the JSON file in `tests/input/` following the naming convention above.
2. Add the filename (without `.json`) to the `test_files` vector in both
   `tests/test_small.R` and `tests/test_api.R`.
3. Run `Rscript tests/test_small.R` to verify it passes.

### What to check when a test fails

1. **400 error** — usually a validation failure; check the `message` field and compare against `validate_request()`.
2. **`recommendation` is empty** — a `tr()` key is missing from `translations.csv` or a `NULL` was passed as a token.
3. **Yield/NR is 0** — spatial lookup returned no data for the coordinates; check that `lat`/`lon` falls within the country's raster extent.
4. **Optimiser returns all-zero rates** — the cost of fertilizer exceeds the maximum investment or revenue gain is negative at all NPK levels.

---

## Development Workflow

### Branching

- `main` — production branch; CI deploys on push
- `feat/*` — feature branches; open a PR to `main`
- `fix/*` — bug fix branches

### Making a change

1. Branch from `main`: `git checkout -b feat/my-feature`
2. Edit the relevant `R/*.R` file.
3. Run `Rscript tests/test_small.R` to catch obvious regressions.
4. For changes to recommendation logic, run the full suite: `Rscript tests/test_full.R`
5. Push and open a PR — Claude will run an automated code review.

### Environment variables (`.env` at project root)

Copy `.env.example` to `.env` and fill in the values you need. Full variable reference is in [SETUP.md](SETUP.md#environment-configuration). Key variables for development:

| Variable | Default | Purpose |
|----------|---------|---------|
| `AKILIMO_ROOT` | `.` | Project root path |
| `API_HOST` | `0.0.0.0` | Plumber bind address |
| `API_PORT` | `8000` | Plumber port |
| `EMAIL_PROVIDER` | `smtp` | `smtp` \| `mailtrap` \| `mailgun` |

The R API reads `.env` on startup via `dotenv::load_dot_env()` in `api.R`. Never commit `.env`.

### Hot-reload dev server

Instead of manually restarting `Rscript api.R` on every file change, use the provided watcher scripts:

```bash
# Linux / macOS
./dev.sh

# Windows
dev.bat
```

Both scripts use [watchexec](https://github.com/watchexec/watchexec) (preferred) or `entr` (Linux/macOS fallback) to restart the server whenever an `.R`, `.csv`, or `.css` file changes under `R/`, `net/`, or `data/input/`.

Install watchexec:
```bash
winget install watchexec.watchexec   # Windows
brew install watchexec               # macOS
cargo install watchexec-cli          # any platform with Rust
```

### Path configuration

`api.R` sets `akpath` from the `AKILIMO_ROOT` env var (defaults to `"."`). All file paths in `get_data.R` are constructed as `file.path(akpath, "data", ...)`. When running locally with `Rscript api.R` from the repo root, this works without any configuration.

---

## Data Science: Models and Methods

### QUEFTS model (`R/quefts.R`)

QUEFTS (Quantitative Evaluation of the Fertility of Tropical Soils) predicts crop yield from nutrient supply. The implementation follows Janssen et al. (1990), adapted for cassava.

**Inputs:**

| Parameter | Description |
|-----------|-------------|
| `QID` | Data frame with soil NPK supply (`N_supply`, `P_supply`, `K_supply` in kg/ha), WLY (kg/ha dry weight), and nutrient recovery fractions |
| `rec` | Candidate NPK application rate (kg/ha) vector |
| `HI` | Harvest Index (default 0.52 for cassava) |

**Algorithm outline:**

1. Compute nutrient use efficiency bounds (`aN`, `dN`, `aP`, `dP`, `aK`, `dK`) from crop-specific dilution and accumulation constants and `HI`.
2. For each nutrient independently, compute maximum (`Y*A`) and minimum (`Y*D`) yields from total nutrient supply (soil + applied).
3. Combine pairwise: for each pair of nutrients, calculate the yield that results from their combined limitation, accounting for the third nutrient.
4. Final yield = average of the six pairwise-combined yields.

The model is evaluated inside the optimiser at each candidate NPK rate — it is not called standalone in normal usage.

### Fertilizer optimisation (`R/optimize_fert.R`)

Uses `optim()` with the `L-BFGS-B` method (bounded quasi-Newton) to find the fertilizer application rates (kg/bag of each available product) that maximise net revenue:

```
NR = (yield_gain × cassava_price) − (fertilizer_cost + application_cost)
```

**Key constraints:**
- All rates ≥ 0 (lower bound)
- Total fertilizer cost ≤ `maxInv` (investment ceiling), enforced via a penalty term inside `optim_NR()`

**Investment ceiling handling:** `invest` is set to `Inf` when `maxInv` is `NA` (no ceiling). The penalty activates only when total cost exceeds the ceiling.

**Dry matter ↔ fresh weight conversion:** `getRFY()` uses the `fd2.csv` dry matter content curve, which varies by harvest day-of-year and country, to convert QUEFTS dry matter output to the fresh weight that the farmer can price.

### Spatial data pipeline

Yield rasters come from the LINTUL-Cassava model run at 0.05° resolution over a grid of planting weeks (typically weeks 1–52). The API:

1. Snaps the request `lat`/`lon` to the nearest 0.05° centroid (`round5min()`).
2. Extracts the cell value for the planting week closest to the request `PD`.
3. Uses this WLY as the yield ceiling in QUEFTS.

Soil data follows the same 0.05° grid. Missing cells (water, urban, out-of-extent) return `NA`; the processor falls back to country medians when soil data is unavailable.

### Schedule Planting logic (`R/process-SP.R`)

SP sweeps a window of ±`PD_window` or ±`HD_window` weeks around the requested date, calling the QUEFTS + optimiser pipeline for each candidate week. It then fits a revenue curve over the window and returns the week with the highest expected net revenue, plus a secondary recommendation if a close alternative avoids planting in a risk period.

---

## Extending the Engine

### Adding a new country

1. **Spatial data:** produce soil NPK and WLY NetCDF files at 0.05° resolution following the existing naming convention; bundle and upload to OSF or Zenodo (see [SETUP.md](../SETUP.md#maintainer-publishing-data-updates)).
2. **`R/AkilimoMain.R`:** add the country code to `VALID_COUNTRIES`.
3. **`R/misc.R`:** add `get_currency()` case and default area unit.
4. **`data/input/Default_prices.csv`:** add default cassava price rows.
5. **`data/input/fd2.csv`:** add dry matter content curve rows (one per harvest day, or copy nearest-country values as a placeholder).
6. **Tests:** add at least one `tests/input/in_*.json` per recommendation type for the new country.

### Adding a new recommendation type

1. Create `R/process-XY.R` following the structure of an existing processor.
2. Add the flag field to `validate_request()` and `parse_request()` in `AkilimoMain.R`.
3. Add the dispatch case in `run_akilimo()`.
4. Add translation keys to `translations.csv`.
5. Add test inputs.

### Adding a translation key

1. Add a row to `data/input/translations.csv` with the key and English text.
2. Use `tr("myKey", lang)` in R code.
3. Add translations for other supported languages as available; missing values fall back to English.

### Adding a new language

1. Add a column to `translations.csv` with the IETF tag (e.g. `fr`).
2. Add `"fr"` to the `lang %in% c("en", "sw", ...)` check in `parse_request()`.
3. Update the API reference docs.
4. Publish updated `translations.csv` to OSF or Zenodo (see [SETUP.md](../SETUP.md#maintainer-publishing-data-updates)).

---

## Common Gotchas

**1. Data not found at startup**
The API sources all `R/*.R` files but does not load data eagerly — data is loaded on first request. If `data/` is missing or incomplete, the first request will fail with a file-not-found error. Run `poetry run setup-data` before starting the server.

**2. Coordinates outside raster extent**
If `lat`/`lon` maps to a cell with no soil or yield data (ocean, desert, outside country boundary), the processor may return `NA` yields or fall back silently. The spatial data covers only the agricultural extent of each supported country. Coordinates that are technically in-country but in an unsupported cell will produce a `norecom` (no recommendation) response.

**3. `mailR` requires Java**
Email dispatch (`R/sms_email.R`) uses `mailR`, which is backed by a Java mail library. If `rJava` cannot find a JVM at startup, the entire server process fails. On Linux: `sudo R CMD javareconf`. On Windows: ensure `JAVA_HOME` is set before starting R.

**4. `from_json()` default types must match expected type**
`from_json("someFlag", body, default_value = FALSE)` returns logical. If you mistakenly pass `default_value = 0`, downstream `isTRUE()` checks will silently fail. Always match the default type to what the code expects.

**5. Area unit conversions happen in `parse_request()`, not in processors**
By the time a processor receives `ds`, `ds$areaHa` is always in hectares regardless of what `areaUnits` the farmer supplied. Do not re-convert inside processor code.

**6. `temp/` directory is cleared on each request**
`setup_temp_dir()` is called at the start of every `run_akilimo()` invocation. Do not write files there expecting them to persist across requests. Use `data/` for persistent data.

**7. Translation table is cached per server process**
Changes to `translations.csv` require a server restart to take effect. During development, source `R/get_data.R` and call `rm(.data_cache)` to bust the cache without restarting.

**8. `QUEFTS` operates in dry-weight kg/ha; processors work in fresh-weight t/ha**
All conversions between dry and fresh weight go through `getRFY()` / `getRDY()` in `misc.R`. If you are adding a new calculation that involves yield, always use these helpers — do not apply a fixed conversion factor.
