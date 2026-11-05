# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

R-based REST API for agricultural (cassava farming) recommendations. Supported countries: Nigeria (NG), Tanzania (TZ), Rwanda (RW), Ghana (GH), Burundi (BI).

## Directories to Ignore

- **`old/`** — archived/obsolete files; not part of the active codebase
- **`R/preprocess/`** — offline data preparation scripts; not sourced by the API

Do not analyse, modify, or source files from these directories.

## First-Time Setup

All runtime data files (images, CSVs, soil data, yield data) are hosted on **OSF** and **Zenodo** — both contain the same content. Setup scripts live in `scripts/` and are managed with Poetry.

```bash
cd scripts
cp .env.example .env
# edit .env: set OSF_NODE_ID (for OSF) or ZENODO_RECORD_ID (for Zenodo)

poetry install
poetry run setup-data                      # OSF (default)
poetry run setup-data --source zenodo      # or Zenodo
```

Re-running is safe — already-extracted files are preserved.

### Publishing data files (maintainers only)

```bash
cd scripts
cp .env.example .env
# edit .env: set OSF_TOKEN (for OSF) or ZENODO_TOKEN (for Zenodo)

poetry install

# 1. Bundle all four data directories → dist/*.tar.gz
poetry run bundle-assets

# 2a. Upload to OSF (recommended)
poetry run upload-osf --new               # first time → prints node ID
poetry run upload-osf                     # update existing (uses OSF_NODE_ID from .env)

# 2b. Upload to Zenodo (alternative)
poetry run upload-zenodo --new            # first time → prints deposit ID
poetry run upload-zenodo                  # update draft (uses ZENODO_DEPOSIT_ID from .env)

# 3. After publishing, update the ID in .env:
#    OSF_NODE_ID=rcjv5        (OSF)
#    ZENODO_RECORD_ID=1234567 (Zenodo)
```

## Commands

### Start the API
```bash
# Development (plain)
Rscript api.R

# Development (hot-reload — restarts on file changes)
./dev.sh        # Linux / macOS
dev.bat         # Windows

# Production (systemd)
systemctl start akilimo-api.service
systemctl restart akilimo-api.service
systemctl status akilimo-api.service
journalctl -u akilimo-api.service -f
```

`API_HOST` and `API_PORT` in `.env` control the bind address and port (defaults: `0.0.0.0:8000`).

### Run Tests
```bash
# Full regression suite (3203 test cases)
Rscript tests/test_full.R

# API integration tests (requires running server on port 8000)
Rscript tests/test_api.R

# Small/quick test subset
Rscript tests/test_small.R

# WeasyPrint PDF smoke test
Rscript tests/test_weasyprint_smoke.R

# What CI runs (no data files needed)
Rscript tests/check_parse.R
Rscript tests/test_akilimo_db.R
Rscript -e 'tinytest::run_test_file("tests/test_pdf_template.R")'   # PDF template labels and rows
```

### Single test via curl
```bash
# Start server first, then run an existing fixture:
curl -X POST http://localhost:8000/compute \
  -H "Content-Type: application/json" \
  --data "@./tests/input/in_1_TZ_FR_starch_factory_riskAtt0.json"
```

Test fixtures are in `tests/input/` named `in_{N}_{COUNTRY}_{TYPE}_{key_params}.json`.
When adding a new fixture, also register it in the `test_files` vector in
`tests/test_small.R` and `tests/test_api.R`.

## Architecture

See [docs/DATA-FLOW.md](docs/DATA-FLOW.md) for a diagram of how a request moves through validation, data lookup, optimisation, and PDF/email generation.

### Request Flow
```
POST /compute (api.R)
    → run_akilimo(postBody) (R/AkilimoMain.R)
        → validate_request(): field/range checks + is_point_covered() coverage gate
          (R/get_data.R — rejects lat/lon outside the country's yield grid with a 400)
        → Parses all fields
        → Creates temp dir: temp/{YYYYMMDD_HHMMSS}_{COUNTRY}_{TYPE}_{rand4}/
        → Dispatches to processor based on request flags:
            process-FR.R  — Fertilizer Recommendation
            process-IC.R  — Intercropping (NG maize / TZ sweet potato)
            process-PP.R  — Post-Planting tillage
            process-SP.R  — Scheduled Planting
        → Each processor: loads spatial data → QUEFTS → optimisation → returns result list
        → generate_pdfs() → build_*_pdf() → WeasyPrint → PDF files
        → sendEmailReport() if requested
        → Assembles JSON response (PDF failures never suppress the recommendation)

GET /coverage, GET /coverage/<country> (api.R)
    → get_coverage_geojson() (R/coverage.R)
        → Builds (and caches, per server process) a GeoJSON polygon footprint
          per country from the same yield grid is_point_covered() uses
        → Returns a GeoJSON FeatureCollection for consumption by other services
```

### Key Modules
| File | Role |
|------|------|
| `api.R` | Plumber server entry; bind address/port from `API_HOST`/`API_PORT` |
| `R/AkilimoMain.R` | Core orchestrator (`run_akilimo()`); validation, parsing, dispatch |
| `R/quefts.R` | QUEFTS crop growth model |
| `R/optimize_fert.R` | Cost-benefit fertilizer optimization |
| `R/get_data.R` | Loads soil/yield data from NetCDF and CSV; `is_point_covered()` coverage gate |
| `R/coverage.R` | Builds/caches per-country GeoJSON coverage footprints for `GET /coverage` |
| `R/fertilizers.R` | Parses fertilizer types, prices, NPK content |
| `R/markdown.R` | Shared helpers: `FERT_COLOUR`, `FERT_LABEL`, `calc_fertilizer_recom()` |
| `R/html_helpers.R` | HTML fragment builders for WeasyPrint PDFs; `.PDF_LABELS` label lookup |
| `R/pdf_builders.R` | `build_fr/ic/pp/sp_pdf()` — one builder per recommendation type |
| `R/email.R` | PDF generation orchestration; email dispatch (smtp/mailtrap/mailgun) |
| `R/misc.R` | `tr(key, lang, ...)` translation helper, `get_currency()`, `getRFY()`/`getRDY()` |

### PDF pipeline
PDF reports are generated by [WeasyPrint](https://weasyprint.org) (Python CLI). Install with `pip install weasyprint`. No browser or Java required. See [docs/TECH-DEBT.md § PDF pipeline](docs/TECH-DEBT.md#pdf-pipeline-weasyprint) for architecture notes.

### Translation system
User-facing recommendation text lives in `data/input/translations.csv` (wide format: one row per key, columns `en`, `sw`, `rw`). The `tr(key, lang, ...)` function in `misc.R` does the lookup with automatic English fallback. Strings support `{token}` placeholders for dynamic values. See [docs/TRANSLATIONS.md](docs/TRANSLATIONS.md).

PDF section headings and UI labels are stored in `.PDF_LABELS` in `R/html_helpers.R` and looked up via `html_label(key, lang)`. They are kept separate from `translations.csv` because they contain HTML markup not suitable for plain-text email bodies.

### Data Dependencies
Spatial data lives under `./data/` (gitignored):
- `data/yield/` — NetCDF rasters: `{COUNTRY}_WLY_LINTUL_2020SP.nc`
- `data/input/` — CSV lookup tables (translations, defaults, prices)
  - `translations.csv` — wide format: `key,en,sw,rw`

### Path and Server Configuration
`AKILIMO_ROOT` env var sets `akpath` (defaults to `"."`). `API_HOST` and `API_PORT` control the Plumber bind address and port (defaults: `0.0.0.0:8000`). All file paths are relative to `akpath`.

## Required R Packages
```r
c("plumber", "limSolve", "ncdf4", "httr", "mailR", "base64enc",
  "ggplot2", "ragg", "scales", "jsonlite", "DBI", "RSQLite",
  "dotenv", "lubridate", "dplyr", "tidyr", "tinytest", "sf")
```

## API Request Schema
POST `/compute` JSON payload requires:
- `country` — NG | TZ | RW | GH | BI
- `lang` — response language: `en` (default) | `sw` (Swahili); independent of country
- `lat`, `lon` — coordinates
- `area`, `areaUnits` — farm size (ha, acre, are, m2, string, ekari)
- Boolean flags: `FR`, `PP`, `IC`, `SPP`, `SPH`
- Dates: `PD`, `HD` (YYYY-MM-DD, planting/harvest)
- Crop params: `cassUP`, `cassUW`, `cassPD`, `FCY`, `CMP`, `maxInv`
- Fertilizer entries: `*available`, `*CostperBag`, `*BagWt` per fertilizer type
- User: `userName`, `userEmail`, `userField`
- Notification flags: `email`

Full field reference with types and examples: [docs/API-REFERENCE.md](docs/API-REFERENCE.md)

## Deployment

Two instances run in parallel on the same server:

| Instance | Branch | Directory | Service | Workflow |
|----------|--------|-----------|---------|----------|
| Production | `main` | `/mnt/data/extra_storage/projects/new_akilimo` | `akilimo-api.service` | `pipeline.yml` |
| Beta | `experimental` | `/data/extra_storage/projects/akilimo-beta` | `akilimo-api-beta.service` | `pipeline.yml` |

Branch, directory and service per instance are defined once in `.github/deploy-targets.json` (read by the `.github/actions/resolve-target` action) — edit them there, not in the workflows.

A single workflow, `pipeline.yml` ("Build and deploy"), runs on every push and pull request, with one job per stage:

```
Plan → Wait for more pushes → Check R syntax
                            → Install R packages → Test price database / Test PDF templates
     → Pull runtime data (manual runs, optional) → Update code and packages → Restart service
     → Check health → Tag release (production only)
```

`Plan` matches the branch to a target in `deploy-targets.json`; any other branch or a pull request stops after the checks. Server jobs run over SSH using the repo variables `SERVER_HOST`, `SERVER_USER` and the secret `SERVER_SSH_KEY`, only after every check passes, and share a per-target lock. `Check health` fails if `GET /health` does not respond within 180s. R packages are cached under a key made from `install_packages.R`: `Install R packages` installs them only when that file changes or no cache exists, otherwise it just finds the cache; the test jobs only restore it. Shared steps live in composite actions under `.github/actions/` (`resolve-target`, `setup-r-env`, `pull-data`).

Runtime data (OSF/Zenodo) is pulled by the `Pull runtime data` job when "Build and deploy" is run by hand with `setup_data` ticked, or on its own by `setup-zenodo-data.yml` ("Data setup") with a `target` of `production` or `beta`. Both targets share the `ZENODO_RECORD_ID` / `OSF_NODE_ID` secrets.

Service templates: `systemd/akilimo-api.service.example` (production), `systemd/akilimo-api-beta.service.example` (beta). Resource limits: 2GB RAM, 2 CPU cores, 65536 FDs.

On pull requests and feature-branch pushes, `pipeline.yml` runs only the package install and check jobs (no data files needed); a newer push cancels the older run.
