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
# Development
Rscript api.R

# Production (systemd)
systemctl start akilimo-api.service
systemctl restart akilimo-api.service
systemctl status akilimo-api.service
journalctl -u akilimo-api.service -f
```

### Run Tests
```bash
# Full regression suite (3203 test cases)
Rscript tests/test_full.R

# API integration tests (requires running server on port 8000)
Rscript tests/test_api.R

# Small/quick test subset
Rscript tests/test_small.R

# PDF generation smoke test (requires PhantomJS or Chrome)
# Renders all 8 Rmd templates and checks each output is a valid PDF.
# PhantomJS is auto-installed via webshot::install_phantomjs() if missing.
Rscript tests/test_pdf.R
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

### Request Flow
```
POST /compute (api.R)
    → run_akilimo(postBody) (R/AkilimoMain.R)
        → Parses JSON, extracts country/coords/flags
        → Dispatches to processor(s) based on request flags:
            process-FR.R  — Fertilizer Recommendation
            process-IC.R  — Intercropping
            process-PP.R  — Post-Planting
            process-SP.R  — Soil Preparation
        → Each processor: loads spatial data → QUEFTS crop model → fertilizer optimization
        → Assembles JSON response with recommendations + HTML report
```

### Key Modules
| File | Role |
|------|------|
| `api.R` | Plumber server entry (port 8000, 0.0.0.0) |
| `R/AkilimoMain.R` | Core orchestrator (`run_akilimo()`) |
| `R/quefts.R` | QUEFTS crop growth model |
| `R/optimize_fert.R` | Cost-benefit fertilizer optimization |
| `R/get_data.R` | Loads soil/yield data from NetCDF and CSV |
| `R/fertilizers.R` | Parses fertilizer types, prices, NPK content |
| `R/markdown.R` | Generates HTML recommendation reports |
| `R/sms_email.R` | Notification dispatch |
| `R/misc.R` | `tr(key, lang, ...)` translation helper, `get_currency()`, `getRFY()`/`getRDY()` |

### Translation system
User-facing text lives in `data/input/translations.csv` (wide format: one row per key, columns `en`, `sw`, `rw`). The `tr(key, lang, ...)` function in `misc.R` does the lookup with automatic English fallback. Strings support `{token}` placeholders for dynamic values. See [docs/TRANSLATIONS.md](docs/TRANSLATIONS.md).

### Data Dependencies
Spatial data lives under `./data/` (gitignored):
- `data/yield/` — NetCDF rasters: `{COUNTRY}_WLY_LINTUL_2020SP.nc`
- `data/input/` — CSV lookup tables (translations, defaults, prices)
  - `translations.csv` — wide format: `key,en,sw,rw`

### Path Configuration
`api.R` sets `akpath` based on hostname. On unknown hosts it defaults to `"."`. The data directory is always relative to `akpath`.

## Required R Packages
```r
c("plumber", "limSolve", "ncdf4", "httr", "webshot", "mailR", "knitr", "leaflet", "tinytest")
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
- User: `userName`, `userEmail`, `userPhoneNr`, `userField`
- Notification flags: `email`, `SMS`

Full field reference with types and examples: [docs/API-REFERENCE.md](docs/API-REFERENCE.md)

## Deployment

Production runs as a systemd service. See `systemd/akilimo-api.service.example` for the service template (resource limits: 2GB RAM, 2 CPU cores, 65536 FDs).

GitHub Actions (`deploy-production.yml`) deploys on push to `main` via SSH. Required secrets: `SERVER_HOST`, `SERVER_USER`, `SERVER_SSH_KEY`.

PR reviews are automated via `claude-code-review.yml`. Claude can be invoked on issues/PRs by mentioning `@claude`.
