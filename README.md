# Akilimo Recommendation Engine

R-based REST API that generates science-backed cassava fertilizer and crop management recommendations for smallholder farmers across Sub-Saharan Africa. Supported countries: Nigeria (NG), Tanzania (TZ), Rwanda (RW), Ghana (GH), Burundi (BI).

The engine combines the QUEFTS crop model (Quantitative Evaluation of the Fertility of Tropical Soils) with site-specific spatial data and cost-benefit optimization to produce actionable, localized recommendations in multiple languages.

## Quick Start

### 1. Get the code

```bash
git clone https://github.com/masgeek/akilimo-recommendations.git
cd akilimo-recommendations
```

### 2. Install dependencies

**Linux (Debian/Ubuntu)** — automated:

```bash
chmod +x setup.sh
./setup.sh
```

**Windows / macOS** — run the R installer directly, then set up Python:

```bash
Rscript install_packages.R

cd scripts
cp .env.example .env
poetry install
```

See [SETUP.md](SETUP.md) for full installation details and manual instructions.

### 3. Download runtime data

```bash
cd scripts
poetry run setup-data
# Downloads soil, yield, and input data from Zenodo record 19231022
```

### 4. Start the API

```bash
Rscript api.R
# API listens on http://0.0.0.0:8000
```

### 5. Run a test request

```bash
curl -X POST http://localhost:8000/compute --data "@./tests/input/in_1.json"
```

## Architecture

```
POST /compute (api.R)
    → validate_request() + parse_request() (R/AkilimoMain.R)
        → Validates country, coordinates, flags; normalises all fields
        → Dispatches to the first active processor:
            process-FR.R  — Fertilizer Recommendation
            process-IC.R  — Intercropping (cassava–maize or cassava–sweet potato)
            process-PP.R  — Post-Planting (tillage and ridging advice)
            process-SP.R  — Schedule Planting (optimise planting/harvest dates)
        → Each processor:
            1. get_data()         — loads soil NPK (RDS) + yield rasters (NetCDF)
            2. QUEFTS()           — crop growth model: predicts yield from NPK supply
            3. run_Optim_*()      — cost-benefit optimisation: finds max-profit NPK rate
            4. markdown.R         — renders HTML recommendation report
        → Returns JSON with recommendation text, numeric data, and HTML report
```

Key files:

| File | Role |
|------|------|
| `api.R` | Plumber entry point (port 8000) |
| `R/AkilimoMain.R` | Request validation, parsing, and processor dispatch |
| `R/quefts.R` | QUEFTS crop growth model (yield from NPK supply) |
| `R/optimize_fert.R` | Cost-benefit fertilizer rate optimisation (`L-BFGS-B`) |
| `R/get_data.R` | Loads soil RDS files and yield NetCDF rasters; in-memory cache |
| `R/fertilizers.R` | Parses fertilizer types, bag prices, and NPK content |
| `R/misc.R` | `tr(key, lang, ...)` translation helper, `get_currency()`, `getRFY()`/`getRDY()` |
| `R/markdown.R` | Renders Rmd → HTML recommendation report |
| `R/sms_email.R` | Email (mailR/Java) and SMS dispatch |

## Recommendation Types

| Flag | Type | Description |
|------|------|-------------|
| `FR` | Fertilizer Recommendation | Optimal NPK rates and expected yield/revenue gain |
| `IC` | Intercropping | Cassava–maize (NG) or cassava–sweet potato (TZ) intercrop advice |
| `PP` | Post-Planting | Tillage and ridging cost-benefit analysis |
| `SPP` / `SPH` | Schedule Planting | Optimal planting or harvest date window |

Only one recommendation is returned per request. Priority order when multiple flags are set: FR → IC → PP → SP.

## Internationalisation

Response text language is controlled by the `lang` field in the request body (default `"en"`):

| Value | Language |
|-------|----------|
| `"en"` | English (default) |
| `"sw"` | Swahili |

`lang` is independent of `country` — any country can request any supported language. Translation strings live in `data/input/translations.csv`. See [docs/TRANSLATIONS.md](docs/TRANSLATIONS.md) for how to add keys or new languages.

## Testing

```bash
# Full regression suite (3203 cases)
Rscript tests/test_full.R

# API integration tests (server must be running on port 8000)
Rscript tests/test_api.R

# Quick smoke test
Rscript tests/test_small.R
```

## Production Deployment

The API runs as a systemd service. Copy the template and configure it for your server:

```bash
cp systemd/akilimo-api.service.example systemd/akilimo-api.service
# Edit paths and environment variables, then:
sudo ln -s $(pwd)/systemd/akilimo-api.service /etc/systemd/system/akilimo-api.service
sudo systemctl daemon-reload
sudo systemctl enable --now akilimo-api.service
```

Resource defaults: 2 GB RAM, 2 CPU cores, 65536 open files.

Deployment is automated via GitHub Actions (`deploy-production.yml`) on push to `main`.
Required secrets: `SERVER_HOST`, `SERVER_USER`, `SERVER_SSH_KEY`.

### Service management

```bash
sudo systemctl status akilimo-api.service
sudo systemctl restart akilimo-api.service
sudo journalctl -u akilimo-api.service -f
```

## Data Management (maintainers)

Runtime data is hosted on Zenodo record **19231022**. See [SETUP.md](SETUP.md) for the full publishing workflow.

```bash
cd scripts
poetry run bundle-assets          # pack data dirs → dist/*.tar.gz
poetry run upload-zenodo --new    # create Zenodo deposit + upload
```

## Documentation

| Document | Description |
|----------|-------------|
| [SETUP.md](SETUP.md) | Full installation and data download guide |
| [docs/ONBOARDING.md](docs/ONBOARDING.md) | Technical onboarding for developers and data scientists |
| [docs/API-REFERENCE.md](docs/API-REFERENCE.md) | Complete API field reference with examples |
| [docs/TRANSLATIONS.md](docs/TRANSLATIONS.md) | Translation system: CSV format, adding keys/languages, token substitution |
| [docs/CODE-REVIEW.md](docs/CODE-REVIEW.md) | Automated code review (security, logic, performance) |
| [docs/FIX-CHECKLIST.md](docs/FIX-CHECKLIST.md) | Fix tracking for CODE-REVIEW issues |

## License

MIT License — see [LICENSE](LICENSE) for details.

## Contributors

- [@rhijmans](https://github.com/rhijmans)
- [@omilika](https://github.com/omilika)
- [@masgeek](https://github.com/masgeek)
