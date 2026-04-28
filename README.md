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
poetry run setup-data                      # OSF (default)
poetry run setup-data --source zenodo      # or from Zenodo record 19231022
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
| `R/email.R` | PDF generation orchestration; email dispatch (smtp/mailtrap/mailgun) |

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

### Test suites

| Script | What it runs | Prerequisite |
|--------|-------------|--------------|
| `tests/test_small.R` | 29 representative cases in-process | Data downloaded |
| `tests/test_full.R` | 3203 regression cases in-process | Data downloaded |
| `tests/test_api.R` | POSTs all fixtures to the live server | Server on port 8000 |

```bash
# In-process (no server needed)
Rscript tests/test_small.R
Rscript tests/test_full.R

# Against a running server
Rscript api.R &
Rscript tests/test_api.R
```

### Single request via curl

Test fixtures are in `tests/input/` named `in_{N}_{COUNTRY}_{TYPE}_{params}.json`:

```bash
# Start the server first
Rscript api.R

# Run a single fixture
curl -X POST http://localhost:8000/compute \
  -H "Content-Type: application/json" \
  --data "@./tests/input/in_1_TZ_FR_starch_factory_riskAtt0.json"

# Or send an ad-hoc payload
curl -X POST http://localhost:8000/compute \
  -H "Content-Type: application/json" \
  -d '{"country":"NG","lat":7.55,"lon":4.51,"area":1,"areaUnits":"ha",
       "FR":true,"IC":false,"PP":false,"SPP":false,"SPH":false,
       "PD":"2025-05-01","HD":"2026-02-01","FCY":11}'
```

### Adding a new fixture

Name the file `in_{N}_{COUNTRY}_{TYPE}_{key_params}.json` where:
- `N` — next sequential number
- `COUNTRY` — NG, TZ, GH, RW, or BI
- `TYPE` — FR, IC, PP, or SP
- `key_params` — brief description (e.g. `starch_factory_riskAtt1`, `custom_price_maxInv`)

Then add the filename to the `test_files` vector in `tests/test_small.R` and `tests/test_api.R`.

## Deployment

### Production (`main` branch)

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

```bash
sudo systemctl status akilimo-api.service
sudo systemctl restart akilimo-api.service
sudo journalctl -u akilimo-api.service -f
```

### Beta (`experimental` branch)

A parallel beta instance runs alongside production from a separate directory and port. It uses the same server secrets.

```bash
# Clone into the beta directory
git clone <repo-url> /home/akilimo/projects/akilimo-beta
cd /home/akilimo/projects/akilimo-beta
git checkout experimental

# Set a different port in .env (e.g. 8001)
cp .env.example .env   # edit: API_PORT=8001

# Install the service
sudo cp systemd/akilimo-api-beta.service.example /etc/systemd/system/akilimo-api-beta.service
sudo systemctl daemon-reload
sudo systemctl enable --now akilimo-api-beta.service
```

Deployment is automated via GitHub Actions (`deploy-experimental.yml`) on push to `experimental`.
Uses the same secrets as production: `SERVER_HOST`, `SERVER_USER`, `SERVER_SSH_KEY`.

```bash
sudo systemctl status akilimo-api-beta.service
sudo systemctl restart akilimo-api-beta.service
sudo journalctl -u akilimo-api-beta -f
```

## Data Management (maintainers)

Runtime data is hosted on **OSF** and **Zenodo** (both contain identical files). See [SETUP.md](SETUP.md) for the full publishing workflow.

```bash
cd scripts
poetry run bundle-assets           # pack data dirs → dist/*.tar.gz
poetry run upload-osf --new        # create OSF project + upload (recommended)
poetry run upload-zenodo --new     # or create Zenodo deposit + upload
```

## Documentation

| Document | Description |
|----------|-------------|
| [docs/SETUP.md](docs/SETUP.md) | Full installation and data download guide |
| [docs/ONBOARDING.md](docs/ONBOARDING.md) | Technical onboarding for developers and data scientists |
| [docs/API-REFERENCE.md](docs/API-REFERENCE.md) | Complete API field reference with examples |
| [docs/TRANSLATIONS.md](docs/TRANSLATIONS.md) | Translation system: CSV format, adding keys/languages, token substitution |
| [docs/TECH-DEBT.md](docs/TECH-DEBT.md) | Open issues, deferred tech debt, and architecture notes |

## License

MIT License — see [LICENSE](LICENSE) for details.

## Contributors

- [@rhijmans](https://github.com/rhijmans)
- [@omilika](https://github.com/omilika)
- [@masgeek](https://github.com/masgeek)
