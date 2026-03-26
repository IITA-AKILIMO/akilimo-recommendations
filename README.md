# Akilimo Recommendation Engine

R-based REST API that generates cassava fertilizer and crop management recommendations. Supported countries: Nigeria (NG), Tanzania (TZ), Rwanda (RW), Ghana (GH), Burundi (BI).

## Quick Start

### 1. Get the code

```bash
git clone https://github.com/masgeek/akilimo-recommendations.git
cd akilimo-recommendations
```

### 2. Run the setup script

The `setup.sh` script installs all system packages, R packages, Python tooling, and downloads runtime data from Zenodo in one step:

```bash
chmod +x setup.sh
./setup.sh
```

For manual installation or non-Debian systems, see [SETUP.md](SETUP.md).

### 3. Start the API

```bash
Rscript api.R
# API listens on http://0.0.0.0:8000
```

### 4. Run a test request

```bash
curl -X POST http://localhost:8000/compute --data "@./tests/input/in_1.json"
```

## Architecture

```
POST /compute (api.R)
    → run_akilimo() (R/AkilimoMain.R)
        → Parses request: country, coordinates, flags
        → Dispatches to processor(s):
            process-FR.R  — Fertilizer Recommendation
            process-IC.R  — Intercropping
            process-PP.R  — Post-Planting
            process-SP.R  — Soil Preparation
        → QUEFTS crop model + fertilizer optimization
        → Returns JSON with recommendations + HTML report
```

Key files:

| File | Role |
|------|------|
| `api.R` | Plumber entry point (port 8000) |
| `R/AkilimoMain.R` | Core orchestrator |
| `R/quefts.R` | QUEFTS crop growth model |
| `R/optimize_fert.R` | Cost-benefit fertilizer optimization |
| `R/get_data.R` | Loads NetCDF rasters and CSV lookup tables |

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

## License

MIT License — see [LICENSE](LICENSE) for details.

## Contributors

- [@rhijmans](https://github.com/rhijmans)
- [@omilika](https://github.com/omilika)
- [@masgeek](https://github.com/masgeek)
