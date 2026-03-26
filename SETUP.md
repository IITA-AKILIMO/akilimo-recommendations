# Data Setup Guide

All runtime data files (images, CSV tables, soil data, yield data) are hosted on Zenodo and must be downloaded before the API can serve requests.

## Prerequisites

- Python 3.13+
- [Poetry](https://python-poetry.org/docs/#installation)

## First-Time Setup

### 1. Configure the environment

```bash
cd scripts
cp .env.example .env
```

Open `scripts/.env` and set the Zenodo record ID:

```dotenv
ZENODO_RECORD_ID=19231022
```

> The record ID `19231022` is the published Zenodo deposit that contains all Akilimo data assets.
> Leave all other values at their defaults unless instructed otherwise.

### 2. Install Python dependencies

```bash
cd scripts
poetry install
```

### 3. Download all data files

```bash
poetry run setup-data
```

This downloads and extracts four asset bundles from Zenodo into the correct project directories:

| Bundle | Extracted to |
|--------|-------------|
| `net-assets.tar.gz` | `net/` (recommendation images) |
| `data-input.tar.gz` | `data/input/` (CSV lookup tables) |
| `soil-data.tar.gz` | `data/soil/` (soil NPK RDS files) |
| `yield-data.tar.gz` | `data/yield/` (yield NetCDF rasters) |

Re-running is safe — already-extracted files are preserved.

### 4. Verify

Start the API and run the smoke test:

```bash
# Terminal 1
Rscript api.R

# Terminal 2
curl -X POST http://localhost:8000/compute --data "@./tests/input/in_1.json"
```

---

## CLI Reference

```bash
# Download from a specific record (overrides .env)
poetry run setup-data --zenodo-id 19231022

# Use the Zenodo sandbox (for testing, not real data)
poetry run setup-data --sandbox
```

---

## Maintainer: Publishing Data Updates

### 1. Bundle the data directories

```bash
cd scripts
poetry run bundle-assets
# Produces dist/net-assets.tar.gz, dist/data-input.tar.gz,
#          dist/soil-data.tar.gz,  dist/yield-data.tar.gz
```

Bundle specific groups only:

```bash
poetry run bundle-assets --only soil --only yield
```

### 2. Upload to Zenodo

Add your Zenodo personal access token to `scripts/.env`:

```dotenv
ZENODO_TOKEN=your-token-here   # scope: deposit:write
```

First upload (creates a new deposit):

```bash
poetry run upload-zenodo --new
# Prints the deposit ID — save it in .env as ZENODO_DEPOSIT_ID
```

Update an existing draft:

```bash
poetry run upload-zenodo                          # uses ZENODO_DEPOSIT_ID from .env
poetry run upload-zenodo --deposit-id 1234567     # explicit ID
```

Test uploads against the sandbox before going live:

```bash
poetry run upload-zenodo --new --sandbox
```

### 3. Publish and update the record ID

1. Open the deposit URL printed after upload.
2. Review metadata and click **Publish**.
3. Copy the published **Record ID** and update `scripts/.env`:

```dotenv
ZENODO_RECORD_ID=<new-record-id>
```

Update this file and any deployment documentation so other users get the correct record ID.
