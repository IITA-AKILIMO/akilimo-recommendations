# Setup Guide

All runtime data files (images, CSV tables, soil data, yield data) are hosted on **OSF** (Open Science Framework) and **Zenodo** and must be downloaded before the API can serve requests. Both sources contain the same files — choose whichever works best for you.

---

## Automated Setup (recommended)

The `setup.sh` script handles everything: system packages, R packages, Python tooling, and data download.

```bash
chmod +x setup.sh
./setup.sh
```

Options:

```bash
./setup.sh --no-system   # skip apt-get (manage system packages yourself)
./setup.sh --no-data     # skip data download (do it later)
```

The script targets **Debian/Ubuntu**. For other distros, pass `--no-system` and install the [system packages](#system-packages) manually before running.

---

## Manual Setup

### System packages

**Debian/Ubuntu:**

```bash
sudo apt-get update
sudo apt-get install -y \
  pandoc \
  openjdk-17-jre-headless \
  libnetcdf-dev libhdf5-dev \
  wkhtmltopdf \
  libssl-dev libcurl4-openssl-dev libxml2-dev
```

**Purpose:**

| Package | Required by |
|---------|------------|
| `pandoc` | knitr / rmarkdown — Rmd → HTML conversion |
| `openjdk-17-jre-headless` | mailR (sends email via Java) |
| `libnetcdf-dev`, `libhdf5-dev` | ncdf4 R package — reads yield raster data |
| `wkhtmltopdf` | webshot — renders HTML reports to PDF |
| `libssl-dev`, `libcurl4-openssl-dev` | httr R package — HTTP requests |
| `libxml2-dev` | xml2 R package (dependency of several packages) |

### R packages

Run the provided installer script — works on Linux, macOS, and Windows:

```bash
Rscript install_packages.R
```

This installs all 21 required packages, downloads phantomjs for `webshot`, and checks that rJava is working for `mailR`.

**Java requirement for `mailR`:**

| Platform | Action |
|----------|--------|
| Linux | `sudo apt-get install openjdk-17-jre-headless && sudo R CMD javareconf` |
| Windows | Install JDK from [adoptium.net](https://adoptium.net), set `JAVA_HOME`, re-run the script |
| macOS | `brew install openjdk`, then `sudo R CMD javareconf` |

### Environment configuration

There are two separate `.env` files — one per subsystem:

| File | Used by | Copy from |
|------|---------|-----------|
| `.env` (project root) | R API — server path, SMS/email credentials | `.env.example` |
| `scripts/.env` | Python data scripts — OSF/Zenodo IDs and tokens | `scripts/.env.example` |

```bash
# R API credentials
cp .env.example .env

# Python / OSF / Zenodo config
cp scripts/.env.example scripts/.env
```

Neither file is committed (both matched by `.gitignore`).

### Python data scripts

Requires Python 3.13+ and [Poetry](https://python-poetry.org/docs/#installation).

```bash
cd scripts
cp .env.example .env    # OSF_NODE_ID and ZENODO_RECORD_ID=19231022 are pre-set
poetry install
```

### Download runtime data

The data is available from two sources — **OSF** (default) and **Zenodo**. Both contain identical files.

```bash
cd scripts

# OSF (default — no token required for public projects)
poetry run setup-data
poetry run setup-data --source osf --osf-node-id rcjv5

# Zenodo (alternative)
poetry run setup-data --source zenodo
poetry run setup-data --source zenodo --zenodo-id 19231022
poetry run setup-data --source zenodo --sandbox   # sandbox.zenodo.org
```

This downloads and extracts these bundles:

| Bundle | Extracted to | Contents |
|--------|-------------|----------|
| `net-assets.tar.gz` | `net/` | Recommendation images (PNG) |
| `data-input.tar.gz` | `data/input/` | CSV lookup tables (prices, translations, defaults) |
| `soil-data.tar.gz` | `data/soil/` | Soil NPK RDS files |
| `yield-data.tar.gz` | `data/yield/` | Yield NetCDF rasters (LINTUL model) |

Re-running is safe — already-extracted files are preserved.

---

## Verify the setup

```bash
# Terminal 1
Rscript api.R

# Terminal 2
curl -X POST http://localhost:8000/compute --data "@./tests/input/in_1.json"
```

---

## Maintainer: Publishing Data Updates

### 1. Bundle the data directories

```bash
cd scripts
poetry run bundle-assets
# Produces: dist/net-assets.tar.gz  dist/data-input.tar.gz
#           dist/soil-data.tar.gz   dist/yield-data.tar.gz
```

Bundle specific groups only:

```bash
poetry run bundle-assets --only soil --only yield
```

### 2. Upload — choose OSF or Zenodo

#### Option A: OSF (recommended)

Add your token to `scripts/.env`:

```dotenv
OSF_TOKEN=your-personal-access-token   # scope: osf.full_write
```

First upload — creates a new project:

```bash
poetry run upload-osf --new
# Prints node ID — save it as OSF_NODE_ID in .env
```

Update an existing project:

```bash
poetry run upload-osf                          # uses OSF_NODE_ID from .env
poetry run upload-osf --node-id abc12          # or pass explicitly
```

Test against the OSF sandbox first:

```bash
poetry run upload-osf --new --sandbox
```

After upload, open the project URL, adjust metadata, and make the project **public** to allow unauthenticated downloads.  
Update `scripts/.env` and `scripts/.env.example` with the node ID:

```dotenv
OSF_NODE_ID=rcjv5
```

#### Option B: Zenodo

Add your token to `scripts/.env`:

```dotenv
ZENODO_TOKEN=your-personal-access-token   # scope: deposit:write
```

First upload — creates a new deposit:

```bash
poetry run upload-zenodo --new
# Prints deposit ID — save it as ZENODO_DEPOSIT_ID in .env
```

Update an existing draft:

```bash
poetry run upload-zenodo                           # uses ZENODO_DEPOSIT_ID from .env
poetry run upload-zenodo --deposit-id 1234567      # or pass explicitly
```

Test against the sandbox first:

```bash
poetry run upload-zenodo --new --sandbox
```

### 3. Publish and update the record ID (Zenodo)

1. Open the deposit URL printed after upload.
2. Review metadata and click **Publish**.
3. Copy the published **Record ID** and update `scripts/.env` and `scripts/.env.example`:

```dotenv
ZENODO_RECORD_ID=<new-record-id>
```

Also update the record ID in this file and in `setup.sh` so other users download the correct version.
