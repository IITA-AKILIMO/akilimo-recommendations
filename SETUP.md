# Setup Guide

All runtime data files (images, CSV tables, soil data, yield data) are hosted on Zenodo and must be downloaded before the API can serve requests.

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
./setup.sh --no-data     # skip Zenodo download (do it later)
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

```bash
Rscript -e "
install.packages(c(
  'plumber', 'limSolve', 'ncdf4', 'httr', 'mailR',
  'webshot', 'knitr', 'rmarkdown',
  'leaflet', 'mapview',
  'jsonlite', 'tidyr', 'dplyr', 'plyr', 'lubridate',
  'ggplot2', 'kableExtra', 'scales', 'png',
  'randomForest', 'tinytest'
), repos = 'https://cloud.r-project.org')
"
```

After installing `webshot`, fetch phantomjs (used to render HTML → PDF):

```r
webshot::install_phantomjs()
```

If `mailR` fails to load, reconfigure R's Java environment:

```bash
sudo R CMD javareconf
```

### Python data scripts

Requires Python 3.13+ and [Poetry](https://python-poetry.org/docs/#installation).

```bash
cd scripts
cp .env.example .env    # ZENODO_RECORD_ID=19231022 is pre-set
poetry install
```

### Download runtime data

```bash
cd scripts
poetry run setup-data
```

This downloads and extracts four bundles from Zenodo record **19231022**:

| Bundle | Extracted to | Contents |
|--------|-------------|----------|
| `net-assets.tar.gz` | `net/` | Recommendation images (PNG) |
| `data-input.tar.gz` | `data/input/` | CSV lookup tables (prices, translations, defaults) |
| `soil-data.tar.gz` | `data/soil/` | Soil NPK RDS files |
| `yield-data.tar.gz` | `data/yield/` | Yield NetCDF rasters (LINTUL model) |

Re-running is safe — already-extracted files are preserved.

To use a different Zenodo record or the sandbox:

```bash
poetry run setup-data --zenodo-id 19231022
poetry run setup-data --sandbox
```

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

### 2. Upload to Zenodo

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

### 3. Publish and update the record ID

1. Open the deposit URL printed after upload.
2. Review metadata and click **Publish**.
3. Copy the published **Record ID** and update `scripts/.env` and `scripts/.env.example`:

```dotenv
ZENODO_RECORD_ID=<new-record-id>
```

Also update the record ID in this file and in `setup.sh` so other users download the correct version.
