#!/usr/bin/env bash
# =============================================================================
# Akilimo Recommendation Engine — full environment setup
#
# Installs all system packages, R packages, Python tooling, and downloads
# runtime data from OSF (default) or Zenodo.
#
# Usage:
#   chmod +x setup.sh
#   ./setup.sh              # full setup (system + R + Python + data)
#   ./setup.sh --no-data    # skip data download
#   ./setup.sh --no-system  # skip apt/system packages (e.g. you manage them)
# =============================================================================

set -euo pipefail

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

INSTALL_SYSTEM=true
DOWNLOAD_DATA=true

for arg in "$@"; do
  case $arg in
    --no-system) INSTALL_SYSTEM=false ;;
    --no-data)   DOWNLOAD_DATA=false  ;;
    --help|-h)
      echo "Usage: $0 [--no-system] [--no-data]"
      exit 0
      ;;
  esac
done

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

info()    { echo "[INFO]  $*"; }
success() { echo "[OK]    $*"; }
warn()    { echo "[WARN]  $*" >&2; }
die()     { echo "[ERROR] $*" >&2; exit 1; }

require_cmd() {
  command -v "$1" &>/dev/null || die "'$1' is not installed or not in PATH."
}

# ---------------------------------------------------------------------------
# 1. System packages (Debian/Ubuntu)
# ---------------------------------------------------------------------------

if $INSTALL_SYSTEM; then
  info "Installing system packages..."

  if ! command -v apt-get &>/dev/null; then
    warn "apt-get not found. Install system packages manually:"
    warn "  pandoc  openjdk-17-jre-headless  libnetcdf-dev  libhdf5-dev"
    warn "  wkhtmltopdf  libssl-dev  libcurl4-openssl-dev  libxml2-dev"
  else
    sudo apt-get update -qq
    sudo apt-get install -y \
      pandoc \
      openjdk-17-jre-headless \
      libnetcdf-dev \
      libhdf5-dev \
      wkhtmltopdf \
      libssl-dev \
      libcurl4-openssl-dev \
      libxml2-dev \
      libfontconfig1-dev \
      libharfbuzz-dev \
      libfribidi-dev \
      libfreetype6-dev \
      libpng-dev \
      libtiff5-dev \
      libjpeg-dev
    success "System packages installed."
  fi
fi

# ---------------------------------------------------------------------------
# 2. R packages
# ---------------------------------------------------------------------------

info "Installing R packages..."
require_cmd Rscript

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
Rscript "$SCRIPT_DIR/install_packages.R"

success "R packages ready."

# ---------------------------------------------------------------------------
# R API environment file
# ---------------------------------------------------------------------------

if [ ! -f "$SCRIPT_DIR/.env" ]; then
  cp "$SCRIPT_DIR/.env.example" "$SCRIPT_DIR/.env"
  info "Created .env from .env.example — edit it to set AKILIMO_ROOT, Plivo, and email credentials."
fi

# ---------------------------------------------------------------------------
# 3. Python / Poetry setup
# ---------------------------------------------------------------------------

info "Setting up Python data scripts..."

SCRIPTS_DIR="$(cd "$(dirname "$0")/scripts" && pwd)"

if ! command -v poetry &>/dev/null; then
  warn "Poetry not found. Installing via official installer..."
  curl -sSL https://install.python-poetry.org | python3 -
  export PATH="$HOME/.local/bin:$PATH"
fi

require_cmd poetry

(
  cd "$SCRIPTS_DIR"

  if [ ! -f .env ]; then
    cp .env.example .env
    info "Created scripts/.env from .env.example."
    info "Edit scripts/.env to set OSF_NODE_ID (OSF) or ZENODO_RECORD_ID (Zenodo)."
  fi

  poetry install --no-interaction
)

success "Python dependencies installed."

# ---------------------------------------------------------------------------
# 4. Download runtime data
# ---------------------------------------------------------------------------

if $DOWNLOAD_DATA; then
  info "Downloading runtime data (OSF by default; set --source zenodo in scripts/.env to use Zenodo)..."
  (
    cd "$SCRIPTS_DIR"
    poetry run setup-data
  )
  success "Runtime data downloaded and extracted."
else
  info "Skipping data download (--no-data)."
  info "Run later: cd scripts && poetry run setup-data"
fi

# ---------------------------------------------------------------------------
# Done
# ---------------------------------------------------------------------------

echo ""
echo "======================================================================"
echo "  Setup complete."
echo ""
echo "  Start the API:"
echo "    Rscript api.R"
echo ""
echo "  Run tests:"
echo "    Rscript tests/test_small.R"
echo "======================================================================"
