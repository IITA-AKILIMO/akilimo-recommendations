#!/bin/sh
# Build the scheduled-planting (SP) NetCDF files that are not in the OSF data bundle.
#
# RW_WLY_LINTUL_2020SP.nc is derived from data/yield/Rwanda_WLY_LINTUL.RDS (see
# tools/build_sp_nc.R), so it is built here instead of being published. An
# existing file is kept, so a file published in the bundle always wins. Does
# nothing, and never fails, when the yield data is not there yet.
#
# Run from anywhere:  sh tools/ensure_sp_files.sh
# Used by CI, by the deploy's code update and by the server data pull.

cd "$(dirname "$0")/.." || exit 1

if [ ! -f data/yield/RW_WLY_LINTUL_2020SP.nc ] && [ -f data/yield/Rwanda_WLY_LINTUL.RDS ]; then
  echo "Building the Rwanda scheduled-planting file..."
  Rscript tools/build_sp_nc.R RW
fi

# Burundi uses the same LINTUL output (53 weekly planting dates, all year); its
# scheduled-planting soil is the FR soil by yield class (R/get_data.R, get_sp_soil)
if [ ! -f data/yield/BI_WLY_LINTUL_2020SP.nc ] && [ -f data/yield/Burundi_WLY_LINTUL.RDS ]; then
  echo "Building the Burundi scheduled-planting file..."
  Rscript tools/build_sp_nc.R BI
fi
