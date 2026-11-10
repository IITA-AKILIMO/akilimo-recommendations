#!/usr/bin/env Rscript
# Build the scheduled-planting (SP) NetCDF files from the wide LINTUL yield RDS.
#
# The RDS files used by FR (data/yield/Rwanda_WLY_LINTUL.RDS and
# Burundi_WLY_LINTUL.RDS) hold the same LINTUL output in wide form: one row per
# cell and planting date, one column per daysOnField. The SP code reads a
# NetCDF with dims (daysOnField, plant, cell); this script reshapes and writes
# that file without a new LINTUL run.
#
# Usage:
#   Rscript tools/build_sp_nc.R          # Rwanda only (BI waits for its soil decision)
#   Rscript tools/build_sp_nc.R RW BI    # explicit countries
#
# Published data is owner-only: after building, run scripts/bundle-assets and
# upload to OSF/Zenodo, then run the "Data setup" workflow.

script_path <- normalizePath(commandArgs(trailingOnly = FALSE)[
    grep("--file=", commandArgs(trailingOnly = FALSE))
], mustWork = FALSE)
if (length(script_path) > 0) {
    setwd(dirname(dirname(script_path)))
}

round5min <- function(x) {
    round(floor(x * 10) / 10 + ifelse(x - (floor(x * 10) / 10) < 0.05, 0.025, 0.075), 3)
}

long2lon <- function(x) {
    cn <- names(x)
    cn[cn == "long"] <- "lon"
    colnames(x) <- cn
    x
}

cellFromLonLat <- function(lon, lat, res = 0.05) {
    rown <- floor((90 - lat) / res)
    coln <- floor((lon + 180) / res)
    rown * 360 / res + coln + 1
}

yield_files <- c(
    RW = "Rwanda_WLY_LINTUL.RDS",
    BI = "Burundi_WLY_LINTUL.RDS"
)

args <- toupper(commandArgs(trailingOnly = TRUE))
countries <- if (length(args) > 0) args else "RW"
unknown <- setdiff(countries, names(yield_files))
if (length(unknown) > 0) {
    stop("Unknown country code(s): ", paste(unknown, collapse = ", "))
}

build_sp_nc <- function(country) {
    rds_path <- file.path("data", "yield", yield_files[[country]])
    if (!file.exists(rds_path)) {
        stop("Missing yield file: ", rds_path)
    }

    w <- long2lon(readRDS(rds_path))
    cols <- grep("^WLY_", names(w), value = TRUE)
    if (length(cols) == 0) {
        stop("No WLY_ columns in ", rds_path)
    }

    cell <- cellFromLonLat(round5min(w$lon), round5min(w$lat))
    plant <- as.integer(w$plantingDate)
    days <- as.integer(sub("WLY_", "", cols))

    d <- data.frame(
        cell        = rep(cell, times = length(cols)),
        plantDay    = rep(plant, times = length(cols)),
        daysOnField = rep(days, each = nrow(w)),
        WLY         = unlist(w[cols], use.names = FALSE),
        stringsAsFactors = FALSE
    )

    bad <- is.na(d$WLY) | is.na(d$cell) | is.na(d$plantDay) | is.na(d$daysOnField)
    if (any(bad)) {
        stop(sprintf("%s has %d missing yield/cell/date values", country, sum(bad)))
    }
    if (anyDuplicated(d[, c("cell", "plantDay", "daysOnField")])) {
        stop(country, " has duplicate cell x planting date x daysOnField rows")
    }

    d <- d[order(d$cell, d$plantDay, d$daysOnField), ]

    out <- file.path("data", "yield", sprintf("%s_WLY_LINTUL_2020SP.nc", country))
    dir_cell <- ncdf4::ncdim_def("cell", "", as.integer(sort(unique(d$cell))))
    dir_plant <- ncdf4::ncdim_def("plant", "", as.integer(sort(unique(d$plantDay))))
    dir_days <- ncdf4::ncdim_def("daysOnField", "", as.integer(sort(unique(d$daysOnField))))
    var_wly <- ncdf4::ncvar_def(
        "WLY", "kg/ha", list(dir_days, dir_plant, dir_cell), -1,
        longname = "water limited yield", prec = "double"
    )

    nc <- ncdf4::nc_create(out, var_wly)
    on.exit(ncdf4::nc_close(nc), add = TRUE)
    ncdf4::ncvar_put(nc, var_wly, d$WLY)

    message(sprintf(
        "Wrote %s: %d cells, %d planting dates, %d harvest ages, %d values",
        out, dir_cell$len, dir_plant$len, dir_days$len, nrow(d)
    ))

    invisible(list(path = out, data = d, colnames = cols))
}

# Re-read one cell and compare every value with the source RDS.
verify_sp_nc <- function(country, built) {
    nc <- ncdf4::nc_open(built$path)
    on.exit(ncdf4::nc_close(nc), add = TRUE)

    probe_cells <- unique(built$data$cell)[
        seq(1, length(unique(built$data$cell)), length.out = min(3, length(unique(built$data$cell))))
    ]
    for (probe in probe_cells) {
        off <- which(nc$dim$cell$vals == probe)
        x <- ncdf4::ncvar_get(nc, varid = NA, start = c(1, 1, off), count = c(-1, -1, 1))
        got <- as.vector(t(x))
        dd <- built$data[built$data$cell == probe, ]
        expected <- dd$WLY[order(dd$daysOnField, dd$plantDay)]
        if (!isTRUE(all.equal(got, expected, tolerance = 0))) {
            stop(sprintf("Verification failed for %s cell %d", country, probe))
        }
    }

    message(sprintf("Verified %s against %d sampled cells", built$path, length(probe_cells)))
}

for (country in countries) {
    built <- build_sp_nc(country)
    verify_sp_nc(country, built)
}
