#!/usr/bin/env Rscript
# Extract supported lat/lon coordinates for each country from the yield and
# soil data files and write data/input/supported_coords.csv.
#
# Usage:
#   Rscript tools/list_supported_coords.R
#
# The output lists every grid cell (0.05° resolution) that has BOTH yield data
# AND soil data — the same lookup that process-FR.R performs at runtime.
# Coordinates outside this list will trigger "out of recommendation domain".

script_path <- normalizePath(commandArgs(trailingOnly = FALSE)[
    grep("--file=", commandArgs(trailingOnly = FALSE))
], mustWork = FALSE)
if (length(script_path) > 0) {
    setwd(dirname(dirname(script_path)))
}

# ---- helpers (mirrors get_data.R) -------------------------------------------

round5min <- function(x) {
    round(floor(x * 10) / 10 + ifelse(x - (floor(x * 10) / 10) < 0.05, 0.025, 0.075), 3)
}

long2lon <- function(x) {
    cn <- names(x)
    cn[cn == "long"] <- "lon"
    colnames(x) <- cn
    x
}

fix_colnames <- function(cn) {
    cn[cn == "plantingDate"] <- "pl_Date"
    cn[cn == "weekNr"]       <- "PlweekNr"
    gsub("WLY_", "", cn)
}

# ---- yield coords per country -----------------------------------------------

yield_files <- list(
    NG = list(path = "data/yield/Nigeria_WLY_LINTUL_2020.RDS",  fix = FALSE),
    TZ = list(path = "data/yield/Tanzania_WLY_LINTUL_2020.RDS", fix = FALSE),
    RW = list(path = "data/yield/Rwanda_WLY_LINTUL.RDS",        fix = TRUE),
    GH = list(path = "data/yield/Ghana_WLY_LINTUL.RDS",         fix = TRUE),
    BI = list(path = "data/yield/Burundi_WLY_LINTUL.RDS",       fix = TRUE)
)

yield_coords <- lapply(names(yield_files), function(cc) {
    info <- yield_files[[cc]]
    if (!file.exists(info$path)) {
        warning(sprintf("Missing yield file for %s: %s", cc, info$path))
        return(NULL)
    }
    message(sprintf("Loading yield data for %s ...", cc))
    w <- readRDS(info$path)
    if (info$fix) colnames(w) <- fix_colnames(colnames(w))
    w <- long2lon(w)
    w$lat <- round5min(w$lat)
    w$lon <- round5min(w$lon)
    coords <- unique(w[, c("lon", "lat")])
    coords$country <- cc
    coords
})
names(yield_coords) <- names(yield_files)

# ---- soil coords per country (FCY class 1 as representative) ----------------
# All FCY classes cover the same spatial extent within a country, so we check
# FCY1 to determine whether soil data exists at a given coordinate.

soil_for_country <- function(cc) {
    if (cc %in% c("NG", "TZ")) {
        # Both countries share predicted_soil_properties.rds (no country column).
        # Load once and cache; spatial extent already matches each country's yield
        # coverage so the intersection will filter to the right cells.
        f <- "data/soil/predicted_soil_properties.rds"
        if (!file.exists(f)) return(NULL)
        s <- long2lon(readRDS(f))
    } else {
        f <- sprintf("data/soil/%s_FCY1_soilNPK.RDS", cc)
        if (!file.exists(f)) return(NULL)
        s <- readRDS(f)
    }
    s$lat <- round5min(s$lat)
    s$lon <- round5min(s$lon)
    unique(s[, c("lon", "lat")])
}

# ---- intersect yield and soil -----------------------------------------------

message("\nBuilding coordinate list (yield ∩ soil) per country ...\n")

all_coords <- do.call(rbind, lapply(names(yield_files), function(cc) {
    yc <- yield_coords[[cc]]
    if (is.null(yc)) return(NULL)

    message(sprintf("  %s: %d yield points", cc, nrow(yc)))

    sc <- tryCatch(soil_for_country(cc), error = function(e) NULL)
    if (!is.null(sc) && nrow(sc) > 0) {
        message(sprintf("       %d soil points", nrow(sc)))
        combined <- merge(yc, sc, by = c("lon", "lat"))
        message(sprintf("       %d intersection points", nrow(combined)))
        combined$country <- cc
        combined
    } else {
        message(sprintf("       soil data unavailable — using yield coords only"))
        yc
    }
}))

# ---- write output -----------------------------------------------------------

out_path <- "data/input/supported_coords.csv"
write.csv(all_coords[order(all_coords$country, all_coords$lat, all_coords$lon), ],
          out_path, row.names = FALSE)

message(sprintf("\nWrote %d rows to %s\n", nrow(all_coords), out_path))

# ---- summary ----------------------------------------------------------------

cat("Supported coordinate summary:\n")
cat(sprintf("  %-4s  %6s  lon [%8s, %8s]  lat [%8s, %8s]\n",
            "CC", "points", "min", "max", "min", "max"))
for (cc in names(yield_files)) {
    sub <- all_coords[all_coords$country == cc, ]
    if (nrow(sub) == 0) next
    cat(sprintf("  %-4s  %6d  lon [%8.3f, %8.3f]  lat [%8.3f, %8.3f]\n",
                cc, nrow(sub),
                min(sub$lon), max(sub$lon),
                min(sub$lat), max(sub$lat)))
}
