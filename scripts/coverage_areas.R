#!/usr/bin/env Rscript
# Determines the geographic coverage area for recommendations per country,
# based on the lon/lat grid cells actually present in the yield data.
# Uses the same build_country_footprint() as the API's GET /coverage
# (R/coverage.R), so the files written here match what the API serves.
#
# Usage:
#   Rscript scripts/coverage_areas.R
#
# Outputs (in scripts/output/, gitignored):
#   coverage_bbox.csv     - min/max lon/lat per country
#   coverage_<CC>.geojson - footprint polygon per country
#   coverage_all.geojson  - all countries combined, one feature per country

suppressPackageStartupMessages({
    library(sf)
})

akpath <- Sys.getenv("AKILIMO_ROOT", ".")
setwd(akpath)
source(file.path("R", "get_data.R"))
# REC_TYPE_COUNTRIES (used by coverage.R) is defined in AkilimoMain.R
source(file.path("R", "AkilimoMain.R"))
source(file.path("R", "coverage.R"))

out_dir <- file.path("scripts", "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

bbox_rows <- list()
features <- list()

for (cc in names(.coverage_yield_files)) {
    f <- data_path(.coverage_yield_files[[cc]])
    if (!file.exists(f)) {
        warning(sprintf("[%s] file not found: %s", cc, f))
        next
    }
    message(sprintf("[%s] reading %s ...", cc, f))
    w <- long2lon(readRDS(f))
    pts <- unique(w[, c("lon", "lat")])
    pts <- pts[is.finite(pts$lon) & is.finite(pts$lat), ]

    bbox_rows[[cc]] <- data.frame(
        country = cc,
        n_cells = nrow(pts),
        min_lon = min(pts$lon), max_lon = max(pts$lon),
        min_lat = min(pts$lat), max_lat = max(pts$lat)
    )

    feat <- build_country_footprint(cc)
    features[[cc]] <- feat

    st_write(feat, file.path(out_dir, paste0("coverage_", cc, ".geojson")),
              driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
    message(sprintf("[%s] %d cells, bbox lon[%.3f,%.3f] lat[%.3f,%.3f]",
                     cc, nrow(pts),
                     min(pts$lon), max(pts$lon), min(pts$lat), max(pts$lat)))
}

bbox_df <- do.call(rbind, bbox_rows)
write.csv(bbox_df, file.path(out_dir, "coverage_bbox.csv"), row.names = FALSE)

all_sf <- do.call(rbind, features)
st_write(all_sf, file.path(out_dir, "coverage_all.geojson"),
          driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)

message("\nDone. Outputs written to ", normalizePath(out_dir))
print(bbox_df)
