# ---------------------------------------------------------------------------
# Coverage footprints — the geographic area covered by recommendation data,
# per country. Derived from the same yield grid that process-FR.R and
# is_point_covered() (get_data.R) already use, so "covered" means exactly
# "a recommendation can actually be computed here".
#
# Footprints are built once per server process and cached in .data_cache
# (see get_data.R), since the underlying yield grids never change at runtime.
# ---------------------------------------------------------------------------

.coverage_yield_files <- list(
    NG = "yield/Nigeria_WLY_LINTUL_2020.RDS",
    TZ = "yield/Tanzania_WLY_LINTUL_2020.RDS",
    RW = "yield/Rwanda_WLY_LINTUL.RDS",
    GH = "yield/Ghana_WLY_LINTUL.RDS",
    BI = "yield/Burundi_WLY_LINTUL.RDS"
)

.coverage_cell_deg <- 0.05  # matches cellFromLonLat() resolution in get_data.R

build_country_footprint <- function(country) {
    f <- data_path(.coverage_yield_files[[country]])
    if (!file.exists(f)) stop(paste("No yield data available for country:", country))

    w <- long2lon(readRDS(f))
    pts <- unique(w[, c("lon", "lat")])
    pts <- pts[is.finite(pts$lon) & is.finite(pts$lat), ]

    # Grid points are cell centres (x.x25 / x.x75), and round5min() snaps any
    # request coordinate to the centre of the 0.05° cell containing it. So the
    # exact covered area is the union of those cells as lon/lat squares.
    # Corners are rounded so neighbouring cells share identical edges and
    # merge cleanly; the union is planar, which is correct for a lon/lat grid.
    half <- .coverage_cell_deg / 2
    cells <- lapply(seq_len(nrow(pts)), function(i) {
        x0 <- round(pts$lon[i] - half, 3); x1 <- round(pts$lon[i] + half, 3)
        y0 <- round(pts$lat[i] - half, 3); y1 <- round(pts$lat[i] + half, 3)
        sf::st_polygon(list(matrix(c(x0, y0, x1, y0, x1, y1, x0, y1, x0, y0), ncol = 2, byrow = TRUE)))
    })

    old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
    on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
    footprint <- suppressMessages(sf::st_union(sf::st_sfc(cells, crs = 4326)))
    # Drop the collinear vertices left along shared cell edges
    footprint <- suppressMessages(suppressWarnings(
        sf::st_simplify(footprint, dTolerance = 1e-6, preserveTopology = TRUE)
    ))

    sf::st_sf(country = country, n_cells = nrow(pts), geometry = footprint)
}

sf_to_geojson <- function(x) {
    tmp <- tempfile(fileext = ".geojson")
    on.exit(unlink(tmp), add = TRUE)
    sf::st_write(x, tmp, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
    paste(readLines(tmp, warn = FALSE), collapse = "\n")
}

# Returns a GeoJSON string: one Feature per country (country = NULL), or a
# single-country Feature(Collection) when `country` is given.
get_coverage_geojson <- function(country = NULL) {
    valid_countries <- names(.coverage_yield_files)
    if (!is.null(country)) {
        country <- toupper(country)
        if (!country %in% valid_countries)
            stop(paste("Invalid country:", country, "— must be one of:", paste(valid_countries, collapse = ", ")))
        return(cached_read(paste0("coverage_geojson_", country), function() {
            sf_to_geojson(build_country_footprint(country))
        }))
    }
    cached_read("coverage_geojson_all", function() {
        all_sf <- do.call(rbind, lapply(valid_countries, build_country_footprint))
        sf_to_geojson(all_sf)
    })
}
