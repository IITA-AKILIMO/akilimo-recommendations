# ---------------------------------------------------------------------------
# Coverage footprints — the geographic area where a recommendation type can be
# computed, per country.
#
#   FR (default): the yield grid that process-FR.R and is_point_covered()
#                 (get_data.R) use, so "covered" means "/compute accepts it".
#   SP:           the FR grid, kept only where the planting-window NetCDF
#                 ({CC}_WLY_LINTUL_2020SP.nc) and SoilData_4Country.RDS both
#                 have data, the two inputs getSPrecommendations() needs.
#                 Empty for countries without SP data (RW, BI).
#   IC and PP give the same advice anywhere in a country, so they have no
#   footprint of their own; each feature lists them in `rec_types`.
#
# Footprints are built once per server process and cached in .data_cache
# (see get_data.R), since the underlying grids never change at runtime.
# ---------------------------------------------------------------------------

.coverage_yield_files <- list(
    NG = "yield/Nigeria_WLY_LINTUL_2020.RDS",
    TZ = "yield/Tanzania_WLY_LINTUL_2020.RDS",
    RW = "yield/Rwanda_WLY_LINTUL.RDS",
    GH = "yield/Ghana_WLY_LINTUL.RDS",
    BI = "yield/Burundi_WLY_LINTUL.RDS"
)

.coverage_cell_deg <- 0.05  # matches cellFromLonLat() resolution in get_data.R
COVERAGE_TYPES <- c("FR", "SP")

# Grid points (cell centres) where `type` can be computed in `country`
coverage_points <- function(country, type = "FR") {
    f <- data_path(.coverage_yield_files[[country]])
    if (!file.exists(f)) stop(paste("No yield data available for country:", country))

    w <- long2lon(readRDS(f))
    pts <- unique(w[, c("lon", "lat")])
    pts <- pts[is.finite(pts$lon) & is.finite(pts$lat), ]
    if (type != "SP") return(pts)

    if (!country %in% REC_TYPE_COUNTRIES$SP) return(pts[0, ])
    nc_file <- paste0("data/yield/", country, "_WLY_LINTUL_2020SP.nc")
    if (!file.exists(nc_file)) return(pts[0, ])
    nc <- ncdf4::nc_open(nc_file)
    nc_cells <- nc$dim$cell$vals
    ncdf4::nc_close(nc)
    pts <- pts[cellFromLonLat(pts$lon, pts$lat) %in% nc_cells, ]

    soil <- cached_read("soil_NPK4", function() long2lon(readRDS(data_path("soil/SoilData_4Country.RDS"))))
    soil <- soil[!is.na(soil$soilN) & !is.na(soil$soilP) & !is.na(soil$soilK), ]
    key <- function(d) paste(round(d$lon, 3), round(d$lat, 3))
    pts[key(pts) %in% key(soil), ]
}

build_country_footprint <- function(country, type = "FR") {
    pts <- coverage_points(country, type)

    # Grid points are cell centres (x.x25 / x.x75), and round5min() snaps any
    # request coordinate to the centre of the 0.05° cell containing it. So the
    # exact covered area is the union of those cells as lon/lat squares.
    # Corners are rounded so neighbouring cells share identical edges and
    # merge cleanly; the union is planar, which is correct for a lon/lat grid.
    if (nrow(pts) == 0) {
        footprint <- sf::st_sfc(sf::st_multipolygon(), crs = 4326)
    } else {
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
    }

    # rec_types: the recommendation types /compute serves in this country, comma-separated
    # (GeoJSON via GDAL has no portable array property). FR and SP need the point inside
    # their footprint; IC and PP use no location data. Source: REC_TYPE_COUNTRIES (AkilimoMain.R).
    rec_types <- names(Filter(function(cc) country %in% cc, REC_TYPE_COUNTRIES))
    sf::st_sf(country = country, type = type, n_cells = nrow(pts),
              rec_types = paste(rec_types, collapse = ","), geometry = footprint)
}

sf_to_geojson <- function(x) {
    tmp <- tempfile(fileext = ".geojson")
    on.exit(unlink(tmp), add = TRUE)
    sf::st_write(x, tmp, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
    paste(readLines(tmp, warn = FALSE), collapse = "\n")
}

# Returns a GeoJSON string: one Feature per country (country = NULL), or a
# single-country Feature(Collection) when `country` is given. `type` picks the
# recommendation type whose footprint is drawn (FR or SP).
get_coverage_geojson <- function(country = NULL, type = "FR") {
    type <- toupper(if (is.null(type) || !nzchar(type)) "FR" else type)
    if (!type %in% COVERAGE_TYPES)
        stop(paste0("Invalid type: ", type, " — must be one of: ", paste(COVERAGE_TYPES, collapse = ", "),
                    " (IC and PP do not depend on location; see each feature's rec_types)"))

    valid_countries <- names(.coverage_yield_files)
    if (!is.null(country)) {
        country <- toupper(country)
        if (!country %in% valid_countries)
            stop(paste("Invalid country:", country, "— must be one of:", paste(valid_countries, collapse = ", ")))
        return(cached_read(paste0("coverage_geojson_", country, "_", type), function() {
            sf_to_geojson(build_country_footprint(country, type))
        }))
    }
    cached_read(paste0("coverage_geojson_all_", type), function() {
        all_sf <- do.call(rbind, lapply(valid_countries, build_country_footprint, type = type))
        sf_to_geojson(all_sf)
    })
}
