
data_path <- function(f) file.path("./data/", f)

# ---------------------------------------------------------------------------
# In-memory cache for static data files (loaded once per server process)
# ---------------------------------------------------------------------------
.data_cache <- new.env(parent = emptyenv())

cached_read <- function(key, loader) {
    if (!exists(key, envir = .data_cache, inherits = FALSE)) {
        assign(key, loader(), envir = .data_cache)
    }
    get(key, envir = .data_cache, inherits = FALSE)
}

long2lon <- function(x) {
	cn <- names(x)
	cn[cn=="long"] <- "lon"
	colnames(x) <- cn
	x
}

round5min <- function(x) {
	round(floor(x * 10) / 10 + ifelse(x - (floor(x * 10) / 10) < 0.05, 0.025, 0.075), 3)
}

cellFromLonLat <- function(lon, lat, res=0.05) {
	rown <- floor((90 - lat) / res)
	coln <- floor((lon + 180) / res)
	rown * 360/res + coln + 1
}


get_WLY_15M_ncdf <- function(country, lon, lat) {

	cell <- cellFromLonLat(lon, lat)
	if (country == "BI") return(NULL)

	f <- paste0("data/yield/", country, "_WLY_LINTUL_2020SP.nc")
	if (!file.exists(f)) {
		warning(sprintf("NetCDF file not found: %s", f))
		return(NULL)
	}
	nc <- ncdf4::nc_open(f)
	off <- which(nc$dim$cell$vals == cell)
	if (length(off) != 1) {
		ncdf4::nc_close(nc)
		warning(sprintf("Cell %d not found in %s (lon=%.3f, lat=%.3f)", cell, f, lon, lat))
		return(NULL)
	}
	
	x <- ncdf4::ncvar_get(nc, varid=NA, start=c(1,1,off), count=c(-1,-1,1))
	ncdf4::nc_close(nc)
	
	x <- data.frame(WLY=as.vector(t(x)), pl_Date=as.numeric(nc$dim$plant$vals), 
					daysOnField=as.numeric(rep(nc$dim$daysOnField$vals, each=ncol(x))))

	x$PlweekNr = floor(x$pl_Date / 7 ) + 1
	x
}



get_data <- function(x, country, FCY, lon, lat) {
	
	if (x == "TRNS") {
		cached_read("TRNS", function() {
			TRNS <- read.csv(data_path("input/translations_TEST.csv"), stringsAsFactors = FALSE)
			unquote <- function(x) gsub(pattern = "\"", replacement = "", x)
			data.frame(lapply(TRNS, unquote))
		})
	} else if (x == "default_prices") {
		cached_read("default_prices", function() {
			out <- read.csv(data_path("input/Default_prices.csv"))
			out$Country[out$Country == "BU"] <- "BI"
			out
		})
	} else if (x == "starch_prices") {
		cached_read("starch_prices", function() read.csv(data_path("input/starchPrices.csv")))
	} else if (x == "dry_matter") {
		cached_read("dry_matter", function() read.csv(data_path("input/fd2.csv")))
	} else if (x == "soil_NPK-4") {
		out <- readRDS(data_path("soil/SoilData_4Country.RDS"))
		out <- long2lon(out)
		lat <- round5min(lat)
		lon <- round5min(lon)
		out[out$lon == lon & out$lat == lat, ]
	} else if (x == "soil_NPK") {

		Yclass <- as.integer(cut(FCY, breaks = c(-Inf, 7.5, 15, 22.5, 30, Inf), right=FALSE))

		if (country %in% c("NG", "TZ")) {
			p <- get_data("predicted_soil_properties")
			lat <- round5min(lat)
			lon <- round5min(lon)
			p[p$CONclass == Yclass & p$lon == lon & p$lat == lat, ]		
		} else {
			f <- data_path(paste0("soil/", country, "_FCY", Yclass, "_soilNPK.RDS"))
			soil <- readRDS(f)
			lat <- round5min(lat)
			lon <- round5min(lon)
			soil <- soil[round(soil$lon, 3)==lon & round(soil$lat,3)==lat, ]
			if (nrow(soil) == 0) {
				warning(sprintf("No soil data for country=%s FCY-class=%d lon=%.3f lat=%.3f", country, Yclass, lon, lat))
				return(soil)
			}
			soil <- soil[, c("lat", "lon", "soilN", "soilP", "soilK")]
			soil$rec_N <- 0.5
			soil$rec_P <- 0.15
			soil$rec_K <- 0.5
			soil$rel_N <- 1
			soil$rel_P <- soil$soilP / soil$soilN
			soil$rel_K <- soil$soilK / soil$soilN
			soil
		}
	} else if (x == "predicted_soil_properties") {
		soil <- readRDS(data_path("soil/predicted_soil_properties.rds"))
		soil$rec_N <- 0.5
		soil$rec_P <- 0.15
		soil$rec_K <- 0.5
		soil$rel_N <- 1
		soil$rel_P <- soil$soilP / soil$soilN
		soil$rel_K <- soil$soilK / soil$soilN
		long2lon(soil)
	} else if (x == "WLY_365") {
		fix_cn <- function(cn) {
			cn[cn == "plantingDate"] <- "pl_Date"
			cn[cn == "weekNr"] <- "PlweekNr"
			gsub("WLY_", "", cn)
		}
		if (country == "NG") {
			w <- readRDS(data_path("yield/Nigeria_WLY_LINTUL_2020.RDS"))
		} else if (country == "TZ") {
			w <- readRDS(data_path("yield/Tanzania_WLY_LINTUL_2020.RDS"))
		} else if (country == "RW") {
			w <- readRDS(data_path("yield/Rwanda_WLY_LINTUL.RDS"))
			colnames(w) <- fix_cn(colnames(w))
		} else if (country == "GH") {
			w <- readRDS(data_path("yield/Ghana_WLY_LINTUL.RDS"))
			colnames(w) <- fix_cn(colnames(w))
		} else if (country == "BI") {
			w <- readRDS(data_path("yield/Burundi_WLY_LINTUL.RDS"))
			colnames(w) <- fix_cn(colnames(w))
		} else {
			stop(paste("WLY_365", "not available for", country))
		}
		# should be fixed in files.
		w <- long2lon(w)
		lat <- round5min(lat)
		lon <- round5min(lon)
		w[round(w$lon,3)==lon & round(w$lat,3)==lat, ]
	} else if (x == "WLY_15M_ncdf") {
		get_WLY_15M_ncdf(country, lon, lat)
	} else {
		stop(paste("no such data:", x))
	}
}
