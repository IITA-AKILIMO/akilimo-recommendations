
add_path <- function(f) file.path("./data/", f)	

get_data <- function(x, country, FCY, lon, lat) {
	
	if (x == "TRNS") {
		TRNS <- read.csv(add_path("input/translations_TEST.csv"), stringsAsFactors = FALSE)
		unquote <- function(x) gsub(pattern = "\"", replacement = "", x)
		data.frame(lapply(TRNS, unquote))
	} else if (x == "default_prices") {
		read.csv(add_path("input/Default_prices.csv"))
	} else if (x == "starch_prices") {
		read.csv(add_path("input/starchPrices.csv"))
	} else if (x == "dry_matter") {
		read.csv(add_path("input/fd2.csv"))
	} else if (x == "soil_NPK-4") {
		readRDS(add_path("soil/SoilData_4Country.RDS"))
	} else if (x == "soil_NPK") {
		fcyy <- ifelse(FCY < 7.5, "FCY1",
              ifelse(FCY >= 7.5 & FCY < 15, "FCY2",
              ifelse(FCY >= 15 & FCY < 22.5, "FCY3",
              ifelse(FCY >= 22.5 & FCY < 30, "FCY4", "FCY5"))))
		f <- add_path(paste0("soil/", country, "_", fcyy, "_soilNPK.RDS"))
		soil <- readRDS(f)
		soil <- soil[round(soil$lon, 3)==lon & round(soil$lat,3)==lat, ]
		if (nrow(soil) == 0) return(soil)
		#soil$location <- paste(soil$lat, soil$lon, sep = "_")
		#soil$Zone <- country
		soil <- soil[, c("location", "lat", "lon", "soilN", "soilP", "soilK")]
		soil$rec_N <- 0.5
		soil$rec_P <- 0.15
		soil$rec_K <- 0.5
		soil$rel_N <- 1
		soil$rel_P <- soil$soilP / soil$soilN
		soil$rel_K <- soil$soilK / soil$soilN
		soil
	} else if (x == "predicted_soil_properties") {
		soil <- readRDS(add_path("soil/predicted_soil_properties.rds"))
		soil$rec_N <- 0.5
		soil$rec_P <- 0.15
		soil$rec_K <- 0.5
		soil$rel_N <- 1
		soil$rel_P <- soil$soilP / soil$soilN
		soil$rel_K <- soil$soilK / soil$soilN
		soil			
	} else if (x == "WLY_365") {
		if (country == "NG") {
			w <- readRDS(add_path("yield/Nigeria_WLY_LINTUL_2020.RDS"))
		} else if (country == "TZ") {
			w <- readRDS(add_path("yield/Tanzania_WLY_LINTUL_2020.RDS"))
		} else if (country == "RW") {
			w <- readRDS(add_path("yield/Rwanda_WLY_LINTUL.RDS"))
			w$pl_Date <- w$plantingDate
			w$PlweekNr <- w$weekNr
			colnames(w) <- gsub("WLY_", "", colnames(w))
		} else if (country == "GH") {
			w <- readRDS(add_path("yield/Ghana_WLY_LINTUL.RDS"))
			w$pl_Date <- w$plantingDate
			w$PlweekNr <- w$weekNr
			colnames(w) <- gsub("WLY_", "", colnames(w))
		} else if (country == "BU") {
			w <- readRDS(add_path("yield/Burundi_WLY_LINTUL.RDS"))
			w$pl_Date <- w$plantingDate
			w$PlweekNr <- w$weekNr
			colnames(w) <- gsub("WLY_", "", colnames(w))
			#w$location <- paste(w$lat, w$long, sep = "_")
		} else {
			stop(paste("WLY_365", "not available for", country))
		}
		# should be fixed in files.
		w[round(w$long,3)==lon & round(w$lat,3)==lat, ]
		
	} else if (x == "WLY_15M") {
		if (country == "NG") {
			readRDS(add_path("yield/Nigeria_WLY_LINTUL_2020_Server.RDS"))
		} else if (country == "TZ") {
			readRDS(add_path("yield/Tanzania_WLY_LINTUL_2020_Server.RDS"))
		} else if (country == "GH") {
			readRDS(add_path("yield/Ghana_WLY_LINTUL_SP.RDS"))
		} else if (country == "BU") {
			readRDS(add_path("yield/Burundi_WLY_LINTUL_SP.RDS"))
		} else {
			stop(paste("WLY_15M", "not available for", country))
		}
	} else {
		stop("no such data")
	}
}
