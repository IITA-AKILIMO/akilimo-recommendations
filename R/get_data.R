
data_path <- function(f) file.path("./data/", f)	

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
	
	f <- paste0("data/yield/", country, "_WLY_LINTUL_2020SP.nc")
	nc <- ncdf4::nc_open(f)
	off <- which(nc$dim$cell$vals == cell)
	if (length(off) != 1) return(NULL)
	
	x <- ncdf4::ncvar_get(nc, varid=NA, start=c(1,1,off), count=c(-1,-1,1))
	ncdf4::nc_close(nc)
	
	x <- data.frame(WLY=as.vector(t(x)), pl_Date=as.numeric(nc$dim$plant$vals), 
					daysOnField=as.numeric(rep(nc$dim$daysOnField$vals, each=ncol(x))))

	x$PlweekNr = floor(x$pl_Date / 7 ) + 1
	x
}



get_data <- function(x, country, FCY, lon, lat) {
	
	if (x == "TRNS") {
		TRNS <- read.csv(data_path("input/translations_TEST.csv"), stringsAsFactors = FALSE)
		unquote <- function(x) gsub(pattern = "\"", replacement = "", x)
		data.frame(lapply(TRNS, unquote))
	} else if (x == "default_prices") {
		out <- read.csv(data_path("input/Default_prices.csv"))
		out$Country[out$Country == "BU"] <- "BI"
		out
		# guess price for missing item
		#rbind(out, data.frame(Country="NG", Item="NPK201226", Price=15000))
	} else if (x == "starch_prices") {
		read.csv(data_path("input/starchPrices.csv"))
	} else if (x == "dry_matter") {
		read.csv(data_path("input/fd2.csv"))
	} else if (x == "RF_soil") {
		CONc <- as.integer(cut(FCY, breaks = c(-Inf, 7.5, 15, 22.5, 30, Inf), right=FALSE))
		p <- get_data("predicted_soil_properties")
		lat <- round5min(lat)
		lon <- round5min(lon)
		p[p$CONclass == CONc & p$lon == lon & p$lat == lat, ]		
	} else if (x == "soil_NPK-4") {
		out <- readRDS(data_path("soil/SoilData_4Country.RDS"))
		out <- long2lon(out)
		lat <- round5min(lat)
		lon <- round5min(lon)
		out[out$lon == lon & out$lat == lat, ]
	} else if (x == "soil_NPK") {
		fcyy <- cut(FCY, breaks=c(-Inf, 7.5, 15, 22.5, 30, Inf), right=FALSE, labels=paste0("FCY", 1:5))
		f <- data_path(paste0("soil/", country, "_", fcyy, "_soilNPK.RDS"))
		soil <- readRDS(f)
		lat <- round5min(lat)
		lon <- round5min(lon)
		soil <- soil[round(soil$lon, 3)==lon & round(soil$lat,3)==lat, ]
		if (nrow(soil) == 0) return(soil)
		#soil$location <- paste(soil$lat, soil$lon, sep = "_")
		#soil$Zone <- country
		soil <- soil[, c("lat", "lon", "soilN", "soilP", "soilK")]
		soil$rec_N <- 0.5
		soil$rec_P <- 0.15
		soil$rec_K <- 0.5
		soil$rel_N <- 1
		soil$rel_P <- soil$soilP / soil$soilN
		soil$rel_K <- soil$soilK / soil$soilN
		soil
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
		if (country == "NG") {
			w <- readRDS(data_path("yield/Nigeria_WLY_LINTUL_2020.RDS"))
		} else if (country == "TZ") {
			w <- readRDS(data_path("yield/Tanzania_WLY_LINTUL_2020.RDS"))
		} else if (country == "RW") {
			w <- readRDS(data_path("yield/Rwanda_WLY_LINTUL.RDS"))
			w$pl_Date <- w$plantingDate
			w$PlweekNr <- w$weekNr
			colnames(w) <- gsub("WLY_", "", colnames(w))
		} else if (country == "GH") {
			w <- readRDS(data_path("yield/Ghana_WLY_LINTUL.RDS"))
			w$pl_Date <- w$plantingDate
			w$PlweekNr <- w$weekNr
			colnames(w) <- gsub("WLY_", "", colnames(w))
		} else if (country == "BI") {
			w <- readRDS(data_path("yield/Burundi_WLY_LINTUL.RDS"))
			w$pl_Date <- w$plantingDate
			w$PlweekNr <- w$weekNr
			colnames(w) <- gsub("WLY_", "", colnames(w))
			#w$location <- paste(w$lat, w$long, sep = "_")
		} else {
			stop(paste("WLY_365", "not available for", country))
		}
		# should be fixed in files.
		w <- long2lon(w)
		lat <- round5min(lat)
		lon <- round5min(lon)
		w[round(w$lon,3)==lon & round(w$lat,3)==lat, ]

	} else if (x == "WLY_15M") {
		if (country == "NG") {
			w <- readRDS(data_path("yield/Nigeria_WLY_LINTUL_2020_Server.RDS"))
		} else if (country == "TZ") {
			w <- readRDS(data_path("yield/Tanzania_WLY_LINTUL_2020_Server.RDS"))
		} else if (country == "GH") {
			w <- readRDS(data_path("yield/Ghana_WLY_LINTUL_SP.RDS"))
		#} else if (country == "BI") {
		#	w <- readRDS(data_path("yield/Burundi_WLY_LINTUL_SP.RDS"))
		} else {
			stop(paste("WLY_15M", "not available for", country))
		}
		long2lon(w)
	} else if (x == "WLY_15M_ncdf") {
		get_WLY_15M_ncdf(country, lon, lat)
	} else {
		stop("no such data")
	}
}
