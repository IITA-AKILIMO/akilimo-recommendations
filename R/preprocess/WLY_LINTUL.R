
library(terra)
r <- rast(res=.05)
cnts <- rev(c("NG", "TZ", "GH"))

for (country in cnts) {
	print(country); flush.console()
	if (country == "NG") {
		w <- readRDS(data_path("yield/Nigeria_WLY_LINTUL_2020_Server.RDS"))
	} else if (country == "TZ") {
		w <- readRDS(data_path("yield/Tanzania_WLY_LINTUL_2020_Server.RDS"))
	} else if (country == "GH") {
		w <- readRDS(data_path("yield/Ghana_WLY_LINTUL_SP.RDS"))
	} else {
		stop()
	}
	w$cell <- cellFromXY(r, w[, c("long", "lat")])
	d <- w[, c("cell", "pl_Date", "daysOnField", "water_limited_yield")]
	if (country == "TZ") {
		i <- !duplicated(d[, c("cell", "pl_Date", "daysOnField")])
		d <- d[i,]
	}
	colnames(d) <- c("cell", "plantDay", "daysOnField", "WLY")
	d <- d[order(d$cell, d$plantDay, d$daysOnField), ]
	dimCell <- ncdim_def("cell", "", as.integer(sort(unique(d$cell)) ))
	dimPlant <- ncdim_def("plant", "", as.integer(sort(unique(d$plantDay)) ))
	dimHarv <- ncdim_def("daysOnField", "", as.integer(sort(unique(d$daysOnField)) ))
	varWLY <- ncvar_def("WLY", "kg/ha", list(dimHarv, dimPlant, dimCell), -1, longname="water limited yield", prec="double")

	f <- paste0("data/yield/", country, "_WLY_LINTUL_2020SP.nc")
	ncnew <- nc_create(f, varWLY)
	ncvar_put( ncnew, varWLY, d$WLY, start=c(1,1,1), count=c(dimHarv$len, dimPlant$len, dimCell$len) )
	nc_close(ncnew)
}
	

lon = 7.825
lat = 4.525

fromnc <- function(country, lon, lat) {

	cellFromLonLat <- function(lon, lat, res=0.05) {
		row = floor((90 - lat) / res)
		col = floor((lon + 180) / res)
		row * 360/res + col + 1
	}
	cell <- cellFromLonLat(lon, lat)
	
	f <- paste0("data/yield/", country, "_WLY_LINTUL_2020SP.nc")
	nc <- ncdf4::nc_open(f)
	off <- which(nc$dim$cell$vals == cell)
	if (length(off) != 1) return(NULL)
	
	x <- ncdf4::ncvar_get(nc, varid=NA, start=c(1,1,off), count=c(-1,-1,1))
	ncdf4::nc_close(nc)
	colnames(x) <- nc$dim$plant$vals
	rownames(x) <- nc$dim$daysOnField$vals
	x
}

d = fromnc("NG", lon, lat)
