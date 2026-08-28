
## Planting date and cultivar recommendation, based on DSSAT crop model simulations.
## For every grid point in the DSSAT output, a small factorial of planting dates x cultivars
## was simulated; here we look up the grid point nearest to the farmer's location and rank
## the simulated combinations by harvested yield (HWAH) to find the best planting date and cultivar.

nearest_dssat_point <- function(dssat, lat, lon) {
	pts <- unique(dssat[, c("XLAT", "LONG")])
	d2 <- (pts$XLAT - lat)^2 + (pts$LONG - lon)^2
	i <- which.min(d2)
	list(XLAT = pts$XLAT[i], LONG = pts$LONG[i], dist = sqrt(d2[i]))
}


#' @param lat
#' @param lon
#' @param country
#' @param crop
#'
#' @return a data.frame of simulated planting date x cultivar combinations at the
#'   nearest DSSAT grid point, sorted from best to worst expected yield (HWAH); or
#'   NULL if no DSSAT data is available near the requested location.
#' @export
getPDCrecommendations <- function(lat, lon, country, crop) {

	dssat <- try(get_data("dssat", country = country, crop = crop, pipeline = "planting-date-and-cultivar"), silent = TRUE)
	if (inherits(dssat, "try-error") || NROW(dssat) == 0) return(NULL)

	np <- nearest_dssat_point(dssat, lat, lon)
	if (np$dist > 0.75) return(NULL)

	ds <- dssat[dssat$XLAT == np$XLAT & dssat$LONG == np$LONG, ]
	ds <- ds[ds$HWAH > 0, ]
	if (nrow(ds) == 0) return(NULL)

	ds <- ds[order(-ds$HWAH, ds$PDAT), c("PDAT", "Variety", "Cultivar", "HWAH", "CWAM", "MDAT")]
	rownames(ds) <- NULL

	write.csv(ds, "./temp/PDC_rec.csv", row.names = FALSE)
	ds
}


process_PDC <- function(PDC, country, crop, lat, lon, PD, user, userField, area, areaUnits) {

	res <- NULL
	if (!identical(crop, "maize")) {
		recText <- paste0("AKILIMO does not yet have a planting date and cultivar recommendation for ", crop, ".")
	} else {
		res <- getPDCrecommendations(lat = lat, lon = lon, country = country, crop = crop)

		if (!is.data.frame(res)) {
			recText <- "We do not have a planting date and cultivar recommendation for your location because your location is out of the recommendation domain AKILIMO is currently serving."
		} else {
			best <- res[1, ]
			recText <- paste0(
				"For your location, the best expected outcome is planting the ", best$Cultivar,
				" cultivar on ", format(best$PDAT, "%d %B %Y"),
				", with an expected yield of ", round(best$HWAH / 1000, 1), " t/ha."
			)
		}
	}

	list(rec_type = "PDC", recommendation = recText, data = res)
}
