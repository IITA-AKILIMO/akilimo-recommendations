
this <- system('hostname', TRUE)
if (this == "denovo") {
	akpath <- "C:/github/omilika/akilimo-recommendations"
} else {
	akpath <- "."

}
setwd(akpath)

# soil nutrient supply for five control-yield classes. 
# The use of control yield (and the reason for making classes) needs to be evaluated. In the absence of fertilizer use data, using yield data to compute soil fertility seems circular modeling.


new_predict_RFwrapper <- function() {
  
  # NOT data
  sINS <- read.csv("./data/input/NOT_GIS_CON_2020.csv")
  sINS$CON <- NULL 
  
  # Soil data for farmer's field
  sISRIC <- readRDS("./data/input/ISRIC_SoilData_2020.RDS")
# sISRIC <- sISRIC[sISRIC$lat == lat & sISRIC$long == lon, ]
  sISRIC$soilN <- sISRIC$soilP <- sISRIC$soilK <- 0
  sISRIC$CONclass <- NA

  #combine to get the same data type and to make factors with shared levels
	sISR <- sISRIC[, names(sINS)] 
	sISR$group <- "predict"
	sINS$group <- "train"
	x <- rbind(sISR, sINS)
	x$ncluster <- as.factor(x$ncluster)
	x$country <- as.factor(x$country)
	x$CONclass <- as.factor(x$CONclass)
	y <- x[x$group=="predict",] # soil to predict to 
	y$long <- sISRIC$long
	y$lat <- sISRIC$lat
	x <- x[x$group=="train", ]
	x$group <- y$group <- NULL
	Ntrain <- subset(x, select = -c(soilP, soilK))
	Ptrain <- subset(x, select = -c(soilN, soilK))
	Ktrain <- subset(x, select = -c(soilN, soilP))
  
  ## Random Forest soil N, P, K
	set.seed(444)
	RF_N <- randomForest::randomForest(log(soilN) ~ ., Ntrain, ntree = 500)
	RF_P <- randomForest::randomForest(log(soilP) ~ ., Ptrain, ntree = 500)
	RF_K <- randomForest::randomForest(log(soilK) ~ ., Ktrain, ntree = 500)

	z <- lapply(1:5, function(i) {
		y$CONclass[1:nrow(y)] <- paste0("class", i)
		data.frame(
				long = y$long,
				lat = y$lat,
				CONclass = i,
				soilN = exp(predict(RF_N, y)), 
				soilP = exp(predict(RF_P, y)),
				soilK = exp(predict(RF_K, y)))
	})
	
	do.call(rbind, z)
  
}


#p <- new_predict_RFwrapper() 
#saveRDS(p, "./data/input/predicted_soil_properties.rds")

