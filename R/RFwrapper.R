
# now using precomputed data 
Rfmodel_values <- function(FCY, lat, lon) { 
#	cat(FCY, ", ", lat, ", ", lon, "\n")
	CONc <- as.integer(cut(FCY, breaks = c(-Inf, 7.5, 15, 22.5, 30, Inf), right=FALSE))
	p <- get_data("predicted_soil_properties")
	p[p$CONclass == CONc & p$lon == lon & p$lat == lat, ]
}


# this new version of leaner and meaner. It uses all the data instead of sample (why would you?), but only 250 trees (100 is usually enough, 1000 is excessive). However, this leads to small differences with the original and the quefts optimization is very sensitive to that. So currently not being used. 

# Also, predictions could be precomputed for the five con-classes. 
# The use of the FCY (and the reason for making classes) needs to be evaluated. In the absence of fertilizer use data, using yield data to compute soil fertility seems circular modeling.


new_Rfmodel_Wrapper <- function(FCY, country, lat, lon) {
  
  # compute CON class
  class_from_con <- function(x) {
	cut(x, breaks = c(-Inf, 7.5, 15, 22.5, 30, Inf), right = FALSE, labels = paste0("class", 1:5))
  }
    
  # NOT data
  sINS <- read.csv("./data/input/NOT_GIS_CON_2020.csv")
  sINS$CON <- NULL 
  
  # Soil data for farmer's field
  sISRIC <- readRDS("./data/input/ISRIC_SoilData_2020.RDS")
  sISRIC <- sISRIC[sISRIC$lat == lat & sISRIC$long == lon, ]
  sISRIC$soilN <- sISRIC$soilP <- sISRIC$soilK <- 0
  sISRIC$CONclass <- class_from_con(FCY)

  #combine to get the same data type and to make factors with shared levels
  sISRIC <- sISRIC[, names(sINS)] 
  x <- rbind(sISRIC[1,], sINS)
  x$ncluster <- as.factor(x$ncluster)
  x$country <- as.factor(x$country)
  x$CONclass <- as.factor(x$CONclass)
  y <- x[1,] # soil to predict to 
  x <- x[-1, ] # model train data

  Ntrain <- subset(x, select = -c(soilP, soilK))
  Ptrain <- subset(x, select = -c(soilN, soilK))
  Ktrain <- subset(x, select = -c(soilN, soilP))
  
  ## Random Forest soil N, P, K
  set.seed(444)
  RF_N <- randomForest::randomForest(log(soilN) ~ ., Ntrain, ntree = 250)
  RF_P <- randomForest::randomForest(log(soilP) ~ ., Ptrain, ntree = 250)
  RF_K <- randomForest::randomForest(log(soilK) ~ ., Ktrain, ntree = 250)
  
  ## use the random forest model and get the soil NPK estimates for the whole area
	out <- data.frame(
		location = paste(lat, lon, sep = "_"),
		lat = lat, 	
		lon = lon,
		soilN = exp(predict(RF_N, y) ),
		soilP = exp(predict(RF_P, y)),
		soilK = exp(predict(RF_K, y)),
		Zone = country,
		rec_N = 0.5,
		rec_P = 0.15,
		rec_K = 0.5,
		rel_N = 1
	)
	out$rel_P = out$soilP / out$soilN
	out$rel_K = out$soilK / out$soilN
	out
}




#' RF model reads the NOT data and add the FCY to train a random forest model and then use the model to predict INS for the user coordinates

# this new version was taken from the file on the test server that was not in github. 
# I assume that Siya made these changes. The values are somewhat different than with the original 
# function, but not by much. And this does introduce a number of good improvements.
old_Rfmodel_Wrapper <- function(FCY, country, lat, lon) {
  #library(randomForest)
  
  # Helper function to compute CON class
  class_from_con <- function(x) {
    cut(x, breaks = c(-Inf, 7.5, 15, 22.5, 30, Inf),  right = FALSE,
        labels = c("class1", "class2", "class3", "class4", "class5"))
  }
  
  # Columns to convert
#  numeric_cols <- c(
#    "Clay_5", "Clay_15", "Clay_30", "silt_5", "silt_15", "silt_30", "BD_5",
#    "BD_15", "BD_30", "CEC_5", "CEC_15", "CEC_30", "TotalN", "Mn", "B", "Ca", 
#    "Fe", "Cu", "Al", "Mg", "Na", "CON"
#  )
  
  # Prepare GIS data
  GIS_soilINS_modData2 <- read.csv("./data/input/NOT_GIS_CON_2020.csv")
  GIS_soilINS_modData2$ncluster = as.factor(GIS_soilINS_modData2$ncluster)
  GIS_soilINS_modData2$CONclass = class_from_con(GIS_soilINS_modData2$CON)
  GIS_soilINS_modData2$country = as.factor(GIS_soilINS_modData2$country)

  # Prepare point data
  ISRIC_SoilData <- readRDS("./data/input/ISRIC_SoilData_2020.RDS")
  ISRIC_SoilData <- ISRIC_SoilData[ISRIC_SoilData$lat == lat & ISRIC_SoilData$long == lon, ]
  ISRIC_SoilData <- unique(ISRIC_SoilData) #? needed

  ISRIC_SoilData$ncluster = as.factor(ISRIC_SoilData$ncluster)
  ISRIC_SoilData$CON = FCY  # Use a default value
  ISRIC_SoilData$CONclass = class_from_con(ISRIC_SoilData$CON)
    
  ISRIC_SoilData$soilN <- 0
  ISRIC_SoilData$soilP <- 0
  ISRIC_SoilData$soilK <- 0
  
#  trainData <- droplevels(ISRIC_SoilData[, colnames(GIS_soilINS_modData2)])
  trainData <- ISRIC_SoilData[, colnames(GIS_soilINS_modData2)]
  
  trainData$use <- "Valid"
  GIS_soilINS_modData2$use <- "train"
  factoring <- rbind(GIS_soilINS_modData2, trainData)
  
  GIS_soilINS_modData2 <- factoring[factoring$use == "train",]
  GIS_soilINS_modData2 <- subset(GIS_soilINS_modData2, select = -c(use))
  
  ISRIC_SoilData <- factoring[factoring$use == "Valid",]
  ISRIC_SoilData <- subset(ISRIC_SoilData, select = -c(use))
  
  ### Data partioning #RH: what for?
  set.seed(444)
  ind <- sample(2, nrow(GIS_soilINS_modData2), replace = TRUE, prob = c(0.7, 0.3)) ## where conrtol yield is used as a covariate
  trainData <- GIS_soilINS_modData2[ind == 1,]
  testData <- GIS_soilINS_modData2[ind == 2,]
  
  Ndata_Train <- subset(trainData, select = -c(soilP, soilK))
  Pdata_Train <- subset(trainData, select = -c(soilN, soilK))
  Kdata_Train <- subset(trainData, select = -c(soilN, soilP))
  
  Ndata_Valid <- subset(testData, select = -c(soilP, soilK))
  Pdata_Valid <- subset(testData, select = -c(soilN, soilK))
  Kdata_Valid <- subset(testData, select = -c(soilN, soilP))
  
  ## Coustome control parameter
  #custom <- trainControl(method="repeatedcv", number=10, repeats=5, verboseIter=TRUE)
#  require(caret)
  #custom <- caret::trainControl(method = "oob", number = 10)
  ##########################################################################
  ## Random Forest soilN:
  ##########################################################################
  set.seed(444)
  RF_N1 <- randomForest::randomForest(log(soilN) ~ ., subset(Ndata_Train, select = -c(CON)), importance = TRUE, ntree = 1000)
  
  ##########################################################################
  ## Random Forest "soilP"
  ##########################################################################
  set.seed(773)
  RF_P1 <- randomForest::randomForest(log(soilP) ~ ., subset(Pdata_Train, select = -c(CON)), importance = TRUE, ntree = 1000)
  
  ##########################################################################
  ## Random Forest soilK" R sq. 0.60 if control is used, 0.29 otherwise
  ##########################################################################
  set.seed(773)
  RF_K1 <- randomForest::randomForest(log(soilK) ~ ., subset(Kdata_Train, select = -c(CON)), importance = TRUE, ntree = 1000)
  
  ##########################################################################
  ## use the random forest model and get the soil NPK estimates for the whole area
  ##########################################################################
#  ISRIC_SoilData <- ISRIC_SoilData[, c("soilN", "soilP", "soilK", "exchK", "olsenP", "Clay_5", "Clay_15", "Clay_30", "percentSOM_5", "percentSOM_15", "percentSOM_30",
#                                       "pH_5", "pH_15", "pH_30", "silt_5", "silt_15", "silt_30", "BD_5", "BD_15", "BD_30", "CEC_5", "CEC_15", "CEC_30", "percentSOC_5",
#                                       "percentSOC_15", "percentSOC_30", "FC_5", "FC_15", "FC_30", "wp_5", "wp_15", "wp_30", "sws_5", "sws_15", "sws_30",
#                                       "TotalN", "Mn", "B", "Ca", "Fe", "Cu", "Al", "Mg", "Na", "ncluster", "country", "CON", "CONclass")]
#  ISRIC_SoilData <- subset(ISRIC_SoilData, select = -(CON))
  
  # ISRIC_SoilData$country <- as.factor(ISRIC_SoilData$country)
  # ISRIC_SoilData$ncluster <- as.factor(ISRIC_SoilData$ncluster)
  
  ISRIC_SoilData$soilN <- exp(predict(RF_N1, ISRIC_SoilData))
  ISRIC_SoilData$soilP <- exp(predict(RF_P1, ISRIC_SoilData))
  ISRIC_SoilData$soilK <- exp(predict(RF_K1, ISRIC_SoilData))
  
  ISRIC_SoilData$rec_N <- 0.5
  ISRIC_SoilData$rec_P <- 0.15
  ISRIC_SoilData$rec_K <- 0.5
  ISRIC_SoilData$rel_N <- 1
  ISRIC_SoilData$rel_P <- ISRIC_SoilData$soilP / ISRIC_SoilData$soilN
  ISRIC_SoilData$rel_K <- ISRIC_SoilData$soilK / ISRIC_SoilData$soilN
  ISRIC_SoilData$lat <- lat
  ISRIC_SoilData$long <- lon
  ISRIC_SoilData$location <- paste(ISRIC_SoilData$lat, ISRIC_SoilData$long, sep = "_")
  ISRIC_SoilData$Zone <- country
  ISRIC_SoilData[, c("location", "lat", "long", "soilN", "soilP", "soilK", "Zone", "rec_N", "rec_P", "rec_K", "rel_N", "rel_P", "rel_K")]
}

