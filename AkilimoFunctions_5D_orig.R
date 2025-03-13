



#SHORT DEF:   Function to obtain tillage recommendations (step 4 of 6 steps).
#RETURNS:     dataframe with cost benefit for various combinations of ploughing and ridging.
#DESCRIPTION: Function to obtain recommendations on ploughing and ridging. Returns a dataframe with all possible combinations of
#             ploughing (none, manual, tractor) and ridging (none, manual, tractor), ordered by decreasing net returns and increasing
#             tillage intensity (riding then ploughing)
#INPUT:       See Cassava Crop Manager function for details
getPPrecommendations <- function(areaHa,
                                 costLMO,
                                 ploughing, #select one
                                 ridging, #select one,
                                 method_ploughing, #select one
                                 method_ridging, #select one
                                 FCY,
                                 rootUP,
                                 riskAtt) {
  #creating ploughing and ridging scenarios
  ds <- expand.grid(method_ploughing = c("N/A", "manual", "tractor"), method_ridging = c("N/A", "manual", "tractor"))
  ds$ploughing <- ifelse(ds$method_ploughing == "N/A", FALSE, TRUE)
  ds$ridging <- ifelse(ds$method_ridging == "N/A", FALSE, TRUE)
  ds$cost_ploughing <- ifelse(ds$method_ploughing == "N/A", 0,
                              ifelse(ds$method_ploughing == "manual",
                                     costLMO[costLMO$operation == "ploughing" & costLMO$method == "manual",]$costHa,
                                     costLMO[costLMO$operation == "ploughing" & costLMO$method == "tractor",]$costHa))
  ds$cost_ridging <- ifelse(ds$method_ridging == "N/A", 0,
                            ifelse(ds$method_ridging == "manual",
                                   costLMO[costLMO$operation == "ridging" & costLMO$method == "manual",]$costHa,
                                   costLMO[costLMO$operation == "ridging" & costLMO$method == "tractor",]$costHa))
  ds <- na.omit(ds)
  #adding cost saving for weeding
  ds$cost_weeding <- ifelse(ds$ridging, -costLMO[costLMO$operation == "weeding1",]$costHa, 0)

  #adding expected yields
  yd <- expand.grid(ploughing = c(FALSE, TRUE), ridging = c(TRUE, FALSE), YL = c("low", "high"))
  yd$RY <- c(rep(10, 4), 20, 25, 15, 22)
  yd <- yd[yd$YL == ifelse(FCY < 12.5, "low", "high"),]
  ds <- merge(ds, yd)
  ds$RP <- ds$RY * areaHa

  #calculating total cost, gross and net revenue
  ds$TC <- (ds$cost_ploughing + ds$cost_ridging + ds$cost_weeding) * areaHa
  ds$GR <- ds$RP * rootUP
  ds$NR <- ds$GR - ds$TC
  
  #minimal required net revenue increase from fertilizer needed (taking into account risk attitude of user)
  # ds$dNRmin <- ds$TC * ifelse(riskAtt == 0, 2.8, ifelse(riskAtt == 1, 2, 1.2))
  ds$dNRmin <- ds$TC * ifelse(riskAtt == 0, 1.8, ifelse(riskAtt == 1, 1, 0.2))
  
  ds <- droplevels(ds[ds$NR > ds$dNRmin, ])

  ds <- subset(ds, select = -c(cost_ploughing, cost_ridging, cost_weeding, YL, RY))
  ds <- ds[order(-ds$NR, ds$ridging, ds$ploughing),] #order by decreasing net revenue, increasing ridging and increasing ploughing so that recommendation is first row

  #comparing to current practice
  ds$CP <- ifelse(ds$ploughing == ploughing &
                    ds$method_ploughing == method_ploughing &
                    ds$ridging == ridging &
                    ds$method_ridging == method_ridging, TRUE, FALSE)
  ds$dTC <- ds$TC - ds[ds$CP == TRUE,]$TC
  ds$dRP <- ds$RP - ds[ds$CP == TRUE,]$RP
  ds$dGR <- ds$GR - ds[ds$CP == TRUE,]$GR
  ds$dNR <- ds$NR - ds[ds$CP == TRUE,]$NR

  return(ds)

}



#SHORT DEF:   Function to obtain recommendations on cassava-maize intercropping.
#RETURNS:     list of 2 dataframes: (i) cost benefit analysis for most profitable system, and (ii) fertilizer rates to apply.
#DESCRIPTION: Function to obtain recommendations on cassava-maize intercropping.
#             Returns (i) a 1-row dataframe cost-benefit parameters (extra yield, cost and net revenue, and whether to apply
#             fertilizer and to plant maize at high density, and why (not)) , and (ii) a data.frame with types of fertilizer and rates to apply (zeros included).
#INPUT:       See Cassava Crop Manager function for details
getICrecommendations <- function(areaHa = 1,
                                 CMP = 1:5,
                                 cobUP,
                                 fertilizers,
                                 riskAtt = c(0, 1, 2)) {

  if (!require("limSolve")) install.packages("limSolve"); library("limSolve")

  #calculating expected yield increase from fertilizer
  maizeY <- data.frame(CMP = 1:5,
                       dY = c(0, 6500, 4000, 2500, 0))

  dMY <- maizeY[maizeY$CMP == CMP,]$dY
  dMP <- dMY * areaHa #extra maize production for the area of the field

  #extra gross revenue from fertilizer
  dGR <- dMP * cobUP

  if (dGR == 0) {
    reason_F <- ifelse(CMP == 1, "of low soil fertility", "of high soil fertility")
    dTC <-  0
    FRATE <- 0
  } else {
    #calculating fertilizer requirement
    E <- t(data.matrix(fertilizers[, 2:4]))
    F <- c(91, 21, 37.5) #ideally 2 bags of urea + 6 bags of NPK15:15:15
    G <- diag(nrow(fertilizers))
    H <- rep(0, nrow(fertilizers))
    Cost <- fertilizers$price

    #calculating fertilizer recommendation and total cost of fertilizer
    FRATE <- linp(E, F, G, H, Cost)$X
    FRATE[FRATE < 25] <- 0 #dropping all rates less than 25 kg/ha
    FRATE <- FRATE * areaHa #adjusting to field area

    #calculating total cost
    dTC <- c(FRATE %*% fertilizers$price)

    #evaluating if a solution was found
    if (dTC == 0) {
      dGR <- 0
      dMP <- 0
      reason_F <- "appropriate fertilizer is not available"
    }else {
      reason_F <- "appropriate fertilizer is available"
    }
  }

  #net revenue increase from fertilizer
  dNR <- dGR - dTC

  #minimal required net revenue increase from fertilizer needed (taking into account risk attitude of user)
  dNRmin <- dTC * ifelse(riskAtt == 0, 1.8, ifelse(riskAtt == 1, 1, 0.2))

  #check profitability of fertilizer use
  if (dNR > dNRmin) {
    rec_F <- TRUE
    reason_F <- "fertilizer use is sufficiently profitable"
  }else {
    dMP <- 0
    dTC <- 0
    dGR <- 0
    dNR <- 0
    FRATE <- 0
    rec_F <- FALSE
    reason_F <- "fertilizer use is not sufficiently profitable"
  }

  #recommendation on high density maize planting
  rec_D <- ifelse(rec_F == TRUE | CMP == 5, TRUE, FALSE)
  reason_D <- ifelse(rec_F == TRUE, "fertilizer use is recommended", ifelse(CMP == 5, "of high soil fertility", NA))


  #output
  if (!is.na(reason_D)) {
    rec <- data.frame(dMP = dMP, #extra maize production expected (in nr of cobs)
                      dNR = dNR, #net revenue increase from fertilizer use (in local currency)
                      dTC = dTC, #extra cost for fertilizer use (in local currency)
                      rec_F = rec_F, #TRUE or FALSE indicating if fertilizer application is recommended
                      rec_D = rec_D, #TRUE or FALSE indicating if high density maize planting is recommended
                      reason_F = reason_F, #reason why fertilizer application is not recommended
                      reason_D = reason_D)  #reason why high maize density is recommended


  }else {
    rec <- data.frame(dMP = dMP, #extra maize production expected (in nr of cobs)
                      dNR = dNR, #net revenue increase from fertilizer use (in local currency)
                      dTC = dTC, #extra cost for fertilizer use (in local currency)
                      rec_F = rec_F, #TRUE or FALSE indicating if fertilizer application is recommended
                      rec_D = rec_D, #TRUE or FALSE indicating if high density maize planting is recommended
                      reason_F = reason_F) #reason why fertilizer application is not recommended


  }

  fertilizer_rates <- data.frame(type = fertilizers$type, rate = FRATE) #fertilizer rates to apply
  fertilizer_rates <- fertilizer_rates[fertilizer_rates$rate > 0,]
  
  if(CMP == 1){
    rec$reason_F <- "Your soil is very poor. You need to improve soil fertility before considering investing in fertilizer. You should apply compost or manure, or fallow for at least 2 years. Plant maize at low density (20,000 plants per hectare) and saw the seeds at 50 cm within rows."
  }else if (CMP == 5){
    rec$reason_F <- "Your soil is very fertile. It is likely that your maize yield will not improve much after fertilizer application. Plant maize at high density (40,000 plants per hectare) and sow the seeds at 25 cm within rows."
  }
  

  return(list(rec = rec,
              fertilizer_rates = fertilizer_rates))

}


## schedule planting and harvest dates is absed on searching highest return on investemnt within 1- or 2-months window around the user intended plant/harvest dates;  
## For every location,pecalculated INS for the FCY class and WLY are sourced and then current yield is modelled, 
## a data frame is created constituting relevant planting and harvest windows, for every combination predicted yield is scaled 
## based on farmer-reported current yield relative to modelled current and Water linited Yield and for every 
## combiantion of planting and harest weeks, gross revenue is computed and a data frame is returned. 

#' Title see pratmeters definition in R_Wrapper_5C.R
#'
#' @param areaHa 
#' @param country 
#' @param lat 
#' @param lon 
#' @param PD 
#' @param HD 
#' @param PD_window 
#' @param HD_window 
#' @param saleSF 
#' @param nameSF 
#' @param FCY 
#' @param rootUP 
#' @param rootUP_m1 
#' @param rootUP_m2 
#' @param rootUP_p1 
#' @param rootUP_p2 
#'
#' @return
#' @export
#'
#' @examples
getSPrecommendations <- function(areaHa,
                                 country,
                                 lat,
                                 lon,
                                 PD,
                                 HD,
                                 PD_window,
                                 HD_window,
                                 saleSF,
                                 nameSF,
                                 FCY,
                                 rootUP,
                                 rootUP_m1,
                                 rootUP_m2,
                                 rootUP_p1,
                                 rootUP_p2) {

  #rounding lat and lon to centroid of 5x5km pixel
  latr <- as.factor(floor(lat * 10) / 10 + ifelse(lat - (floor(lat * 10) / 10) < 0.05, 0.025, 0.075))
  lonr <- as.factor(floor(lon * 10) / 10 + ifelse(lat - (floor(lon * 10) / 10) < 0.05, 0.025, 0.075))
  latr <- as.numeric(levels(latr))
  lonr <- as.numeric(levels(lonr))



  SoilData_fcy1 <- readRDS("SoilData_4Country.RDS")
  SoilData <- SoilData_fcy1[SoilData_fcy1$long == lonr & SoilData_fcy1$lat == latr,]


  if (country == "NG") {
    WLY_15M <- readRDS("Nigeria_WLY_LINTUL_2020_Server.RDS")
  } else if (country == "TZ"){
    WLY_15M <- readRDS("Tanzania_WLY_LINTUL_2020_Server.RDS")
  } else if (country == "GH"){
    WLY_15M <- readRDS("Ghana_WLY_LINTUL_SP.RDS")
  }
  

  latlon <- paste(latr, lonr, sep = "_")

  WLYDataLintul <- WLY_15M[WLY_15M$location == latlon,] ##WLY_15M[WLY_15M$long == lonr & WLY_15M$lat == latr, ]
  WLY_CY <- NULL
  if (nrow(WLYDataLintul) > 0) {
    WLYDataLintul <- merge(WLYDataLintul, data.frame(daysOnField = seq(235, 455, 7), haw = 34:65), by = "daysOnField")
    for (k in 1:nrow(WLYDataLintul)) {
      #print(k)
      wlyd <- WLYDataLintul[k,]
      if (!is.na(SoilData$soilN)) {
        wlyd$Current_Yield <- QUEFTS_WLY_CY(SoilData = SoilData, country = country, wlyd = wlyd) # in kg/ha dry
        WLY_CY <- rbind(WLY_CY, wlyd)
      }
    }
  }



  TRNS <- read.csv("translations_TEST.csv", stringsAsFactors = FALSE)
  frnotrec_ng <- gsub(pattern = "\"", replacement = "", TRNS$frnotrec[1]); 
  frnotrec_tz <- gsub(pattern = "\"", replacement = "", TRNS$frnotrec[2]);
  frnotrec_rw <- gsub(pattern = "\"", replacement = "", TRNS$frnotrec[3])

  if (is.null(WLY_CY)) {
    # if(!file.exists(flp)){
    #issuereturn
    #trans
    ds <- NULL
    #return("No recommendations available for this location because your location is not within the recommendation domain of AKILIMO.")

    if (country == "NG" | country == "GH") {
      return("We do not have fertilizer recommendation for your location because your location is out of the recommendation domain AKILIMO is currently serving.")
    }else if (country == "TZ"){
      return("Hatuna mapendekezo yoyote kwa eneo lako kwa sababu eneo lako liko nje la eneo ambalo AKILIMO linafanya kazi kwa sasa")
    }else {
      return("kinyarwanda here")
    }

  } else {


    yld <- unique(data.frame(plw = WLY_CY$PlweekNr, haw = WLY_CY$haw, CY = WLY_CY$Current_Yield, WY = WLY_CY$water_limited_yield)) ## is still dry weight
    yld$CY <- round(yld$CY / 1000, digits = 0)
    yld$WY <- round(yld$WY / 1000, digits = 0)


    #constituting df with yields within relevant planting and harvest windows
    ds <- expand.grid(rPWnr = seq((-4 * PD_window), (4 * PD_window), by = 2),
                      rHWnr = seq((-4 * HD_window), (4 * HD_window), by = 2))

    ds$PD <- as.Date(PD) + ds$rPWnr * 7
    ds$HD <- as.Date(HD) + ds$rHWnr * 7
 
    # ds <- droplevels(ds[ds$PD >= Sys.Date(),])


    ds$plw <- as.numeric(format(ds$PD, format = "%W")) + 1
    ds$haw <- round(as.numeric(ds$HD - ds$PD) / 7)
    ds <- merge(ds, yld)


    #converting dry yields to fresh root yields
    # ds$RFCY <- getRFY(HD = ds$HD, RDY = ds$CY, country = country)
    # ds$RFWY <- getRFY(HD = ds$HD, RDY = ds$WY, country = country)
    #
    for (k in 1:nrow(ds)) {
      ds$RFCY[k] <- getRFY(HD = ds$HD[k], RDY = ds$CY[k], country = "NG") ## TZ function is giving very strange values, need to be checked
      ds$RFWY[k] <- getRFY(HD = ds$HD[k], RDY = ds$WY[k], country = "NG")
    }


    #scaling predicted yield based on farmer-reported current yield relative to modelled CY and WY:
    ds$RY <- (ds$RFWY - ds$RFCY) / (13.5 - 1.5) / 2.5 * (FCY - 1.5 * 2.5) + ds$RFCY
    ds$RP <- ds$RY * areaHa

    #If selling to a starch factory: rootUP is determined by starch content of roots
    if (saleSF) {

      ds$SC <- 0.75 * ds$WY / ds$RFWY * 100
      #SF <- read.csv("E:/03-projects/ACAI/ODK briefcase storage/created forms/DSTs/media_SPHS/starchPrices.csv")
      SF <- read.csv("starchPrices.csv")
      #SF <- read.csv("D:/ACAI_Wrapper/cloud_compute/starchPrices.csv")
      SF <- SF[SF$starchFactory == nameSF,]
      price <- NULL
      for (i in 1:nrow(ds)) {
        price <- c(price, max(SF[SF$minStarch < ds[i,]$SC,]$price))
      }
      ds$rootUP <- price
      ds <- subset(ds, select = -SC)
      #If not selling to a starch factory, user needs to specify rootUP across the harvest window
    }else {
      dp <- data.frame(rHWnr = c(-8, -4, 0, 4, 8),
                       rootUP = c(rootUP_m2, rootUP_m1, rootUP, rootUP_p1, rootUP_p2))
      dpm <- suppressWarnings(loess(rootUP ~ rHWnr, data = dp))
      dpp <- data.frame(rHWnr = seq((-4 * HD_window), (4 * HD_window), by = 2),
                        rootUP = predict(dpm, data.frame(rHWnr = seq((-4 * HD_window), (4 * HD_window), by = 2))))
      ds <- merge(ds, dpp)
    }

    ds$GR <- ds$RP * ds$rootUP
    ds$CP <- ifelse(ds$rPWnr == 0 & ds$rHWnr == 0, TRUE, FALSE)
    ds$dGR <- ds$GR - ds[ds$CP == TRUE,]$GR

    #sort by decreasing GR, increasing HWnr, decreasing PWnr
    #recommendation is highest GR, earliest harvesting, latest planting combination
    ds <- ds[order(-ds$dGR, ds$rHWnr, -ds$rPWnr),]
    write.csv(ds, "SP_rec.csv", row.names = FALSE)
    return(ds)
  }

}


#######################################################################
## FR & SP
#######################################################################


#' function to creat a data frame with fertilizers
#' @return: data frame with (type, N_cont, P_cont, K_cont, price) The NPK is elemental concentration and price is per kg of fertilizer
#' @example
#TODO: price of fertilizers for tanzania is different so from GPS we need to define the zone and take the correct price accordingly. default is at the end of the script
fertilizerFunc <- function(NPK201216available = TRUE, NPK201216CostperBag = NA, NPK201216BagWt = 50,
                           ureaavailable = TRUE, ureaCostperBag = NA, ureaBagWt = 50,
                           MOPavailable = TRUE, MOPCostperBag = NA, MOPBagWt = 50,
                           DAPavailable = TRUE, DAPCostperBag = NA, DAPBagWt = 50,
                           NPK201010available = TRUE, NPK201010CostperBag = NA, NPK201010BagWt = 50,
                           NPK151515available = TRUE, NPK151515CostperBag = NA, NPK151515BagWt = 50,
                           TSPavailable = TRUE, TSPCostperBag = NA, TSPBagWt = 50,
                           NPK171717available = FALSE, NPK171717CostperBag = NA, NPK171717BagWt = 50,
                           CANavailable = TRUE, CANCostperBag = NA, CANBagWt = 50,
                           SSPavailable = TRUE, SSPCostperBag = NA, SSPBagWt = 50,
                           NPK112221available = FALSE, NPK112221CostperBag = NA, NPK112221BagWt = 50, 
                           NPK251010available = FALSE, NPK251010CostperBag = NA, NPK251010BagWt = 50, 
                           NPK152020available = FALSE, NPK152020CostperBag = NA, NPK152020BagWt = 50, 
                           NPK23105available = FALSE, NPK23105CostperBag = NA, NPK23105BagWt = 50, 
                           NPK123017available = FALSE, NPK123017CostperBag = NA, NPK123017BagWt = 50, 
                           
                           FOMIIMBURAavailable = FALSE, FOMIIMBURACostperBag = NA, FOMIIMBURABagWt = 50, 
                           FOMIBAGARAavailable = FALSE, FOMIBAGARACostperBag = NA, FOMIBAGARABagWt = 50, 
                           FOMITOTAHAZAavailable = FALSE, FOMITOTAHAZACostperBag = NA, FOMITOTAHAZABagWt = 50,
                           
                           newFert1name = NA, newFert1N_cont = NA, newFert1P2O5 = NA, newFert1K2O = NA, newFert1CostperBag = NA, newFert1BagWt = NA,
                           newFert2name = NA, newFert2N_cont = NA, newFert2P2O5 = NA, newFert2K2O = NA, newFert2CostperBag = NA, newFert2BagWt = NA,
                           newFert3name = NA, newFert3N_cont = NA, newFert3P2O5 = NA, newFert3K2O = NA, newFert3CostperBag = NA, newFert3BagWt = NA,
                           newFert4name = NA, newFert4N_cont = NA, newFert4P2O5 = NA, newFert4K2O = NA, newFert4CostperBag = NA, newFert4BagWt = NA,
                           newFert5name = NA, newFert5N_cont = NA, newFert5P2O5 = NA, newFert5K2O = NA, newFert5CostperBag = NA, newFert5BagWt = NA, country) {
  Default_prices <- read.csv("Default_prices.csv")
  if (country == "NG") {
    if (ureaCostperBag == 0) { ureaCostperBag <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "urea", ]$Price } else { ureaCostperBag <- as.numeric(ureaCostperBag) }
    if (MOPCostperBag == 0) { MOPCostperBag <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "MOP", ]$Price }else { MOPCostperBag <- as.numeric(MOPCostperBag) }
    if (DAPCostperBag == 0) { DAPCostperBag <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "DAP", ]$Price }else { DAPCostperBag <- as.numeric(DAPCostperBag) }
    if (NPK201010CostperBag == 0) { NPK201010CostperBag <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "NPK201010", ]$Price }else { NPK201010CostperBag <- as.numeric(NPK201010CostperBag) }
    if (NPK151515CostperBag == 0) { NPK151515CostperBag <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "NPK151515", ]$Price }else { NPK151515CostperBag <- as.numeric(NPK151515CostperBag) }
    if (TSPCostperBag == 0) { TSPCostperBag <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "TSP", ]$Price }else { TSPCostperBag <- as.numeric(TSPCostperBag) }
    if (NPK171717CostperBag == 0) { NPK171717CostperBag <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "NPK171717", ]$Price }else { NPK171717CostperBag <- as.numeric(NPK171717CostperBag) }
    if (CANCostperBag == 0) { CANCostperBag <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "CAN", ]$Price }else { CANCostperBag <- as.numeric(CANCostperBag) }
    if (SSPCostperBag == 0) { SSPCostperBag <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "SSP", ]$Price }else { SSPCostperBag <- as.numeric(SSPCostperBag) }
    if (NPK201216CostperBag == 0) { NPK201216CostperBag <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "NPK201216", ]$Price }else { NPK201216CostperBag <- as.numeric(NPK201216CostperBag) }
  }else if(country == "TZ"){
    if (ureaCostperBag == 0) { ureaCostperBag <- Default_prices[Default_prices$Country == "TZ" & Default_prices$Item == "urea", ]$Price }else { ureaCostperBag <- as.numeric(ureaCostperBag) } ##65000
    if (MOPCostperBag == 0) { MOPCostperBag <- Default_prices[Default_prices$Country == "TZ" & Default_prices$Item == "MOP", ]$Price  }else { MOPCostperBag <- as.numeric(MOPCostperBag) } ##120000
    if (DAPCostperBag == 0) { DAPCostperBag <- Default_prices[Default_prices$Country == "TZ" & Default_prices$Item == "DAP", ]$Price  }else { DAPCostperBag <- as.numeric(DAPCostperBag) } ##85000
    if (NPK201010CostperBag == 0) { NPK201010CostperBag <- Default_prices[Default_prices$Country == "TZ" & Default_prices$Item == "NPK201010", ]$Price  }else { NPK201010CostperBag <- as.numeric(NPK201010CostperBag) }
    if (TSPCostperBag == 0) { TSPCostperBag <- Default_prices[Default_prices$Country == "TZ" & Default_prices$Item == "TSP", ]$Price  }else { TSPCostperBag <- as.numeric(TSPCostperBag) } #80000
    if (NPK171717CostperBag == 0) { NPK171717CostperBag <- Default_prices[Default_prices$Country == "TZ" & Default_prices$Item == "NPK171717", ]$Price  }else { NPK171717CostperBag <- as.numeric(NPK171717CostperBag) } ##61000
    if (CANCostperBag == 0) { CANCostperBag <- Default_prices[Default_prices$Country == "TZ" & Default_prices$Item == "CAN", ]$Price  }else { CANCostperBag <- as.numeric(CANCostperBag) } #65000
   }else if(country == "RW"){
    if (ureaCostperBag == 0) { ureaCostperBag <- Default_prices[Default_prices$Country == "RW" & Default_prices$Item == "urea", ]$Price }else { ureaCostperBag <- as.numeric(ureaCostperBag) }#564*50
    if (MOPCostperBag == 0) { MOPCostperBag <- Default_prices[Default_prices$Country == "RW" & Default_prices$Item == "MOP", ]$Price}else { MOPCostperBag <- as.numeric(MOPCostperBag) }#558*50 
    if (DAPCostperBag == 0) { DAPCostperBag <- Default_prices[Default_prices$Country == "RW" & Default_prices$Item == "DAP", ]$Price }else { DAPCostperBag <- as.numeric(DAPCostperBag) } #633*50
    if (NPK171717CostperBag == 0) { NPK171717CostperBag <- Default_prices[Default_prices$Country == "RW" & Default_prices$Item == "NPK171717", ]$Price}else { NPK171717CostperBag <- as.numeric(NPK171717CostperBag) }#713 *50
   }else if(country == "GH"){
     if (ureaCostperBag == 0) { ureaCostperBag <- Default_prices[Default_prices$Country == "GH" & Default_prices$Item == "urea", ]$Price }else { ureaCostperBag <- as.numeric(ureaCostperBag) }
     if (NPK112221CostperBag == 0) { NPK112221CostperBag <- Default_prices[Default_prices$Country == "GH" & Default_prices$Item == "NPK112221", ]$Price}else { NPK112221CostperBag <- as.numeric(NPK112221CostperBag) }
     if (NPK251010CostperBag == 0) { NPK251010CostperBag <- Default_prices[Default_prices$Country == "GH" & Default_prices$Item == "NPK251010", ]$Price }else { NPK251010CostperBag <- as.numeric(NPK251010CostperBag) }
     if (NPK152020CostperBag == 0) { NPK152020CostperBag <- Default_prices[Default_prices$Country == "GH" & Default_prices$Item == "NPK152020", ]$Price}else { NPK152020CostperBag <- as.numeric(NPK152020CostperBag) }
     if (NPK201010CostperBag == 0) { NPK201010CostperBag <- Default_prices[Default_prices$Country == "GH" & Default_prices$Item == "NPK201010", ]$Price }else { NPK201010CostperBag <- as.numeric(NPK201010CostperBag) }
     if (NPK23105CostperBag == 0)  { NPK23105CostperBag <- Default_prices[Default_prices$Country == "GH" & Default_prices$Item == "NPK23105", ]$Price46}else { NPK23105CostperBag <- as.numeric(NPK23105CostperBag) } 
     if (NPK123017CostperBag == 0) { NPK123017CostperBag <- Default_prices[Default_prices$Country == "GH" & Default_prices$Item == "NPK123017", ]$Price }else { NPK123017CostperBag <- as.numeric(NPK123017CostperBag) } 
   }else if(country == "BU"){
     if (FOMIIMBURACostperBag == 0) { FOMIIMBURACostperBag <- Default_prices[Default_prices$Country == "BU" & Default_prices$Item == "FOMIIMBURA", ]$Price} else { FOMIIMBURACostperBag <- as.numeric(FOMIIMBURACostperBag) }
     if (FOMIBAGARACostperBag == 0) { FOMIBAGARACostperBag <- Default_prices[Default_prices$Country == "BU" & Default_prices$Item == "FOMIBAGARA", ]}else { FOMIBAGARACostperBag <- as.numeric(FOMIBAGARACostperBag) }
     if (FOMITOTAHAZACostperBag == 0) { FOMITOTAHAZACostperBag <- Default_prices[Default_prices$Country == "BU" & Default_prices$Item == "FOMITOTAHAZA", ]}else { FOMITOTAHAZACostperBag <- as.numeric(FOMITOTAHAZACostperBag) }
   }
  
  
  ureaFert <- data.frame(type = 'Urea', available = ureaavailable, N_cont = 0.46, P_cont = 0, K_cont = 0, costPerBag = ureaCostperBag, bagWeight = ureaBagWt)
  MOPFert <- data.frame(type = 'MOP', available = MOPavailable, N_cont = 0.00, P_cont = 0.00, K_cont = 0.60, costPerBag = MOPCostperBag, bagWeight = MOPBagWt)
  DAPFert <- data.frame(type = 'DAP', available = DAPavailable, N_cont = 0.18, P_cont = 0.20, K_cont = 0.0, costPerBag = DAPCostperBag, bagWeight = DAPBagWt)
  NPK201010Fert <- data.frame(type = 'NPK20_10_10', available = NPK201010available, N_cont = 0.20, P_cont = 0.044, K_cont = 0.083, costPerBag = NPK201010CostperBag, bagWeight = NPK201010BagWt)
  NPK151515Fert <- data.frame(type = 'NPK15_15_15', available = NPK151515available, N_cont = 0.15, P_cont = 0.07, K_cont = 0.125, costPerBag = NPK151515CostperBag, bagWeight = NPK151515BagWt)
  TSPFert <- data.frame(type = 'TSP', available = TSPavailable, N_cont = 0.0, P_cont = 0.2, K_cont = 0.0, costPerBag = TSPCostperBag, bagWeight = TSPBagWt)
  NPK171717Fert <- data.frame(type = 'NPK17_17_17', available = NPK171717available, N_cont = 0.17, P_cont = 0.074, K_cont = 0.15, costPerBag = NPK171717CostperBag, bagWeight = NPK171717BagWt) # TODO get price
  NPK201216Fert <- data.frame(type = 'NPK20_12_26', available = NPK201216available, N_cont = 0.20, P_cont = 0.052, K_cont = 0.216, costPerBag = NPK201216CostperBag, bagWeight = NPK201216BagWt)
  CANFert <- data.frame(type = 'CAN', available = CANavailable, N_cont = 0.27, P_cont = 0.00, K_cont = 0.00, costPerBag = CANCostperBag, bagWeight = CANBagWt) ## not correct value TODO check
  SSPFert <- data.frame(type = 'SSP', available = SSPavailable, N_cont = 0.00, P_cont = 0.15, K_cont = 0.00, costPerBag = SSPCostperBag, bagWeight = SSPBagWt) ## not correct value TODO check
  
  FOMIIMBURAFert <- data.frame(type = 'FOMI_IMBURA', available = FOMIIMBURAavailable, N_cont = 0.09, P_cont = 0.0968, K_cont = 0.0332, costPerBag = FOMIIMBURACostperBag, bagWeight = FOMIIMBURABagWt)
  FOMIBAGARAFert <- data.frame(type = 'FOMI_BAGARA', available = FOMIBAGARAavailable, N_cont = 0.11, P_cont = 0.00, K_cont = 0.1826, costPerBag = FOMIBAGARACostperBag, bagWeight = FOMIBAGARABagWt) ## not correct value TODO check
  FOMITOTAHAZAFert <- data.frame(type = 'FOMI_TOTAHAZA', available = FOMITOTAHAZAavailable, N_cont = 0.21, P_cont = 0.00, K_cont = 0.0664, costPerBag = FOMITOTAHAZACostperBag, bagWeight = FOMITOTAHAZABagWt) ## not correct value TODO check
  
  
  NPK112221Fert <- data.frame(type = 'NPK112221', available = NPK112221available, N_cont = 0.11, P_cont = 0.1, K_cont = 0.17, costPerBag = 160, bagWeight = 50)
  NPK251010Fert <- data.frame(type = 'NPK251010', available = NPK251010available, N_cont = 0.25, P_cont = 0.044, K_cont = 0.083, costPerBag = 150, bagWeight = 50)
  NPK152020Fert <- data.frame(type = 'NPK152020', available = NPK152020available, N_cont = 0.15, P_cont = 0.088, K_cont = 0.166, costPerBag = 156, bagWeight = 50)
  NPK201010Fert <- data.frame(type = 'NPK201010', available = NPK201010available, N_cont = 0.20, P_cont = 0.044, K_cont = 0.083, costPerBag = 146, bagWeight = 50)
  NPK23105Fert <- data.frame(type = 'NPK23105', available = NPK23105available, N_cont = 0.23, P_cont = 0.044, K_cont = 0.0415, costPerBag = 146, bagWeight = 50)
  NPK123017Fert <- data.frame(type = 'NPK123017', available = NPK123017available, N_cont = 0.12, P_cont = 0.132, K_cont = 0.14, costPerBag = 160, bagWeight = 50)
  

  if (country == "NG") {
    fd_cont <- rbind(ureaFert, MOPFert, DAPFert, CANFert, NPK171717Fert, NPK151515Fert, NPK201010Fert, TSPFert, SSPFert, NPK201216Fert)
  }else if (country == "TZ") {
    fd_cont <- rbind(ureaFert, MOPFert, DAPFert, CANFert, NPK171717Fert, NPK151515Fert, NPK201010Fert, TSPFert, SSPFert)
  }else if (country == "RW") {
    fd_cont <- rbind(ureaFert, MOPFert, DAPFert, NPK171717Fert)
  }else if (country == "GH") {
    fd_cont <- rbind(ureaFert, NPK112221Fert, NPK251010Fert, NPK152020Fert, NPK201010Fert, NPK23105Fert, NPK123017Fert)
  }else if (country == "BU") {
    fd_cont <- rbind(FOMIIMBURAFert, FOMIBAGARAFert, FOMITOTAHAZAFert)
  }


  fd_cont <- droplevels(fd_cont[fd_cont$available == TRUE,])
  fd_cont$costPerBag <- as.numeric(fd_cont$costPerBag)
  fd_cont$price <- fd_cont$costPerBag / fd_cont$bagWeight
  fd_cont <- subset(fd_cont, select = -c(available))

  if (any(newFert1name != "NA" |
            newFert2name != "NA" |
            newFert3name != "NA" |
            newFert4name != "NA" |
            newFert5name != "NA")) {
    OtherFertilizers <- data.frame(expand.grid(type = c(newFert1name, newFert2name, newFert3name, newFert4name, newFert5name)),
                                   expand.grid(N_cont = c(newFert1N_cont, newFert2N_cont, newFert3N_cont, newFert4N_cont, newFert5N_cont)),
                                   expand.grid(P2O5_cont = c(newFert1P2O5, newFert2P2O5, newFert3P2O5, newFert4P2O5, newFert5P2O5)),
                                   expand.grid(K2O_cont = c(newFert1K2O, newFert2K2O, newFert3K2O, newFert4K2O, newFert5K2O)),
                                   expand.grid(newFertCostperBag = c(newFert1CostperBag, newFert2CostperBag, newFert3CostperBag, newFert4CostperBag, newFert5CostperBag)),
                                   expand.grid(newFertBagWt = c(newFert1BagWt, newFert2BagWt, newFert3BagWt, newFert4BagWt, newFert5BagWt)))

    OtherFertilizers <- droplevels(OtherFertilizers[as.numeric(as.factor(OtherFertilizers$type)) == 1,])
    OtherFertilizers$N_cont <- as.numeric(as.character(OtherFertilizers$N_cont))
    OtherFertilizers$P2O5_cont <- as.numeric(as.character(OtherFertilizers$P2O5_cont))
    OtherFertilizers$K2O_cont <- as.numeric(as.character(OtherFertilizers$K2O_cont))
    OtherFertilizers$newFertCostperBag <- as.numeric(as.character(OtherFertilizers$newFertCostperBag))
    OtherFertilizers$newFertBagWt <- as.numeric(as.character(OtherFertilizers$newFertBagWt))


    if (nrow(OtherFertilizers) > 0) {
      newfert <- NULL
      for (k in 1:nrow(OtherFertilizers)) {
        OF <- OtherFertilizers[k,]

        if (OF$N_cont == 0) {
          N_cont <- 0
        }else {
          #N_cont <- round(as.numeric(OF$N_cont)/100,digits=3)
          N_cont <- OF$N_cont
        }

        if (OF$P2O5_cont == 0) {
          P_cont <- 0
        }else {
          P_cont <- round(0.44 * as.numeric(OF$P2O5_cont), digits = 3)
        }

        if (OF$K2O_cont == 0) {
          K_cont <- 0
        }else {
          K_cont <- round(0.83 * as.numeric(OF$K2O_cont), digits = 3)
        }

        fnew <- data.frame(type = OF$type, N_cont = N_cont,
                           P_cont = P_cont, K_cont = K_cont,
                           costPerBag = OF$newFertCostperBag, bagWeight = OF$newFertBagWt)
        newfert <- rbind(newfert, fnew)
      }
      newfert$price <- newfert$costPerBag / newfert$bagWeight

      fd_cont <- rbind(fd_cont, newfert)
    }
  }
  return(fd_cont)
}





#SHORT DEF:   Function to convert root DM yield into root fresh matter yield (RFY)
#RETURNS:     RFY: root fresh yield in the same units as root DM yield input
#DESCRIPTION: Function to predict root FM yield based on date of harvest and country, using data from gravimetric starch measurements conducted across ACAI trials.
#INPUT:       HD: harvest date (Date format)
#             RDY: root dry matter yield (user's units)
#             country = c("NG", "TZ")

getRFY <- function(HD,
                   RDY,
                   country) {
  d <- as.numeric(strftime(HD, format = "%j"))
  fd <- read.csv("fd2.csv") #data.frame with day of the year (dayNr = [1..366]) and %DM (DMCont = [0..100], by country)
  DC <- merge(data.frame(dayNr = d), fd[fd$country == "NG",], sort = FALSE)$DMCont
  RFY <- RDY / DC * 100
  return(RFY)

}



#SHORT DEF:   Function to convert root FM yield into root dry matter yield (RDY): user define CY in FM in ton/ha, QUEFTS require Cy in DM kg/ha
#RETURNS:     RDY: root dry yield in the same units as root FM yield input
#INPUT:       HD: harvest date (Date format)
#             RFY: root fresh matter yield (user's units)
#             country = c("NG", "TZ")
## current yield is given by the user as FM ton/ha, we need to change it to DM in Kg/ha for QUEFTS

getRDY <- function(HD, RFY, country) {
  if (HD > 366) {
    HD <- HD - 366
  }
  d <- HD
  fd <- read.csv("fd2.csv")
  DC <- merge(data.frame(dayNr = d), fd[fd$country == country,], sort = FALSE)$DMCont
  RDY <- (RFY * DC) / 100
  return(RDY)

}


#' get optimized fertilizer rate, and associated yield for the recommended rate and net revenue given cost and investment
#'
#' @param rootUP root price
#' @param QID soil data
#' @param fertilizer fertilizer types and prices
#' @param invest investment capacity
#' @param plDate planting date
#' @param WLYData WLY data , this is not needed check and remove
#' @param lat 
#' @param lon 
#' @param areaHa farm size
#' @param HD harvest date
#' @param WLY water limitied yeild
#' @param DCY dry current wt, the output of QUEFTS
#' @param country 
run_Optim_NG2 <- function(rootUP, QID, fertilizer, invest, plDate, WLYData, lat, lon, areaHa, HD, WLY, DCY, country) {

  ## input of CY and WLY are in dry wt in KG/ha
  QID$water_limited_yield <- WLY
  initial <- rep(0, nrow(fertilizer))
  lowerST <- rep(0, nrow(fertilizer))

  ## both CY and TY should be changed to user land size in ton/ha and fresh wt
  CY_user <- ((getRFY(HD = HD, RDY = DCY, country = "NG")) / 1000) * areaHa ## TZ model is extrememly high
  WLY_user <- ((getRFY(HD = HD, RDY = WLY, country = "NG")) / 1000) * areaHa

## this is where the optimization is done, and thereuslt is the NPK rate that gives max profit
  FR <- optim(par = initial, fn = optim_NR, lower = lowerST, method = "L-BFGS-B", control = list(fnscale = -1), rootUP = rootUP,
              QID = QID, CY = DCY, fertilizer = fertilizer, invest = invest, HD = HD, country = country)$par



  if (all(FR == 0)) {
    return(data.frame(lat = lat, lon = lon, plDate, N = 0, P = 0, K = 0, WLY = WLY_user, CurrentY = CY_user, TargetY = CY_user, TC = 0, NR = 0))
  }else {

    fertilizer$FR <- FR

    ## NPK rate for ha of land
    N <- as.vector(FR %*% fertilizer$N_cont)
    P <- as.vector(FR %*% fertilizer$P_cont)
    K <- as.vector(FR %*% fertilizer$K_cont)
    rec <- c(N, P, K)

    ## NPK rate for user land size
    NPK_user <- rec * areaHa

    ## TY for ha of land
    TY <- QUEFTS1_Pedotransfer(QID, rec)    # Yield possible at recommended NPK in kg/ha dry wt.

    ## both CY and TY should be changed to user land size in ton/ha and fresh wt
    TY_user <- ((getRFY(HD = HD, RDY = TY, country = "NG")) / 1000) * areaHa

    # CY_user <- CY * areaHa * 0.003
    # TY_user <- TY * areaHa * 0.003
    # WLY_user <- QID$water_limited_yield * areaHa * 0.003

    ## reporting the recommended fertilizers
    Recomfr <- fertilizer[fertilizer$FR > 0,]
    Recomfr$FR <- round(Recomfr$FR * areaHa, digits=0)


    ## total cost per ha
    TC <- as.numeric(Recomfr$FR %*% Recomfr$price)

    ## net revenue on the users land size
    GR <- (TY_user - CY_user) * rootUP                      # Gross revenue given root up is for fresh wt ton/ha
    TC <- round_any(TC, 100)
    GR <- round_any(GR, 100)
    
    NR <- round(GR - TC, digits = 0)                                                # Net Revenue

    Recomfr_wide <- spread(Recomfr[, c('type', 'FR')], type, FR)

    d1 <- data.frame(lat = lat, lon = lon, plDate, N = NPK_user[1], P = NPK_user[2], K = NPK_user[3],
                     WLY = WLY_user, CurrentY = CY_user, TargetY = TY_user, TC = TC, NR = NR)
    d2 <- cbind(d1, Recomfr_wide)
    row.names(d2) <- NULL
    if (d2$NR <= 0 | d2$TargetY <= d2$CurrentY) {
      fertinfo <- subset(d2, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
      fertinfo$N <- fertinfo$P <- fertinfo$K <- fertinfo$TC <- fertinfo$NR <- 0
      fertinfo$TargetY <- fertinfo$CurrentY
      d2 <- fertinfo
    }

    return(d2)
  }

}


#' is a function called within run_Optim_NG2 as function for optim. 
#'
#' @param fertRate is the different NPK rate created and passed to the optim in serch for a combination that gives max prifit 
#' @param rootUP root prise
#' @param QID soil data
#' @param CY current yiueld
#' @param fertilizer fertilizer types and prices
#' @param invest investment capacity
#' @param HD harvest date
#' @param country 
optim_NR <- function(fertRate, rootUP, QID, CY, fertilizer, invest, HD, country) {
  f_price <- fertilizer$price
  TC <- sum(fertRate * f_price)

  ## Kg of Urea, Kg of NPK151515, Kg of NPK201010, Kg of MOP

  N <- as.vector(fertRate %*% fertilizer$N_cont)
  P <- as.vector(fertRate %*% fertilizer$P_cont)
  K <- as.vector(fertRate %*% fertilizer$K_cont)

  rec <- c(N, P, K)

  TotalYield <- QUEFTS1_Pedotransfer(QID, rec)

  AdditionalYield <- (getRFY(HD = HD, RDY = (TotalYield - CY), country = country)) / 1000 ## DM is converted to FW and then from KG/ha to ton/ha
  #AdditionalYield <- (TotalYield - CY)*0.003
  PriceYield <- AdditionalYield * rootUP
  NetRev <- PriceYield - TC
  if (!is.na(invest) & TC > invest) { NetRev <- NetRev - (invest - TC)^2 } #penalize NR if costs exceed investment cap
  return(NetRev)
}




#' computes target yield in tonnes/ha from a given NPK rate, it is used within profit optimization and other fucntions need to predict yield from NPK
#' @param QID a data frame containing soil NPK, WLY (kg/ha dry wt.),
#' @param rec recomended NPK rate
#' @returnType
#' @return target yield in ton/ha dry matter
#'
#' @author Meklit
#' @export
QUEFTS1_Pedotransfer <- function(QID, rec) {
  QID$WLY <- QID$water_limited_yield

  crop_param <- cbind(NUE(HI = 0.52), data.frame(rN = 0, rP = 0, rK = 0, max_yield = QID$WLY, tolerance = 0.01))    ## nutrient use efficiency of the crop


  Queft_Input_Data_Var1 <- cbind(QID, crop_param)
  indata <- Queft_Input_Data_Var1[, c("lat", "long", "WLY", "aN", "dN", "aP", "dP", "aK", "dK", "rN", "rP", "rK", "soilN", "soilP", "soilK", "max_yield", "tolerance")]

  N_rate <- rec[1]
  P_rate <- rec[2]
  K_rate <- rec[3]

  TargetYield_from_NPK <- NPK_TargetYield_forOutput(NutrUse_soilNPK = indata, N_rate, P_rate, K_rate)

  return(TargetYield_from_NPK$TargetYield)
}


NUE <- function(HI, CmaxNroots = 6.6, CminNroots = 2.5, CmaxNtops = 17.9, CminNtops = 7.9, CmaxProots = 1.5, CminProots = 0.8, CmaxPtops = 2.8, CminPtops = 0.9,
                CmaxKroots = 11, CminKroots = 2.8, CmaxKtops = 18.8, CminKtops = 3.4) {
  aN = round(1000 * HI / (HI * CmaxNroots + (1 - HI) * CmaxNtops), digits = 0)
  dN = round(1000 * HI / (HI * CminNroots + (1 - HI) * CminNtops), digits = 0)

  aP = round(1000 * HI / (HI * CmaxProots + (1 - HI) * CmaxPtops), digits = 0)
  dP = round(1000 * HI / (HI * CminProots + (1 - HI) * CminPtops), digits = 0)

  aK = round(1000 * HI / (HI * CmaxKroots + (1 - HI) * CmaxKtops), digits = 0)
  dK = round(1000 * HI / (HI * CminKroots + (1 - HI) * CminKtops), digits = 0)

  return(data.frame(aN = aN, dN = dN, aP = aP, dP = dP, aK = aK, dK = dK))

}


#' Used within QUEFTS1_Pedotransfer function
#' using the output of function "NPK_TargetYield_forinput" and a data frame per lon and lat for intended NPK input
#' this function calculates the yield that can be obtained for intended NPK rate.
#' @param NutrUse_soilNPK
#' @param NPKdata: needs to be provided
#' @return
#'
#' @author Meklit
#' @export
NPK_TargetYield_forOutput <- function(NutrUse_soilNPK, N_rate, P_rate, K_rate) {
  NutrUse_soilNPK$N_rate <- N_rate
  NutrUse_soilNPK$P_rate <- P_rate
  NutrUse_soilNPK$K_rate <- K_rate

  ## Supply of nutrients to the crop
  NutrUse_soilNPK$SN <- NutrUse_soilNPK$N_rate + NutrUse_soilNPK$soilN
  NutrUse_soilNPK$SP <- NutrUse_soilNPK$P_rate + NutrUse_soilNPK$soilP
  NutrUse_soilNPK$SK <- NutrUse_soilNPK$K_rate + NutrUse_soilNPK$soilK

  ## Actual Uptake of nutrients: crop param + nutrient supply
  tmp <- ddply(NutrUse_soilNPK, .(lat, long), actual_uptake_tool)
  NutrUse_soilNPK <- merge(NutrUse_soilNPK, tmp, by = c("lat", "long"))

  ## max and min yield: actual uptake and crop param. min of N uptake constrianed by availability of P, K and water
  maxminY <- ddply(NutrUse_soilNPK, .(lat, long), max_min_yields_tools)
  NutrUse_soilNPK <- merge(NutrUse_soilNPK, maxminY, by = c("lat", "long"))

  ## final yield: min yield for combined uptake of 2 nutrients assuming the 3rd is not limiting, should be < WLY, and take meanof the six combinations
  Target_Yield <- ddply(NutrUse_soilNPK, .(lat, long), quefts_tools)
  TY <- data.frame(lat = Target_Yield$lat, lon = Target_Yield$long, TargetYield = Target_Yield$FinalYield)

  return(TY)
}

## part of QUEFTS model
actual_uptake_tool <- function(ds_supply) {
  with(ds_supply,
  {
    UNP <- nutrient_uptake(S1 = SN, S2 = SP, d1 = dN, a1 = aN, d2 = dP, a2 = aP, r1 = rN, r2 = rP)
    UNK <- nutrient_uptake(S1 = SN, S2 = SK, d1 = dN, a1 = aN, d2 = dK, a2 = aK, r1 = rN, r2 = rK)
    UNW <- water_dependent_nutrient_uptake(S1 = SN, WLY = WLY, d1 = dN, a1 = aN, r1 = rN)
    UN <- min(UNP, UNK, UNW)


    UPN <- nutrient_uptake(S1 = SP, S2 = SN, d1 = dP, a1 = aP, d2 = dN, a2 = aN, r1 = rP, r2 = rN)
    UPK <- nutrient_uptake(S1 = SP, S2 = SK, d1 = dP, a1 = aP, d2 = dK, a2 = aK, r1 = rP, r2 = rK)
    UPW <- water_dependent_nutrient_uptake(S1 = SP, WLY = WLY, d1 = dP, a1 = aP, r1 = rP)
    UP <- min(UPN, UPK, UPW)


    UKN <- nutrient_uptake(S1 = SK, S2 = SN, d1 = dK, a1 = aK, d2 = dN, a2 = aN, r1 = rK, r2 = rN)
    UKP <- nutrient_uptake(S1 = SK, S2 = SP, d1 = dK, a1 = aK, d2 = dP, a2 = aP, r1 = rK, r2 = rP)
    UKW <- water_dependent_nutrient_uptake(S1 = SK, WLY = WLY, d1 = dK, a1 = aK, r1 = rK)
    UK <- min(UKN, UKP, UKW)


    return(data.frame(UN = UN, UP = UP, UK = UK))
  })
}


#' Nutrient uptake depends on the soil supply of the nutrient and the supply of other nutrients
nutrient_uptake <- function(S1 = NA, S2 = NA, d1 = NA, a1 = NA, d2 = NA, a2 = NA, r1 = NA, r2 = NA) {
  # N, P and K uptakes based on QUEFTS
  if (S1 < r1 + ((S2 - r2) * a2 / d1)) {
    uptakeX_givenY = S1
  } else if (S1 > r1 + ((S2 - r2) * (2 * d2 / a1 - a2 / d1))) {
    uptakeX_givenY = r1 + (S2 - r2) * (d2 / a1)
  } else {
    uptakeX_givenY = S1 - 0.25 * (S1 - r1 - (S2 - r2) * (a2 / d1))^2 / ((S2 - r2) * (d2 / a1 - a2 / d1))
  }
  # Nutrient uptake given availability of other nutrient
  return(uptakeX_givenY)
}



## part of QUEFTS model
water_dependent_nutrient_uptake <- function(S1 = NA, WLY = NA, d1 = NA, a1 = NA, r1 = NA) {
  if (S1 < r1 + WLY / d1) {
    uptakeX_givenWater = S1
  } else if (S1 > r1 + 2 * WLY / a1 - WLY / d1) {
    uptakeX_givenWater = WLY / a1
  } else {
    uptakeX_givenWater = S1 - 0.25 * (S1 - r1 - WLY / d1)^2 / (WLY / a1 - WLY / d1)
  }
  return(uptakeX_givenWater)
}



## part of QUEFTS
max_min_yields_tools <- function(dss) {

  YNA <- max((dss$UN - dss$rN), 0) * dss$aN
  YND <- max((dss$UN - dss$rN), 0) * dss$dN
  YPA <- max((dss$UP - dss$rP), 0) * dss$aP
  YPD <- max((dss$UP - dss$rP), 0) * dss$dP
  YKA <- max((dss$UK - dss$rK), 0) * dss$aK
  YKD <- max((dss$UK - dss$rK), 0) * dss$dK


  return(data.frame(YNA = YNA, YND = YND, YPA = YPA, YPD = YPD, YKA = YKA, YKD = YKD))

}


## part of QUEFTS
quefts_tools <- function(supply_wly) {
  # Actual uptake of nutrients.
  tmp <- actual_uptake_tool(supply_wly)
  supply_wly$UN <- tmp[[1]]
  supply_wly$UP <- tmp[[2]]
  supply_wly$UK <- tmp[[3]]

  # Maximum and minimum yields, depending on maximum accumulation and dilution.
  yields <- max_min_yields_tools(supply_wly)
  supply_wly$YNA <- yields$YNA
  supply_wly$YND <- yields$YND
  supply_wly$YPA <- yields$YPA
  supply_wly$YPD <- yields$YPD
  supply_wly$YKA <- yields$YKA
  supply_wly$YKD <- yields$YKD

  # Final yield based on the combinations of nutrient uptake and minimum + maximum yields.
  supply_wly$FinalYield <- final_yield_tools(supply_wly)

  return(supply_wly)
}


#' after setting fertilizer recommendation <25 kg/ha Urea, MOP or Nafaka, target yield with the remaining recommended fertilizer is  re-estimated  and
#'  total cost, gross and net revenue are re calcuated.
#' @param rootUP cassava root price
#' @param zone
#' @param wdd has dry wt
#' @param rdd has fresh wt
#' @param fertilizer
#' @author Meklit
#' @export
Rerun_25kgKa_try <- function(rootUP, rdd, fertilizer, QID, onlyFert, country, WLY = WLY, DCY = DCY, HD = HD, areaHa = areaHa) {


  QID$water_limited_yield <- WLY
  fertilizer <- merge(fertilizer, onlyFert, by = 'type')
  TC <- (sum(fertilizer$price %*% fertilizer$rate)) * areaHa
  TC <- round_any(TC, 100)
  N <- as.vector(fertilizer$rate %*% fertilizer$N_cont)
  P <- as.vector(fertilizer$rate %*% fertilizer$P_cont)
  K <- as.vector(fertilizer$rate %*% fertilizer$K_cont)
  rec <- c(N, P, K)

  ## NPK rate for user land size
  NPK_user <- rec * areaHa

  TY <- QUEFTS1_Pedotransfer(QID, rec)                    #dry wt yield in kg/ha
  #TY_user  <- ((getRFY(HD = as.Date(HD), RDY = TY, country = country))/1000) * areaHa
  TY_user <- ((getRFY(HD = HD, RDY = TY, country = country)) / 1000) * areaHa
  CY_user <- ((getRFY(HD = HD, RDY = DCY, country = country)) / 1000) * areaHa


  rdd$CurrentY <- CY_user
  rdd$TargetY <- TY_user
  rdd$TC <- TC
  nr <- (rdd$TargetY - rdd$CurrentY) * rootUP
  nr <- round_any(nr, 100)
  rdd$NR <- nr - rdd$TC
  rdd$N <- NPK_user[1]
  rdd$P <- NPK_user[2]
  rdd$K <- NPK_user[3]

  if (rdd$TargetY <= rdd$CurrentY) {
    rdd$N <- rdd$P <- rdd$K <- rdd$TC <- rdd$NR <- 0
    rdd$TargetY <- CY_user
  }

  if (rdd$NR <= 0 | rdd$TargetY <= rdd$CurrentY) {
    fertinfo <- subset(rdd, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
    fertinfo$N <- fertinfo$P <- fertinfo$K <- fertinfo$TC <- fertinfo$NR <- 0
    fertinfo$TargetY <- fertinfo$CurrentY
    rdd <- fertinfo
  }

  return(rdd)
}


### see if profit is > (0.18 * total cost) + total cost
## if not set the recommnedation to zero
NRabove18Cost <- function(ds, riskAtt) {
  
  #minimal required net revenue increase from fertilizer needed (taking into account risk attitude of user)
  # dNRmin <- ifelse(riskAtt == 0, 2.8, ifelse(riskAtt == 1, 2, 1.2))
  dNRmin <- ifelse(riskAtt == 0, 1.8, ifelse(riskAtt == 1, 1, 0.2))
  
  
  #if(ds$NR <= (ds$TC + (ds$TC * 0.18))){
  print("compueting error happens here")
  if (ds$NR < ds$TC * dNRmin) {
    fertRecom <- subset(ds, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
    fertRecom$N <- fertRecom$P <- fertRecom$K <- fertRecom$TC <- fertRecom$NR <- 0
    fertRecom$TargetY <- fertRecom$CurrentY
    
    onlyFert <- subset(ds, select = -c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR, harvestDate))
    row.names(onlyFert) <- NULL
    for (j in 1:ncol(onlyFert)) {
      onlyFert[, j] <- 0
    }
    fertRecom <- cbind(fertRecom, onlyFert)
    ds <- fertRecom
  }
  row.names(ds) <- NULL
  return(ds)
}

#' Yield calculated based on the combined uptake of 2 nutrients, while taking into account the availability of the third nutrient.
yield_nutrients_combined <- function(U1 = NA, d1 = NA, a1 = NA, Y2A = NA, Y2D = NA, Y3D = NA, r1 = NA) {
  # Determine which nutrient limited yield is lowest.
  YxD = min(Y2D, Y3D)
  # If the uptake of one of the nutrients, and therefore the yield associated with that
  # nutrient, is zero the overall yield is also zero.
  if (U1 == 0 || YxD == 0) {
    Y12 = 0
  }else {
    Y12 = Y2A + (2 * (YxD - Y2A) * (U1 - r1 - Y2A / d1)) / (YxD / a1 - Y2A / d1) -
      (YxD - Y2A) * (U1 - r1 - Y2A / d1)^2 / (YxD / a1 - Y2A / d1)^2
  }
  # Return the calculated yield based on the uptake of nutrients 1 and 2
  return(Y12)
}


## part f QUEFTS fucntion
final_yield_tools <- function(Uptake_Yield) {
  with(Uptake_Yield,
  {
    YNP <- yield_nutrients_combined(U1 = UN, d1 = dN, a1 = aN, Y2A = YPA, Y2D = YPD, Y3D = YKD, r1 = rN)
    YNK <- yield_nutrients_combined(U1 = UN, d1 = dN, a1 = aN, Y2A = YKA, Y2D = YKD, Y3D = YPD, r1 = rN)
    YPN <- yield_nutrients_combined(U1 = UP, d1 = dP, a1 = aP, Y2A = YNA, Y2D = YND, Y3D = YKD, r1 = rP)
    YPK <- yield_nutrients_combined(U1 = UP, d1 = dP, a1 = aP, Y2A = YKA, Y2D = YKD, Y3D = YND, r1 = rP)
    YKN <- yield_nutrients_combined(U1 = UK, d1 = dK, a1 = aK, Y2A = YNA, Y2D = YND, Y3D = YPD, r1 = rK)
    YKP <- yield_nutrients_combined(U1 = UK, d1 = dK, a1 = aK, Y2A = YPA, Y2D = YPD, Y3D = YND, r1 = rK)

    # Make sure the nutrient limited yields do not exceed the maximum possible yield = WLY
    YNPc <- min(c(YNP, YND, YPD, YKD, WLY))
    YNKc <- min(c(YNK, YND, YPD, YKD, WLY))
    YPNc <- min(c(YPN, YND, YPD, YKD, WLY))
    YPKc <- min(c(YPK, YND, YPD, YKD, WLY))
    YKNc <- min(c(YKN, YND, YPD, YKD, WLY))
    YKPc <- min(c(YKP, YND, YPD, YKD, WLY))

    #Final estimate
    YEc <- mean(c(YNPc, YNKc, YPNc, YPKc, YKNc, YKPc))

    return(YEc)
  })
}



###########################################################################################################################
## sms, email and R markdown
###########################################################################################################################


#SHORT DEF:   Function to send SMS report.
#RETURNS:     Nothing. SMS report are sent.
#             TODO: build in checks to log if SMS report was successfully sent.
#DESCRIPTION: Function using Plivo service to send SMS texts to phonenumber specified.
#             Note: Plivo credentials are hardcoded! Do not share!!!
#             TODO: use scan function to read credentials from csv input file.
#INPUT:       text: Vector of body text to be sent by SMS. Elements should not exceed 1600 character limit!
#             src: source phone number, starting with country code, default 254727876796
#             dst: destination phone number, starting with country code, e.g., 234789123456

sendSMSReport <- function(SMStext, src = "254727876796", dst, userField) {
  if (is.list(res)) {
    #plivio account details
    AUTH_ID = "MANDM1MDCYNWU4NGEZZW"
    AUTH_TOKEN = "M2Q2MmQ0NjI3ZjNjOTBkYjMyNGMzNzUzODdmZTc3"
    url = "https://api.plivo.com/v1/Account/MANDM1MDCYNWU4NGEZZW/Message/"

    for (i in SMStext) {
      if (nchar(i) <= 1600) {
        POST(url,
             authenticate(AUTH_ID, AUTH_TOKEN),
             body = list(src = src, dst = dst, text = i))
      }else {
        print("Text message exceeds 1600 character limit. Message not sent")
      }
    }
  }
}

#sendSMSReport <- function(text, src="254702974480", dst){
##sendSMSReport <- function(text, src="254727876796", dst){
#  if(!library("httr")){
#    install.packages("httr")
#    library("httr")
#  }
#
#  #plivio account details
#  AUTH_ID="MANDM1MDCYNWU4NGEZZW"
#  AUTH_TOKEN="M2Q2MmQ0NjI3ZjNjOTBkYjMyNGMzNzUzODdmZTc3"
#  url="https://api.plivo.com/v1/Account/MANDM1MDCYNWU4NGEZZW/Message/"
#
#  for(i in text){
#    if(nchar(i)<=1600){
#      POST(url,
#           authenticate(AUTH_ID,AUTH_TOKEN),
#           body=list(src=src, dst=dst, text=i))
#    }else{
#      print("Text message exceeds 1600 character limit. Message not sent")
#    }
#  }
#}

#' function to send mail
sendEmailReport <- function(userEmail, FR, IC, PP, SP, FRrecom, ICrecom, country, PPrecom, SPrecom, userPhoneNr) {

print(paste("Running email generation FR=",FR,"IC=",IC,"PP=",PP,"SP=",SP,"FRrecom=", FRrecom, "ICrecom=",ICrecom))
  if (FR == TRUE &
    IC == FALSE &
    FRrecom == TRUE &
    country == "NG" ) {
    print("Generating PDF file")
    if (file.exists(paste("fertilizer_advice_", userPhoneNr, ".pdf",sep="")))
    webshot::rmdshot('FR_markdown_VFT.Rmd', file = paste0('fertilizer_advice_', userPhoneNr, ".pdf"), delay = 3)
  }
  
  if (FR == TRUE &
      IC == FALSE &
      FRrecom == TRUE &
      country == "GH" ) {
    if (file.exists(paste("fertilizer_advice_", userPhoneNr, ".pdf",sep="")))
      webshot::rmdshot('FR_markdown_VFT.Rmd', file = paste0('fertilizer_advice_', userPhoneNr, ".pdf"), delay = 3)
  }

  if (FR == TRUE &
    IC == FALSE &
    FRrecom == TRUE &
    country == "TZ") {
    #if (file.exists("fertilizer_advice_swa.pdf"))
      webshot::rmdshot('FR_markdown_swa.Rmd', file = paste0('fertilizer_advice_swa_', userPhoneNr, ".pdf"), delay = 3)
  }

  if (IC == TRUE &
    FR == FALSE &
    country == "NG" &
    ICrecom == TRUE) {
    if (file.exists("intercrop_advice_VFT.pdf"))
      webshot::rmdshot('IC_markdown_VFT.Rmd', file = paste0('intercrop_advice_', userPhoneNr, ".pdf"), delay = 3)
  }

  if (IC == TRUE &
    FR == FALSE &
    country == "TZ" &
    ICrecom == TRUE) {
    if (file.exists("CIS_VFT.pdf"))
      webshot::rmdshot('CIS_markdown_swa.Rmd', file = paste0('CIS_advice_', userPhoneNr, ".pdf"), delay = 3)
  }

  if (PP == TRUE & PPrecom == TRUE & country == "NG") {
    if (file.exists("PP_advice_VFT.pdf"))
      webshot::rmdshot('PP_markdownVFT.Rmd', file = paste0('PP_advice_', userPhoneNr, ".pdf"), delay = 3)
  }

  if (PP == TRUE & PPrecom == TRUE & country == "TZ") {
    if (file.exists("PP_advice_swa.pdf"))
      webshot::rmdshot('PP_markdown_swa.Rmd', file = paste0('PP_advice_swa_', userPhoneNr, ".pdf"), delay = 3)
  }

  if (SP == TRUE & SPrecom == TRUE & country == "NG") {
    if (file.exists("SP_advice_VFT.pdf"))
      webshot::rmdshot('SP_markdownVFT.Rmd', file = paste0('SP_advice_', userPhoneNr, ".pdf"), delay = 3)
    if (file.exists("spgg.png")) file.remove("spgg.png")
  }
  if (SP == TRUE & SPrecom == TRUE & country == "GH") {
    if (file.exists("SP_advice_VFT.pdf"))
      webshot::rmdshot('SP_markdownVFT.Rmd', file = paste0('SP_advice_', userPhoneNr, ".pdf"), delay = 3)
    if (file.exists("spgg.png")) file.remove("spgg.png")
  }

  if (SP == TRUE & SPrecom == TRUE & country == "TZ") {
    if (file.exists("SP_advice_swa.pdf"))
      webshot::rmdshot('SP_markdown_swa.Rmd', file = paste0('SP_advice_swa_', userPhoneNr, ".pdf"), delay = 3)
    if (file.exists("spgg.png")) file.remove("spgg.png")
  }
  
  listofPDFs <- NULL
  if (FR == TRUE & FRrecom == TRUE & country == "NG") { listofPDFs <- c(listofPDFs, paste("fertilizer_advice_", userPhoneNr, ".pdf", sep = "")) }
  if (FR == TRUE & FRrecom == TRUE & country == "GH") { listofPDFs <- c(listofPDFs, paste("fertilizer_advice_", userPhoneNr, ".pdf", sep = "")) }
  
  
  if (FR == TRUE & FRrecom == TRUE & country == "TZ") { listofPDFs <- c(listofPDFs, paste("fertilizer_advice_swa_", userPhoneNr, ".pdf", sep = "")) }

  if (PP == TRUE & PPrecom == TRUE & country == "NG") { listofPDFs <- c(listofPDFs, paste("PP_advice_", userPhoneNr, ".pdf", sep = "")) }

  if (PP == TRUE & PPrecom == TRUE & country == "TZ") { listofPDFs <- c(listofPDFs, paste("PP_advice_swa_", userPhoneNr, ".pdf", sep = "")) }

  if (SP == TRUE & SPrecom == TRUE & country == "NG") { listofPDFs <- c(listofPDFs, paste("SP_advice_", userPhoneNr, ".pdf", sep = "")) }
  if (SP == TRUE & SPrecom == TRUE & country == "GH") { listofPDFs <- c(listofPDFs, paste("SP_advice_", userPhoneNr, ".pdf", sep = "")) }
  
  if (SP == TRUE & SPrecom == TRUE & country == "TZ") { listofPDFs <- c(listofPDFs, paste("SP_advice_swa_", userPhoneNr, ".pdf", sep = "")) }

  if (IC == TRUE & country == "NG" & ICrecom == TRUE) { listofPDFs <- c(listofPDFs, paste("intercrop_advice_", userPhoneNr, ".pdf", sep = "")) }

  if (IC == TRUE & country == "TZ" & ICrecom == TRUE) { listofPDFs <- c(listofPDFs, paste("CIS__advice", userPhoneNr, ".pdf", sep = "")) }



  if (!is.null(listofPDFs)) {
    send.mail(from = "AKILIMO@cgiar.org",
              to = as.character(userEmail),
              subject = "AKILIMO recommendation",
              body = "Please find attached the recommendation. \n Best Regards, \n AKILIMO",
              authenticate = TRUE,
              attach.files = dput(as.character(listofPDFs)),
              smtp = list(host.name = "smtp.office365.com", port = 587,
                          user.name = "AKILIMO@cgiar.org", passwd = "Fatelord@2022#", tls = TRUE))
    if (file.exists(listofPDFs)) file.remove(listofPDFs)
  }

}



#'function to put data used in the markdown .rmd. It selects the number of bags of fertilizerm the color, the money stubs for cost, sale and profit 
#'It reads the FR_MarkdownText, SP_MarkdownText, IC_MarkdownText, BPP_MarkdownText saved output which brings together the user info and based on 
#' the recommendation the fertilizer types, amount, bag colur, cost and profit 
fertilizerAdviseTable <- function(FR, IC, country, areaUnits) {


  suppressWarnings(if (file.exists("datall1.csv")) file.remove("datall1.csv"))
  suppressWarnings(if (file.exists("datall2.csv")) file.remove("datall2.csv"))
  suppressWarnings(if (file.exists("datall3.csv")) file.remove("datall3.csv"))
  suppressWarnings(if (file.exists("datall4.csv")) file.remove("datall4.csv"))
  suppressWarnings(if (file.exists("datall5.csv")) file.remove("datall5.csv"))
  suppressWarnings(if (file.exists("datall6.csv")) file.remove("datall6.csv"))


  if (FR == TRUE & IC == FALSE) {
    acairm <- read.csv("FR_MarkDownText.csv")
  }else if (IC == TRUE & FR == FALSE) {
    if (country == "TZ") {
      acairm <- read.csv("CIS_MarkDownText.csv")
    }else if (country == "NG") {
      acairm <- read.csv("IC_MarkDownText.csv")
    }
  }else {
    return("FR and IC can not be true together")
  }


  acairm$currency <- ifelse(acairm$country == "NG", "NGN",
                            ifelse(acairm$country == "TZ", "TZS", ifelse(acairm$country == "BU", "BIF","GHS") ))

  ## Loop
  Nrfert <- length(grep("fertilizer", colnames(acairm)))
  if (Nrfert > 0) {
    for (j in 1:Nrfert) {
      colNames <- c(paste(c("fertilizer", "bags", "cost", "total_cost", "kgs", "unit", "costPerBag"), j, sep = ""))
      colNames <- c(colNames, c("currency", "field_area", "unit_field"))
      dat <- acairm[, colNames]
      dat$bag <- dat[, paste("bags", j, sep = "")]
      
#TO included code for NPK1520202 to read from orange bags then set grey bags other
      fertColCode<- "green"
      if (dat[, 1] == "Urea") {
        fertColCode <- "green"
        dat[, 1] <- "Urea"
      }else if (dat[, 1] == "NPK15_15_15") {
        fertColCode <- "blue"
        dat[, 1] <- "NPK15:15:15"
      }else if (dat[, 1] == "NPK20_10_10") {
        fertColCode <- "yellow"
        dat[, 1] <- "NPK20:10:10"
      }else if (dat[, 1] == "NPK17_17_17") {
        fertColCode <- "purple"
        dat[, 1] <- "NPK17:17:17"
      }else if (dat[, 1] == "NPK20_12_16") {
        fertColCode <- "royal"
        dat[, 1] <- "NPK20:12:16+2Mg"
      }else if (dat[, 1] == "NPK152020") {
        fertColCode <- "orange"
        dat[, 1] <- "NPK15:20:20"
      }else if (dat[, 1] == "FOMI_TOTAHAZA") {
        fertColCode <- "red"
        dat[, 1] <- "FOMI-TOTAHAZA"
      }else if (dat[, 1] == "FOMI_IMBURA") {
        fertColCode <- "redMG"
        dat[, 1] <- "FOMI-IMBURA"
      }else if (dat[, 1] == "FOMI_BAGARA") {
        fertColCode <- "grey"
        dat[, 1] <- "FOMI-BAGARA"
      }


      if (dat$bag == 0.5) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/half.png)', sep = ""))
      }else if (dat$bag == 1) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/1.png)', sep = ""))
      }else if (dat$bag == 1.5) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/1_5.png)', sep = ""))
      }else if (dat$bag == 2) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/2.png)', sep = ""))
      }else if (dat$bag == 2.5) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/2_5.png)', sep = ""))
      }else if (dat$bag == 3) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/3.png)', sep = ""))
      }else if (dat$bag == 3.5) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/3_5.png)', sep = ""))
      }else if (dat$bag == 4) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/4.png)', sep = ""))
      }else if (dat$bag == 4.5) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/4_5.png)', sep = ""))
      }else if (dat$bag == 5) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/5.png)', sep = ""))
      }else if (dat$bag == 5.5) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/5_5.png)', sep = ""))
      }else if (dat$bag == 6) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/6.png)', sep = ""))
      }else if (dat$bag == 6.5) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/6_5.png)', sep = ""))
      }else if (dat$bag == 7) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/7.png)', sep = ""))
      }else if (dat$bag == 7.5) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/7_5.png)', sep = ""))
      }else if (dat$bag == 8) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/8.png)', sep = ""))
      }else if (dat$bag == 8.5) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/8_5.png)', sep = ""))
      }else if (dat$bag == 9) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/9.png)', sep = ""))
      }else if (dat$bag == 9.5) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/9_5.png)', sep = ""))
      }else if (dat$bag == 10) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/10.png)', sep = ""))
      }
      # colnames(dat) <-  gsub(j,"", colnames(dat))
      #datall <- rbind(datall, dat)

      # if(country == "TZ"){
      #   dat$unit1 <- "TZ50kg bag"
      # }
      #

      # if(country == "TZ" & areaUnits == "ha"){
      #   dat$unit_field <- "TZha"
      # }else if(country == "TZ" & areaUnits == "acre"){
      #   dat$unit_field <- "TZacre"
      # }

      fn <- paste("datall", j, ".csv", sep = "")
      write.csv(dat, fn, row.names = FALSE)

    }

  }

  if (min(acairm$sum_total, acairm$revenue) == acairm$sum_total) {
    ratioFertCost <- 1
    ratioTotalSale <- round(acairm$totalSalePrice / acairm$sum_total, digits = 0)
    ratioRevenue <- round(acairm$revenue / acairm$sum_total, digits = 0)
  }else {
    ratioRevenue <- 1
    ratioFertCost <- round(acairm$sum_total / acairm$revenue, digits = 0)
    ratioTotalSale <- round(acairm$totalSalePrice / acairm$revenue, digits = 0)

  }

  acairm$revenue <- formatC(acairm$revenue, format = "f", big.mark = ",", digits = 0)
  acairm$totalSalePrice <- formatC(acairm$totalSalePrice, format = "f", big.mark = ",", digits = 0)
  acairm$sum_total <- formatC(acairm$sum_total, format = "f", big.mark = ",", digits = 0)
  # acairm$sum_total <- formatC(signif(acairm$sum_total, digits=3), format="f", big.mark=",", digits=0)


  totalCostmoney <- data.frame(title = paste(acairm$sum_total, acairm$currency, sep = " "))
  totalSalemoney <- data.frame(title = paste(acairm$totalSalePrice, acairm$currency, sep = " "))
  totalRevenuemoney <- data.frame(title = paste(acairm$revenue, acairm$currency, sep = " "))

  if (ratioFertCost == 1) {
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture1.png)')
  } else if (ratioFertCost == 2) {
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture2.png)')
  }else if (ratioFertCost == 3) {
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture3.png)')
  }else if (ratioFertCost == 4) {
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture4.png)')
  }else if (ratioFertCost == 5) {
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture5.png)')
  }else if (ratioFertCost == 6) {
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture6.png)')
  }else if (ratioFertCost == 7) {
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture7.png)')
  }else if (ratioFertCost == 8) {
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture8.png)')
  }else if (ratioFertCost == 9) {
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture9.png)')
  }else if (ratioFertCost == 10) {
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture10.png)')
  }
  write.csv(totalCostmoney, "totalCostmoney.csv", row.names = FALSE)

  if (ratioTotalSale == 1) {
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture1.png)')
  } else if (ratioTotalSale == 2) {
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture2.png)')
  }else if (ratioTotalSale == 3) {
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture3.png)')
  }else if (ratioTotalSale == 4) {
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture4.png)')
  }else if (ratioTotalSale == 5) {
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture5.png)')
  }else if (ratioTotalSale == 6) {
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture6.png)')
  }else if (ratioTotalSale == 7) {
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture7.png)')
  }else if (ratioTotalSale == 8) {
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture8.png)')
  }else if (ratioTotalSale == 9) {
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture9.png)')
  }else if (ratioTotalSale == 10) {
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture10.png)')
  }
  write.csv(totalSalemoney, "totalSalemoney.csv", row.names = FALSE)


  if (ratioRevenue == 1) {
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture1.png)')
  } else if (ratioRevenue == 2) {
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture2.png)')
  }else if (ratioRevenue == 3) {
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture3.png)')
  }else if (ratioRevenue == 4) {
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture4.png)')
  }else if (ratioRevenue == 5) {
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture5.png)')
  }else if (ratioRevenue == 6) {
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture6.png)')
  }else if (ratioRevenue == 7) {
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture7.png)')
  }else if (ratioRevenue == 8) {
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture8.png)')
  }else if (ratioRevenue == 9) {
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture9.png)')
  }else if (ratioRevenue == 10) {
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture10.png)')
  }
  write.csv(totalRevenuemoney, "totalRevenuemoney.csv", row.names = FALSE)


  #return(acairm)
}




#'  @param fertilizers: data frame with type, N_cont, P_cont, K_cont, price. Price is per kg of fertilizer
#'  @param NG_CY_Fertdata: data frame with lat,long,fert_N,fert_P,fert_K, water_limited_yield, CurrentYield,location, pl_Date,zone, harvestDay, harvestmonth, daysOnField
#'  @param SoilData: data frame with lat,long,soilN,soilP,soilK,rec_N, rec_P, rec_K, rel_N,rel_P,rel_K
#'  @param rootUP: a price of 1 tonne of cassava in freshwt. It is used as freshwt price, after QUEFTS give drywt root yield (kg/ha) it is converted to freshwt in tonne/ha and this price is then used
#'  @param areaHa is area of land in ha
#'  @return a data frame with ...
getFRrecomMarkdown <- function(lat, lon, PD, HD, maxInv, fertilizers, GPS_fertRecom,
                               area, areaUnits, userName, userPhoneCC = NA, userPhoneNr = NA, cassPD, userField, userEmail, cassUP) {

  is.even <- function(x) x %% 2 == 0
  rootConv <- data.frame(cassPD = c("roots", "chips", "flour", "gari"),
                         conversion = c(1, 3, 3.2, 3.5))

  GPS_fertRecom2 <- suppressWarnings(data.frame(lapply(GPS_fertRecom, function(x) as.numeric(as.character(x)))))
  fertilizer <- fertilizers[fertilizers$type %in% colnames(GPS_fertRecom),]
  onlyFert <- subset(GPS_fertRecom, select = -c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
  onlyFert <- gather(onlyFert, type, amountKg)
  fertilizer_amount <- merge(fertilizer, onlyFert, by = "type")
  fertilizer_amount$cost <- fertilizer_amount$price * 50
  fertilizer_amount$Nrbags25 <- signif(fertilizer_amount$amountKg / 25, digits = 0) ##
  fertilizer_amount$bags50Kg <- fertilizer_amount$Nrbags25 / 2
  fertilizer_amount$total_cost <- fertilizer_amount$price *
    fertilizer_amount$bags50Kg *
    50

  fertilizer_amount <- fertilizer_amount[, c('type', 'cost', 'amountKg', 'bags50Kg', 'total_cost')]
  FA <- NULL
  for (k in 1:nrow(fertilizer_amount)) {
    fat <- fertilizer_amount[k,]
    colnames(fat) <- paste(c('fertilizer', 'cost', 'kgs', 'bags', 'total_cost'), k, sep = "")
    if (k == 1) {
      FA <- fat
    }else {
      FA <- cbind(FA, fat)
    }
  }

  phone <- paste(userPhoneCC, userPhoneNr, sep = "")
  field_area <- paste(area, areaUnits, sep = " ")
  sum_total <- round(sum(fertilizer_amount$total_cost), digits = 0)
  unitcassava <- paste(cassUW, "kg bag of ", cassPD, sep = "")
  bags_totalroot <- round(GPS_fertRecom2$TargetY - GPS_fertRecom2$CurrentY, digits = 0) ## root yield increase in tonnes
  product <- cassPD
  if (cassPD != "roots") {
    bags_totalproduct <- round(bags_totalroot / rootConv[rootConv$cassPD == cassPD,]$conversion, digits = 0)
  }else {
    bags_totalproduct <- bags_totalroot
  }
  totalSalePrice <- round(GPS_fertRecom2$NR + GPS_fertRecom$TC, digits = 0)
  revenue <- round(GPS_fertRecom2$NR, digits = 0)
  current_yield <- paste(round(GPS_fertRecom2$CurrentY, digits = 0), " tonnes per ", field_area, sep = "")


  T_csv <- data.frame(name = userName, phone = phone, field = userField, field_area = field_area, unit_field = areaUnits,
                      plant_date = PD, hvst_date = HD, product,
                      current_yield,
                      email = userEmail, latitude = lat, longitude = lon,
                      costcassava = cassUP, unitcassava = unitcassava, maxinvest = maxInv,
                      sum_total, bags_total = bags_totalroot, unit = "Kg", totalSalePrice = totalSalePrice, revenue)

  T_csv <- cbind(T_csv, FA)

  fnc <- "MarkDownTextD.csv"
  if (file.exists(fnc)) file.remove(fnc)
  write.csv(T_csv, "MarkDownTextD.csv", row.names = FALSE)


  fText <- c()
  for (k in 1:nrow(fertilizer_amount)) {
    fc <- fertilizer_amount[k,]
    text1 <- paste(round(fc$amountKg, digits = 0), ' kg of ', fc$type, sep = "")
    fText <- c(fText, text1)
  }

  recText <- paste("Hello ", T_csv$name, "! If you plant cassava on ", PD, " and harvest on ", HD, ", we recommend you apply ", paste(fText, collapse = ", "),
                   " on your field of ", T_csv$field_area, ". This investment will cost you ", sum_total, " ", T_csv$currency, ". ",
                   "We expect you will increase your cassava", product, "production by ", bags_totalproduct, " and your net returns by ",
                   revenue, " ", T_csv$currency, ". Thank you for using our services!", sep = "")
  return(recText)
}





#' RF model works only if the factr levels are exactly identical to the data used to develop the model,
#' it reads the NOT data and add the FCY to train a random forest model and then use the model to predict INS for the user GPS
Rfmodel_Wrapper <- function(FCY, country, lat, lon) {
  require(randomForest)
  require(caret)


  GIS_soilINS_modData2 <- read.csv("NOT_GIS_CON_2020.csv")
  GIS_soilINS_modData2$Clay_5 <- as.numeric(GIS_soilINS_modData2$Clay_5)
  GIS_soilINS_modData2$Clay_15 <- as.numeric(GIS_soilINS_modData2$Clay_15)
  GIS_soilINS_modData2$Clay_30 <- as.numeric(GIS_soilINS_modData2$Clay_30)
  GIS_soilINS_modData2$silt_5 <- as.numeric(GIS_soilINS_modData2$silt_5)
  GIS_soilINS_modData2$silt_15 <- as.numeric(GIS_soilINS_modData2$silt_15)
  GIS_soilINS_modData2$silt_30 <- as.numeric(GIS_soilINS_modData2$silt_30)
  GIS_soilINS_modData2$BD_5 <- as.numeric(GIS_soilINS_modData2$BD_5)
  GIS_soilINS_modData2$BD_15 <- as.numeric(GIS_soilINS_modData2$BD_15)
  GIS_soilINS_modData2$BD_30 <- as.numeric(GIS_soilINS_modData2$BD_30)
  GIS_soilINS_modData2$CEC_5 <- as.numeric(GIS_soilINS_modData2$CEC_5)
  GIS_soilINS_modData2$CEC_15 <- as.numeric(GIS_soilINS_modData2$CEC_15)
  GIS_soilINS_modData2$CEC_30 <- as.numeric(GIS_soilINS_modData2$CEC_30)
  GIS_soilINS_modData2$TotalN <- as.numeric(GIS_soilINS_modData2$TotalN)
  GIS_soilINS_modData2$Mn <- as.numeric(GIS_soilINS_modData2$Mn)
  GIS_soilINS_modData2$B <- as.numeric(GIS_soilINS_modData2$B)
  GIS_soilINS_modData2$Ca <- as.numeric(GIS_soilINS_modData2$Ca)
  GIS_soilINS_modData2$Fe <- as.numeric(GIS_soilINS_modData2$Fe)
  GIS_soilINS_modData2$Cu <- as.numeric(GIS_soilINS_modData2$Cu)
  GIS_soilINS_modData2$Al <- as.numeric(GIS_soilINS_modData2$Al)
  GIS_soilINS_modData2$Mg <- as.numeric(GIS_soilINS_modData2$Mg)
  GIS_soilINS_modData2$Na <- as.numeric(GIS_soilINS_modData2$Na)
  GIS_soilINS_modData2$ncluster <- as.factor(GIS_soilINS_modData2$ncluster)
  GIS_soilINS_modData2$CON <- as.numeric(GIS_soilINS_modData2$CON)


  GIS_soilINS_modData2$CONclass <- ifelse(GIS_soilINS_modData2$CON < 7.5, "class1",
                                          ifelse(GIS_soilINS_modData2$CON >= 7.5 & GIS_soilINS_modData2$CON < 15, "class2",
                                                 ifelse(GIS_soilINS_modData2$CON >= 15 & GIS_soilINS_modData2$CON < 22.5, "class3",
                                                        ifelse(GIS_soilINS_modData2$CON >= 22.5 & GIS_soilINS_modData2$CON < 30, "class4", "class5"))))

  GIS_soilINS_modData2$CONclass <- as.factor(GIS_soilINS_modData2$CONclass)

  ISRIC_SoilData <- readRDS("ISRIC_SoilData_2020.RDS")
  ISRIC_SoilData <- unique(ISRIC_SoilData[ISRIC_SoilData$lat == lat & ISRIC_SoilData$long == lon,])

  ISRIC_SoilData$Clay_5 <- as.numeric(ISRIC_SoilData$Clay_5)
  ISRIC_SoilData$Clay_15 <- as.numeric(ISRIC_SoilData$Clay_15)
  ISRIC_SoilData$Clay_30 <- as.numeric(ISRIC_SoilData$Clay_30)
  ISRIC_SoilData$silt_5 <- as.numeric(ISRIC_SoilData$silt_5)
  ISRIC_SoilData$silt_15 <- as.numeric(ISRIC_SoilData$silt_15)
  ISRIC_SoilData$silt_30 <- as.numeric(ISRIC_SoilData$silt_30)
  ISRIC_SoilData$BD_5 <- as.numeric(ISRIC_SoilData$BD_5)
  ISRIC_SoilData$BD_15 <- as.numeric(ISRIC_SoilData$BD_15)
  ISRIC_SoilData$BD_30 <- as.numeric(ISRIC_SoilData$BD_30)
  ISRIC_SoilData$CEC_5 <- as.numeric(ISRIC_SoilData$CEC_5)
  ISRIC_SoilData$CEC_15 <- as.numeric(ISRIC_SoilData$CEC_15)
  ISRIC_SoilData$CEC_30 <- as.numeric(ISRIC_SoilData$CEC_30)
  ISRIC_SoilData$TotalN <- as.numeric(ISRIC_SoilData$TotalN)
  ISRIC_SoilData$Mn <- as.numeric(ISRIC_SoilData$Mn)
  ISRIC_SoilData$B <- as.numeric(ISRIC_SoilData$B)
  ISRIC_SoilData$Ca <- as.numeric(ISRIC_SoilData$Ca)
  ISRIC_SoilData$Fe <- as.numeric(ISRIC_SoilData$Fe)
  ISRIC_SoilData$Cu <- as.numeric(ISRIC_SoilData$Cu)
  ISRIC_SoilData$Al <- as.numeric(ISRIC_SoilData$Al)
  ISRIC_SoilData$Mg <- as.numeric(ISRIC_SoilData$Mg)
  ISRIC_SoilData$Na <- as.numeric(ISRIC_SoilData$Na)
  ISRIC_SoilData$ncluster <- as.factor(ISRIC_SoilData$ncluster)
  ISRIC_SoilData$CON <- FCY ## this value is only to standardaize the for the RF, other wise it gets teh value form the user input


  ISRIC_SoilData$CONclass <- ifelse(ISRIC_SoilData$CON < 7.5, "class1",
                                    ifelse(ISRIC_SoilData$CON >= 7.5 & ISRIC_SoilData$CON < 15, "class2",
                                           ifelse(ISRIC_SoilData$CON >= 15 & ISRIC_SoilData$CON < 22.5, "class3",
                                                  ifelse(ISRIC_SoilData$CON >= 22.5 & ISRIC_SoilData$CON < 30, "class4", "class5"))))


  ISRIC_SoilData$CONclass <- as.factor(ISRIC_SoilData$CONclass)

  ISRIC_SoilData$soilN <- 0
  ISRIC_SoilData$soilP <- 0
  ISRIC_SoilData$soilK <- 0

  trianData <- droplevels(ISRIC_SoilData[, colnames(GIS_soilINS_modData2)])

  trianData$use <- "Valid"
  GIS_soilINS_modData2$use <- "train"
  factoring <- rbind(GIS_soilINS_modData2, trianData)

  GIS_soilINS_modData2 <- factoring[factoring$use == "train",]
  GIS_soilINS_modData2 <- subset(GIS_soilINS_modData2, select = -c(use))

  ISRIC_SoilData <- factoring[factoring$use == "Valid",]
  ISRIC_SoilData <- subset(ISRIC_SoilData, select = -c(use))

  ### Data partioning
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
  require(caret)
  custom <- trainControl(method = "oob", number = 10)
  ##########################################################################
  ## Random Forest soilN:
  ##########################################################################
  set.seed(444)
  RF_N1 <- randomForest(log(soilN) ~ ., subset(Ndata_Train, select = -c(CON)), importance = TRUE, ntree = 1000)

  ##########################################################################
  ## Random Forest "soilP"
  ##########################################################################
  set.seed(773)
  RF_P1 <- randomForest(log(soilP) ~ ., subset(Pdata_Train, select = -c(CON)), importance = TRUE, ntree = 1000)

  ##########################################################################
  ## Random Forest soilK" R sq. 0.60 if control is used, 0.29 otherwise
  ##########################################################################
  set.seed(773)
  RF_K1 <- randomForest(log(soilK) ~ ., subset(Kdata_Train, select = -c(CON)), importance = TRUE, ntree = 1000)

  ##########################################################################
  ## use the random forest model and get the soil NPK estimates for the whole area
  ##########################################################################
  ISRIC_SoilData$soilN <- 0
  ISRIC_SoilData$soilP <- 0
  ISRIC_SoilData$soilK <- 0


  ISRIC_SoilData <- ISRIC_SoilData[, c("soilN", "soilP", "soilK", "exchK", "olsenP", "Clay_5", "Clay_15", "Clay_30", "percentSOM_5", "percentSOM_15", "percentSOM_30",
                                       "pH_5", "pH_15", "pH_30", "silt_5", "silt_15", "silt_30", "BD_5", "BD_15", "BD_30", "CEC_5", "CEC_15", "CEC_30", "percentSOC_5",
                                       "percentSOC_15", "percentSOC_30", "FC_5", "FC_15", "FC_30", "wp_5", "wp_15", "wp_30", "sws_5", "sws_15", "sws_30",
                                       "TotalN", "Mn", "B", "Ca", "Fe", "Cu", "Al", "Mg", "Na", "ncluster", "country", "CON", "CONclass")]
  ISRIC_SoilData <- subset(ISRIC_SoilData, select = -(CON))

  ISRIC_SoilData$country <- as.factor(ISRIC_SoilData$country)
  ISRIC_SoilData$ncluster <- as.factor(ISRIC_SoilData$ncluster)

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
  ISRIC_SoilData <- ISRIC_SoilData[, c("location", "lat", "long", "soilN", "soilP", "soilK", "Zone", "rec_N", "rec_P", "rec_K", "rel_N", "rel_P", "rel_K")]
  return(ISRIC_SoilData)

}




#' The soil NPK as obtained from random forest model
#' @param SoilData soil data with INS
#' @param wlyd:  water limited yield
#' @return modelled current yield for a location
#'
#' @author Meklit
#' @export
QUEFTS_WLY_CY <- function(SoilData = SoilData, country = country, wlyd = wlyd) {
  #wly_plDate <- wly_data[wly_data$plantingDate == pl_Date, c("lat", "long", "wly_KgHa")]
  wlyd$long <- wlyd$lon
  wly_plDate <- wlyd[, c("lat", "long", "water_limited_yield")]

  # colnames(wly_plDate) <- c("lat", "long", "water_limited_yield")
  Quefts_Input_Data_wly <- merge(SoilData, wly_plDate, by = c("lat", "long"))

  ## HI: Median for Nigeria=0.55 and Tanzania=0.52. Q3, Nigeria=0.63 and Tanzania=0.61
  if (country == "NG"|country == "GH") {
    crop_param <- cbind(NUE(HI = 0.55), data.frame(rN = 0, rP = 0, rK = 0, max_yield = Quefts_Input_Data_wly$water_limited_yield, tolerance = 0.01))
  }else {
    crop_param <- cbind(NUE(HI = 0.55), data.frame(rN = 0, rP = 0, rK = 0, max_yield = Quefts_Input_Data_wly$water_limited_yield, tolerance = 0.01))
  }

  ## 1. get soil nutrient supply
  Queft_Input_Data_Var <- cbind(Quefts_Input_Data_wly, crop_param)
  supply <- getsupply(Queft_Input_Data_Var) ## to get yield at zero input level


  ## 2. Current yield:
  actualUptake <- merge(supply, ddply(supply, .(lat, long), actual_uptake_tool), by = c("lat", "long"))
  minmax_Yield <- merge(actualUptake, ddply(actualUptake, .(lat, long), max_min_yields_tools), by = c("lat", "long"))
  Current_Yield <- ddply(minmax_Yield, .(lat, long), final_yield_tools) ## yield at zero input
  colnames(Current_Yield) <- c("lat", "long", "CurrentYield")
  Yield_Fertilizer <- merge(wly_plDate, Current_Yield, by = c("lat", "long"))
  Yield_Fertilizer$CurrentYield <- ifelse(Yield_Fertilizer$CurrentYield > Yield_Fertilizer$water_limited_yield,
                                          as.character(as.numeric(Yield_Fertilizer$water_limited_yield)), as.numeric(Yield_Fertilizer$CurrentYield))
  return(Yield_Fertilizer$CurrentYield)
}



#######################################################################
## FR & SP
#######################################################################
#'  @param fertilizers: data frame with type, N_cont, P_cont, K_cont, price. Price is per kg of fertilizer
#'  @param lat: decimal degrees
#'  @param lon: decimal degrees
#'  @param pd: planting day in the form of the ith day of the year
#'  @param pw: planting week of the year
#'  @param HD: Character, Harvest data (date format)
#'  @param had: number of days the crop was on the field between planting and harvest
#'  @param maxInv: how much the user is willing to invest on his total land
#'  @param fertilizers: a data frame with fertilizer types and prices
#'  @param rootUP: a price of 1 tonne of cassava in freshwt. It is used as freshwt price,
#'  @param areaHa is area of land in ha
#'  @param country should be NG or TZ
#'  @param FCY  based on user input five values based on user input are passed, the app converts the value per ha so it is always per ha that comes
#'  @param riskAtt c(0, 1, 2): Risk attitude of the farmer
#'  @FCY farmers current yield, used as control yield in the random forest model
#'  @return a data frame with lat,lon, plDate,N, P, K, WLY, CurrentY, TargetY, TC, NR, harvestDate and rates of fertilizer (if any)
#'  @example getFRrecommendations(lat = 4.775, lon = 8.415, PD = 254, HD=350, maxInv = 72000, fertilizers=fertilizers, rootUP = 17000, areaHa=3, country="NG", FCY=11.25)
getFRrecommendations <- function(lat, lon, pd, pw, HD, had, maxInv, fertilizers, rootUP, areaHa, country, FCY, riskAtt) {

  ########### getting CY
  latr <- as.factor(floor(lat * 10) / 10 + ifelse(lat - (floor(lat * 10) / 10) < 0.05, 0.025, 0.075))
  lonr <- as.factor(floor(lon * 10) / 10 + ifelse(lat - (floor(lon * 10) / 10) < 0.05, 0.025, 0.075))
  lat2 <- as.numeric(levels(latr))
  lon2 <- as.numeric(levels(lonr))

  latlon <- paste(lat2, lon2, sep = "_")

  ## get WLY:get PDand HD to the closest daes fr which we have WLY
  if (country == "NG") {
    WLY_365 <- readRDS("Nigeria_WLY_LINTUL_2020.RDS")
  }else if (country == "TZ") {
    WLY_365 <- readRDS("Tanzania_WLY_LINTUL_2020.RDS")
  }else if (country == "RW") {
    WLY_365 <- readRDS("Rwanda_WLY_LINTUL.RDS")
    WLY_365$pl_Date <- WLY_365$plantingDate
    WLY_365$PlweekNr <- WLY_365$weekNr
    colnames(WLY_365) <- gsub("WLY_", "", colnames(WLY_365))
  }else if(country == "GH"){
    WLY_365 <- readRDS("Ghana_WLY_LINTUL.RDS")
    WLY_365$pl_Date <- WLY_365$plantingDate
    WLY_365$PlweekNr <- WLY_365$weekNr
    colnames(WLY_365) <- gsub("WLY_", "", colnames(WLY_365))
  }else if(country == "BU"){
    WLY_365 <- readRDS("Burundi_WLY_LINTUL.RDS")
    WLY_365$pl_Date <- WLY_365$plantingDate
    WLY_365$PlweekNr <- WLY_365$weekNr
    colnames(WLY_365) <- gsub("WLY_", "", colnames(WLY_365))
    WLY_365$location <- paste(WLY_365$lat, WLY_365$long, sep="_")
  }


  pdates <- data.frame(wlyPD = unique(WLY_365$pl_Date))
  pdates$diff <- pd - pdates$wlyPD
  pdates$absdiff <- abs(pdates$diff)
  PD2 <- pdates[pdates$absdiff == min(abs(pdates$diff)), "wlyPD"]
  hdates <- data.frame(wlyHD = seq(214, 455, 7))
  hdates$diff <- abs(had - hdates$wlyHD)
  HD2 <- hdates[hdates$diff == min(abs(hdates$diff)), "wlyHD"]


  wlypd <- WLY_365[WLY_365$location == latlon & WLY_365$pl_Date == PD2,] ##WLY_15M[WLY_15M$long == lonr & WLY_15M$lat == latr, ]

  #  wlypd <- WLY_365[WLY_365$lon==lon2 & WLY_365$lat == lat2 & WLY_365$pl_Date == PD2, ]
  if (nrow(wlypd) == 0) {
    if (country == "NG" | country == "GH" | country == "BU"  ) {
      return("We do not have fertilizer recommendation for your location because your location is out of the recommendation domain AKILIMO is currently serving.")
    }else if (country == "RW") {
      return("kinyarwanda here")
      }else{
      return("Hatuna mapendekezo yoyote  kwa eneo lako kwa sababu eneo lako liko nje la eneo ambalo AKILIMO linafanya kazi kwa sasa")
    }

  }else {

    wlydata <- wlypd[, c("lat", "long", "pl_Date", "location")]
    colnames(wlydata) <- c("lat", "lon", "pl_Date", "location")
    wlydata$water_limited_yield <- wlypd[, colnames(wlypd) == HD2]
    wlydata$zone <- country
    wlydata$daysOnField <- had
    wlydata <- wlydata[, c("lat", "lon", "water_limited_yield", "location", "pl_Date", "zone", "daysOnField")]

    ## get soil NPK
    #ISRIC_SoilData_t <- getISRICData(lat=lat2, lon=lon2, country = country)
    #SoilData <- Rfmodel_Wrapper(ISRIC_SoilData=ISRIC_SoilData_t, FCY=FCY, country=country)
    if(country %in% c("NG", "TZ")){
      SoilData <- Rfmodel_Wrapper(FCY = FCY, country = country, lat = lat2, lon = lon2)
    }else if (country == "RW") {
      fcyy <- ifelse(FCY < 7.5, "FCY1",
             ifelse(FCY >= 7.5 & FCY < 15, "FCY2",
                    ifelse(FCY >= 15 & FCY < 22.5, "FCY3",
                           ifelse(FCY >= 22.5 & FCY < 30, "FCY4", "FCY5"))))
      SoilData <- readRDS(paste("RW_", fcyy, "_soilNPK.RDS", sep=""))
      SoilData$location <- paste(SoilData$lat, SoilData$lon, sep="_")
      SoilData <- SoilData[SoilData$location == wlydata$location, ]
      SoilData$Zone <- "RW"
      SoilData$rec_N <- 0.5
      SoilData$rec_P <- 0.15
      SoilData$rec_K <- 0.5
      SoilData$rel_N <- 1
      SoilData$rel_P <- SoilData$soilP / SoilData$soilN
      SoilData$rel_K <- SoilData$soilK / SoilData$soilN
      SoilData$long <- SoilData$lon
      SoilData <- SoilData[, c("location", "lat", "long", "soilN", "soilP", "soilK", "Zone", "rec_N", "rec_P", "rec_K", "rel_N", "rel_P", "rel_K")]
    }else if (country == "GH") {
      fcyy <- ifelse(FCY < 7.5, "FCY1",
                     ifelse(FCY >= 7.5 & FCY < 15, "FCY2",
                            ifelse(FCY >= 15 & FCY < 22.5, "FCY3",
                                   ifelse(FCY >= 22.5 & FCY < 30, "FCY4", "FCY5"))))
      SoilData <- readRDS(paste("GH_", fcyy, "_soilNPK.RDS", sep=""))
      SoilData$location <- paste(SoilData$lat, SoilData$lon, sep="_")
      SoilData <- SoilData[SoilData$location == wlydata$location, ]
      SoilData$Zone <- "RW"
      SoilData$rec_N <- 0.5
      SoilData$rec_P <- 0.15
      SoilData$rec_K <- 0.5
      SoilData$rel_N <- 1
      SoilData$rel_P <- SoilData$soilP / SoilData$soilN
      SoilData$rel_K <- SoilData$soilK / SoilData$soilN
      SoilData$long <- SoilData$lon
      SoilData <- SoilData[, c("location", "lat", "long", "soilN", "soilP", "soilK", "Zone", "rec_N", "rec_P", "rec_K", "rel_N", "rel_P", "rel_K")]
    }else if (country == "BU") {
      fcyy <- ifelse(FCY < 7.5, "FCY1",
                     ifelse(FCY >= 7.5 & FCY < 15, "FCY2",
                            ifelse(FCY >= 15 & FCY < 22.5, "FCY3",
                                   ifelse(FCY >= 22.5 & FCY < 30, "FCY4", "FCY5"))))
      SoilData <- readRDS(paste("BU_", fcyy, "_soilNPK.RDS", sep=""))
      SoilData$location <- paste(SoilData$lat, SoilData$lon, sep="_")
      SoilData <- SoilData[SoilData$location == wlydata$location, ]
      SoilData$Zone <- "BU"
      SoilData$rec_N <- 0.5
      SoilData$rec_P <- 0.15
      SoilData$rec_K <- 0.5
      SoilData$rel_N <- 1
      SoilData$rel_P <- SoilData$soilP / SoilData$soilN
      SoilData$rel_K <- SoilData$soilK / SoilData$soilN
      SoilData$long <- SoilData$lon
      SoilData <- SoilData[, c("location", "lat", "long", "soilN", "soilP", "soilK", "Zone", "rec_N", "rec_P", "rec_K", "rel_N", "rel_P", "rel_K")]
    }
    
    
  
  
    ## get CY
    wlydata$Current_Yield <- QUEFTS_WLY_CY(SoilData = SoilData, country = country, wlyd = wlydata)
    WLYData <- wlydata
    WLYData$weekNr <- pw
   
    #############################
    ## 1. get WLY, CY, fert recom and soil data
    WLY <- WLYData$water_limited_yield ## DM in kg/ha
    DCY <- WLYData$Current_Yield ## DM in kg/ha


    ## 2. change investment from given areaHa to 1ha
    InvestHa <- (maxInv / areaHa)


    ## 3. optimize the fertilizer recommendation for maxInv in local currency and provide expected target yield in kg
    fert_optim <- run_Optim_NG2(rootUP = rootUP, QID = SoilData, fertilizer = fertilizers, invest = InvestHa, plDate = WLYData$pl_Date,
                                WLYData = WLYData, lat = lat, lon = lon, areaHa, HD = HD, DCY = DCY, WLY = WLY, country = country)

    if (fert_optim$NR == 0) { ## no fertilizer recommendation
      fertilizer_rates <- NULL
      return(list(rec = fert_optim, fertilizer_rates = fertilizer_rates))
    }else {
      fertinfo <- subset(fert_optim, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
      onlyFert <- subset(fert_optim, select = -c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))

      ## 4. remove ferilizer application < 25 kg/ha and re run the TY and NR calculation
      RecomperHa <- onlyFert / areaHa
      RecomperHa2 <- gather(RecomperHa, type, rate)
      onlyFert2 <- droplevels(RecomperHa2[RecomperHa2$rate > 25,])

      if (nrow(onlyFert2) == 0) { ## if all fertilizer recom < 25 kg/ha all will be set to 0
        fertinfo$N <- fertinfo$P <- fertinfo$K <- fertinfo$NR <- fertinfo$TC <- 0
        fertinfo$TargetY <- fertinfo$CurrentY
        fertilizer_rates <- NULL
        return(list(rec = fertinfo, fertilizer_rates = fertilizer_rates))
      }else if (ncol(onlyFert) == nrow(onlyFert2)) { ## if all fertilizer recom are >= 25 kg/ha they will be kept and only checked for NR >= 18% of invest
        Reset_fert_Cont <- fert_optim
        GPS_fertRecom <- NRabove18Cost(ds = Reset_fert_Cont, riskAtt=riskAtt)
        rec <- subset(GPS_fertRecom, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
        frates <- subset(GPS_fertRecom, select = -c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
        frates2 <- gather(frates, type, rate)
        return(list(rec = rec, fertilizer_rates = frates2))

      }else {
        fert25 <- spread(onlyFert2, type, rate) ## when some fertilizer recom are dropped b/c < 25 kg/ha, ty and NR should be recalculated
        fert_optim2 <- cbind(fertinfo, fert25)
        fertilizer <- fertilizers[fertilizers$type %in% onlyFert2$type,]
        Reset_fert_Cont <- Rerun_25kgKa_try(rootUP = rootUP, rdd = fert_optim2, fertilizer = fertilizer, QID = SoilData, onlyFert = onlyFert2,
                                            country = country, WLY = WLY, DCY = DCY, HD = HD, areaHa = areaHa)
        if (Reset_fert_Cont$NR <= 0) { ## after rerunning after avoiding <25KG/ha fertilizers, if NR <=0
          fertilizer_rates <- NULL
          return(list(rec = Reset_fert_Cont, fertilizer_rates = fertilizer_rates))
        }else {
          GPS_fertRecom <- NRabove18Cost(ds = Reset_fert_Cont,riskAtt=riskAtt)
          rec <- subset(GPS_fertRecom, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
          frates <- subset(GPS_fertRecom, select = -c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
          frates2 <- gather(frates, type, rate)
          return(list(rec = rec, fertilizer_rates = frates2))

        }
      }
    }
  }
}



#' Title makes the text shown for FR recom
#'
#' @param ds output of getFRrecommendations
#' @param country 
#' @param fertilizers is the fertilizer data frame, but with abit of work the input in ds can work and this argumnet could be dropped
#' @param rootUP root price
#'
#' @return  the advice as text in the right language to show in the app
#' @export
#'
#' @examples
getFRrecText <- function(ds, country, fertilizers, rootUP) {
  TRNS <- read.csv("translations_TEST.csv", stringsAsFactors = FALSE)
  norecom_ng <- gsub(pattern = "\"", replacement = "", TRNS$norecom[1]); norecom_tz <- gsub(pattern = "\"", replacement = "", TRNS$norecom[2]); norecom_rw <- gsub(pattern = "\"", replacement = "", TRNS$norecom[3]);
  notapply_ng <- gsub(pattern = "\"", replacement = "", TRNS$notapply[1]); notapply_tz <- gsub(pattern = "\"", replacement = "", TRNS$notapply[2]); notapply_rw <- gsub(pattern = "\"", replacement = "", TRNS$notapply[3])
  
  
  rec <- ds$rec
  
  frate <- ds$fertilizer_rates
  
  if (is.null(rec)) {
    
    #trans
    recom <- if (country == "NG" | country == "GH" | country == "BU") {
      paste(norecom_ng)
    }else if (country == "TZ"){
      paste(norecom_tz)
    }else{
      paste(norecom_rw)
    }
    
    
  }else{
    
    if (rec$TC == 0) {
      
      #trans
      recom <- if (country == "NG" | country == "GH" | country == "BU") {
        paste(notapply_ng)
      }else if (country == "TZ"){
        paste(norecom_tz)
      }else {
        paste(notapply_rw)
      }
      


      #TODO: This does not provide details on the reasons why we do not recommend to apply fertilizer.
      #This might either be due to
      #1. unfavourable price ratios (root price over fertilizer price is too low),
      #2 low yield potential (unfavourable planting / harvest date and low WLY),
      #3. high soil fertility and low response (high FCY or high indigenous nutrient supply).


    }else {


      currency <- ifelse(country == "NG", "NGN", ifelse(country == "RW", "RWF", ifelse(country == "GH", "GHS", ifelse(country == "BU", "BIF" , "TZS") )))
  
      fertilizerTypes <- frate$type
      fertilizerRates <- round(frate$rate, digits = 0)

      bags <- round(fertilizerRates / 50, digits = 1)
      Bagsfull <- trunc(bags)
      bagshalf <- bags - floor(bags)
      bagshalf <- ifelse(bagshalf >= 0.25 & bagshalf <= 0.75, 0.5, ifelse(bagshalf < 0.25, 0, 1))
      bags <- Bagsfull + bagshalf
   

      sum_total = ds$rec$TC
      fertilizers_recom <- fertilizers[fertilizers$type %in% ds$fertilizer_rates$type,]
      fertilizers_recom <- merge(fertilizers_recom, ds$fertilizer_rates, by = 'type')
      fertilizers_recom$rate <- round(fertilizers_recom$rate, digits = 0)
      fertilizers_recom$cost <- round(fertilizers_recom$rate, digits = 0) * fertilizers_recom$price
      sum_total <- sum(fertilizers_recom$cost)
      totalSalePrice <- round(ds$rec$TC + ds$rec$NR, digits = 0)
      revenue = totalSalePrice - sum_total
      revenue <-  round_any(revenue, 100)

      fertilizers <- droplevels(fertilizers[fertilizers$type %in% frate$type,])
      TC <- formatC(round(sum_total, digits = 0), format = "f", big.mark = ",", digits = 0)

   
      NR <- formatC(revenue, format = "f", big.mark = ",", digits = 0)
      DY <- signif(rec$TargetY - rec$CurrentY, digits = 2)

      
      werec_ng <- gsub(pattern = "\"", replacement = "", TRNS$werec[1]); werec_tz <- gsub(pattern = "\"", replacement = "", TRNS$werec[2]); werec_rw <- gsub(pattern = "\"", replacement = "", TRNS$werec[3])
      kgof_ng <- gsub(pattern = "\"", replacement = "", TRNS$kgof[1]); kgof_tz <- gsub(pattern = "\"", replacement = "", TRNS$kgof[2]); kgof_rw <- gsub(pattern = "\"", replacement = "", TRNS$kgof[3]);area_ng <- gsub(pattern = "\"", replacement = "", TRNS$area[1]); area_tz <- gsub(pattern = "\"", replacement = "", TRNS$area[2]);
      area_rw <- gsub(pattern = "\"", replacement = "", TRNS$area[3])
      willc_ng <- gsub(pattern = "\"", replacement = "", TRNS$willc[1]); willc_tz <- gsub(pattern = "\"", replacement = "", TRNS$willc[2]); willc_rw <- gsub(pattern = "\"", replacement = "", TRNS$willc[3]);extrap_ng <- gsub(pattern = "\"", replacement = "", TRNS$extrap[1]); extrap_tz <- gsub(pattern = "\"", replacement = "", TRNS$extrap[2]);
      extrap_rw <- gsub(pattern = "\"", replacement = "", TRNS$extrap[3])
      tonof_ng <- gsub(pattern = "\"", replacement = "", TRNS$tonof[1]); tonof_tz <- gsub(pattern = "\"", replacement = "", TRNS$tonof[2]);tonof_rw <- gsub(pattern = "\"", replacement = "", TRNS$tonof[3]); netincr_ng <- gsub(pattern = "\"", replacement = "", TRNS$netincr[1]); netincr_tz <- gsub(pattern = "\"", replacement = "", TRNS$netincr[2]);
      netincr_rw <- gsub(pattern = "\"", replacement = "", TRNS$netincr[3])
      of_tz <- gsub(pattern = "\"", replacement = "", TRNS$of[2]); of_rw <- gsub(pattern = "\"", replacement = "", TRNS$of[3]);
      
      recom <- if (country == "NG" | country == "GH" | country == "BU") {
        paste0(werec_ng, "\n",
               paste0(fertilizerRates, kgof_ng, fertilizerTypes, collapse = "\n"), 
               area_ng, "\n",
               willc_ng, currency, " ", TC, ".\n",
               extrap_ng, DY, tonof_ng,
               netincr_ng, currency, " ", NR, ".")
      }else if (country == "TZ"){
        paste0(werec_tz, " ", "\n",
               paste0(kgof_tz, fertilizerRates, of_tz, fertilizerTypes, collapse = "\n"),
               area_tz, "\n",
               willc_tz, currency, " ", TC, ".\n",
               extrap_tz, " ", DY, tonof_tz,
               netincr_tz, " ", currency, " ", NR, ".")
        
      }else if (country == "RW"){
        paste0(werec_rw, " ", "\n",
               paste0(kgof_rw, fertilizerRates, of_rw, fertilizerTypes, collapse = "\n"),
               area_rw, "\n",
               willc_rw, currency, " ", TC, ".\n",
               extrap_rw, " ", DY, tonof_rw,
               netincr_rw, " ", currency, " ", NR, ".")
        
      }
      

      #TODO: This only provides the minimal information to return to the user. We may consider adding following information:
      #1. Split regime - how should this fertilizer application be distributed over time?
      #2. Best application method - furrow or full ring application.
      #3. Possible better alternative fertilizers...
      #4. Importance of good agronomic practices
      #5. Possible issues with the input data - very high fertilizer prices or very low root price, very low or very high FCY, very low or very high WY,...

    }
  }

  recom <- gsub("  ", " ", recom)
  recom <- gsub("  ", " ", recom)
  return(recom)

}


#' Title
#'
#' @param ds is a dataframe and out put of getSPrecommendations function 
#' @param country 
#' @param PD planting date
#' @param HD harest date
#'
#' @return the recommnendation for SP to be shown in the app
#' @export
#'
#' @examples
getSPrecText <- function(ds, country, PD, HD) {

  TRNS <- read.csv("translations_TEST.csv", stringsAsFactors = FALSE)
  norecom_ng <- gsub(pattern = "\"", replacement = "", TRNS$norecom[1]); norecom_tz <- gsub(pattern = "\"", replacement = "", TRNS$norecom[2]); norecom_rw <- gsub(pattern = "\"", replacement = "", TRNS$norecom[3]);
  notapply_ng <- gsub(pattern = "\"", replacement = "", TRNS$notapply[1]); notapply_tz <- gsub(pattern = "\"", replacement = "", TRNS$notapply[2]);notapply_rw <- gsub(pattern = "\"", replacement = "", TRNS$notapply[3])
  recrev_ng <- gsub(pattern = "\"", replacement = "", TRNS$recrev[1]); recrev_tz <- gsub(pattern = "\"", replacement = "", TRNS$recrev[2]); recrev_rw <- gsub(pattern = "\"", replacement = "", TRNS$recrev[3])
  hvsdate_ng <- gsub(pattern = "\"", replacement = "", TRNS$hvsdate[1]); hvsdate_tz <- gsub(pattern = "\"", replacement = "", TRNS$hvsdate[2]); hvsdate_rw <- gsub(pattern = "\"", replacement = "", TRNS$hvsdate[3]); 
  nochange_ng <- gsub(pattern = "\"", replacement = "", TRNS$nochange[1]); nochange_tz <- gsub(pattern = "\"", replacement = "", TRNS$nochange[2]); nochange_rw <- gsub(pattern = "\"", replacement = "", TRNS$nochange[3])
  recPln_ng <- gsub(pattern = "\"", replacement = "", TRNS$recPln[1]); recPln_tz <- gsub(pattern = "\"", replacement = "", TRNS$recPln[2]); recPln_rw <- gsub(pattern = "\"", replacement = "", TRNS$recPln[3]);
  recHvs_ng <- gsub(pattern = "\"", replacement = "", TRNS$recHvs[1]); recHvs_tz <- gsub(pattern = "\"", replacement = "", TRNS$recHvs[2]); recHvs_rw <- gsub(pattern = "\"", replacement = "", TRNS$recHvs[3])
  wks_ng <- gsub(pattern = "\"", replacement = "", TRNS$wks[1]); wks_tz <- gsub(pattern = "\"", replacement = "", TRNS$wks[2]); wks_rw <- gsub(pattern = "\"", replacement = "", TRNS$wks[3]);
  recPlnP_ng <- gsub(pattern = "\"", replacement = "", TRNS$recPlnP[1]); recPlnP_tz <- gsub(pattern = "\"", replacement = "", TRNS$recPlnP[2]);recPlnP_rw <- gsub(pattern = "\"", replacement = "", TRNS$recPlnP[3])
  early_ng <- gsub(pattern = "\"", replacement = "", TRNS$early[1]); early_tz <- gsub(pattern = "\"", replacement = "", TRNS$early[2]); early_rw <- gsub(pattern = "\"", replacement = "", TRNS$early[3]);  
  late_ng <- gsub(pattern = "\"", replacement = "", TRNS$late[1]); late_tz <- gsub(pattern = "\"", replacement = "", TRNS$late[2]); late_rw <- gsub(pattern = "\"", replacement = "", TRNS$late[3])
  recPhv_ng <- gsub(pattern = "\"", replacement = "", TRNS$recPhv[1]); recPhv_tz <- gsub(pattern = "\"", replacement = "", TRNS$recPhv[2]); recPhv_rw <- gsub(pattern = "\"", replacement = "", TRNS$recPhv[3]);
  
  
  if (is.null(ds)) {
    
    rec <- if (country == "NG" | country == "GH" ) {
      norecom_ng
    }else if (country == "TZ"){
      norecom_tz
    }else{
      norecom_rw
    }
    
  }else {
    
    if (ds[1,]$CP) {
      if (country == "NG" | country == "GH") {
        rec <- paste0(recrev_ng, " ( ", format(ds[1,]$PD, "%d %B %Y"), " ) ",
                      hvsdate_ng, " ( ", format(ds[1,]$HD, "%d %B %Y"), " ) ",
                      nochange_ng)
      }else if (country == "TZ"){
        rec <- paste0(recrev_tz, " ( ", format(ds[1,]$PD, "%d %B %Y"), " ) ",
                      hvsdate_tz, " ( ", format(ds[1,]$HD, "%d %B %Y"), " ) ",
                      nochange_tz)
      }else{
        rec <- paste0(recrev_rw, " ( ", format(ds[1,]$PD, "%d %B %Y"), " ) ",
                      hvsdate_rw, " ( ", format(ds[1,]$HD, "%d %B %Y"), " ) ",
                      nochange_rw)
      }
      

      #TODO: This does not provide details on the reasons. This might either be due to
      #1. unfavourable price conditions at other planting/harvest dates,
      #2. low starch content at later harvest dates (if selling to as starch factory)
      #3. unattractive yields (at earlier harvest dates),
      #4. combination of both.
      #We may also want to include some information on the impact on cropping practices, requirement to ridge, risks of pest and disease issues,...
      
    }else {
      
      if (ds[1,]$PD != ds[ds$CP == TRUE,]$PD) {
        #trans
        if (country == "NG" | country == "GH") {
          recP <-paste0(recPln_ng, format(ds[1,]$PD, "%d %B %Y"), ", ",
                        abs(ds[1,]$rPWnr), " ", wks_ng, " ", ifelse(ds[1,]$rPWnr < 0, early_ng, late_ng), " ", recPlnP_ng, "\n")
          
        }else if (country == "TZ"){
          recP <-paste0(recPln_tz, " ", format(ds[1,]$PD, "%d %B %Y"), ", ",
                        wks_tz, " ", abs(ds[1,]$rPWnr), " ", " ", ifelse(ds[1,]$rPWnr < 0, early_tz, late_tz), " ", recPlnP_tz, "\n")
        }else{
          recP <-paste0(recPln_rw, " ", format(ds[1,]$PD, "%d %B %Y"), ", ",
                        wks_rw, " ", abs(ds[1,]$rPWnr), " ", " ", ifelse(ds[1,]$rPWnr < 0, early_tz, late_rw), " ", recPlnP_rw, "\n")
        }
        
      }else{
        # recP <- NULL
        if (country == "NG" | country == "GH") {
          recP <-paste0("Your revenue will be highest at your proposed planting date, on ",format(ds[1,]$PD, "%d %B %Y"), ".",sep="")
        }else if (country == "TZ"){
          recP <- paste0("Mapato yako yatakuwa makubwa zaidi ukipanda tarehe, ",format(ds[1,]$PD, "%d %B %Y"), ".",sep="")
        }else{
          recP <- paste0("kinyarwanda, ",format(ds[1,]$PD, "%d %B %Y"), ".",sep="")
        }
        
      }
      
      
      if (ds[1,]$HD != ds[ds$CP == TRUE,]$HD) {
        if (country == "NG" | country == "GH") {
          recH <-  paste0(recHvs_ng, format(ds[1,]$PD, "%d %B %Y"), ", ",
                          abs(ds[1,]$rPWnr), " ", wks_ng, " ", ifelse(ds[1,]$rPWnr < 0, early_ng, late_ng), recPhv_ng, "\n")
        }else if (country == "TZ"){
          recH <- paste0(recHvs_tz, " ", format(ds[1,]$PD, "%d %B %Y"), ", ", wks_tz, " ",
                         abs(ds[1,]$rPWnr), " ", ifelse(ds[1,]$rPWnr < 0, early_tz, late_tz), recPhv_tz, "\n")
        }else{
          recH <- paste0(recHvs_rw, " ", format(ds[1,]$PD, "%d %B %Y"), ", ", wks_rw, " ",
                         abs(ds[1,]$rPWnr), " ", ifelse(ds[1,]$rPWnr < 0, early_rw, late_rw), recPhv_rw, "\n")
        }
      }else {
        recH <- NULL
        if(country == "NG" | country == "GH"){
          recH <- paste0("Your selected harvest date is optimal, harvest your cassava on ",format(ds[1,]$HD, "%d %B %Y"), ".", sep=" ")
        }else if (country == "TZ"){
          recH <- paste0("Tarehe uliyochagua ya kuvuna ni bora, vuna mihogo yako tarehe ",format(ds[1,]$HD, "%d %B %Y"), ".", sep=" ")
        }else{
          recH <- paste0("kinyarwanda ",format(ds[1,]$HD, "%d %B %Y"), ".", sep=" ")
        }
      }
      
      
      DP <- signif(ds[1,]$RP - ds[ds$CP == TRUE,]$RP, digits = 2)
      currency <- ifelse(country == "NG", "NGN", ifelse(country == "RW", "RWF", ifelse(country == "GH", "GHS", "TZS")))
      dGR <- formatC(signif(ds[1,]$dGR, digits = 3), format = "f", big.mark = ",", digits = 0)
      rechange_ng <- gsub(pattern = "\"", replacement = "", TRNS$rechange[1]); rechange_tz <- gsub(pattern = "\"", replacement = "", TRNS$rechange[2]);rechange_rw <- gsub(pattern = "\"", replacement = "", TRNS$rechange[3])
      hvst_ng <- gsub(pattern = "\"", replacement = "", TRNS$hvst[1]); hvst_tz <- gsub(pattern = "\"", replacement = "", TRNS$hvst[2]); hvst_rw <- gsub(pattern = "\"", replacement = "", TRNS$hvst[3])
      plnt_ng <- gsub(pattern = "\"", replacement = "", TRNS$plnt[1]); plnt_tz <- gsub(pattern = "\"", replacement = "", TRNS$plnt[2]); plnt_rw <- gsub(pattern = "\"", replacement = "", TRNS$plnt[3])
      recRatt1_ng <- gsub(pattern = "\"", replacement = "", TRNS$recRatt1[1]); recRatt1_tz <- gsub(pattern = "\"", replacement = "", TRNS$recRatt1[2]); recRatt1_rw <- gsub(pattern = "\"", replacement = "", TRNS$recRatt1[3])
      recRatt2_ng <- gsub(pattern = "\"", replacement = "", TRNS$recRatt2[1]); recRatt2_tz <- gsub(pattern = "\"", replacement = "", TRNS$recRatt2[2]);recRatt2_rw <- gsub(pattern = "\"", replacement = "", TRNS$recRatt2[3])
      exp_ng <- gsub(pattern = "\"", replacement = "", TRNS$exp[1]); exp_tz <- gsub(pattern = "\"", replacement = "", TRNS$exp[2]); exp_rw <- gsub(pattern = "\"", replacement = "", TRNS$exp[3])
      dec_ng <- gsub(pattern = "\"", replacement = "", TRNS$dec[1]); dec_tz <- gsub(pattern = "\"", replacement = "", TRNS$dec[2]);dec_rw <- gsub(pattern = "\"", replacement = "", TRNS$dec[3])
      inc_ng <- gsub(pattern = "\"", replacement = "", TRNS$inc[1]); inc_tz <- gsub(pattern = "\"", replacement = "", TRNS$inc[2]); inc_rw <- gsub(pattern = "\"", replacement = "", TRNS$inc[3]) 
      root_ng <- gsub(pattern = "\"", replacement = "", TRNS$root[1]); root_tz <- gsub(pattern = "\"", replacement = "", TRNS$root[2]); root_rw <- gsub(pattern = "\"", replacement = "", TRNS$root[3])
      notot_ng <- gsub(pattern = "\"", replacement = "", TRNS$notot[1]); notot_tz <- gsub(pattern = "\"", replacement = "", TRNS$notot[2]); notot_rw <- gsub(pattern = "\"", replacement = "", TRNS$notot[3])
      optim_ng <- gsub(pattern = "\"", replacement = "", TRNS$optim[1]); optim_tz <- gsub(pattern = "\"", replacement = "", TRNS$optim[2]); optim_rw <- gsub(pattern = "\"", replacement = "", TRNS$optim[3])
      ton_ng <- gsub(pattern = "\"", replacement = "", TRNS$ton[1]); ton_tz <- gsub(pattern = "\"", replacement = "", TRNS$ton[2]); ton_rw <- gsub(pattern = "\"", replacement = "", TRNS$ton[3])
      but_ng <- gsub(pattern = "\"", replacement = "", TRNS$but[1]); but_tz <- gsub(pattern = "\"", replacement = "", TRNS$but[2]); but_rw <- gsub(pattern = "\"", replacement = "", TRNS$but[3])
      and_ng <- gsub(pattern = "\"", replacement = "", TRNS$and[1]); and_tz <- gsub(pattern = "\"", replacement = "", TRNS$and[2]); and_rw <- gsub(pattern = "\"", replacement = "", TRNS$and[3])
      valinc_ng <- gsub(pattern = "\"", replacement = "", TRNS$valinc[1]); valinc_tz <- gsub(pattern = "\"", replacement = "", TRNS$valinc[2]);valinc_rw <- gsub(pattern = "\"", replacement = "", TRNS$valinc[3])
      
      
      if (DP == 0) {
        
        if (dGR == 0) {
          if (country == "NG" | country == "GH") {
            recR <-  paste0(rechange_ng,
                            ifelse(!is.null(recH), paste(hvst_ng), paste(plnt_ng)))
          }else if (country == "TZ"){
            recR <- paste0(rechange_tz,
                           ifelse(!is.null(recH), paste(hvst_tz), paste(plnt_tz)))
          }else{
            recR <- paste0(rechange_rw,
                           ifelse(!is.null(recH), paste(hvst_rw), paste(plnt_rw)))
          }
          
        } else {
          if (country == "NG" | country == "GH") {
            recR <- paste0(recRatt1_ng, currency, " ", dGR, " ", recRatt2_ng)
          }else if (country == "TZ"){
            recR <- paste0(recRatt1_tz, currency, " ", dGR, " ", recRatt2_tz)
          }else{
            recR <- paste0(recRatt1_rw, currency, " ", dGR, " ", recRatt2_rw)
          }
        }
      } else {
        if (dGR == 0) {
          if (country == "NG" | country == "GH") {
            recR <- paste0(exp_ng,
                           ifelse(DP < 0, paste(dec_ng), paste(inc_ng)), root_ng, abs(DP), " ", ton_ng,
                           notot_ng,
                           ifelse(!is.null(recH), paste(hvst_ng), paste(plnt_ng)))
          }else if (country == "TZ"){
            recR <- paste0(exp_tz,
                           ifelse(DP < 0, paste(dec_tz), paste(inc_tz)), root_tz, " ", abs(DP), " ", ton_tz,
                           notot_tz,
                           ifelse(!is.null(recH), paste(hvst_tz), paste(plnt_tz)))
            
          }else{
            recR <- paste0(exp_rw,
                           ifelse(DP < 0, paste(dec_rw), paste(inc_rw)), root_rw, " ", abs(DP), " ", ton_rw,
                           notot_rw,
                           ifelse(!is.null(recH), paste(hvst_rw), paste(plnt_rw)))
          }
        }else {
          
          if (country == "NG" | country == "GH") {
            recR <-  paste0(exp_ng,
                            ifelse(DP < 0, paste(dec_ng), paste(inc_ng)), root_ng, abs(DP), " ", ton_ng,
                            ifelse(DP < 0, paste(but_ng), paste(and_ng)),
                            valinc_ng, currency, " ", dGR, ".")
          }else if (country == "TZ"){
            recR <-  paste0(exp_tz,
                            ifelse(DP < 0, paste(dec_tz), paste(inc_tz)), root_tz, " ", abs(DP), ton_tz,
                            ifelse(DP < 0, paste(but_tz), paste(and_tz)),
                            valinc_tz, currency, " ", dGR, ".")
          }else{
            recR <-  paste0(exp_rw,
                            ifelse(DP < 0, paste(dec_rw), paste(inc_rw)), root_rw, " ", abs(DP), ton_rw,
                            ifelse(DP < 0, paste(but_rw), paste(and_rw)),
                            valinc_rw, currency, " ", dGR, ".")
          }
          
          
        }
      }
      
      if(ds[1,]$PD != ds[ds$CP == TRUE,]$PD & ds[1,]$HD != ds[ds$CP == TRUE,]$HD){
        
        if (country == "NG" | country == "GH") {
          rec <- paste0(recrev_ng, " ( ", format(ds[1,]$PD, "%d %B %Y"), " ) ",
                        hvsdate_ng, " ( ", format(ds[1,]$HD, "%d %B %Y"), " ) ",
                        nochange_ng)
        }else if (country == "TZ"){
          rec <- paste0(recrev_tz, " ( ", format(ds[1,]$PD, "%d %B %Y"), " ) ",
                        hvsdate_tz, " ( ", format(ds[1,]$HD, "%d %B %Y"), " ) ",
                        nochange_tz)
        }else{
          rec <- paste0(recrev_rw, " ( ", format(ds[1,]$PD, "%d %B %Y"), " ) ",
                        hvsdate_rw, " ( ", format(ds[1,]$HD, "%d %B %Y"), " ) ",
                        nochange_rw)
        }
      }else{
        rec <- paste0(recP, recH, recR)
      }
      
      
      #TODO: This only provides the minimal information to return to the user. We may consider adding following information:
      #1. Risks of harvesting later (especially in CBSD-affected areas)
      #2. Importance of using the right varieties
      #3. Reasons underlying recommendations (driven by yield, price or both)
      #4. Implications on agronomic practices, requirements for ridging, fertilizer application,...
      #5. Possible issues with the input data - especially if user provides unrealistic prices.
    }
  }
  rec <- gsub("  ", " ", rec)

  return(rec)
}




#' Title
#'
#' @param ds is output of getPPrecommendations
#' @param country 
#'
#' @return the advice as text to print in app
#' @export
#'
#' @examples
getPPrecText <- function(ds, country = c("NG", "TZ", "RW")) {

  TRNS <- read.csv("translations_TEST.csv", stringsAsFactors = FALSE)
  no_ng <- gsub(pattern = "\"", replacement = "", TRNS$no[1]); no_tz <- gsub(pattern = "\"", replacement = "", TRNS$no[2]); no_rw <- gsub(pattern = "\"", replacement = "", TRNS$no[3])
  plo_ng <- gsub(pattern = "\"", replacement = "", TRNS$plo[1]); plo_tz <- gsub(pattern = "\"", replacement = "", TRNS$plo[2]); plo_rw <- gsub(pattern = "\"", replacement = "", TRNS$plo[3])
  ridg_ng <- gsub(pattern = "\"", replacement = "", TRNS$ridg[1]); ridg_tz <- gsub(pattern = "\"", replacement = "", TRNS$ridg[2]); ridg_rw <- gsub(pattern = "\"", replacement = "", TRNS$ridg[3])
  decnet_ng <- gsub(pattern = "\"", replacement = "", TRNS$decnet[1]); decnet_tz <- gsub(pattern = "\"", replacement = "", TRNS$decnet[2]);decnet_rw <- gsub(pattern = "\"", replacement = "", TRNS$decnet[3])
  werec_ng <- gsub(pattern = "\"", replacement = "", TRNS$werec[1]); werec_tz <- gsub(pattern = "\"", replacement = "", TRNS$werec[2]);werec_rw <- gsub(pattern = "\"", replacement = "", TRNS$werec[3]) 
  plofol_ng <- gsub(pattern = "\"", replacement = "", TRNS$plofol[1]); plofol_tz <- gsub(pattern = "\"", replacement = "", TRNS$plofol[2]);plofol_rw <- gsub(pattern = "\"", replacement = "", TRNS$plofol[3])
  noridg_ng <- gsub(pattern = "\"", replacement = "", TRNS$noridg[1]); noridg_tz <- gsub(pattern = "\"", replacement = "", TRNS$noridg[2]); noridg_rw <- gsub(pattern = "\"", replacement = "", TRNS$noridg[3])
  thank_ng <- gsub(pattern = "\"", replacement = "", TRNS$thank[1]); thank_tz <- gsub(pattern = "\"", replacement = "", TRNS$thank[2]); thank_rw <- gsub(pattern = "\"", replacement = "", TRNS$thank[3])
  plodir_ng <- gsub(pattern = "\"", replacement = "", TRNS$noplo[1]); plodir_tz <- gsub(pattern = "\"", replacement = "", TRNS$noplo[2]); plodir_rw <- gsub(pattern = "\"", replacement = "", TRNS$noplo[3])
  zerot_ng <- gsub(pattern = "\"", replacement = "", TRNS$zerot[1]); zerot_tz <- gsub(pattern = "\"", replacement = "", TRNS$zerot[2]); zerot_rw <- gsub(pattern = "\"", replacement = "", TRNS$zerot[3])
  changcost_ng <- gsub(pattern = "\"", replacement = "", TRNS$changcos[1]); changcost_tz <- gsub(pattern = "\"", replacement = "", TRNS$changcos[2]); changcost_rw <- gsub(pattern = "\"", replacement = "", TRNS$changcos[3])
  this_ng <- gsub(pattern = "\"", replacement = "", TRNS$this[1]); this_tz <- gsub(pattern = "\"", replacement = "", TRNS$this[2]); this_rw <- gsub(pattern = "\"", replacement = "", TRNS$this[3])
  decr_ng <- gsub(pattern = "\"", replacement = "", TRNS$decr[1]); decr_tz <- gsub(pattern = "\"", replacement = "", TRNS$decr[2]); decr_rw <- gsub(pattern = "\"", replacement = "", TRNS$decr[3])
  incr_ng <- gsub(pattern = "\"", replacement = "", TRNS$incr[1]); incr_tz <- gsub(pattern = "\"", replacement = "", TRNS$incr[2]); incr_rw <- gsub(pattern = "\"", replacement = "", TRNS$incr[3])
  costb_ng <- gsub(pattern = "\"", replacement = "", TRNS$costb[1]); costb_tz <- gsub(pattern = "\"", replacement = "", TRNS$costb[2]); costb_rw <- gsub(pattern = "\"", replacement = "", TRNS$costb[3])
  rtprod_ng <- gsub(pattern = "\"", replacement = "", TRNS$rtprod[1]); rtprod_tz <- gsub(pattern = "\"", replacement = "", TRNS$rtprod[2]); rtprod_rw <- gsub(pattern = "\"", replacement = "", TRNS$rtprod[3])
  optim_ng <- gsub(pattern = "\"", replacement = "", TRNS$optim[1]); optim_tz <- gsub(pattern = "\"", replacement = "", TRNS$optim[2]); optim_rw <- gsub(pattern = "\"", replacement = "", TRNS$optim[3]);
  plofol2_tz <- gsub(pattern = "\"", replacement = "", TRNS$plofol2[2])
  
  
  ds$method_ploughing <- as.character(ds$method_ploughing)
  ds$method_ridging <- as.character(ds$method_ridging)
  #CHANGE
  ds$method_ploughing <- ifelse(country == "TZ" & ds$method_ploughing == "tractor", "kwa trekta", as.character(ds$method_ploughing))
  ds$method_ploughing <- ifelse(country == "TZ" & ds$method_ploughing == "manual", "kwa jembe la mkono", as.character(ds$method_ploughing))
  ds$method_ridging <- ifelse(country == "TZ" & ds$method_ridging == "tractor", "kwa trekta", as.character(ds$method_ridging))
  ds$method_ridging <- ifelse(country == "TZ" & ds$method_ridging == "manual", "kwa jembe la mkono", as.character(ds$method_ridging))
  
  if (ds[1,]$CP) {
    
    #trans
    rec <- if (country == "NG") {
      paste0(optim_ng,
             ifelse(ds[1,]$method_ploughing == "N/A", paste(no_ng), ds[1,]$method_ploughing), paste(plo_ng),
             ifelse(ds[1,]$method_ridging == "N/A", paste(no_ng), ds[1,]$method_ridging), paste(ridg_ng), "\n", " ",
             decnet_ng)
    }else if (country == "TZ"){
      paste0(optim_tz,
             ifelse(ds[1,]$method_ploughing == "N/A", paste(no_tz), ds[1,]$method_ploughing), paste(plo_tz),
             ifelse(ds[1,]$method_ridging == "N/A", paste(no_tz), paste(ridg_tz), ds[1,]$method_ridging), paste(ridg_tz), "\n", " ",
             decnet_tz)
    }else{
      paste0(optim_rw,
             ifelse(ds[1,]$method_ploughing == "N/A", paste(no_rw), ds[1,]$method_ploughing), paste(plo_rw),
             ifelse(ds[1,]$method_ridging == "N/A", paste(no_rw), ds[1,]$method_ridging), paste(ridg_rw), "\n", " ",
             decnet_rw)
    }
  }else {
    
    #trans
    if (ds[1,]$ploughing & ds[1,]$ridging) { recT <- if (country == "NG") {
      paste0(werec_ng, ds[1,]$method_ploughing, plofol_ng, ds[1,]$method_ridging, ridg_ng, "\n")
    }else if (country == "TZ"){
      paste0(werec_tz, plofol_tz, ds[1,]$method_ploughing, plofol2_tz, ds[1,]$method_ridging, ridg_tz, "\n")
    }else{
      paste0(werec_rw, ds[1,]$method_ploughing, plofol_rw, ds[1,]$method_ridging, ridg_rw, "\n")
    }
    }
    
    if (ds[1,]$ploughing & !ds[1,]$ridging) { recT <- if (country == "NG") {
      paste0(werec_ng, ds[1,]$method_ploughing, noridg_ng, "\n")
    }else if (country == "TZ"){
      paste0(werec_tz, ds[1,]$method_ploughing, noridg_tz, "\n")
    }else{
      paste0(werec_rw, ds[1,]$method_ploughing, noridg_rw, "\n") 
    }
    }
    
    # print("error turry")
    if (!ds[1,]$ploughing & ds[1,]$ridging) { recT <- if (country == "NG") {
      paste0(plodir_ng, ds[1,]$method_ridging, ridg_ng, "\n")
    }else if (country == "TZ") {
      paste0(plodir_tz, ds[1,]$method_ridging, ridg_tz, "\n")
    }else{
      paste0(plodir_rw, ds[1,]$method_ridging, ridg_rw, "\n")
    }
    }
    
    if (!ds[1,]$ploughing & !ds[1,]$ridging) { recT <- if (country == "NG") {
      paste(zerot_ng, "\n")
    }else if (country == "TZ"){
      paste(zerot_tz, "\n")
    }else{
      paste(zerot_rw, "\n")
     }
    }
    
    currency <- ifelse(country == "NG", "NGN", ifelse(country == "RW", "RWF", ifelse(country == "GH", "GHS", "TZS")))
    dTC <- formatC(signif(abs(ds[1,]$dTC), digits = 3), format = "f", big.mark = ",", digits = 0)
    dNR <- formatC(signif(ds[1,]$dNR, digits = 3), format = "f", big.mark = ",", digits = 0)
    dRP <- signif(ds[1,]$dRP, digits = 2)
    
    rtcurr_ng <- gsub(pattern = "\"", replacement = "", TRNS$rtcurr[1]); rtcurr_tz <- gsub(pattern = "\"", replacement = "", TRNS$rtcurr[2]); rtcurr_rw <- gsub(pattern = "\"", replacement = "", TRNS$rtcurr[3])
    netinc_ng <- gsub(pattern = "\"", replacement = "", TRNS$netinc[1]); netinc_tz <- gsub(pattern = "\"", replacement = "", TRNS$netinc[2]); netinc_rw <- gsub(pattern = "\"", replacement = "", TRNS$netinc[3])
    rtwill_ng <- gsub(pattern = "\"", replacement = "", TRNS$rtwill[1]); rtwill_tz <- gsub(pattern = "\"", replacement = "", TRNS$rtwill[2]); by_ng <- gsub(pattern = "\"", replacement = "", TRNS$by[1]); by_tz <- gsub(pattern = "\"", replacement = "", TRNS$by[2])
    tonb_ng <- gsub(pattern = "\"", replacement = "", TRNS$tonb[1]); tonb_tz <- gsub(pattern = "\"", replacement = "", TRNS$tonb[2]); tonb_rw <- gsub(pattern = "\"", replacement = "", TRNS$tonb[3])
    netno_ng <- gsub(pattern = "\"", replacement = "", TRNS$netno[1]); netno_tz <- gsub(pattern = "\"", replacement = "", TRNS$netno[2]); netno_rw <- gsub(pattern = "\"", replacement = "", TRNS$netno[3])
    incomp_ng <- gsub(pattern = "\"", replacement = "", TRNS$incomp[1]); incomp_tz <- gsub(pattern = "\"", replacement = "", TRNS$incomp[2]); incomp_rw <- gsub(pattern = "\"", replacement = "", TRNS$incomp[3])
    recap_ng <- gsub(pattern = "\"", replacement = "", TRNS$recap[1]); recap_tz <- gsub(pattern = "\"", replacement = "", TRNS$recap[2]); recap_rw <- gsub(pattern = "\"", replacement = "", TRNS$recap[3])
    kgof_ng <- gsub(pattern = "\"", replacement = "", TRNS$kgof[1]); kgof_tz <- gsub(pattern = "\"", replacement = "", TRNS$kgof[2]); kgof_rw <- gsub(pattern = "\"", replacement = "", TRNS$kgof[3])
    area_ng <- gsub(pattern = "\"", replacement = "", TRNS$area[1]); area_tz <- gsub(pattern = "\"", replacement = "", TRNS$area[2]); area_rw <- gsub(pattern = "\"", replacement = "", TRNS$area[3])
    willc_ng <- gsub(pattern = "\"", replacement = "", TRNS$willc[1]); willc_tz <- gsub(pattern = "\"", replacement = "", TRNS$willc[2]); willc_rw <- gsub(pattern = "\"", replacement = "", TRNS$willc[3])
    extrap_ng <- gsub(pattern = "\"", replacement = "", TRNS$extrap[1]); extrap_tz <- gsub(pattern = "\"", replacement = "", TRNS$extrap[2]); extrap_rw <- gsub(pattern = "\"", replacement = "", TRNS$extrap[3])
    tonof_ng <- gsub(pattern = "\"", replacement = "", TRNS$tonof[1]); tonof_tz <- gsub(pattern = "\"", replacement = "", TRNS$tonof[2]); tonof_rw <- gsub(pattern = "\"", replacement = "", TRNS$tonof[3])
    netincr_ng <- gsub(pattern = "\"", replacement = "", TRNS$netincr[1]); netincr_tz <- gsub(pattern = "\"", replacement = "", TRNS$netincr[2]); netincr_rw <- gsub(pattern = "\"", replacement = "", TRNS$netincr[3])
    
    if (dTC == 0) {
      
      #trans
      recC <- if (country == "NG") {
        paste(changcost_ng)
      }else if (country == "TZ"){
        paste(changcost_tz)
      }else{
        paste(changcost_rw)
      }
      
    }else {
      #trans
      recC <- if (country == "NG") {
        paste0(this_ng, ifelse(ds[1,]$dTC < 0, paste(decr_ng), paste(incr_ng)), costb_ng, currency, " ", dTC, ". ")
      }else if (country == "TZ"){
        paste0(this_tz, ifelse(ds[1,]$dTC < 0, paste(decr_tz), paste(incr_tz)), costb_tz, currency, " ", dTC, ". ")
      }else{
        paste0(this_rw, ifelse(ds[1,]$dTC < 0, paste(decr_rw), paste(incr_rw)), costb_rw, currency, " ", dTC, ". ")
      }
      
    }
    
    
    #trans
    
    if (dRP == 0 & dNR == 0) { recP <- if (country == "NG") {
      rtprod_ng
    }else if (country == "TZ") {
      rtprod_tz
    }else{
      rtprod_rw
    }
    }
    if (dRP == 0 & dNR > 0) { recP <- if (country == "NG") {
      paste0(rtcurr_ng, netinc_ng, currency, " ", dNR, ".")
    }else if (country == "TZ") {
      paste0(rtcurr_tz, netinc_tz, currency, " ", dNR, ".")
    }else{
      paste0(rtcurr_rw, netinc_rw, currency, " ", dNR, ".")
    }
    }
    
    if (dRP != 0 & dNR == 0) { recP <- if (country == "NG") {
      paste0(rtwill_ng, ifelse(ds[1,]$TC < 0, paste(decr_ng), paste(incr_ng)), by_ng, dRP, tonb_ng,
             netno_ng)
    }else if (country == "TZ"){
      paste0(rtwill_tz, ifelse(ds[1,]$TC < 0, paste(decr_tz), paste(incr_tz)), by_tz, dRP, tonb_tz,
             netno_tz)
    }else{
      paste0(rtwill_rw, ifelse(ds[1,]$TC < 0, paste(decr_rw), paste(incr_rw)), by_rw, dRP, tonb_rw,
             netno_rw)
    }
    }
    if (dRP != 0 & dNR != 0) { recP <- if (country == "NG") {
      paste0(rtwill_ng, ifelse(ds[1,]$TC < 0, paste(decr_ng), paste(incr_ng)), by_ng, dRP, tonb_ng, ifelse(ds[1,]$TC < 0, but_ng, and_ng),
             netinc_ng, currency, " ", dNR, incomp_ng)
    }else if (country == "TZ") {
      
      paste0(rtwill_tz, ifelse(ds[1,]$TC < 0, paste(decr_tz), paste(incr_tz)), by_tz, dRP, tonb_tz, ifelse(ds[1,]$TC < 0, but_tz, and_tz),
             netinc_tz, currency, " ", dNR, incomp_tz)
    }else{
      paste0(rtwill_rw, ifelse(ds[1,]$TC < 0, paste(decr_rw), paste(incr_rw)), by_tz, dRP, tonb_rw, ifelse(ds[1,]$TC < 0, but_rw, and_rw),
             netinc_rw, currency, " ", dNR, incomp_rw)
    }
    }
    rec <- paste0(recT, recC, recP)

  }
  #TODO: This only provides the minimal information to return to the user. We may consider adding following information:
  #1. Beware that planting on flat may not be advisable in your specific conditions. You should ridge if the land is sometimes very wet (water-logging problems), if controlling weed is very challenging, if the soil is very clayey, or if you plan to harvest during the dry season.
  #2. We currently do not consider costs and benefits of harrowing - we have not investigated this.
  #3. Explicit reasons underlying recommendations (driven by cost-saving or revenue increase).
  #4. Our selection of the best option may differ from the one by the farmer. A farmer may be willing to choose an option that has a lower net revenue change than the recommended, but also a lower cost.
  #5. Possible issues with the input data - especially if user provides unrealistic prices.
  
  rec <- gsub("  ", " ", rec)

  return(rec)

}


#' @param ds is output of getICrecommendations
#' @param maizePD mmaize product
getICrecText <- function(ds, maizePD) {

    
    if (!ds[["rec"]]$rec_F) {
      
      #trans
      
      recF <- paste0("Fertilizer use is not recommended because ", ds[["rec"]]$reason_F)
      
    }else {
      
      dTC <- formatC(signif(ds[["rec"]]$dTC, digits = 3), format = "f", big.mark = ",", digits = 0)
      dNR <- formatC(signif(ds[["rec"]]$dNR, digits = 3), format = "f", big.mark = ",", digits = 0)
      dMP <- signif(ds[["rec"]]$dMP, digits = 2)
      currency <- "NGN"
      ds[["fertilizer_rates"]]$rate <- round(ds[["fertilizer_rates"]]$rate, digits = 0)
      
      if (maizePD == "grain") {
        #1 kg of grain ~ 7.64 cobs
        dMP <- round(dMP / 7.64, digits = 0)
        
        #trans
        recF <- paste0("We recommend applying\n",
                       paste0(ds[["fertilizer_rates"]]$rate, " kg of ", ds[["fertilizer_rates"]]$type, collapse = "\n"), " ",
                       "\nfor the area of your field.\n",
                       "This will cost ", currency, " ", dTC, ". ",
                       "We expect an extra production of ", dMP, " kg of maize for the area of your field, ",
                       "and a net value increase of ", currency, " ", dNR, ".\n")
        
      }else {
        
        #trans
        recF <- paste0("We recommend applying\n",
                       paste0(ds[["fertilizer_rates"]]$rate, " kg of ", ds[["fertilizer_rates"]]$type, collapse = "\n"), " ",
                       "\nfor the area of your field.\n",
                       "This will cost ", currency, " ", dTC, ". ",
                       "We expect an extra production of ", dMP, " cobs for the area of your field, ",
                       "and a net value increase of ", currency, " ", dNR, ".\n")
      }
    }
    
    if (!is.null(ds[["rec"]]$reason_D)) {
      
      #trans
      recD <- ifelse(ds[["rec"]]$rec_D, "Plant your maize intercrop at high density: 1 m between rows and 25 cm within row (40,000 plants per hectare).",
                     paste0("Plant your maize intercrop at low density: 1 m between rows and 50 cm within row (20,000 plants per hectare) because ", ds[["rec"]]$reason_D, "."))
    }else {
      
      #trans
      recD <- ifelse(ds[["rec"]]$rec_D, "Plant your maize intercrop at high density: 1 m between rows and 25 cm within row (40,000 plants per hectare).",
                     paste0("Plant your maize intercrop at low density: 1 m between rows and 50 cm within row (20,000 plants per hectare)."))
    }
    
    
    rec <- paste0(recF, recD)
    
    #TODO: This only provides the minimal information to return to the user. We may consider adding following information:
    #1. Make sure they grow the right maize variety (should be mature in 95 days max), and the right cassava variety. Should also make recommendations on the right cassava variety.
    #2. Fertilizer application will also increase cassava yield - but this is not accounted for in cost-benefit calculations.
    #3. Some explanation included on why fertilizer is not recommended, or why high density is not recommended - need to evaluate if this is not too cryptic.
    #4. Possible issues with the input data - especially if user provides unrealistic prices for maize produce / fertilizers.
    #5. Currently reports the increase in maize production in nr of cobs, even if the user reported to sell as grain (NEEDS TO BE URGENTLY ADDRESSED - CONFUSING! Requires adapting the getICrecommendations function)
    rec <- gsub("  ", " ", rec)
  
 

  return(rec)

}



## process the recom output as Markdown input
FR_MarkdownText <- function(rr, fertilizers, userName, country, userPhoneNr, userField, area, areaUnits, PD, HD, email, lat, lon,
                            rootUP, cassPD, cassUW, maxInv, userPhoneCC) {
  #
  bags_total = round(rr$rec$TargetY, digits = 1)
  totalSalePrice = rr$rec$TC + rr$rec$NR
  revenue = rr$rec$NR
  current_yield = rr$rec$CurrentY
  sum_total = rr$rec$TC

  currency <- ifelse(country == "NG", "NGN", ifelse(country == "RW", "RWF", ifelse(country == "GH", "GHS", ifelse(country == "BU", "BIF","TZS"))))

  MarkDownTextD <- data.frame(name = userName, country = country, phone = userPhoneNr, field = userField, field_area = area,
                              unit_field = areaUnits, plant_date = PD, hvst_date = HD, current_yield = current_yield,
                              email = email, latitude = lat, longitude = lon, userPhoneCC = userPhoneCC,
                              costcassava = rootUP, unitcassava = cassPD, maxinvest = maxInv,
                              sum_total = sum_total, bags_total = bags_total, product = cassPD,
                              totalSalePrice = totalSalePrice, revenue = revenue, currency = currency, cassUW = cassUW)

  MarkDownTextD$costcassava <- formatC(signif(MarkDownTextD$costcassava, digits = 4), format = "f", big.mark = ",", digits = 0)
  MarkDownTextD$maxinvest <- formatC(signif(MarkDownTextD$maxinvest, digits = 4), format = "f", big.mark = ",", digits = 0)

  write.csv(MarkDownTextD, "personalized_info.csv", row.names = FALSE)

  fertilizers_recom <- fertilizers[fertilizers$type %in% rr$fertilizer_rates$type,]
  if (nrow(fertilizers_recom) > 0) {
    fertilizers_recom <- merge(fertilizers_recom, rr$fertilizer_rates, by = 'type')
    fertilizers_recom$rate <- round(fertilizers_recom$rate, digits = 0)
    fertilizers_recom$bags <- round(fertilizers_recom$rate / fertilizers_recom$bagWeight, digits = 1)
    fertilizers_recom$cost <- round(fertilizers_recom$rate, digits = 0) * fertilizers_recom$price
    #fertilizers_recom$cost <- (fertilizers_recom$rate *  fertilizers_recom$costPerBag) / fertilizers_recom$bagWeight
    Bagsfull <- trunc(fertilizers_recom$bags)
    bagshalf <- fertilizers_recom$bags - floor(fertilizers_recom$bags)
    bagshalf <- ifelse(bagshalf >= 0.25 & bagshalf <= 0.75, 0.5, ifelse(bagshalf < 0.25, 0, 1))
    fertilizers_recom$bags <- Bagsfull + bagshalf

    sum_total <- round(sum(fertilizers_recom$cost), digits = 0)
    MarkDownTextD$sum_total <- sum_total
    MarkDownTextD$revenue <- MarkDownTextD$totalSalePrice - sum_total
    write.csv(MarkDownTextD, "personalized_info.csv", row.names = FALSE)


    ff <- NULL
    for (j in 1:nrow(fertilizers_recom)) {
      dd <- data.frame(fertilizer = fertilizers_recom$type[j],
                       cost = fertilizers_recom$price[j],
                       costPerBag = fertilizers_recom$costPerBag[j],
                       unit = paste(fertilizers_recom$bagWeight[j], "kg bag", sep = ''),
                       kgs = fertilizers_recom$rate[j],
                       rep = NA,
                       bags = fertilizers_recom$bags[j],
                       total_cost = fertilizers_recom$cost[j])
      names(dd) <- paste(names(dd), j, sep = "")
      if (j == 1) {
        ff <- dd
      }else {
        ff <- cbind(ff, dd)
      }
    }
    MarkDownTextD <- cbind(MarkDownTextD, ff)
    write.csv(MarkDownTextD, "FR_MarkDownText.csv", row.names = FALSE)
  }
}


IC_MarkdownText <- function(rr, fertilizers, userName, country,
                            userPhoneNr, userField, area, areaUnits,
                            PD, HD, email, lat, lon,
                            rootUP, cassPD, maxInv, userPhoneCC, CMP,
                            maizeUW, maizePD, cassUW, maizeUP, nameSF,saleSF,riskAtt ) {


  current_yield = rr$rec$dMP ## this is increase in maize yield
  totalSalePrice = rr$rec$dTC + rr$rec$dNR
  revenue = rr$rec$dNR
  sum_total = rr$rec$dTC
  currency <- ifelse(country == "NG", "NGN", ifelse(country == "RW", "RWF", ifelse(country == "GH", "GHS", "TZS")))
  dMP <- rr$rec$dMP


  currency <- ifelse(country == "NG", "NGN", ifelse(country == "RW", "RWF", ifelse(country == "GH", "GHS", "TZS")))

  MarkDownTextD <- data.frame(name = userName, country = country, phone = userPhoneNr, field = userField, field_area = area,
                              unit_field = areaUnits, plant_date = PD, hvst_date = HD, userPhoneCC = userPhoneCC,
                              email = email, latitude = lat, longitude = lon, product = cassPD, costcassava = rootUP, unitcassava = cassPD, maxinvest = maxInv,
                              currency = currency, maizeUP = maizeUP, maizeUW = maizeUW, maizePD = maizePD,
                              sum_total = sum_total, cassUW = cassUW, totalSalePrice = totalSalePrice, revenue = revenue, dMP = dMP,
                              saleSF = saleSF, nameSF = nameSF, CMP = CMP, riskAtt = riskAtt
  )

  MarkDownTextD$maxinvest <- as.numeric(as.character(MarkDownTextD$maxinvest))
  MarkDownTextD$costcassava <- formatC(signif(MarkDownTextD$costcassava, digits = 4), format = "f", big.mark = ",", digits = 0)
  MarkDownTextD$maxinvest <- formatC(signif(MarkDownTextD$maxinvest, digits = 4), format = "f", big.mark = ",", digits = 0)

  if (CMP == 1) {
    MarkDownTextD$CMP <- "About Knee height (~50 cm)"
  }else if (CMP == 2) {
    MarkDownTextD$CMP <- "About chest height (~150 cm)"
  }else if (CMP == 3) {
    MarkDownTextD$CMP <- "Larger than a person with yellowish leaves (~200 cm)"
  }else if (CMP == 4) {
    MarkDownTextD$CMP <- "Larger than a person with green leaves (~200 cm)"
  }else if (CMP == 5) {
    MarkDownTextD$CMP <- "Larger than a person with dark green leaves (~200 cm)"
  }

  if (MarkDownTextD$maizePD == "fresh_cob") {
    MarkDownTextD$unitproduct <- paste(MarkDownTextD$currency, " ", MarkDownTextD$maizeUP, " per ",
                                       MarkDownTextD$maizePD, ".", sep = "")
  }else {
    MarkDownTextD$unitproduct <- paste(MarkDownTextD$currency, " ", MarkDownTextD$maizeUP, " per ", MarkDownTextD$maizeUW,
                                       " kg of grain.", sep = "")
  }

  write.csv(MarkDownTextD, "personalized_info.csv", row.names = FALSE)

  fertilizers_recom <- fertilizers[fertilizers$type %in% rr$fertilizer_rates$type,]
  if (nrow(fertilizers_recom) > 0) {
    fertilizers_recom <- merge(fertilizers_recom, rr$fertilizer_rates, by = 'type')
    fertilizers_recom$rate <- round(fertilizers_recom$rate, digits = 0)
    fertilizers_recom$cost <- round(fertilizers_recom$rate, digits = 0) * fertilizers_recom$price
    fertilizers_recom$bags <- round(fertilizers_recom$rate / fertilizers_recom$bagWeight, digits = 1)
    Bagsfull <- trunc(fertilizers_recom$bags)
    bagshalf <- fertilizers_recom$bags - floor(fertilizers_recom$bags)
    bagshalf <- ifelse(bagshalf >= 0.25 & bagshalf <= 0.75, 0.5, ifelse(bagshalf < 0.25, 0, 1))
    fertilizers_recom$bags <- Bagsfull + bagshalf


    MarkDownTextD$sum_total <- sum(fertilizers_recom$cost)
    MarkDownTextD$revenue = MarkDownTextD$totalSalePrice - MarkDownTextD$sum_total
    write.csv(MarkDownTextD, "personalized_info.csv", row.names = FALSE)


    ff <- NULL
    for (j in 1:nrow(fertilizers_recom)) {
      dd <- data.frame(fertilizer = fertilizers_recom$type[j],
                       cost = fertilizers_recom$price[j],
                       costPerBag = fertilizers_recom$costPerBag[j],
                       unit = paste(fertilizers_recom$bagWeight[j], "kg bag", sep = ''),
                       kgs = fertilizers_recom$rate[j],
                       rep = NA,
                       bags = fertilizers_recom$bags[j],
                       total_cost = round(fertilizers_recom$rate[j], digits = 0) * fertilizers_recom$price[j])
      names(dd) <- paste(names(dd), j, sep = "")
      if (j == 1) {
        ff <- dd
      }else {
        ff <- cbind(ff, dd)
      }
    }

    MarkDownTextD <- cbind(MarkDownTextD, ff, rr$rec)
    write.csv(MarkDownTextD, "IC_MarkDownText.csv", row.names = FALSE)
  }
}


CIS_MarkdownText <- function(rr, fertilizers, userName, country,
                             userPhoneNr, userField, area, areaUnits,
                             PD, HD, email, lat, lon,
                             rootUP, cassPD, cassUW, maxInv, userPhoneCC,
                             sweetPotatoUP, sweetPotatoPD, tuberUP, sweetPotatoUW) {

  #current_yield = rr$rec$dMP ## this is increase in maize yield
  totalSalePrice = rr$rec$dTC + rr$rec$dNR
  revenue = rr$rec$dNR
  sum_total = rr$rec$dTC
  currency <- ifelse(country == "NG", "NGN", ifelse(country == "RW", "RWF", ifelse(country == "GH", "GHS", "TZS")))
  #dMP <- rr$rec$dMP

  currency <- ifelse(country == "NG", "NGN", ifelse(country == "RW", "RWF", ifelse(country == "GH", "GHS", "TZS")))

  MarkDownTextD <- data.frame(name = userName, country = country, phone = userPhoneNr, field = userField, field_area = area,
                              unit_field = areaUnits, plant_date = PD, hvst_date = HD, userPhoneCC = userPhoneCC,
                              email = email, latitude = lat, longitude = lon, product = cassPD,
                              costcassava = rootUP, unitcassava = cassPD, maxinvest = maxInv, currency = currency, sum_total = sum_total,
                              product = cassPD, totalSalePrice = totalSalePrice, revenue = revenue, currency = currency, cassUW = cassUW,
                              sweetPotatoUW = sweetPotatoUW, sweetPotatoUP = sweetPotatoUP, sweetPotatoPD = sweetPotatoPD, tuberUP = tuberUP)


  MarkDownTextD$costcassava <- formatC(signif(MarkDownTextD$costcassava, digits = 4), format = "f", big.mark = ",", digits = 0)
  MarkDownTextD$maxinvest <- formatC(signif(MarkDownTextD$maxinvest, digits = 4), format = "f", big.mark = ",", digits = 0)


  write.csv(MarkDownTextD, "personalized_info.csv", row.names = FALSE)

  fertilizers_recom <- fertilizers[fertilizers$type %in% rr$fertilizer_rates$type,]
  if (nrow(fertilizers_recom) > 0) {
    fertilizers_recom <- merge(fertilizers_recom, rr$fertilizer_rates, by = 'type')
    fertilizers_recom$rate <- round(fertilizers_recom$rate, digits = 0)
    fertilizers_recom$cost <- round(fertilizers_recom$rate, digits = 0) * fertilizers_recom$price
    fertilizers_recom$bags <- round(fertilizers_recom$rate / fertilizers_recom$bagWeight, digits = 1)
    Bagsfull <- trunc(fertilizers_recom$bags)
    bagshalf <- fertilizers_recom$bags - floor(fertilizers_recom$bags)
    bagshalf <- ifelse(bagshalf >= 0.3 & bagshalf <= 0.65, 0.5, ifelse(bagshalf < 0.3, 0, 1))
    fertilizers_recom$bags <- Bagsfull + bagshalf

    MarkDownTextD$sum_total <- sum(fertilizers_recom$cost)
    MarkDownTextD$revenue = MarkDownTextD$totalSalePrice - MarkDownTextD$sum_total
    write.csv(MarkDownTextD, "personalized_info.csv", row.names = FALSE)


    ff <- NULL
    for (j in 1:nrow(fertilizers_recom)) {
      dd <- data.frame(fertilizer = fertilizers_recom$type[j],
                       cost = fertilizers_recom$price[j],
                       costPerBag = fertilizers_recom$costPerBag[j],
                       unit = paste(fertilizers_recom$bagWeight[j], "kg bag", sep = ''),
                       kgs = round(fertilizers_recom$rate[j], digits = 0),
                       rep = NA,
                       bags = fertilizers_recom$bags[j],
                       total_cost = round(fertilizers_recom$rate[j], digits = 0) * fertilizers_recom$price[j])


      names(dd) <- paste(names(dd), j, sep = "")
      if (j == 1) {
        ff <- dd
      }else {
        ff <- cbind(ff, dd)
      }
    }

    MarkDownTextD <- cbind(MarkDownTextD, ff, rr$rec)
    write.csv(MarkDownTextD, "CIS_MarkDownText.csv", row.names = FALSE)
  }
}


PPSP_MarkdownText <- function(rr, fname, userName = userName, country = country,
                              userPhoneNr = userPhoneNr, userField = userField, area = area, areaUnits = areaUnits,
                              PD = PD, HD = HD, email = email, lat = lat, lon = lon,
                              rootUP = rootUP, cassPD = cassPD, cassUW = cassUW, maxInv = maxInv) {

  currency <- ifelse(country == "NG", "NGN", ifelse(country == "RW", "RWF", ifelse(country == "GH", "GHS", "TZS")))

  MarkDownTextD <- data.frame(name = userName, country = country, phone = userPhoneNr, field = userField, field_area = area,
                              unit_field = areaUnits, plant_date = PD, hvst_date = HD,
                              email = email, latitude = lat, longitude = lon,
                              costcassava = rootUP, unitcassava = cassPD,
                              maxinvest = maxInv, cassUW = cassUW, product = cassPD, currency = currency)

  write.csv(MarkDownTextD, "personalized_info.csv", row.names = FALSE)

  fname2 <- paste(fname, "_MarkDownText.csv", sep = '')
  write.csv(MarkDownTextD, "PP_MarkDownText.csv", row.names = FALSE)
}


PP_MarkdownText <- function(userName, country, userPhoneNr, userField, area, areaUnits, PD, HD, email, lat, lon, rootUP, cassPD, cassUW,
                            maxInv, ploughing, ridging, method_ploughing, method_ridging, userPhoneCC) {
  MarkDownTextD <- data.frame(name = userName, country = country, phone = userPhoneNr, field = userField, field_area = area,
                              unit_field = areaUnits, plant_date = PD, hvst_date = HD,
                              email = email, latitude = lat, longitude = lon,
                              costcassava = rootUP, unitcassava = cassPD, cassUW = cassUW,
                              maxinvest = maxInv, product = cassPD, ploughing = ploughing, ridging = ridging,
                              method_ploughing = method_ploughing, method_ridging = method_ridging, userPhoneCC = userPhoneCC)


  write.csv(MarkDownTextD, "PP_MarkDownText.csv", row.names = FALSE)
}


### create a data frame from user info which iwll be used within teh markdown file to create the file for the pdf document to be shared via email. 
SP_MarkdownText <- function(userName, country, userPhoneNr, userField, area, areaUnits, PD, HD, email, lat, lon, saleSF, nameSF,
                            maxInv, ploughing, ridging, method_ploughing, method_ridging, userPhoneCC, CMP, riskAtt,
                            PD_window, HD_window, cassPD, cassUW, cassUP, cassUP_m1, cassUP_m2, cassUP_p1, cassUP_p2) {
  MarkDownTextD <- data.frame(name = userName, country = country, phone = userPhoneNr, field = userField, field_area = area,
                              unit_field = areaUnits, plant_date = PD, hvst_date = HD,
                              email = email, latitude = lat, longitude = lon,
                              maxinvest = maxInv, saleSF = saleSF, nameSF = nameSF, CMP = CMP, riskAtt = riskAtt, PD, HD,
                              PD_window = PD_window,
                              HD_window = HD_window, cassPD = cassPD,
                              cassUW = cassUW, cassUP = cassUP, cassUP_m1 = cassUP_m1, cassUP_m2 = cassUP_m2,
                              cassUP_p1 = cassUP_p1, cassUP_p2 = cassUP_p2, userPhoneCC = userPhoneCC)
  write.csv(MarkDownTextD, "SP_MarkDownText.csv", row.names = FALSE)
}

#SHORT DEF:   Function to obtain recommendations on cassava-sweet potato intercropping.
#RETURNS:     list of 2 dataframes: (i) cost benefit analysis for most profitable system, and (ii) fertilizer rates to apply.
#DESCRIPTION: Function to obtain recommendations on cassava-sweet potato intercropping.
#             Returns (i) a 1-row dataframe cost-benefit parameters (extra yield, cost and net revenue, and whether to apply
#             fertilizer and whether to intercrop, and why (not)) , and (ii) a data.frame with types of fertilizer and rates to apply (zeros included).
#INPUT:       See Cassava Crop Manager function for details
getCISrecommendations <- function(areaHa = 1,
                                  FCY = 11,
                                  tuberUP,
                                  rootUP,
                                  fertilizers,
                                  riskAtt = c(0, 1, 2)) {

  if (!require("limSolve")) install.packages("limSolve"); library("limSolve")

  #calculating expected yield increase from fertilizer
  FSY <- 0.7 * FCY #expected yield of a sweet potato monocrop
  FSY <- FSY * areaHa #extra sweet potato production for the area of the field TODO check with Pieter
  GR_MC <- FCY * rootUP #gross revenue of cassava monocrop
  # GR_IC <- GR_MC * 0.6 + 0.8 * FSY * tuberUP #gross revenue of cassava-sweet potato intercrop
  GR_IC <- GR_MC * 0.8 + 0.6 * FSY * tuberUP
  rec_IC <- GR_MC < GR_IC

  if (rec_IC) {
    #calculating fertilizer requirement
    E <- t(data.matrix(fertilizers[, 2:4]))
    #F <- c(68, 19.6, 56.8) #ideally 2 bags of urea + 6 bags of NPK15:15:15
    F <- c(68, 33.2, 60) #ideally 2 bags of urea + 8 bags of NPK17:17:17
    G <- diag(nrow(fertilizers))
    H <- rep(0, nrow(fertilizers))
    Cost <- fertilizers$price

    #calculating fertilizer recommendation and total cost of fertilizer
    FR <- linp(E, F, G, H, Cost)$X
    FR[FR < 25] <- 0 #dropping all rates less than 25 kg/ha
    FR <- FR * areaHa #adjusting to field area

    #calculating total cost
    dTC <- c(FR %*% fertilizers$price)
    # dGR <- ifelse(FCY > 20, 0, FCY * 0.6 * 0.4 * rootUP + FSY * 0.8 * 0.2 * tuberUP) #gross revenue increase: 40% yield increase in cassava + 20% yield increase in sweet potato, but not in fields with yields above 20 t/ha

    #gross revenue increase: 40% yield increase in cassava + 20% yield increase in sweet potato, but not in fields with yields above 20 t/ha in yield classes 1-3, 20% in cassava and 10% in sweet potato in yield class 4, and 0 in yield class 5
    # dGR <- ifelse(FCY > 30, 0, ifelse(FCY > 20, FCY * 0.6 * 0.2 * rootUP + FSY * 0.8 * 0.1 * tuberUP, FCY * 0.6 * 0.4 * rootUP + FSY * 0.8 * 0.2 * tuberUP))
    dGR <- ifelse(FCY > 30, 0, ifelse(FCY > 20, FCY * 0.8 * 0.2 * rootUP + FSY * 0.6 * 0.1 * tuberUP, FCY * 0.8 * 0.4 * rootUP + FSY * 0.6 * 0.2 * tuberUP))  #NEW CALCULATION
    

    #evaluating if a solution was found
    if (dTC == 0) {
      dGR <- 0

      #trans


      reason_F <- "Mbolea sahihi haipatikani"
      rec_F <- FALSE
    }else {

      #trans
      reason_F <- "Tunakushauri usitumie mbolea kwa sababu itakuongezea gharama hatimae utapata hasara"
      rec_F <- TRUE
    }
  }else {
    dTC <- 0
    FR <- 0
    dGR <- 0

    #trans
    reason_F <- "Kilimo mchanganyiko haupendekezwi.Panda muhogo peke yake."
    rec_F <- FALSE
  }

  #net revenue increase from fertilizer
  dNR <- dGR - dTC

  if (dTC > 0) {
    #minimal required net revenue increase from fertilizer needed (taking into account risk attitude of user)
    dNRmin <- dTC * ifelse(riskAtt == 0, 1.8, ifelse(riskAtt == 1, 1, 0.2))

    #check profitability of fertilizer use
    if (dNR > dNRmin) {
      rec_F <- TRUE

      #trans
      reason_F <- "Matumizi ya mbolea inapendekezwa"
    }else {
      dTC <- 0
      dGR <- 0
      dNR <- 0
      FR <- FR * 0
      rec_F <- FALSE

      #trans
      reason_F <- "Tunakushauri usitumie mbolea kwa sababu itakuongezea gharama hatimae utapata hasara"
    }
  }

  #output
  rec <- data.frame(rec_IC = rec_IC, #boolean indicating whether intercropping is recommended (more profitable than monocropping cassava)
                    rec_F = rec_F, #TRUE or FALSE indicating if fertilizer application is recommended
                    dNR = dNR, #net revenue increase from fertilizer use (in local currency)
                    dTC = dTC, #extra cost for fertilizer use (in local currency)
                    reason_F = reason_F #reason why fertilizer application is not recommended
  )

  fertilizer_rates <- data.frame(type = fertilizers$type, rate = FR) #fertilizer rates to apply
  fertilizer_rates <- fertilizer_rates[fertilizer_rates$rate > 0,]

  return(list(rec = rec, fertilizer_rates = fertilizer_rates))

}



#' @param ds is output of getCISrecommendations
#' @param country 
#'
#' @return the advice as text to print in app
getCISrecText <- function(ds) {

  if (!ds[["rec"]]$rec_IC) {

    # recIC <- "Intercropping is not recommended. Growing a cassava monocrop will give you a higher profit.\n"
    # recF  <- "If you consider investing in fertilizer, please use our Fertilizer Recommendations Tool to obtain fertilizer advice for a cassava monocrop."

    recIC <- "Kilimo mchanganyiko haipendekezwi. Kupanda muhogo peke yake utakupatia faida ya juu.\n"
    recF <- "Kama ungependa kuwekeza katika mbolea, tafadhali tumia chombo chetu cha mapandekezo ya mbolea ili kupata ushauri wa kupanda muhogo bila mseto."


  }else {

    recIC <- "Tunapendekeza kuchanganya muhogo na viazi vitamu (kilimo mseto). Hii itakupatia faida ya juu na mapato ya haraka kutoka kwenye viazi vitamu."
    #recIC <- "This will generate a higher profit overall, and also give you access to early income from sweet potato.\n"

    if (!ds[["rec"]]$rec_F) {

      #recF <- paste0("Fertilizer use is not recommended because ", ds[["rec"]]$reason_F, ".\n")
      recF <- paste0("Haishauriwi kutumia  mbolea kwa sababu ", ds[["rec"]]$reason_F, ".\n")

    }else {

      dTC <- formatC(signif(ds[["rec"]]$dTC, digits = 3), format = "f", big.mark = ",", digits = 0)
      dNR <- formatC(signif(ds[["rec"]]$dNR, digits = 3), format = "f", big.mark = ",", digits = 0)
      #currency <- ifelse(country == "NG", "NGN", "TZS")
      currency <- "TZS"

      #recF <- paste0("We recommend applying\n",
      # paste0(round(ds[["fertilizer_rates"]]$rate), " kg of ", ds[["fertilizer_rates"]]$type, collapse="\n"),
      # "\nfor the area of your field.\n",
      # "This will cost ", currency, " ", dTC, ". ",
      # "We expect a net value increase of ", currency, " ", dNR, " for the area of your field.")


      recF <- paste0("Tunapendekeza utumie\n",
                     paste0("kilo ", round(ds[["fertilizer_rates"]]$rate), " ya ", ds[["fertilizer_rates"]]$type, collapse = "\n"), " ",
                     "\nkatika eneo la shamba lako.\n",
                     "Hii itagharimu shilingi ", currency, " ", dTC, ". ",
                     "Tunatarajia jumla ya ongezeko la thamani kwa ", currency, " ", dNR, " katika eneo la shamba lako.")
    }

  }

  rec <- paste0(recIC, recF)

  #TODO: This only provides the minimal information to return to the user. We may consider adding following information:
  #1. Make sure they grow the right sweet potato variety (Mayai), and the right cassava variety (Kizimbani).
  #2. We purposefully did not include recommendations on the exact yield increases as the data is rather weak to justify this. Can be added later when more data is available.
  #3. Some explanation included on why fertilizer is not recommended, or why intercropping is not recommended - need to evaluate if this is not too cryptic.
  #4. Possible issues with the input data - especially if user provides unrealistic prices for sweet potato/cassava produce / fertilizers.
  rec <- gsub("  ", " ", rec)
  return(rec)

}


#SHORT DEF:   Function to obtain recommendations on land clearing (step 2 of 6 steps).
#RETURNS:     dataframe with recommendations on whether to slash and/or to spray.
#DESCRIPTION: Function to obtain recommendations on land clearing (slashing and spraying) based on decision tree in the paper-based tool
#INPUT:       See Cassava Crop Manager function for details

getWMrecommendations <- function(fallowType = c(NA, "bush", "broad_leaves", "grass", "none"),
                                 fallowHeight = c(NA, 100, 150, 200),
                                 fallowGreen = c(NA, TRUE, FALSE),
                                 problemWeeds = c(NA, TRUE, FALSE)) {
  slash <- ifelse(fallowType == "bush" & fallowHeight > 100 |
                    fallowType == "broad_leaves" & fallowGreen == FALSE |
                    fallowType == "broad_leaves" &
                    fallowGreen == TRUE &
                    fallowHeight > 150 |
                    fallowType == "grass" & fallowHeight > 150,
                  TRUE, FALSE)
  
  spray <- ifelse(fallowType == "bush" & fallowHeight <= 100 |
                    fallowType == "broad_leaves" &
                    fallowGreen == TRUE &
                    fallowHeight <= 150 |
                    fallowType == "grass" |
                    fallowType == "none" & problemWeeds == TRUE,
                  TRUE, FALSE)
  
  ds <- data.frame(operation = c("slash", "spray"), rec = c(slash, spray))
  
  return(ds)
  
}



#' part of QUEFTS
#' @param dss
#' @returnType
#' @return
#'
#' @author Meklit
#' @export
getsupply <- function(dss) {
  supply <- data.frame(lat = dss$lat, long = dss$long, rel_N = dss$rel_N, rel_P = dss$rel_P, rel_K = dss$rel_K, SN = dss$soilN, SP = dss$soilP, SK = dss$soilK, water_limited_yield = dss$water_limited_yield,
                       aN = dss$aN, dN = dss$dN, aP = dss$aP, dP = dss$dP, aK = dss$aK, dK = dss$dK, rN = dss$rN, rP = dss$rP, rK = dss$rK, max_yield = dss$max_yield, tolerance = dss$tolerance,
                       WLY = dss$water_limited_yield)
}





#SHORT DEF:   Function to send SMS report.
#RETURNS:     Nothing. SMS report are sent.
#             TODO: build in checks to log if SMS report was successfully sent.
#DESCRIPTION: Function using Plivo service to send SMS texts to phonenumber specified.
#             Note: Plivo credentials are hardcoded! Do not share!!!
#             TODO: use scan function to read credentials from csv input file.
#INPUT:       text: Vector of body text to be sent by SMS. Elements should not exceed 1600 character limit!
#             src: source phone number, starting with country code, default 254727876796
#             dst: destination phone number, starting with country code, e.g., 234789123456

sendSMSReport <- function(text, src = "userTel#withCountryCode", dst) {
  
  # if(!require("httr")){install.packages("httr"); library("httr")}
  
  #plivio account details
  AUTH_ID = "MANDM1MDCYNWU4NGEZZW"
  AUTH_TOKEN = "M2Q2MmQ0NjI3ZjNjOTBkYjMyNGMzNzUzODdmZTc3"
  url = "https://api.plivo.com/v1/Account/MANDM1MDCYNWU4NGEZZW/Message/"
  
  for (i in text) {
    if (nchar(i) <= 1600) {
      POST(url,
           authenticate(AUTH_ID, AUTH_TOKEN),
           body = list(src = src, dst = dst, text = i))
    }else {
      print("Text message exceeds 1600 character limit. Message not sent")
      
    }
  }
}









#####################################################################################################3
#####################################################################################################3
## no more used functions
#####################################################################################################3




#' gets lat or lon and find the closest matching lat and lon in the WLY data, at 0.05 (5Km) resolution, used to source ISRIC data 
# getclosestcoor <- function(ll) {
#   seqdec <- data.frame(ss = seq(from = 0.025, to = 0.975, by = 0.05))
#   lf <- floor(ll)
#   ld <- as.numeric(paste("0.", strsplit(gsub('\\.', "_", ll), "_")[[1]][2], sep = ""))
#   seqdec$diff <- seqdec$ss - ld
#   ll2 <- lf + seqdec[seqdec$diff == min(abs(seqdec$diff)),]$ss
#   return(ll2)
# }


#' This was an attempt to make soil dat asourcing and fitting the random forest model in real time, but as it adds too much time to give advice it is not used
#' @param FCY farmers current yield, used as control yeidl for RF model
#' @param country should be NG or TZ
#' @param PD the ith number of the year
#' @param HD how many days the crop was on the field between planting and harvest
#' @example ISRIC_FRmodel_CY(lat=4.775, lon=8.425, country = "NG", FCY=3.75, PD = 254, HD= 350)
# ISRIC_FRmodel_CY <- function(lat = lat, lon = lon, country = country, FCY = FCY, PD = PD, HD = HD) {
#   
#   ## get closest pixel with WLY data
#   lat2 <- getclosestcoor(ll = lat)
#   lon2 <- getclosestcoor(ll = lon)
#   
#   ## get soil NPK
#   ISRIC_SoilData_t <- getISRICData(lat = lat, lon = lon, country = country)
#   SoilData_fcy <- Rfmodel_Wrapper(ISRIC_SoilData = ISRIC_SoilData_t, FCY, country = country)
#   
#   ## get WLY:get PDand HD to the closest daes fr which we have WLY
#   Nigeria_WLY_365 <- readRDS("NG_WLY_LINTUL.RDS")
#   # Nigeria_WLY_365 <- readRDS("NG_WLY_LINTUL.RDS")
#   pdates <- data.frame(wlyPD = unique(Nigeria_WLY_365$pl_Date))
#   pdates$diff <- PD - pdates$wlyPD
#   PD2 <- pdates[pdates$diff == min(abs(pdates$diff)), "wlyPD"]
#   hdates <- data.frame(wlyHD = seq(214, 361, 7))
#   hdates$diff <- HD - hdates$wlyHD
#   HD2 <- hdates[hdates$diff == min(abs(hdates$diff)), "wlyHD"]
#   Nigeria_WLY_365[Nigeria_WLY_365$pl_Date == PD2,]
#   Nigeria_WLY_365[Nigeria_WLY_365$lat == lat2,]
#   wlypd <- Nigeria_WLY_365[Nigeria_WLY_365$lon == lon2 &
#                              Nigeria_WLY_365$lat == lat2 &
#                              Nigeria_WLY_365$pl_Date == PD2,]
#   wlydata <- wlypd[, c("lat", "lon", "pl_Date", "location")]
#   wlydata$water_limited_yield <- wlypd[, colnames(wlypd) == HD2]
#   wlydata$zone <- country
#   wlydata$daysOnField <- HD
#   wlydata <- wlydata[, c("lat", "lon", "water_limited_yield", "location", "pl_Date", "zone", "daysOnField")]
#   
#   ## get CY
#   require(plyr)
#   wlydata$Current_Yield <- QUEFTS_WLY_CY(SoilData = SoilData_fcy, country = country, wlyd = wlydata)
#   Planting_Nigeria <- data.frame(st = seq(1, 365, 7), en = (seq(1, 365, 7) + 454), weekNr = seq(1:53))
#   WLYData <- merge(wlydata, Planting_Nigeria[, c("st", "weekNr"),], by.x = "pl_Date", by.y = "st")
#   return(WLYData)
#   
# }
# 




#' country will be NG or TZ
# getISRICData <- function(lat = lat, lon = lon, country = country) {
#   #setwd("/home/akilimo/projects/gisdata")
#   setwd("/home/akilimo//gisdata")
#   p <- data.frame(x = lon, y = lat)
#   
#   ######################################################
#   ## Clay content (0-2 micro meter) mass fraction in %; for ODK NOT data
#   #####################################################
#   
#   list_tif <- c("CLYPPT_M_sl1_250m_ll",
#                 "CLYPPT_M_sl2_250m_ll",
#                 "CLYPPT_M_sl3_250m_ll")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   
#   Clay <- stack(list_tif)
#   Clay_data <- as.data.frame(raster::extract(Clay, p))
#   Clay_data$long <- p$x
#   Clay_data$lat <- p$y
#   colnames(Clay_data) <- c("Clay_5", "Clay_15", "Clay_30", "long", "lat")
#   Clay_data$Clay_averaged <- ((Clay_data$Clay_5 * 5) +
#                                 (Clay_data$Clay_15 * 10) +
#                                 (Clay_data$Clay_30 * 15)) / 30
#   
#   
#   ######################################################
#   ## Cation exchange capacity of soil in cmolc/kg; TODO redownload this layers
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("CECSOL_M_sl1_250m_ll",
#                 "CECSOL_M_sl2_250m_ll",
#                 "CECSOL_M_sl3_250m_ll")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   CEC <- stack(list_tif)
#   CEC_data <- as.data.frame(raster::extract(CEC, p))
#   CEC_data$long <- p$x
#   CEC_data$lat <- p$y
#   colnames(CEC_data) <- c("CEC_5", "CEC_15", "CEC_30", "long", "lat")
#   CEC_data$CEC_averaged <- ((CEC_data$CEC_5 * 5) +
#                               (CEC_data$CEC_15 * 10) +
#                               (CEC_data$CEC_30 * 15)) / 30
#   
#   ######################################################
#   ## Soil organic carbon content (fine earth fraction) in g per kg for ODK NOT data
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("ORCDRC_M_sl1_250m_ll",
#                 "ORCDRC_M_sl2_250m_ll",
#                 "ORCDRC_M_sl3_250m_ll")
#   
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   SOC <- stack(list_tif)
#   SOC_data <- as.data.frame(raster::extract(SOC, p))
#   SOC_data$long <- p$x
#   SOC_data$lat <- p$y
#   colnames(SOC_data) <- c("SOC_5", "SOC_15", "SOC_30", "long", "lat")
#   
#   SOC_data$SOM_5 <- SOC_data$SOC_5 * 2
#   SOC_data$SOM_15 <- SOC_data$SOC_15 * 2
#   SOC_data$SOM_30 <- SOC_data$SOC_30 * 2
#   
#   SOC_data$percentSOC_5 <- SOC_data$SOC_5 / 10
#   SOC_data$percentSOC_15 <- SOC_data$SOC_15 / 10
#   SOC_data$percentSOC_30 <- SOC_data$SOC_30 / 10
#   
#   SOC_data$percentSOM_5 <- SOC_data$SOM_5 / 10
#   SOC_data$percentSOM_15 <- SOC_data$SOM_15 / 10
#   SOC_data$percentSOM_30 <- SOC_data$SOM_30 / 10
#   
#   SOC_data$percentSOC_averaged <- ((SOC_data$percentSOC_5 * 5) +
#                                      (SOC_data$percentSOC_15 * 10) +
#                                      (SOC_data$percentSOC_30 * 15)) / 30
#   SOC_data$percentSOM_averaged <- ((SOC_data$percentSOM_5 * 5) +
#                                      (SOC_data$percentSOM_15 * 10) +
#                                      (SOC_data$percentSOM_30 * 15)) / 30
#   
#   
#   ######################################################
#   ## pH x 10 in H2O for ODK NOT data
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("PHIHOX_M_sl1_250m_ll",
#                 "PHIHOX_M_sl2_250m_ll",
#                 "PHIHOX_M_sl3_250m_ll")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   pH <- stack(list_tif)
#   # extract values for points
#   pH_data <- as.data.frame(raster::extract(pH, p))
#   pH_data$long <- p$x
#   pH_data$lat <- p$y
#   colnames(pH_data) <- c("pH_5", "pH_15", "pH_30", "long", "lat")
#   pH_data$pH_averaged <- ((pH_data$pH_5 * 5) +
#                             (pH_data$pH_15 * 10) +
#                             (pH_data$pH_30 * 15)) / 30
#   pH_data$pH_5 <- pH_data$pH_5 / 10
#   pH_data$pH_15 <- pH_data$pH_15 / 10
#   pH_data$pH_30 <- pH_data$pH_30 / 10
#   pH_data$pH_averaged <- pH_data$pH_averaged / 10
#   
#   
#   ######################################################
#   ## Silt content (2-50 micro meter) mass fraction in % for ODK NOT data
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("SLTPPT_M_sl1_250m_ll",
#                 "SLTPPT_M_sl2_250m_ll",
#                 "SLTPPT_M_sl3_250m_ll")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   Silt <- stack(list_tif)
#   # extract values for points
#   silt_data <- as.data.frame(raster::extract(Silt, p))
#   silt_data$long <- p$x
#   silt_data$lat <- p$y
#   colnames(silt_data) <- c("silt_5", "silt_15", "silt_30", "long", "lat")
#   silt_data$silt_averaged <- ((silt_data$silt_5 * 5) +
#                                 (silt_data$silt_15 * 10) +
#                                 (silt_data$silt_30 * 15)) / 30
#   head(silt_data)
#   
#   
#   ######################################################
#   ## Sand content (50-2000 micro meter) mass fraction in %, for ODK NOT data
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("SNDPPT_M_sl1_250m_ll",
#                 "SNDPPT_M_sl2_250m_ll",
#                 "SNDPPT_M_sl3_250m_ll")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   Sand <- stack(list_tif)
#   # extract values for points
#   Sand_data <- as.data.frame(raster::extract(Sand, p))
#   Sand_data$long <- p$x
#   Sand_data$lat <- p$y
#   colnames(Sand_data) <- c("Sand_5", "Sand_15", "Sand_30", "long", "lat")
#   Sand_data$Sand_averaged <- ((Sand_data$Sand_5 * 5) +
#                                 (Sand_data$Sand_15 * 10) +
#                                 (Sand_data$Sand_30 * 15)) / 30
#   
#   
#   ######################################################
#   ## Bulk density (fine earth) in kg / cubic-meter, for ODK NOT data
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("BLDFIE_M_sl1_250m_ll",
#                 "BLDFIE_M_sl2_250m_ll",
#                 "BLDFIE_M_sl3_250m_ll")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   BD <- stack(list_tif)
#   # extract values for points
#   BD_data <- as.data.frame(raster::extract(BD, p))
#   
#   
#   BD_data$long <- p$x
#   BD_data$lat <- p$y
#   colnames(BD_data) <- c("BD_5", "BD_15", "BD_30", "long", "lat")
#   BD_data$tonsoilha <- ((BD_data$BD_5 * 5) +
#                           (BD_data$BD_15 * 10) +
#                           (BD_data$BD_30 * 15)) / 30
#   BD_data$kgSoilHa <- BD_data$tonsoilha * 1000
#   head(BD_data)
#   
#   ######################################################
#   ## Total Nitrogen (N) content of the soil fine earth fraction in mg/kg (ppm)
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_n_m_agg30cm")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   TN <- raster(list_tif)
#   TN_data <- as.data.frame(raster::extract(TN, p))
#   TN_data$long <- p$x
#   TN_data$lat <- p$y
#   colnames(TN_data) <- c("TotalN", "long", "lat")
#   
#   ######################################################
#   ## Extractable Manganese (Mn) content of the soil fine earth fraction in mg/kg (ppm)
#   ## as measured according to the soil analytical procedure of Mehlich 3 and spatially predicted for 0-30 cm depth interval at 250 m
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_mn_m_agg30cm")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   Mn <- raster(list_tif)
#   Mn_data <- as.data.frame(raster::extract(Mn, p))
#   Mn_data$long <- p$x
#   Mn_data$lat <- p$y
#   colnames(Mn_data) <- c("Mn", "long", "lat")
#   
#   
#   ######################################################
#   ## Extractable Boron (B) content of the soil fine earth fraction in
#   ## mg/100kg (pp100m) as measured according to the soil analytical procedure of Mehlich 3 and spatially predicted for 0-30 cm depth interval at 250 m
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_b_m_agg30cm")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   B <- raster(list_tif)
#   B_data <- as.data.frame(raster::extract(B, p))
#   B_data$long <- p$x
#   B_data$lat <- p$y
#   colnames(B_data) <- c("B", "long", "lat")
#   
#   ######################################################
#   ## Extractable Calcium (Ca) content of the soil fine earth fraction in mg/kg (ppm) as measured according to the soil analytical procedure
#   ## of Mehlich 3 and spatially predicted for 0-30 cm depth interval at 250 m
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_ca_m_agg30cm")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   Ca <- raster(list_tif)
#   Ca_data <- as.data.frame(raster::extract(Ca, p))
#   Ca_data$long <- p$x
#   Ca_data$lat <- p$y
#   colnames(Ca_data) <- c("Ca", "long", "lat")
#   
#   ######################################################
#   ## Extractable Iron (Fe) content of the soil fine earth fraction in mg/kg (ppm)
#   ## as measured according to the soil analytical procedure of Mehlich 3 and spatially predicted for 0-30 cm depth interval at 250 m
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_fe_m_agg30cm")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   Fe <- raster(list_tif)
#   Fe_data <- as.data.frame(raster::extract(Fe, p))
#   Fe_data$long <- p$x
#   Fe_data$lat <- p$y
#   colnames(Fe_data) <- c("Fe", "long", "lat")
#   
#   
#   ######################################################
#   ##  Extractable Sodium Copper (Cu) of the soil fine earth fraction in mg/100kg (pp100m) as measured according
#   ## to the soil analytical procedure of Mehlich 3 and spatially predicted for 0-30 cm depth interval at 250 m
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_cu_m_agg30cm")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   Cu <- raster(list_tif)
#   Cu_data <- as.data.frame(raster::extract(Cu, p))
#   Cu_data$long <- p$x
#   Cu_data$lat <- p$y
#   colnames(Cu_data) <- c("Cu", "long", "lat")
#   
#   
#   ######################################################
#   ## Extractable Aluminium (Al) content of the soil fine earth fraction in mg/kg (ppm) as measured according to
#   ## the soil analytical procedure of Mehlich 3 and spatially predicted for 0-30 cm depth interval at 250 m
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_al_m_agg30cm")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   Al <- raster(list_tif)
#   Al_data <- as.data.frame(raster::extract(Al, p))
#   Al_data$long <- p$x
#   Al_data$lat <- p$y
#   colnames(Al_data) <- c("Al", "long", "lat")
#   
#   
#   ######################################################
#   ##  Extractable Manganese (Mn) content of the soil fine earth fraction in mg/kg (ppm) as measured according to the soil
#   ## canalytical procedure of Mehlich 3 and spatially predicted for 0-30 cm depth interval at 250 m
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_mg_m_agg30cm")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   Mg <- raster(list_tif)
#   Mg_data <- as.data.frame(raster::extract(Mg, p))
#   Mg_data$long <- p$x
#   Mg_data$lat <- p$y
#   colnames(Mg_data) <- c("Mg", "long", "lat")
#   
#   
#   ######################################################
#   ## Extractable Sodium content (Na) of the soil fine earth fraction in mg/kg (ppm) as measured according to the soil
#   ## analytical procedure of Mehlich 3 and spatially predicted for 0-30 cm depth interval at 250 m
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_na_m_agg30cm")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   Na <- raster(list_tif)
#   Na_data <- as.data.frame(raster::extract(Na, p))
#   Na_data$long <- p$x
#   Na_data$lat <- p$y
#   colnames(Na_data) <- c("Na", "long", "lat")
#   
#   
#   ######################################################
#   ##  Extractable Potassium (K) content of the soil fine earth fraction in mg/kg (ppm) as measured according to
#   ## the soil analytical procedure of Mehlich 3 and spatially predicted for 0-30 cm depth interval at 250 m
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_k_m_agg30cm")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   K <- raster(list_tif)
#   K_mehlich3 <- as.data.frame(raster::extract(K, p))
#   K_mehlich3$long <- p$x
#   K_mehlich3$lat <- p$y
#   colnames(K_mehlich3) <- c("K_Mehlich", "long", "lat")
#   K_mehlich3$exchK <- K_mehlich3$K_Mehlich * 1.17
#   
#   ######################################################
#   ## Extractable Phosphorus (P) content of the soil fine earth fraction in mg/100kg (pp100m)
#   ## as measured according to the soil analytical procedure of Mehlich 3 and spatially predicted for 0-30 cm depth interval at 250 m
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_p_m_agg30cm")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   Phos <- raster(list_tif)
#   P_mehlich3 <- as.data.frame(raster::extract(Phos, p))
#   P_mehlich3$long <- p$x
#   P_mehlich3$lat <- p$y
#   colnames(P_mehlich3) <- c("P_Mehlich", "long", "lat")
#   P_mehlich3$P_Mehlich <- P_mehlich3$P_Mehlich / 100
#   
#   
#   ######################################################
#   ## Nutrient clusters based on fuzzy k-means of the soil fine earth fraction and spatially predicted at 250 m spatial
#   ## resolution across sub-Saharan Africa using Machine Learning (ensemble between random forest and gradient boosting)
#   ## using soil data from the Africa Soil Profiles database (AfSP) compiled by AfSIS and recent soil data newly collected by AfSIS in
#   ## partnership with EthioSIS (Ethiopia), GhaSIS (Ghana) and NiSIS (Nigeria as made possible by OCP Africa and IITA), combined with soil
#   ## data as made available by Wageningen University and Research, IFDC, VitalSigns, University of California and the OneAcreFund. [Values M = mean value predicted].
#   ###################################################
#   rm(list_tif)
#   list_tif <- c("af250m_nutrient_ncluster_m")
#   if (country == "NG" ) {
#     list_tif <- paste(list_tif, "NG.tif", sep = "_")
#   }else if (country == "GH" ) {
#     list_tif <- paste(list_tif, "GH.tif", sep = "_")
#   }else if (country == "RW" ) {
#     list_tif <- paste(list_tif, "RW.tif", sep = "_")
#   }else {
#     list_tif <- paste(list_tif, "TZ.tif", sep = "_")
#   }
#   soilclusters <- stack(list_tif)
#   soilClus_data <- as.data.frame(raster::extract(soilclusters, p))
#   soilClus_data$long <- p$x
#   soilClus_data$lat <- p$y
#   colnames(soilClus_data) <- c("ncluster", "long", "lat")
#   
#   
#   ################ merging
#   
#   
#   ISRIC_NOT1 <- merge(Clay_data, CEC_data, by = c("long", "lat"))
#   ISRIC_NOT2 <- merge(ISRIC_NOT1, SOC_data, by = c("long", "lat"))
#   ISRIC_NOT3 <- merge(ISRIC_NOT2, pH_data, by = c("long", "lat"))
#   ISRIC_NOT4 <- merge(ISRIC_NOT3, silt_data, by = c("long", "lat"))
#   ISRIC_NOT5 <- merge(ISRIC_NOT4, Sand_data, by = c("long", "lat"))
#   ISRIC_NOT6 <- merge(ISRIC_NOT5, BD_data, by = c("long", "lat"))
#   ISRIC_NOT7 <- merge(ISRIC_NOT6, K_mehlich3, by = c("long", "lat"))
#   ISRIC_NOT8 <- merge(ISRIC_NOT7, P_mehlich3, by = c("long", "lat"))
#   ISRIC_NOT9 <- merge(ISRIC_NOT8, TN_data, by = c("long", "lat"))
#   ISRIC_NOT10 <- merge(ISRIC_NOT9, Mn_data, by = c("long", "lat"))
#   ISRIC_NOT11 <- merge(ISRIC_NOT10, B_data, by = c("long", "lat"))
#   # ISRIC_NOT12 <- merge(ISRIC_NOT11, Zn_data, by=c("long","lat"))
#   ISRIC_NOT13 <- merge(ISRIC_NOT11, Ca_data, by = c("long", "lat"))
#   ISRIC_NOT14 <- merge(ISRIC_NOT13, Fe_data, by = c("long", "lat"))
#   ISRIC_NOT15 <- merge(ISRIC_NOT14, Cu_data, by = c("long", "lat"))
#   ISRIC_NOT16 <- merge(ISRIC_NOT15, Al_data, by = c("long", "lat"))
#   ISRIC_NOT17 <- merge(ISRIC_NOT16, Mg_data, by = c("long", "lat"))
#   ISRIC_NOT18 <- merge(ISRIC_NOT17, Na_data, by = c("long", "lat"))
#   # ISRIC_NOT19 <- merge(ISRIC_NOT18, K_data, by=c("long","lat"))
#   # ISRIC_NOT20 <- merge(ISRIC_NOT19, P_data, by=c("long","lat"))
#   ISRIC_NOT <- merge(ISRIC_NOT18, soilClus_data, by = c("long", "lat"))
#   
#   ISRIC_NOT <- droplevels(ISRIC_NOT[!is.na(ISRIC_NOT$long),])
#   
#   
#   YS <- NULL
#   for (k in 1:nrow(ISRIC_NOT)) {
#     ssk <- ISRIC_NOT[k,]
#     if (ssk$pH_averaged <= 6.5) {
#       ssk$olsenP <- 0.55 * ssk$P_Mehlich
#     } else if (ssk$pH_averaged <= 7.3 & ssk$pH_averaged > 6.5) {
#       ssk$olsenP <- 0.5 * ssk$P_Mehlich
#     } else {
#       ssk$olsenP <- 0.45 * ssk$P_Mehlich
#     }
#     YS <- rbind(YS, ssk)
#   }
#   
#   
#   ISRIC_NOT <- YS
#   
#   ISRIC_NOT$KgKinHa <- ((ISRIC_NOT$exchK * ISRIC_NOT$kgSoilHa) / 1000000) ## K in kg/ha
#   ISRIC_NOT$KgPinHa <- (ISRIC_NOT$olsenP * ISRIC_NOT$kgSoilHa) / 1000000 ## P kg/ha
#   
#   
#   ######################################################################################################
#   ## pedotransfer function from harvest choice. sand, silt, clay and OM in percentage
#   ######################################################################################################
#   
#   
#   ##### WP ######
#   ISRIC_NOT$wp5a <- (-0.024 * ISRIC_NOT$Sand_5 / 100) +
#     0.487 * ISRIC_NOT$Clay_5 / 100 +
#     0.006 * ISRIC_NOT$percentSOM_5 +
#     0.005 * (ISRIC_NOT$Sand_5 / 100 * ISRIC_NOT$percentSOM_5) - 0.013 * (ISRIC_NOT$Clay_5 / 100 * ISRIC_NOT$percentSOM_5) +
#     0.068 * (ISRIC_NOT$Sand_5 / 100 * ISRIC_NOT$Clay_5 / 100) +
#     0.031
#   ISRIC_NOT$wp_5 <- (ISRIC_NOT$wp5a + (0.14 * ISRIC_NOT$wp5a - 0.02)) * 100
#   
#   ISRIC_NOT$wp15a <- -0.024 * ISRIC_NOT$Sand_15 / 100 +
#     0.487 * ISRIC_NOT$Clay_15 / 100 +
#     0.006 * ISRIC_NOT$percentSOM_15 +
#     0.005 * (ISRIC_NOT$Sand_15 / 100 * ISRIC_NOT$percentSOM_15) - 0.013 * (ISRIC_NOT$Clay_15 / 100 * ISRIC_NOT$percentSOM_15) +
#     0.068 * (ISRIC_NOT$Sand_15 / 100 * ISRIC_NOT$Clay_15 / 100) +
#     0.031
#   ISRIC_NOT$wp_15 <- (ISRIC_NOT$wp15a + (0.14 * ISRIC_NOT$wp15a - 0.02)) * 100
#   
#   ISRIC_NOT$wp30a <- -0.024 * ISRIC_NOT$Sand_30 / 100 +
#     0.487 * ISRIC_NOT$Clay_30 / 100 +
#     0.006 * ISRIC_NOT$percentSOM_30 +
#     0.005 * (ISRIC_NOT$Sand_30 / 100 * ISRIC_NOT$percentSOM_30) - 0.013 * (ISRIC_NOT$Clay_30 / 100 * ISRIC_NOT$percentSOM_30) +
#     0.068 * (ISRIC_NOT$Sand_30 / 100 * ISRIC_NOT$Clay_30 / 100) +
#     0.031
#   ISRIC_NOT$wp_30 <- (ISRIC_NOT$wp30a + (0.14 * ISRIC_NOT$wp30a - 0.02)) * 100
#   
#   ISRIC_NOT$wpA <- -0.024 * ISRIC_NOT$Sand_averaged / 100 +
#     0.487 * ISRIC_NOT$Clay_averaged / 100 +
#     0.006 * ISRIC_NOT$percentSOM_averaged +
#     0.005 * (ISRIC_NOT$Sand_averaged / 100 * ISRIC_NOT$percentSOM_averaged) - 0.013 * (ISRIC_NOT$Clay_averaged / 100 * ISRIC_NOT$percentSOM_averaged) +
#     0.068 * (ISRIC_NOT$Sand_averaged / 100 * ISRIC_NOT$Clay_averaged / 100) +
#     0.031
#   ISRIC_NOT$wp_wAverage <- (ISRIC_NOT$wpA + (0.14 * ISRIC_NOT$wpA - 0.02)) * 100
#   
#   ISRIC_NOT <- subset(ISRIC_NOT, select = -c(wp5a, wp15a, wp30a, wpA))
#   
#   
#   ##### FC ######
#   ISRIC_NOT$FC1 <- -0.251 * ISRIC_NOT$Sand_5 / 100 +
#     0.195 * ISRIC_NOT$Clay_5 / 100 +
#     0.011 * ISRIC_NOT$percentSOM_5 +
#     0.006 * (ISRIC_NOT$Sand_5 / 100 * ISRIC_NOT$percentSOM_5) - 0.027 * (ISRIC_NOT$Clay_5 / 100 * ISRIC_NOT$percentSOM_5) +
#     0.452 * (ISRIC_NOT$Sand_5 / 100 * ISRIC_NOT$Clay_5 / 100) +
#     0.299
#   ISRIC_NOT$FC_5 <- (ISRIC_NOT$FC1 + (1.283 * ISRIC_NOT$FC1^2 -
#                                         0.374 * ISRIC_NOT$FC1 -
#                                         0.015)) * 100
#   
#   
#   ISRIC_NOT$FC2 <- -0.251 * ISRIC_NOT$Sand_15 / 100 +
#     0.195 * ISRIC_NOT$Clay_15 / 100 +
#     0.011 * ISRIC_NOT$percentSOM_15 +
#     0.006 * (ISRIC_NOT$Sand_15 / 100 * ISRIC_NOT$percentSOM_15) - 0.027 * (ISRIC_NOT$Clay_15 / 100 * ISRIC_NOT$percentSOM_15) +
#     0.452 * (ISRIC_NOT$Sand_15 / 100 * ISRIC_NOT$Clay_15 / 100) +
#     0.299
#   ISRIC_NOT$FC_15 <- (ISRIC_NOT$FC2 + (1.283 * ISRIC_NOT$FC2^2 -
#                                          0.374 * ISRIC_NOT$FC2 -
#                                          0.015)) * 100
#   
#   
#   ISRIC_NOT$FC3 <- -0.251 * ISRIC_NOT$Sand_30 / 100 +
#     0.195 * ISRIC_NOT$Clay_30 / 100 +
#     0.011 * ISRIC_NOT$percentSOM_30 +
#     0.006 * (ISRIC_NOT$Sand_30 / 100 * ISRIC_NOT$percentSOM_30) - 0.027 * (ISRIC_NOT$Clay_30 / 100 * ISRIC_NOT$percentSOM_30) +
#     0.452 * (ISRIC_NOT$Sand_30 / 100 * ISRIC_NOT$Clay_30 / 100) +
#     0.299
#   ISRIC_NOT$FC_30 <- (ISRIC_NOT$FC3 + (1.283 * ISRIC_NOT$FC3^2 -
#                                          0.374 * ISRIC_NOT$FC3 -
#                                          0.015)) * 100
#   
#   ISRIC_NOT$FCA <- -0.251 * ISRIC_NOT$Sand_averaged / 100 +
#     0.195 * ISRIC_NOT$Clay_averaged / 100 +
#     0.011 * ISRIC_NOT$percentSOM_averaged +
#     0.006 * (ISRIC_NOT$Sand_averaged / 100 * ISRIC_NOT$percentSOM_averaged) - 0.027 * (ISRIC_NOT$Clay_averaged / 100 * ISRIC_NOT$percentSOM_averaged) +
#     0.452 * (ISRIC_NOT$Sand_averaged / 100 * ISRIC_NOT$Clay_averaged / 100) +
#     0.299
#   ISRIC_NOT$FC_wAverage <- (ISRIC_NOT$FCA + (1.283 * ISRIC_NOT$FCA^2 -
#                                                0.374 * ISRIC_NOT$FCA -
#                                                0.015)) * 100
#   
#   ISRIC_NOT <- subset(ISRIC_NOT, select = -c(FC1, FC2, FC3, FCA))
#   
#   
#   ##### soil water at ######
#   ISRIC_NOT$swsa5 <- 0.278 * (ISRIC_NOT$Sand_5 / 100) +
#     0.034 * (ISRIC_NOT$Clay_5 / 100) +
#     0.022 * ISRIC_NOT$percentSOM_5 -
#     0.018 * (ISRIC_NOT$Sand_5 / 100 * ISRIC_NOT$percentSOM_5) -
#     0.027 * (ISRIC_NOT$Clay_5 / 100 * ISRIC_NOT$percentSOM_5) -
#     0.584 * (ISRIC_NOT$Sand_5 / 100 * ISRIC_NOT$Clay_5 / 100) + 0.078
#   ISRIC_NOT$swsb5 <- (ISRIC_NOT$swsa5 + (0.636 * ISRIC_NOT$swsa5 - 0.107)) * 100
#   ISRIC_NOT$sws_5 <- (ISRIC_NOT$FC_5 / 100 + ISRIC_NOT$swsb5 / 100 - (0.097 * ISRIC_NOT$Sand_5 / 100) + 0.043) * 100
#   
#   
#   ISRIC_NOT$swsa15 <- 0.278 * (ISRIC_NOT$Sand_15 / 100) +
#     0.034 * (ISRIC_NOT$Clay_15 / 100) +
#     0.022 * ISRIC_NOT$percentSOM_15 -
#     0.018 * (ISRIC_NOT$Sand_15 / 100 * ISRIC_NOT$percentSOM_15) -
#     0.027 * (ISRIC_NOT$Clay_15 / 100 * ISRIC_NOT$percentSOM_15) -
#     0.584 * (ISRIC_NOT$Sand_15 / 100 * ISRIC_NOT$Clay_15 / 100) + 0.078
#   ISRIC_NOT$swsb15 <- (ISRIC_NOT$swsa15 + (0.636 * ISRIC_NOT$swsa15 - 0.107)) * 100
#   ISRIC_NOT$sws_15 <- (ISRIC_NOT$FC_15 / 100 + ISRIC_NOT$swsb15 / 100 - (0.097 * ISRIC_NOT$Sand_15 / 100) + 0.043) * 100
#   
#   
#   ISRIC_NOT$swsa30 <- 0.278 * (ISRIC_NOT$Sand_30 / 100) +
#     0.034 * (ISRIC_NOT$Clay_30 / 100) +
#     0.022 * ISRIC_NOT$percentSOM_30 -
#     0.018 * (ISRIC_NOT$Sand_30 / 100 * ISRIC_NOT$percentSOM_30) -
#     0.027 * (ISRIC_NOT$Clay_30 / 100 * ISRIC_NOT$percentSOM_30) -
#     0.584 * (ISRIC_NOT$Sand_30 / 100 * ISRIC_NOT$Clay_30 / 100) + 0.078
#   ISRIC_NOT$swsb30 <- (ISRIC_NOT$swsa30 + (0.636 * ISRIC_NOT$swsa30 - 0.107)) * 100
#   ISRIC_NOT$sws_30 <- (ISRIC_NOT$FC_30 / 100 + ISRIC_NOT$swsb30 / 100 - (0.097 * ISRIC_NOT$Sand_30 / 100) + 0.043) * 100
#   
#   
#   ISRIC_NOT$swsaA <- 0.278 * (ISRIC_NOT$Sand_averaged / 100) +
#     0.034 * (ISRIC_NOT$Clay_averaged / 100) +
#     0.022 * ISRIC_NOT$percentSOM_averaged -
#     0.018 * (ISRIC_NOT$Sand_averaged / 100 * ISRIC_NOT$percentSOM_averaged) -
#     0.027 * (ISRIC_NOT$Clay_averaged / 100 * ISRIC_NOT$percentSOM_averaged) -
#     0.584 * (ISRIC_NOT$Sand_averaged / 100 * ISRIC_NOT$Clay_averaged / 100) + 0.078
#   ISRIC_NOT$swsbA <- (ISRIC_NOT$swsaA + (0.636 * ISRIC_NOT$swsaA - 0.107)) * 100
#   ISRIC_NOT$sws_wAverage <- (ISRIC_NOT$FC_wAverage / 100 + ISRIC_NOT$swsbA / 100 - (0.097 * ISRIC_NOT$Sand_averaged / 100) + 0.043) * 100
#   
#   ISRIC_NOT <- subset(ISRIC_NOT, select = -c(swsa5, swsb5, swsa15, swsb15, swsa30, swsb30, swsaA, swsbA))
#   if (country == "NG") {
#     ISRIC_NOT$country <- "Nigeria"
#   } else if (country == "GH") {
#     ISRIC_NOT$country <- "Ghana"
#   } else if (country == "RW") {
#     ISRIC_NOT$country <- "Rwanda"
#   }else if (country == "TZ") {
#     ISRIC_NOT$country <- "Tanzania"
#   }
# 
#   #setwd("D:/ACAI_Wrapper/AOI_shapefiles")
#   return(ISRIC_NOT)
# }


#' develope the model based on the realtionship between soil NPK, ISRIC soil data and the root yield for the control treatment for the control
#' ISRIC_SoilData is the new location for which we seek soil NPK estimates
#' FCY is Farmer-reported current yield, in tonnes FM per ha (optional, default value = 11)
#' testN_RF the training set for soil N RF model
#' testP_RF the training set for soil P RF model
#' testK_RF the training set for soil K RF model
#' #' It also requires the RF_N1.RData, RF_P1.RData, RF_K1.RData
#' getsoilNPK_RFmodel <- function(ISRIC_SoilData = ISRIC_SoilData, country = country, lat = lat, long = long, Ndata_Train = Ndata_Train, Pdata_Train = Pdata_Train, Kdata_Train = Kdata_Train) {
#' 
#'   ISRIC_SoilData$ncluster <- as.factor(ISRIC_SoilData$ncluster)
#' 
#' 
#'   RF_N_B <- readRDS("RandforestN.RDS")
#'   RF_P_B <- readRDS("RandforestP.RDS")
#'   RF_K_B <- readRDS("RandforestK.RDS")
#' 
#'   ## soil N
#'   dcolnames <- c("soilN", "exchK", "olsenP", "Clay_5", "Clay_15", "Clay_30", "percentSOM_5", "percentSOM_15", "percentSOM_30", "pH_5",
#'                  "pH_15", "pH_30", "silt_5", "silt_15", "silt_30", "BD_5", "BD_15", "BD_30", "CEC_5", "CEC_15", "CEC_30", "percentSOC_5",
#'                  "percentSOC_15", "percentSOC_30", "FC_5", "FC_15", "FC_30", "wp_5", "wp_15", "wp_30", "sws_5", "sws_15", "sws_30",
#'                  "TotalN", "Mn", "B", "Ca", "Fe", "Cu", "Al", "Mg", "Na", "ncluster", "country", "CON")
#' 
#'   ISRIC_SoilData1 <- ISRIC_SoilData[, dcolnames]
#' 
#'   for (f in 1:length(names(ISRIC_SoilData1))) {
#'     levels(ISRIC_SoilData1[, f]) <- levels(Ndata_Train[, f])
#'   }
#'   #
#'   # ISRIC_SoilData1 <- rbind(testN_RF[1,], ISRIC_SoilData1)
#'   # ISRIC_SoilData1 <- ISRIC_SoilData1[-1,]
#'   # ISRIC_SoilData1$ncluster <- as.factor(ISRIC_SoilData1$ncluster)
#'   ISRIC_SoilData$soilN <- exp(predict(RF_N_B, ISRIC_SoilData1))
#' 
#'   ## soil P
#'   Pcolnames <- c("soilP", "exchK", "olsenP", "Clay_5", "Clay_15", "Clay_30", "percentSOM_5", "percentSOM_15", "percentSOM_30", "pH_5",
#'                  "pH_15", "pH_30", "silt_5", "silt_15", "silt_30", "BD_5", "BD_15", "BD_30", "CEC_5", "CEC_15", "CEC_30", "percentSOC_5",
#'                  "percentSOC_15", "percentSOC_30", "FC_5", "FC_15", "FC_30", "wp_5", "wp_15", "wp_30", "sws_5", "sws_15", "sws_30",
#'                  "TotalN", "Mn", "B", "Ca", "Fe", "Cu", "Al", "Mg", "Na", "ncluster", "country", "CON")
#' 
#'   ISRIC_SoilData2 <- ISRIC_SoilData[, Pcolnames]
#'   for (f in 1:length(names(ISRIC_SoilData2))) {
#'     levels(ISRIC_SoilData2[, f]) <- levels(Pdata_Train[, f])
#'   }
#'   #
#'   # ISRIC_SoilData2 <- rbind(testP_RF[1,], ISRIC_SoilData2)
#'   # ISRIC_SoilData2 <- ISRIC_SoilData2[-1,]
#'   ISRIC_SoilData$soilP <- exp(predict(RF_P_B, ISRIC_SoilData2))
#' 
#'   ## soil K
#'   Kcolnames <- c("soilK", "exchK", "olsenP", "Clay_5", "Clay_15", "Clay_30", "percentSOM_5", "percentSOM_15", "percentSOM_30", "pH_5",
#'                  "pH_15", "pH_30", "silt_5", "silt_15", "silt_30", "BD_5", "BD_15", "BD_30", "CEC_5", "CEC_15", "CEC_30", "percentSOC_5",
#'                  "percentSOC_15", "percentSOC_30", "FC_5", "FC_15", "FC_30", "wp_5", "wp_15", "wp_30", "sws_5", "sws_15", "sws_30",
#'                  "TotalN", "Mn", "B", "Ca", "Fe", "Cu", "Al", "Mg", "Na", "ncluster", "country", "CON")
#' 
#'   ISRIC_SoilData3 <- ISRIC_SoilData[, Kcolnames]
#' 
#'   for (f in 1:length(names(ISRIC_SoilData3))) {
#'     levels(ISRIC_SoilData3[, f]) <- levels(Kdata_Train[, f])
#'   }
#'   # ISRIC_SoilData3 <- rbind(testK_RF[1,], ISRIC_SoilData3)
#'   # ISRIC_SoilData3 <- ISRIC_SoilData3[-1,]
#'   ISRIC_SoilData$soilK <- exp(predict(RF_K_B, ISRIC_SoilData3))
#' 
#'   ISRIC_SoilData$rec_N <- 0.5
#'   ISRIC_SoilData$rec_P <- 0.15
#'   ISRIC_SoilData$rec_K <- 0.5
#'   ISRIC_SoilData$rel_N <- 1
#'   ISRIC_SoilData$rel_P <- ISRIC_SoilData$soilP / ISRIC_SoilData$soilN
#'   ISRIC_SoilData$rel_K <- ISRIC_SoilData$soilK / ISRIC_SoilData$soilN
#'   ISRIC_SoilData$lat <- lat
#'   ISRIC_SoilData$long <- long
#'   ISRIC_SoilData$location <- paste(ISRIC_SoilData$lat, ISRIC_SoilData$long, sep = "_")
#'   ISRIC_SoilData$Zone <- country
#'   ISRIC_SoilData <- ISRIC_SoilData[, c("location", "lat", "long", "soilN", "soilP", "soilK", "Zone", "rec_N", "rec_P", "rec_K", "rel_N", "rel_P", "rel_K")]
#' 
#' 
#'   return(ISRIC_SoilData)
#' 
#' }
#' 



#' #' RF model works only if the factor levels are exactly identical to the data used to develp the model,
#' Rfmodel_Wrapper_obsolete <- function(ISRIC_SoilData = ISRIC_SoilData, FCY = FCY, country = country) {
#' 
#'   ### To avoid having a new factor level while using RF for prediction, add the soil data from NG here and remove it
#'   GIS_soilINS_modData2 <- read.csv("NOT_GIS_CON.csv") ## NOT data used to develope the RF model
#'   #setwd("D:/ACAI_Wrapper/cloud_compute")
#'   # GIS_soilINS_modData2 <- read.csv("NOT_GIS_CON.csv")
#'   GIS_soilINS_modData2$Clay_5 <- as.numeric(GIS_soilINS_modData2$Clay_5)
#'   GIS_soilINS_modData2$Clay_15 <- as.numeric(GIS_soilINS_modData2$Clay_15)
#'   GIS_soilINS_modData2$Clay_30 <- as.numeric(GIS_soilINS_modData2$Clay_30)
#'   GIS_soilINS_modData2$silt_5 <- as.numeric(GIS_soilINS_modData2$silt_5)
#'   GIS_soilINS_modData2$silt_15 <- as.numeric(GIS_soilINS_modData2$silt_15)
#'   GIS_soilINS_modData2$silt_30 <- as.numeric(GIS_soilINS_modData2$silt_30)
#'   GIS_soilINS_modData2$BD_5 <- as.numeric(GIS_soilINS_modData2$BD_5)
#'   GIS_soilINS_modData2$BD_15 <- as.numeric(GIS_soilINS_modData2$BD_15)
#'   GIS_soilINS_modData2$BD_30 <- as.numeric(GIS_soilINS_modData2$BD_30)
#'   GIS_soilINS_modData2$CEC_5 <- as.numeric(GIS_soilINS_modData2$CEC_5)
#'   GIS_soilINS_modData2$CEC_15 <- as.numeric(GIS_soilINS_modData2$CEC_15)
#'   GIS_soilINS_modData2$CEC_30 <- as.numeric(GIS_soilINS_modData2$CEC_30)
#'   GIS_soilINS_modData2$TotalN <- as.numeric(GIS_soilINS_modData2$TotalN)
#'   GIS_soilINS_modData2$Mn <- as.numeric(GIS_soilINS_modData2$Mn)
#'   GIS_soilINS_modData2$B <- as.numeric(GIS_soilINS_modData2$B)
#'   GIS_soilINS_modData2$Ca <- as.numeric(GIS_soilINS_modData2$Ca)
#'   GIS_soilINS_modData2$Fe <- as.numeric(GIS_soilINS_modData2$Fe)
#'   GIS_soilINS_modData2$Cu <- as.numeric(GIS_soilINS_modData2$Cu)
#'   GIS_soilINS_modData2$Al <- as.numeric(GIS_soilINS_modData2$Al)
#'   GIS_soilINS_modData2$Mg <- as.numeric(GIS_soilINS_modData2$Mg)
#'   GIS_soilINS_modData2$Na <- as.numeric(GIS_soilINS_modData2$Na)
#'   GIS_soilINS_modData2$ncluster <- as.factor(GIS_soilINS_modData2$ncluster)
#'   GIS_soilINS_modData2$CON <- as.numeric(GIS_soilINS_modData2$CON)
#'   GIS_soilINS_modData2 <- subset(GIS_soilINS_modData2, select = -c(Zn))
#' 
#' 
#'   NG_SoilData <- read.csv('NG_SoilData.csv')
#'   #NG_SoilData <- read.csv('NG_SoilData.csv')
#'   NG_SoilData$Clay_5 <- as.numeric(NG_SoilData$Clay_5)
#'   NG_SoilData$Clay_15 <- as.numeric(NG_SoilData$Clay_15)
#'   NG_SoilData$Clay_30 <- as.numeric(NG_SoilData$Clay_30)
#'   NG_SoilData$silt_5 <- as.numeric(NG_SoilData$silt_5)
#'   NG_SoilData$silt_15 <- as.numeric(NG_SoilData$silt_15)
#'   NG_SoilData$silt_30 <- as.numeric(NG_SoilData$silt_30)
#'   NG_SoilData$BD_5 <- as.numeric(NG_SoilData$BD_5)
#'   NG_SoilData$BD_15 <- as.numeric(NG_SoilData$BD_15)
#'   NG_SoilData$BD_30 <- as.numeric(NG_SoilData$BD_30)
#'   NG_SoilData$CEC_5 <- as.numeric(NG_SoilData$CEC_5)
#'   NG_SoilData$CEC_15 <- as.numeric(NG_SoilData$CEC_15)
#'   NG_SoilData$CEC_30 <- as.numeric(NG_SoilData$CEC_30)
#'   NG_SoilData$TotalN <- as.numeric(NG_SoilData$TotalN)
#'   NG_SoilData$Mn <- as.numeric(NG_SoilData$Mn)
#'   NG_SoilData$B <- as.numeric(NG_SoilData$B)
#'   NG_SoilData$Ca <- as.numeric(NG_SoilData$Ca)
#'   NG_SoilData$Fe <- as.numeric(NG_SoilData$Fe)
#'   NG_SoilData$Cu <- as.numeric(NG_SoilData$Cu)
#'   NG_SoilData$Al <- as.numeric(NG_SoilData$Al)
#'   NG_SoilData$Mg <- as.numeric(NG_SoilData$Mg)
#'   NG_SoilData$Na <- as.numeric(NG_SoilData$Na)
#'   NG_SoilData$ncluster <- as.factor(NG_SoilData$ncluster)
#'   NG_SoilData$country <- "Nigeria"
#'   NG_SoilData$CON <- as.numeric(5)
#' 
#'   NG_SoilData$soilN <- 0
#'   NG_SoilData$soilP <- 0
#'   NG_SoilData$soilK <- 0
#' 
#'   NG_SoilData <- NG_SoilData[, c("soilN", "soilP", "soilK", "exchK", "olsenP", "Clay_5", "Clay_15", "Clay_30", "percentSOM_5", "percentSOM_15", "percentSOM_30",
#'                                  "pH_5", "pH_15", "pH_30", "silt_5", "silt_15", "silt_30", "BD_5", "BD_15", "BD_30", "CEC_5", "CEC_15", "CEC_30", "percentSOC_5",
#'                                  "percentSOC_15", "percentSOC_30", "FC_5", "FC_15", "FC_30", "wp_5", "wp_15", "wp_30", "sws_5", "sws_15", "sws_30",
#'                                  "TotalN", "Mn", "B", "Ca", "Fe", "Cu", "Al", "Mg", "Na", "ncluster", "country", "CON")]
#' 
#' 
#'   NG_SoilData$use <- "Valid"
#'   GIS_soilINS_modData2$use <- "train"
#'   factoring <- rbind(GIS_soilINS_modData2, NG_SoilData)
#' 
#'   for (f in 1:length(names(factoring))) {
#'     levels(GIS_soilINS_modData2[, f]) <- levels(factoring[, f])
#'   }
#' 
#' 
#'   GIS_soilINS_modData3 <- factoring[factoring$use == "train",]
#'   GIS_soilINS_modData3 <- subset(GIS_soilINS_modData3, select = -c(use))
#' 
#'   NG_SoilData <- factoring[factoring$use == "Valid",]
#'   NG_SoilData <- subset(NG_SoilData, select = -c(use))
#' 
#' 
#'   ### Data partioning
#'   set.seed(444)
#'   GIS_soilINS_modData3$ncluster <- as.factor(GIS_soilINS_modData3$ncluster)
#'   ind <- sample(2, nrow(GIS_soilINS_modData3), replace = TRUE, prob = c(0.7, 0.3)) ## where conrtol yield is used as a covariate
#'   trainData <- GIS_soilINS_modData3[ind == 1,]
#'   testData <- GIS_soilINS_modData3[ind == 2,]
#' 
#'   Ndata_Train <- subset(trainData, select = -c(soilP, soilK))
#'   Pdata_Train <- subset(trainData, select = -c(soilN, soilK))
#'   Kdata_Train <- subset(trainData, select = -c(soilN, soilP))
#' 
#'   lat <- ISRIC_SoilData$lat
#'   long <- ISRIC_SoilData$long
#'   ISRIC_SoilData$CON <- as.numeric(FCY)
#' 
#' 
#'   ISRIC_SoilData$soilN <- 0
#'   ISRIC_SoilData$soilP <- 0
#'   ISRIC_SoilData$soilK <- 0
#' 
#'   ISRIC_SoilData <- ISRIC_SoilData[, c("soilN", "soilP", "soilK", "exchK", "olsenP", "Clay_5", "Clay_15", "Clay_30", "percentSOM_5", "percentSOM_15", "percentSOM_30",
#'                                        "pH_5", "pH_15", "pH_30", "silt_5", "silt_15", "silt_30", "BD_5", "BD_15", "BD_30", "CEC_5", "CEC_15", "CEC_30", "percentSOC_5",
#'                                        "percentSOC_15", "percentSOC_30", "FC_5", "FC_15", "FC_30", "wp_5", "wp_15", "wp_30", "sws_5", "sws_15", "sws_30",
#'                                        "TotalN", "Mn", "B", "Ca", "Fe", "Cu", "Al", "Mg", "Na", "ncluster", "country", "CON")]
#' 
#'   for (f in 1:length(names(ISRIC_SoilData))) {
#'     levels(ISRIC_SoilData[, f]) <- levels(trainData[, f])
#'   }
#'   ISRIC_SoilData$country <- as.factor(ISRIC_SoilData$country)
#' 
#'   SoilData_fcy_A <- getsoilNPK_RFmodel(ISRIC_SoilData = ISRIC_SoilData, country = country, lat = lat, long = long, Ndata_Train, Pdata_Train, Kdata_Train)
#' 
#' 
#'   return(SoilData_fcy_A)
#' }
#' 
#' 
#' 
# getFRrecommendations_ODK <- function(areaHa = 1,
#                                      lat,
#                                      lon,
#                                      PD,
#                                      FCY = 11,
#                                      rootUP,
#                                      fertilizers = fertilizers #dataframe containing price of urea, DAP and MOP in USD in local currency
# ) {
#   
#   #rounding lat and lon to centroid of 5x5km pixel
#   latr <- floor(lat * 10) / 10 + ifelse(lat - (floor(lat * 10) / 10) < 0.05, 0.025, 0.075)
#   lonr <- floor(lon * 10) / 10 + ifelse(lat - (floor(lon * 10) / 10) < 0.05, 0.025, 0.075)
#   
#   #pick up the recommended fertilizer application rates
#   flp <- paste0("E:/03-projects/ACAI/ODK briefcase storage/created forms/DSTs/media_FR/inputDST_FR_perpixel/E", lonr, "N", latr, ".csv")
#   
#   if (!file.exists(flp)) {
#     
#     #trans
#     
#     if (country == "NG") {
#       print(norecom_ng)
#     }else if (country == "TZ"){
#       print(norecom_tz)
#     }else{
#       print(norecom_rw)
#     }
#     
#     
#     ds <- NULL
#     
#   }else {
#     
#     res <- read.csv(flp)
#     FCY <- min(5, ceiling(FCY / 7.5))
#     plw <- as.numeric(format(PD, format = "%W"))
#     res <- res[res$PW == plw & res$FCY == FCY,]
#     
#     fertilizer_rates = data.frame(type = gsub("rate", "", names(res)[grepl("rate", names(res))]),
#                                   rate = round(as.numeric(res[, grepl("rate", names(res))]),digits=0))
#     
#     
#     
#     
#     res <- res[, !grepl("rate", names(res))]
#     
#     res <- subset(res, select = -c(PW, FCY))
#     res$TC <- fertilizers$price %*% fertilizer_rates$rate * areaHa
#     res$GR <- (res$TY - res$CY) * rootUP * areaHa
#     
#     if (res$GR < 2 * res$TC) {
#       res$TY <- res$CY
#       res$TC <- 0
#       res$GR <- 0
#       fertilizer_rates$rate <- 0
#     }
#     
#     ds <- list(rec = res, fertilizer_rates = fertilizer_rates)
#     
#   }
#   
#   return(ds)
#   
# }


