

# setwd("/home/akilimo/projects/akilimo_recommendation/ScriptPackage/AKILIMOFun")

#' @name : getWMrecommendations
#' @title : Function to obtain recommendations on land clearing (slashing and spraying) based on decision tree in the paper-based tool
#' @param fallowType : Categorical: c("bush", "broad_leaves", "grass", "none"), type of fallow prior to land clearing
#' @param fallowHeight : Categorical: c(NA, 100, 150, 200), height of the fallow prior to clearing
#' @param fallowGreen Logical: c(TRUE, FALSE), indicating if the fallow is lush, fresh and green (TRUE) or withered, dry or dead (FALSE)
#' @param problemWeeds Logical:  c(TRUE, FALSE), indicating the presence of any problem weeds that need to be controlled by herbicide (Tithonia, Imperata,...)
#'
#' @return : dataframe with recommendations on whether to slash and/or to spray.
#' @export
getWMrecommendations <- function(fallowType = c(NA, "bush", "broad_leaves", "grass", "none"),
                                 fallowHeight = c(NA, 100, 150, 200),
                                 fallowGreen = c(NA, TRUE, FALSE),
                                 problemWeeds = c(NA, TRUE, FALSE)){

  slash <- ifelse(fallowType == "bush" & fallowHeight > 100 |
                    fallowType == "broad_leaves" & fallowGreen==FALSE |
                    fallowType == "broad_leaves" & fallowGreen==TRUE & fallowHeight > 150 |
                    fallowType == "grass" & fallowHeight > 150,
                  TRUE, FALSE)

  spray <- ifelse(fallowType == "bush" & fallowHeight <= 100|
                    fallowType == "broad_leaves" & fallowGreen==TRUE & fallowHeight <= 150 |
                    fallowType == "grass" |
                    fallowType == "none" & problemWeeds==TRUE,
                  TRUE, FALSE)

  ds <- data.frame(operation=c("slash", "spray"), rec=c(slash, spray))

  return(ds)

}




#' @name  :   Function to obtain tillage recommendations (step 4 of 6 steps).
#' @title : Function to obtain tillage recommendations (step 4 of 6 steps). Function to obtain recommendations on ploughing and ridging. Returns a dataframe with all possible combinations of
#             ploughing (none, manual, tractor) and ridging (none, manual, tractor), ordered by decreasing net returns and increasing
#             tillage intensity (riding then ploughing)
#' @param areaHa farm size
#' @param costLMO a data frame with operation, method of land preparatin, cost and area
#' @param ploughing Logical, c(NA, TRUE, FALSE), indicating if the user conducts a ploughing operation in current practice, NA if PP != TRUE
#' @param ridging  Logical, c(NA, TRUE, FALSE), indicating if the user conducts a harrowing operation in current practice, NA if PP != TRUE
#' @param method_ploughing Categorical: c("manual", "tractor", "N/A"), method of ploughing currently applied by the farmer. Note: "N/A" = not applicable becasue nPlough=0
#' @param method_ridging Categorical: c("manual", "tractor", "N/A"), method of ridging currently applied by the farmer. Note: "N/A" = not applicable becasue ridges=FALSE
#' @param FCY Farmer-reported current yield, in tonnes FM per ha (optional, default value = 11)
#' @param rootUP root price
#' @return:   dataframe with cost benefit for various combinations of ploughing and ridging.
#' @export
getPPrecommendations <- function(areaHa,
                                 costLMO,
                                 ploughing, #select one
                                 ridging, #select one,
                                 method_ploughing, #select one
                                 method_ridging, #select one
                                 FCY,
                                 rootUP){

  #creating ploughing and ridging scenarios
  ds <- expand.grid(method_ploughing=c("N/A", "manual", "tractor"), method_ridging=c("N/A", "manual", "tractor"))
  ds$ploughing <- ifelse(ds$method_ploughing=="N/A", FALSE, TRUE)
  ds$ridging <- ifelse(ds$method_ridging=="N/A", FALSE, TRUE)
  ds$cost_ploughing <- ifelse(ds$method_ploughing=="N/A", 0,
                              ifelse(ds$method_ploughing=="manual",
                                     costLMO[costLMO$operation=="ploughing" & costLMO$method=="manual", ]$costHa,
                                     costLMO[costLMO$operation=="ploughing" & costLMO$method=="tractor",]$costHa))
  ds$cost_ridging <- ifelse(ds$method_ridging=="N/A", 0,
                            ifelse(ds$method_ridging=="manual",
                                   costLMO[costLMO$operation=="ridging" & costLMO$method=="manual", ]$costHa,
                                   costLMO[costLMO$operation=="ridging" & costLMO$method=="tractor",]$costHa))
  ds <- na.omit(ds)
  #adding cost saving for weeding
  ds$cost_weeding <- ifelse(ds$ridging, -costLMO[costLMO$operation=="weeding1",]$costHa, 0)

  #adding expected yields
  yd <- expand.grid(ploughing=c(FALSE, TRUE), ridging=c(TRUE,FALSE), YL=c("low", "high"))
  yd$RY <- c(rep(10, 4), 20, 25, 15, 22)
  yd <- yd[yd$YL==ifelse(FCY<12.5, "low", "high"),]
  ds <- merge(ds, yd)
  ds$RP <- ds$RY * areaHa

  #calculating total cost, gross and net revenue
  ds$TC <- (ds$cost_ploughing + ds$cost_ridging + ds$cost_weeding) * areaHa
  ds$GR <- ds$RP * rootUP
  ds$NR <- ds$GR - ds$TC

  ds <- subset(ds, select=-c(cost_ploughing, cost_ridging, cost_weeding, YL, RY))
  ds <- ds[order(-ds$NR, ds$ridging, ds$ploughing),] #order by decreasing net revenue, increasing ridging and increasing ploughing so that recommendation is first row

  #comparing to current practice
  ds$CP  <- ifelse(ds$ploughing==ploughing & ds$method_ploughing==method_ploughing & ds$ridging==ridging & ds$method_ridging==method_ridging, TRUE, FALSE)
  ds$dTC <- ds$TC - ds[ds$CP==TRUE,]$TC
  ds$dRP <- ds$RP - ds[ds$CP==TRUE,]$RP
  ds$dGR <- ds$GR - ds[ds$CP==TRUE,]$GR
  ds$dNR <- ds$NR - ds[ds$CP==TRUE,]$NR

  return(ds)

}



#' @title:   Function to obtain recommendations on cassava-maize intercropping.
#' @name: Function to obtain recommendations on cassava-maize intercropping.
#             Returns (i) a 1-row dataframe cost-benefit parameters (extra yield, cost and net revenue, and whether to apply
#             fertilizer and to plant maize at high density, and why (not)) , and (ii) a data.frame with types of fertilizer and rates to apply (zeros included).
#' @param areaHa farm size
#' @param CMP c(1,2,3,4,5),  Current maize performance, score on a scale of 1 (very yellow and stunted) .. 5 (tall and dark green), NA if IC != TRUE and FR != TRUE, or NA if the user does not know (NA = default)
#' @param cobUP maize cob price
#' @param fertilizers data frame with fertilizer type, NPK and price
#' @param riskAtt c(0, 1, 2): Risk attitude of the farmer, with 0 being very risk-averse (low income farmers who cannot afford to loose on investment), 1 = risk-neutral and 2 = risk-loving (higher income farmers willing to take their chances for higher net returns)
#'
#' @return:     list of 2 dataframes: (i) cost benefit analysis for most profitable system, and (ii) fertilizer rates to apply.
#' @export
getICrecommendations <- function(areaHa = 1,
                                 CMP = 1:5,
                                 cobUP,
                                 fertilizers,
                                 riskAtt = c(0, 1, 2)){


  #calculating expected yield increase from fertilizer
  maizeY <- data.frame(CMP=1:5,
                       dY=c(0, 6500, 4000, 2500, 0))

  dMY <- maizeY[maizeY$CMP == CMP,]$dY
  dMP <- dMY * areaHa #extra maize production for the area of the field

  #extra gross revenue from fertilizer
  dGR <- dMP * cobUP

  if(dGR==0){
    reason_F <- ifelse(CMP==1, "of low soil fertility", "of high soil fertility")
  }else{
    #calculating fertilizer requirement
    E <- t(data.matrix(fertilizers[,2:4]))
    F <- c(91, 21, 37.5) #ideally 2 bags of urea + 6 bags of NPK15:15:15
    G <- diag(nrow(fertilizers))
    H <- rep(0, nrow(fertilizers))
    Cost <- fertilizers$price

    #calculating fertilizer recommendation and total cost of fertilizer
    FRATE <- limSolve::linp(E, F, G, H, Cost)$X
    FRATE[FRATE<25] <- 0 #dropping all rates less than 25 kg/ha
    FRATE <- FRATE * areaHa #adjusting to field area

    #calculating total cost
    dTC <- c(FRATE %*% fertilizers$price)

    #evaluating if a solution was found
    if(dTC==0){
      dGR <- 0
      dMP <- 0
      reason_F <- "appropriate fertilizer is not available"
    }else{
      reason_F <- "appropriate fertilizer is available"
    }
  }

  #net revenue increase from fertilizer
  dNR <- dGR - dTC

  #minimal required net revenue increase from fertilizer needed (taking into account risk attitude of user)
  dNRmin <- dTC * ifelse(riskAtt == 0, 1.8, ifelse(riskAtt == 1, 1, 0.2))

  #check profitability of fertilizer use
  if(dNR > dNRmin){
    rec_F <- TRUE
    reason_F <- "fertilizer use is sufficiently profitable"
  }else{
    dMP <- 0
    dTC <- 0
    dGR <- 0
    dNR <- 0
    FRATE <- FRATE * 0
    rec_F <- FALSE
    reason_F <- "fertilizer use is not sufficiently profitable"
  }

  #recommendation on high density maize planting
  rec_D <- ifelse(rec_F == TRUE | CMP == 5, TRUE, FALSE)
  reason_D <- ifelse(rec_F == TRUE, "fertilizer use is recommended", ifelse(CMP==5, "of high soil fertility", NA))


  #output
  if(!is.na(reason_D)){
    rec <- data.frame(dMP=dMP, #extra maize production expected (in nr of cobs)
                      dNR=dNR, #net revenue increase from fertilizer use (in local currency)
                      dTC=dTC, #extra cost for fertilizer use (in local currency)
                      rec_F=rec_F, #TRUE or FALSE indicating if fertilizer application is recommended
                      rec_D=rec_D, #TRUE or FALSE indicating if high density maize planting is recommended
                      reason_F=reason_F, #reason why fertilizer application is not recommended
                      reason_D=reason_D)  #reason why high maize density is recommended


  }else{
    rec <- data.frame(dMP=dMP, #extra maize production expected (in nr of cobs)
                      dNR=dNR, #net revenue increase from fertilizer use (in local currency)
                      dTC=dTC, #extra cost for fertilizer use (in local currency)
                      rec_F=rec_F, #TRUE or FALSE indicating if fertilizer application is recommended
                      rec_D=rec_D, #TRUE or FALSE indicating if high density maize planting is recommended
                      reason_F=reason_F) #reason why fertilizer application is not recommended


  }

  fertilizer_rates <- data.frame(type=fertilizers$type, rate=FRATE) #fertilizer rates to apply
  fertilizer_rates <- fertilizer_rates[fertilizer_rates$rate > 0,]

  return(list(rec=rec,
              fertilizer_rates=fertilizer_rates))

}


#' @title : get advice for SP
#' @name : getSPrecommendations
#' @param areaHa famr size
#' @param country NG or TZ
#' @param lat latitude
#' @param lon longitude
#' @param PD planting date
#' @param HD harvest date
#' @param PD_window planting window
#' @param HD_window harvest window
#' @param saleSF Logical, c(NA, TRUE, FALSE), indicating if the user is selling roots to a registered starch factory at factory-fixed prices
#' @param nameSF c(NA, "AlliedAtlanticDistilleries", "MatnaStarch", "PsaltryMarketers", "PsaltryOutgrowers",	"Greentech", "ThaiFarm", "FJS"), Name of starch factory where roots will be sold, NA if saleSF = FALSE
#' @param FCY Farmer-reported current yield, in tonnes FM per ha (optional, default value = 11)
#' @param rootUP root price
#' @param rootUP_m1 root price a month before harvest
#' @param rootUP_m2 root price two months before harvest
#' @param rootUP_p1 root price a month after harvest
#' @param rootUP_p2 root price two months after harvest
#'
#' @export
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
                                 rootUP_p2){

  #rounding lat and lon to centroid of 5x5km pixel
  latr <- as.factor(floor(lat*10)/10 + ifelse(lat-(floor(lat*10)/10) < 0.05, 0.025, 0.075))
  lonr <- as.factor(floor(lon*10)/10 + ifelse(lat-(floor(lon*10)/10) < 0.05, 0.025, 0.075))
  latr <- as.numeric(levels(latr))
  lonr <- as.numeric(levels(lonr))


  SoilData_fcy1 <- readRDS("SoilData_fcy1.RDS")
  SoilData <- SoilData_fcy1[SoilData_fcy1$long == lonr & SoilData_fcy1$lat == latr, ]


  if(country == "NG"){
    WLY_15M <- readRDS("Nigeria_WLY_LINTUL_2020_Server.RDS")
  }else{
    WLY_15M <- readRDS("Tanzania_WLY_LINTUL_2020_Server.RDS")
  }

  latlon <- paste(latr, lonr, sep="_")

  WLYDataLintul <- WLY_15M[WLY_15M$location == latlon, ] ##WLY_15M[WLY_15M$long == lonr & WLY_15M$lat == latr, ]
  WLY_CY <- NULL
  if(nrow(WLYDataLintul)>0){
    WLYDataLintul <- merge(WLYDataLintul, data.frame(daysOnField = seq(235, 455, 7), haw = 34:65), by="daysOnField")
    for(k in 1:nrow(WLYDataLintul)){
      #print(k)
      wlyd <- WLYDataLintul[k, ]
      if(!is.na(SoilData$soilN)){
        wlyd$Current_Yield <- QUEFTS_WLY_CY(SoilData=SoilData, country=country, wlyd=wlyd) # in kg/ha dry
        WLY_CY <- rbind(WLY_CY, wlyd)
      }
    }
  }

  TRNS <- read.csv("translations_TEST.csv",  stringsAsFactors = FALSE)
  frnotrec_ng <- gsub(pattern = "\"",replacement = "",TRNS$frnotrec[1]) ;frnotrec_tz <- gsub(pattern = "\"",replacement = "",TRNS$frnotrec[2])



  if(is.null(WLY_CY)){
    ds <- NULL
    if(country == "NG"){
      return("We do not have fertilizer recommendation for your location because your location is out of the recommendation domain AKILIMO is currently serving.")
    }else{
      return("Hatuna mapendekezo yoyote kwa eneo lako kwa sababu eneo lako liko nje la eneo ambalo AKILIMO linafanya kazi kwa sasa")
    }

  }else{
    yld <- unique(data.frame(plw = WLY_CY$PlweekNr, haw =WLY_CY$haw, CY = WLY_CY$Current_Yield, WY= WLY_CY$water_limited_yield)) ## is still dry weight
    yld$CY <- round(yld$CY/1000, digits = 0)
    yld$WY <- round(yld$WY/1000, digits = 0)

    #constituting df with yields within relevant planting and harvest windows
    ds <- expand.grid(rPWnr = seq((-4*PD_window), (4*PD_window), by=2),
                      rHWnr = seq((-4*HD_window), (4*HD_window), by=2))
    ds$PD <- as.Date(PD) + ds$rPWnr*7
    ds$HD <- as.Date(HD) + ds$rHWnr*7
    ds$plw <- as.numeric(format(ds$PD, format = "%W"))+1
    ds$haw <- round(as.numeric(ds$HD - ds$PD)/7)
    ds <- merge(ds, yld)

    for(k in 1: nrow(ds)){
      ds$RFCY[k] <- getRFY(HD = ds$HD[k], RDY = ds$CY[k], country = country)
      ds$RFWY[k] <- getRFY(HD = ds$HD[k], RDY = ds$WY[k], country = country)
    }

    #scaling predicted yield based on farmer-reported current yield relative to modelled CY and WY:
    ds$RY <- (ds$RFWY - ds$RFCY) / (13.5 - 1.5) / 2.5 * (FCY - 1.5 * 2.5) + ds$RFCY
    ds$RP <- ds$RY * areaHa

    #If selling to a starch factory: rootUP is determined by starch content of roots
    if(saleSF){

      ds$SC <- 0.75 * ds$WY / ds$RFWY * 100
      #SF <- read.csv("E:/03-projects/ACAI/ODK briefcase storage/created forms/DSTs/media_SPHS/starchPrices.csv")
      SF <- read.csv("starchPrices.csv")
      #SF <- read.csv("D:/ACAI_Wrapper/cloud_compute/starchPrices.csv")
      SF <- SF[SF$starchFactory == nameSF,]
      price <- NULL
      for(i in 1:nrow(ds)){
        price <- c(price, max(SF[SF$minStarch<ds[i,]$SC,]$price))
      }
      ds$rootUP <- price
      ds <- subset(ds, select=-SC)
      #If not selling to a starch factory, user needs to specify rootUP across the harvest window
    }else{
      dp <- data.frame(rHWnr = c(-8, -4, 0, 4, 8),
                       rootUP = c(rootUP_m2, rootUP_m1, rootUP, rootUP_p1, rootUP_p2))
      dpm <- suppressWarnings(loess(rootUP ~ rHWnr, data=dp))
      dpp <- data.frame(rHWnr = seq((-4*HD_window), (4*HD_window), by=2),
                        rootUP = predict(dpm, data.frame(rHWnr=seq((-4*HD_window), (4*HD_window), by=2))))
      ds <- merge(ds, dpp)
    }

    ds$GR <- ds$RP * ds$rootUP
    ds$CP <- ifelse(ds$rPWnr==0 & ds$rHWnr==0, TRUE, FALSE)
    ds$dGR <- ds$GR - ds[ds$CP==TRUE,]$GR

    #sort by decreasing GR, increasing HWnr, decreasing PWnr
    #recommendation is highest GR, earliest harvesting, latest planting combination
    ds <- ds[order(-ds$dGR, ds$rHWnr, -ds$rPWnr),]
    write.csv(ds, "SP_rec.csv", row.names = FALSE)
    return(ds)
  }

}
#' @name : fertilizerFunc
#' @title : function to creat a data frame with fertilizers
#' @param NPK201216available : TRUE or FALSE
#' @param NPK201216CostperBag : in local currency price for the bag wt
#' @param NPK201216BagWt : c(25,50,75,100)
#' @param ureaavailable : TRUE or FALSE
#' @param ureaCostperBag : in local currency price for the bag wt
#' @param ureaBagWt : c(25,50,75,100)
#' @param MOPavailable : TRUE or FALSE
#' @param MOPCostperBag : in local currency price for the bag wt
#' @param MOPBagWt : c(25,50,75,100)
#' @param DAPavailable : TRUE or FALSE
#' @param DAPCostperBag : in local currency price for the bag wt
#' @param DAPBagWt : c(25,50,75,100)
#' @param NPK201010available : TRUE or FALSE
#' @param NPK201010CostperBag : in local currency price for the bag wt
#' @param NPK201010BagWt : c(25,50,75,100)
#' @param NPK151515available : TRUE or FALSE
#' @param NPK151515CostperBag : in local currency price for the bag wt
#' @param NPK151515BagWt : c(25,50,75,100)
#' @param TSPavailable : TRUE or FALSE
#' @param TSPCostperBag : in local currency price for the bag wt
#' @param TSPBagWt : c(25,50,75,100)
#' @param NPK171717available : TRUE or FALSE
#' @param NPK171717CostperBag : in local currency price for the bag wt
#' @param NPK171717BagWt : c(25,50,75,100)
#' @param Nafakaavailable : TRUE or FALSE
#' @param NafakaCostperBag : in local currency price for the bag wt
#' @param NafakaBagWt : c(25,50,75,100)
#' @param CANavailable : TRUE or FALSE
#' @param CANCostperBag : in local currency price for the bag wt
#' @param CANBagWt : c(25,50,75,100)
#' @param SSPavailable : TRUE or FALSE
#' @param SSPCostperBag : in local currency price for the bag wt
#' @param SSPBagWt : c(25,50,75,100)
#' @param YaraMila_UNIKavailable : TRUE or FALSE
#' @param YaraMila_UNIKCostperBag : in local currency price for the bag wt
#' @param YaraMila_UNIKBagWt : c(25,50,75,100)
#' @param newFert1name : any name used by user
#' @param newFert1N_cont : the N content (in proportion)
#' @param newFert1P2O5 : P2O5 in the fertilizer from which the P  = 0.44*P2O5
#' @param newFert1K2O : K2O in the fertilizer and K contenet = 0.83 * K2O
#' @param newFert1CostperBag : cost for a bag of fertilzer bag defined below
#' @param newFert1BagWt ; is fertilizer sold per 25,50,75,100,... kg
#' @param newFert2name  : any name used by user
#' @param newFert2N_cont: the N content (in proportion)
#' @param newFert2P2O5: P2O5 in the fertilizer from which the P  = 0.44*P2O5
#' @param newFert2K2O : K2O in the fertilizer and K contenet = 0.83 * K2O
#' @param newFert2CostperBag : cost for a bag of fertilzer bag defined below
#' @param newFert2BagWt: ; is fertilizer sold per 25,50,75,100,... kg
#' @param newFert3name : any name used by user
#' @param newFert3N_cont: the N content (in proportion)
#' @param newFert3P2O5: P2O5 in the fertilizer from which the P  = 0.44*P2O5
#' @param newFert3K2O : K2O in the fertilizer and K contenet = 0.83 * K2O
#' @param newFert3CostperBag : cost for a bag of fertilzer bag defined below
#' @param newFert3BagWt ; is fertilizer sold per 25,50,75,100,... kg
#' @param newFert4name : any name used by user
#' @param newFert4N_cont: the N content (in proportion)
#' @param newFert4P2O5: P2O5 in the fertilizer from which the P  = 0.44*P2O5
#' @param newFert4K2O : K2O in the fertilizer and K contenet = 0.83 * K2O
#' @param newFert4CostperBag : cost for a bag of fertilzer bag defined below
#' @param newFert4BagWt ; is fertilizer sold per 25,50,75,100,... kg
#' @param newFert5name : any name used by user
#' @param newFert5N_cont: the N content (in proportion)
#' @param newFert5P2O5: P2O5 in the fertilizer from which the P  = 0.44*P2O5
#' @param newFert5K2O : K2O in the fertilizer and K contenet = 0.83 * K2O
#' @param newFert5CostperBag : cost for a bag of fertilzer bag defined below
#' @param newFert5BagWt ; is fertilizer sold per 25,50,75,100,... kg
#' @param country MG or TZ
#'
#' @return: data frame with (type, N_cont, P_cont, K_cont, price) The NPK is elemental concentration and price is per kg of fertilizer
#' @export
fertilizerFunc <- function(NPK201216available= TRUE, NPK201216CostperBag =NA, NPK201216BagWt=50,
                           ureaavailable=TRUE, ureaCostperBag=NA,ureaBagWt=50,
                           MOPavailable=TRUE, MOPCostperBag=NA, MOPBagWt=50,
                           DAPavailable=TRUE, DAPCostperBag=NA, DAPBagWt=50,
                           NPK201010available=TRUE, NPK201010CostperBag=NA, NPK201010BagWt=50,
                           NPK151515available=TRUE, NPK151515CostperBag=NA, NPK151515BagWt=50,
                           TSPavailable=TRUE, TSPCostperBag=NA, TSPBagWt=50,
                           NPK171717available=FALSE, NPK171717CostperBag=NA, NPK171717BagWt=50,
                           Nafakaavailable=TRUE, NafakaCostperBag=NA, NafakaBagWt=50,
                           CANavailable=TRUE, CANCostperBag=NA, CANBagWt=50,
                           SSPavailable=TRUE, SSPCostperBag=NA, SSPBagWt=50,
                           YaraMila_UNIKavailable=TRUE, YaraMila_UNIKCostperBag=NA, YaraMila_UNIKBagWt=50,
                           newFert1name=NA, newFert1N_cont=NA, newFert1P2O5=NA, newFert1K2O=NA, newFert1CostperBag=NA, newFert1BagWt=NA,
                           newFert2name=NA, newFert2N_cont=NA, newFert2P2O5=NA, newFert2K2O=NA, newFert2CostperBag=NA, newFert2BagWt=NA,
                           newFert3name=NA, newFert3N_cont=NA, newFert3P2O5=NA, newFert3K2O=NA, newFert3CostperBag=NA, newFert3BagWt=NA,
                           newFert4name=NA, newFert4N_cont=NA, newFert4P2O5=NA, newFert4K2O=NA, newFert4CostperBag=NA, newFert4BagWt=NA,
                           newFert5name=NA, newFert5N_cont=NA, newFert5P2O5=NA, newFert5K2O=NA, newFert5CostperBag=NA, newFert5BagWt=NA,country){
  if(country == "NG"){
    if(ureaCostperBag == 0) {ureaCostperBag <- 7500} else {ureaCostperBag <- as.numeric(ureaCostperBag)}
    if(MOPCostperBag == 0) {MOPCostperBag <- 13500}else {MOPCostperBag <- as.numeric(MOPCostperBag)}
    if(DAPCostperBag == 0) {DAPCostperBag <- 13250}else {DAPCostperBag <- as.numeric(DAPCostperBag)}
    if(NPK201010CostperBag == 0) {NPK201010CostperBag <- 7200}else {NPK201010CostperBag <- as.numeric(NPK201010CostperBag)}
    if(NPK151515CostperBag == 0) {NPK151515CostperBag <- 8500}else {NPK151515CostperBag <- as.numeric(NPK151515CostperBag)}
    if(TSPCostperBag == 0) {TSPCostperBag <- 13250}else {TSPCostperBag <- as.numeric(TSPCostperBag)}
    if(NPK171717CostperBag == 0) {NPK171717CostperBag <- 7800}else {NPK171717CostperBag <- as.numeric(NPK171717CostperBag)}
    if(CANCostperBag == 0) {CANCostperBag <- 7500}else {CANCostperBag <- as.numeric(CANCostperBag)}
    if(SSPCostperBag == 0) {SSPCostperBag <- 22364}else {SSPCostperBag <- as.numeric(SSPCostperBag)}
    if (NPK201216CostperBag == 0) { NPK201216CostperBag <- 8000 }else { NPK201216CostperBag <- as.numeric(NPK201216CostperBag) }
  }else{
    if(ureaCostperBag == 0) {ureaCostperBag <- 58000}else {ureaCostperBag <- as.numeric(ureaCostperBag)}##65000
    if(MOPCostperBag == 0) {MOPCostperBag <- 67000}else {MOPCostperBag <- as.numeric(MOPCostperBag)}##120000
    if(DAPCostperBag == 0)  {DAPCostperBag <- 60000}else {DAPCostperBag <- as.numeric(DAPCostperBag)}##85000
    if(NPK201010CostperBag == 0) {NPK201010CostperBag <- 68000}else {NPK201010CostperBag <- as.numeric(NPK201010CostperBag)}
    if(TSPCostperBag == 0) {TSPCostperBag <- 64000}else {TSPCostperBag <- as.numeric(TSPCostperBag)}#80000
    if(NPK171717CostperBag == 0) {NPK171717CostperBag <- 63000}else {NPK171717CostperBag <- as.numeric(NPK171717CostperBag)}##61000
    if(CANCostperBag == 0) {CANCostperBag <- 53000}else {CANCostperBag <- as.numeric(CANCostperBag)}#65000
    if(YaraMila_UNIKCostperBag == 0) {YaraMila_UNIKCostperBag <- 60000}	else {YaraMila_UNIKCostperBag <- as.numeric(YaraMila_UNIKCostperBag)}##61000
  }

  ureaFert <- data.frame(type = 'Urea', available =ureaavailable,  N_cont = 0.46, P_cont = 0, K_cont=0, costPerBag = ureaCostperBag, bagWeight=ureaBagWt )
  MOPFert <- data.frame(type = 'MOP', available = MOPavailable, N_cont = 0.00, P_cont = 0.00, K_cont=0.50, costPerBag = MOPCostperBag, bagWeight=MOPBagWt)
  DAPFert <- data.frame(type = 'DAP',available = DAPavailable,  N_cont = 0.18, P_cont = 0.20, K_cont=0.0, costPerBag = DAPCostperBag, bagWeight=DAPBagWt)
  NPK201010Fert <- data.frame(type = 'NPK20_10_10',available = NPK201010available,  N_cont = 0.20, P_cont = 0.044, K_cont=0.083, costPerBag = NPK201010CostperBag, bagWeight=NPK201010BagWt)
  NPK151515Fert <- data.frame(type = 'NPK15_15_15',available = NPK151515available,  N_cont = 0.15, P_cont = 0.07, K_cont=0.125, costPerBag = NPK151515CostperBag, bagWeight=NPK151515BagWt)
  TSPFert <- data.frame(type = 'TSP',available = TSPavailable,  N_cont = 0.0, P_cont = 0.2, K_cont=0.0, costPerBag = TSPCostperBag, bagWeight=TSPBagWt)
  NPK171717Fert <- data.frame(type = 'NPK17_17_17',available = NPK171717available,  N_cont = 0.17, P_cont = 0.083, K_cont=0.15, costPerBag = NPK171717CostperBag, bagWeight=NPK171717BagWt)# TODO get price
  NPK201216Fert <- data.frame(type = 'NPK20_12_16', available = NPK201216available, N_cont = 0.20, P_cont = 0.052, K_cont = 0.216, costPerBag = NPK201216CostperBag, bagWeight = NPK201216BagWt)
  CANFert <- data.frame(type = 'CAN', available = CANavailable, N_cont = 0.27, P_cont = 0.00, K_cont = 0.00, costPerBag = CANCostperBag, bagWeight = CANBagWt) ## not correct value TODO check
  SSPFert <- data.frame(type = 'SSP', available = SSPavailable, N_cont = 0.00, P_cont = 0.15, K_cont = 0.00, costPerBag = SSPCostperBag, bagWeight = SSPBagWt) ## not correct value TODO check
  YaraMila_UNIKFert <- data.frame(type = 'YaraMila_UNIK', available = YaraMila_UNIKavailable, N_cont = 0.17, P_cont = 0.17, K_cont=0.17, costPerBag = YaraMila_UNIKCostperBag, bagWeight=YaraMila_UNIKBagWt)##


  if(country == "NG"){
    fd_cont <- rbind(ureaFert,MOPFert , DAPFert,CANFert,NPK171717Fert,NPK151515Fert, NPK201010Fert,TSPFert,SSPFert,NPK201216Fert)
  }else{
    fd_cont <- rbind(ureaFert,MOPFert , DAPFert, CANFert, NPK171717Fert, NPK151515Fert, NPK201010Fert,TSPFert,SSPFert,YaraMila_UNIKFert)
  }


  fd_cont <- droplevels(fd_cont[fd_cont$available == TRUE, ])
  fd_cont$costPerBag <- as.numeric(fd_cont$costPerBag)
  fd_cont$price <- fd_cont$costPerBag / fd_cont$bagWeight
  fd_cont <- subset(fd_cont, select=-c(available))

  if(any(newFert1name!="NA"|newFert2name!="NA"|newFert3name!="NA"|newFert4name!="NA"|newFert5name!="NA")){
    OtherFertilizers <- data.frame(expand.grid(type = c(newFert1name, newFert2name, newFert3name, newFert4name, newFert5name)),
                                   expand.grid(N_cont=c(newFert1N_cont, newFert2N_cont, newFert3N_cont, newFert4N_cont, newFert5N_cont)),
                                   expand.grid(P2O5_cont=c(newFert1P2O5, newFert2P2O5, newFert3P2O5, newFert4P2O5, newFert5P2O5)),
                                   expand.grid(K2O_cont=c(newFert1K2O, newFert2K2O, newFert3K2O, newFert4K2O, newFert5K2O)),
                                   expand.grid(newFertCostperBag=c(newFert1CostperBag, newFert2CostperBag, newFert3CostperBag, newFert4CostperBag, newFert5CostperBag)),
                                   expand.grid(newFertBagWt=c(newFert1BagWt, newFert2BagWt, newFert3BagWt, newFert4BagWt, newFert5BagWt)))

    OtherFertilizers <-  droplevels(OtherFertilizers[as.numeric(as.factor(OtherFertilizers$type)) ==1, ])
    OtherFertilizers$N_cont <- as.numeric(as.character(OtherFertilizers$N_cont))
    OtherFertilizers$P2O5_cont <- as.numeric(as.character(OtherFertilizers$P2O5_cont))
    OtherFertilizers$K2O_cont <- as.numeric(as.character(OtherFertilizers$K2O_cont))
    OtherFertilizers$newFertCostperBag <- as.numeric(as.character(OtherFertilizers$newFertCostperBag))
    OtherFertilizers$newFertBagWt <- as.numeric(as.character(OtherFertilizers$newFertBagWt))


    if(nrow(OtherFertilizers)>0){
      newfert <- NULL
      for(k in 1:nrow(OtherFertilizers)){
        OF <- OtherFertilizers[k, ]

        if(OF$N_cont == 0){
          N_cont <- 0
        }else{
          N_cont <- OF$N_cont
        }

        if(OF$P2O5_cont == 0){
          P_cont <- 0
        }else{
          P_cont <- round(0.44/as.numeric(OF$P2O5_cont),digits=3)
        }

        if(OF$K2O_cont == 0){
          K_cont <- 0
        }else{
          K_cont <- round(0.83/as.numeric(OF$K2O_cont),digits=3)
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



#'
#' @name: is a function to convert root DM yield into root fresh matter yield (RFY)
#' @title: Function to predict root FM yield based on date of harvest and country, using data from gravimetric starch measurements conducted across ACAI trials.
#' @param HD harvest date (Date format)
#' @param RDY root dry matter yield (user's units)
#' @param country c("NG", "TZ")
#' @return:     RFY: root fresh yield in the same units as root DM yield input
#'
#' @export
getRFY <- function(HD,
                   RDY,
                   country){
  d  <- as.numeric(strftime(HD, format = "%j"))
  fd <- read.csv("fd2.csv") #data.frame with day of the year (dayNr = [1..366]) and %DM (DMCont = [0..100], by country)
  DC <- merge(data.frame(dayNr=d), fd[fd$country==country,], sort=FALSE)$DMCont
  RFY <- RDY / DC * 100
  return(RFY)
}




#' @name: is a Function to convert root FM yield into root dry matter yield (RDY): user define CY in FM in ton/ha, QUEFTS require Cy in DM kg/ha
#' @title: Function to predict root FM yield based on date of harvest and country, using data from gravimetric starch measurements conducted across ACAI trials.
#'
#' @param HD harvest date (Date format)#'
#' @param RFY root dry matter yield (user's units)
#' @param country  c("NG", "TZ")
#' @return:     RDY: root dry yield in the same units as root FM yield input
#' @export
getRDY <- function(HD, RFY, country){
  if(HD > 366){
    HD <- HD - 366
  }
  d <- HD
  fd <- read.csv("fd2.csv")
  DC <- merge(data.frame(dayNr=d), fd[fd$country==country,], sort=FALSE)$DMCont
  RDY <- (RFY * DC)/100
  return(RDY)
}

#' @name : run_Optim_NG2
#' @title : get optimized fertilizer rate, target yield for the recommended rate and net revenue given cost and investment
#' @param rootUP root price
#' @param QID soil data
#' @param fertilizer fertilizer data frame
#' @param invest capital to invest on fertlizer
#' @param plDate planting date
#' @param WLYData waterlimited yield
#' @param lat latitude
#' @param lon longitude
#' @param areaHa farm size
#' @param HD harvest date
#' @param WLY water limit yield
#' @param DCY dry current yield
#' @param country NG and TZ
#'
#' @return: a data frame with optimized fertilizer rates, TY and prices
#' @export
run_Optim_NG2 <- function(rootUP, QID, fertilizer, invest, plDate, WLYData, lat, lon, areaHa, HD, WLY, DCY, country){
  ## input of CY and WLY are in dry wt in KG/ha
  QID$water_limited_yield <- WLY
  initial <- rep(0, nrow(fertilizer))
  lowerST <- rep(0, nrow(fertilizer))

  ## both CY and TY should be changed to user land size in ton/ha and fresh wt
  CY_user <- ((getRFY(HD = HD, RDY =  DCY , country = country))/1000)  * areaHa
  WLY_user <- ((getRFY(HD = HD, RDY =  WLY , country = country))/1000)  * areaHa


  FR <- optim(par=initial, fn=optim_NR, lower=lowerST, method = "L-BFGS-B", control=list(fnscale=-1), rootUP=rootUP,
              QID=QID, CY=DCY, fertilizer=fertilizer, invest=invest, HD=HD, country = country)$par

  if(all(FR == 0)){
    return(data.frame(lat=lat, lon=lon, plDate, N=0, P=0, K= 0,WLY=WLY_user, CurrentY = CY_user, TargetY = CY_user,  TC =0, NR=0))
  }else{

    fertilizer$FR <- FR

    ## NPK rate for ha of land
    N <- as.vector(FR %*% fertilizer$N_cont)
    P <- as.vector(FR %*% fertilizer$P_cont)
    K <- as.vector(FR %*% fertilizer$K_cont)
    rec <- c(N, P, K)

    ## NPK rate for user land size
    NPK_user <- rec * areaHa

    ## TY for ha of land
    TY <- QUEFTS1_Pedotransfer(QID, rec)	# Yield possible at recommended NPK in kg/ha dry wt.

    ## both CY and TY should be changed to user land size in ton/ha and fresh wt
    TY_user <- ((getRFY(HD = HD, RDY = TY, country = country))/1000) * areaHa


    ## total cost per ha
    TC <- (sum(FR * fertilizer$price))* areaHa

    ## net revenue on the users land size
    GR <- (TY_user - CY_user) * rootUP
    NR <- round(GR - TC, digits=0)

    ## reporting the recommended fertilizers
    Recomfr <- fertilizer[fertilizer$FR > 0, ]
    Recomfr$FR <- Recomfr$FR * areaHa
    Recomfr_wide <- tidyr::spread(Recomfr[, c('type', 'FR')], type, FR)

    d1 <- data.frame(lat=lat, lon=lon, plDate, N=NPK_user[1], P=NPK_user[2], K= NPK_user[3],
                     WLY=WLY_user, CurrentY = CY_user, TargetY = TY_user,  TC =TC, NR=NR)
    d2 <- cbind(d1, Recomfr_wide)
    row.names(d2) <- NULL
    if(d2$NR <=0 | d2$TargetY <= d2$CurrentY){
      fertinfo <- subset(d2, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
      fertinfo$N <- fertinfo$P <- fertinfo$K <- fertinfo$TC <- fertinfo$NR <- 0
      fertinfo$TargetY <- fertinfo$CurrentY
      d2 <- fertinfo
    }
    return(d2)
  }

}


#' @name : optim_NR
#' @title :   Optimize the UREA, TSP and MOP needed to maximize the NR. x1, x2, x3 = Urea, MOP and Nafaka kg/ha.
#' @param fertRate : NPK rate as defined by optimization
#' @param rootUP : cassava root price
#' @param QID : soil data
#' @param CY : current yield in DM in kg/ha
#' @param fertilizer : a data frame with type, NPK content and price
#' @param invest : investment capacity
#' @param HD ; harvest date
#' @param country ; NG or TZ
#' @return: optimized fertilzer rates and yield
#' @export
optim_NR <- function(fertRate, rootUP, QID, CY, fertilizer, invest, HD, country){
  f_price <-fertilizer$price
  TC <- sum(fertRate * f_price)



  N <- as.vector(fertRate %*% fertilizer$N_cont)
  P <- as.vector(fertRate %*% fertilizer$P_cont)
  K <- as.vector(fertRate %*% fertilizer$K_cont)

  rec <- c(N,P,K)

  TotalYield <- QUEFTS1_Pedotransfer(QID, rec)

  AdditionalYield <- (getRFY(HD = HD, RDY = (TotalYield - CY), country = country))/1000

  PriceYield <- AdditionalYield * rootUP
  NetRev <- PriceYield - TC
  if (!is.na(invest) & TC > invest) {NetRev <- NetRev - (invest-TC)^2}
  return(NetRev)
}


#' @name : QUEFTS1_Pedotransfer
#' @title : computes target yield in tonnes/ha from a given NPK rate
#' @param QID a data frame containing soil NPK, WLY (kg/ha dry wt.),
#' @param rec recomended NPK rate
#' @return target yield in ton/ha dry matter
#' @author Meklit
#' @export
QUEFTS1_Pedotransfer <- function(QID, rec){
  QID$WLY <- QID$water_limited_yield

  crop_param <- cbind(NUE(HI=0.52), data.frame(rN=0, rP=0, rK=0, max_yield=QID$WLY, tolerance=0.01))	## nutrient use efficiency of the crop


  Queft_Input_Data_Var1 <- cbind(QID, crop_param)
  indata <- Queft_Input_Data_Var1[,c("lat","long" ,"WLY","aN", "dN", "aP", "dP","aK","dK", "rN", "rP", "rK", "soilN", "soilP", "soilK","max_yield", "tolerance")]

  N_rate <- rec[1]
  P_rate <- rec[2]
  K_rate <- rec[3]

  TargetYield_from_NPK <- NPK_TargetYield_forOutput(NutrUse_soilNPK=indata, N_rate, P_rate, K_rate)

  return(TargetYield_from_NPK$TargetYield)
}


#' @name : NUE
#' @title : nutrient use effciency
#' @param HI : harvest index
#' @param CmaxNroots : QUEFTS parameter
#' @param CminNroots : QUEFTS parameter
#' @param CmaxNtops : QUEFTS parameter
#' @param CminNtops : QUEFTS parameter
#' @param CmaxProots : QUEFTS parameter
#' @param CminProots : QUEFTS parameter
#' @param CmaxPtops : QUEFTS parameter
#' @param CminPtops : QUEFTS parameter
#' @param CmaxKroots : QUEFTS parameter
#' @param CminKroots : QUEFTS parameter
#' @param CmaxKtops : QUEFTS parameter
#' @param CminKtops : QUEFTS parameter
#'
#'@export
NUE <- function(HI, CmaxNroots=6.6, CminNroots=2.5, CmaxNtops=17.9, CminNtops=7.9, CmaxProots=1.5, CminProots=0.8, CmaxPtops=2.8, CminPtops=0.9,
                CmaxKroots=11, CminKroots=2.8, CmaxKtops=18.8, CminKtops=3.4 ){
  aN = round(1000 * HI/(HI * CmaxNroots + (1 - HI) * CmaxNtops), digits=0)
  dN = round(1000 * HI/(HI * CminNroots + (1 - HI) * CminNtops), digits=0)

  aP = round(1000 * HI/(HI * CmaxProots + (1 - HI) * CmaxPtops), digits=0)
  dP = round(1000 * HI/(HI * CminProots + (1 - HI) * CminPtops), digits=0)

  aK = round(1000 * HI/(HI * CmaxKroots + (1 - HI) * CmaxKtops), digits=0)
  dK = round(1000 * HI/(HI * CminKroots + (1 - HI) * CminKtops), digits=0)

  return(data.frame(aN=aN, dN=dN,aP=aP,dP=dP,aK=aK,dK=dK))

}


#' @name : NPK_TargetYield_forOutput
#' @title: using the output of function "NPK_TargetYield_forinput" and a dat frame per lon and lat for intended NPK input
#' this function calculates the yield that can be obtained for intended NPK rate.
#' @param N_rate : recommended N rate
#' @param P_rate : recommended P rate
#' @param K_rate : recommended K rate
#' @param NutrUse_soilNPK : Crop nutrient requirement
#' @author ACAI
#' @export
NPK_TargetYield_forOutput <- function(NutrUse_soilNPK, N_rate, P_rate, K_rate){
  NutrUse_soilNPK$N_rate <- N_rate
  NutrUse_soilNPK$P_rate <- P_rate
  NutrUse_soilNPK$K_rate <- K_rate

  ## Supply of nutrients to the crop
  NutrUse_soilNPK$SN <- NutrUse_soilNPK$N_rate + NutrUse_soilNPK$soilN
  NutrUse_soilNPK$SP <- NutrUse_soilNPK$P_rate + NutrUse_soilNPK$soilP
  NutrUse_soilNPK$SK <- NutrUse_soilNPK$K_rate + NutrUse_soilNPK$soilK

  ## Actual Uptake of nutrients: crop param + nutrient supply
  tmp <- ddply(NutrUse_soilNPK,.(lat, long), actual_uptake_tool)
  NutrUse_soilNPK <- merge(NutrUse_soilNPK, tmp, by=c("lat", "long"))

  ## max and min yield: actual uptake and crop param. min of N uptake constrianed by availability of P, K and water
  maxminY <- ddply(NutrUse_soilNPK,.(lat, long), max_min_yields_tools)
  NutrUse_soilNPK <- merge(NutrUse_soilNPK, maxminY, by=c("lat", "long"))

  ## final yield: min yield for combined uptake of 2 nutrients assuming the 3rd is not limiting, should be < WLY, and take meanof the six combinations
  Target_Yield <- ddply(NutrUse_soilNPK,.(lat, long), quefts_tools)
  TY <- data.frame(lat=Target_Yield$lat, lon=Target_Yield$long, TargetYield=Target_Yield$FinalYield)

  return(TY)
}


#' @name : actual_uptake_tool
#' @title : based on uptake of two of the three nutrients, yield is estimated being limited by the missing.
#' @param ds_supply : nutrient supply
#' @description : part of QUEFTS
#' @export
actual_uptake_tool <- function(ds_supply){
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


         return(data.frame(UN=UN, UP=UP, UK=UK))
       })
}

#' @name : nutrient_uptake
#' @title :  Nutrient uptake depends on the soil supply of the nutrient and the supply of other nutrients
#' @param S1 : soil supply for nutrient 1
#' @param S2 : soil supply for nutrient 2
#' @param d1 : QUEFTS Parameter
#' @param a1 : QUEFTS Parameter
#' @param d2 : QUEFTS Parameter
#' @param a2 : QUEFTS Parameter
#' @param r1 : QUEFTS Parameter
#' @param r2 : QUEFTS Parameter
#' @export
nutrient_uptake <- function(S1=NA, S2=NA, d1=NA, a1=NA, d2=NA, a2=NA, r1=NA, r2=NA) {
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

#' @title : get WLY limited nutrient uptake
#' @name : water_dependent_nutrient_uptake
#' @param S1 : soil supply for nutrient 1
#' @param WLY : water limited yield
#' @param d1 : QUEFTS Parameter
#' @param a1 : QUEFTS Parameter
#' @param r1 : QUEFTS Parameter
#' @export
water_dependent_nutrient_uptake <- function(S1=NA, WLY=NA, d1=NA, a1=NA, r1=NA) {
  if (S1 < r1 + WLY / d1) {
    uptakeX_givenWater = S1
  } else if (S1 > r1 + 2*WLY/a1 - WLY/d1) {
    uptakeX_givenWater = WLY / a1
  } else {
    uptakeX_givenWater = S1 - 0.25 * (S1 - r1 - WLY/d1)^2 / (WLY / a1 - WLY / d1)
  }

  return(uptakeX_givenWater)
}


#' @name : max_min_yields_tools
#' @title  based on npk uptake gives max and min yield
#' @param dss a data frame with nutrient uptake
#' @export
max_min_yields_tools <- function(dss){

  YNA <- max((dss$UN - dss$rN), 0) * dss$aN
  YND <- max((dss$UN - dss$rN), 0) * dss$dN
  YPA <- max((dss$UP - dss$rP), 0) * dss$aP
  YPD <- max((dss$UP - dss$rP), 0) * dss$dP
  YKA <- max((dss$UK - dss$rK), 0) * dss$aK
  YKD <- max((dss$UK - dss$rK), 0) * dss$dK

  return(data.frame(YNA=YNA, YND=YND, YPA=YPA, YPD=YPD, YKA=YKA, YKD=YKD))

}


#' @name : quefts_tools
#' @title : Final yield based on the combinations of nutrient uptake and minimum + maximum yields.
#' @param supply_wly:  : QUEFTS Parameter
#' @export
quefts_tools <- function(supply_wly){
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


#' @name : Rerun_25kgKa_try
#' @title : after setting fertilizer recommendation <25 kg/ha Urea, MOP or Nafaka, target yield with the remaining recommended fertilizer is  re-estimated  and
#'  total cost, gross and net revenue are re calcuated.
#' @param rootUP cassava root price
#' @param rdd has fresh wt
#' @param QID soil data
#' @param onlyFert recommended fertilizers
#' @param country NG or TZ
#' @param WLY water limited yield
#' @param DCY Current yield DM in kg/ha
#' @param HD harvest date
#' @param areaHa farm size in ha or acre
#' @param fertilizer a data frame with type and cost of fertilizers
#' @author Meklit
#' @export
Rerun_25kgKa_try <- function(rootUP, rdd, fertilizer, QID, onlyFert, country, WLY=WLY, DCY = DCY, HD=HD, areaHa=areaHa){

  QID$water_limited_yield <- WLY
  fertilizer <- merge(fertilizer, onlyFert, by='type')
  TC <- (sum(fertilizer$price %*% fertilizer$rate) ) * areaHa
  N  <- as.vector(fertilizer$rate %*% fertilizer$N_cont)
  P  <- as.vector(fertilizer$rate %*% fertilizer$P_cont)
  K  <- as.vector(fertilizer$rate %*% fertilizer$K_cont)
  rec <- c(N, P, K)

  ## NPK rate for user land size
  NPK_user <- rec * areaHa

  TY  <- QUEFTS1_Pedotransfer(QID, rec)					#dry wt yield in kg/ha
  #TY_user  <- ((getRFY(HD = as.Date(HD), RDY = TY, country = country))/1000) * areaHa
  TY_user  <- ((getRFY(HD = HD, RDY = TY, country = country))/1000) * areaHa
  CY_user  <- ((getRFY(HD = HD, RDY = DCY, country = country))/1000) * areaHa


  rdd$CurrentY <- CY_user
  rdd$TargetY <- TY_user
  rdd$TC <- TC
  rdd$NR <- ((rdd$TargetY - rdd$CurrentY)*rootUP) - rdd$TC
  rdd$N <- NPK_user[1]
  rdd$P <- NPK_user[2]
  rdd$K <- NPK_user[3]

  if(rdd$TargetY <= rdd$CurrentY){
    rdd$N <- rdd$P <- rdd$K <- rdd$TC <- rdd$NR <- 0
    rdd$TargetY <- CY_user
  }

  if(rdd$NR <=0 | rdd$TargetY <= rdd$CurrentY){
    fertinfo <- subset(rdd, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
    fertinfo$N <- fertinfo$P <- fertinfo$K <- fertinfo$TC <- fertinfo$NR <- 0
    fertinfo$TargetY <- fertinfo$CurrentY
    rdd <- fertinfo
  }

  return(rdd)
}


#' @name : NRabove18Cost
#' @title   see if profit is > (0.18 * total cost) + total cost
#' @param ds : data frame with fertilzer rate cost and net revenue
#' @export
NRabove18Cost <- function(ds){
  #if(ds$NR <= (ds$TC + (ds$TC * 0.18))){
  if(ds$NR < ds$TC * 0.18){
    fertRecom <- subset(ds, select = c(lat,lon, plDate, N, P, K, WLY, CurrentY,TargetY, TC, NR))
    fertRecom$N <- fertRecom$P <- fertRecom$K <- fertRecom$TC <- fertRecom$NR <- 0
    fertRecom$TargetY <- fertRecom$CurrentY

    onlyFert <- subset(ds, select = -c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR,harvestDate))
    row.names(onlyFert) <- NULL
    for(j in 1:ncol(onlyFert)){
      onlyFert[,j] <-0
    }
    fertRecom <- cbind(fertRecom, onlyFert)
    ds <- fertRecom
  }
  row.names(ds) <- NULL
  return(ds)
}

#' @name : yield_nutrients_combined
#' @title : Yield calculated based on the combined uptake of 2 nutrients, while taking into account the availability of the third nutrient.
#' @param U1 : QUEFTS Parameter
#' @param d1 : QUEFTS Parameter
#' @param a1 : QUEFTS Parameter
#' @param Y2A : QUEFTS Parameter
#' @param Y2D : QUEFTS Parameter
#' @param Y3D : QUEFTS Parameter
#' @param r1 : QUEFTS Parameter
#' @export
yield_nutrients_combined <- function(U1=NA, d1=NA, a1=NA, Y2A=NA, Y2D=NA, Y3D=NA, r1=NA){
  # Determine which nutrient limited yield is lowest.
  YxD = min(Y2D, Y3D)
  # If the uptake of one of the nutrients, and therefore the yield associated with that
  # nutrient, is zero the overall yield is also zero.
  if (U1 == 0 || YxD == 0) {
    Y12 = 0
  }else{
    Y12 = Y2A + (2 * (YxD - Y2A) * (U1 - r1 - Y2A / d1)) / (YxD / a1 - Y2A / d1) -
      (YxD - Y2A) * (U1 - r1 - Y2A / d1)^2 / (YxD / a1 - Y2A / d1)^2
  }
  # Return the calculated yield based on the uptake of nutrients 1 and 2
  return(Y12)
}


#' @name : final_yield_tools
#' @title : gives final possible yield
#' @param Uptake_Yield: nutrient uptake and yield estimates
#' @export
final_yield_tools <- function(Uptake_Yield){
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
#' @name : sendEmailReport
#' @title : function to send mail
#' @param userEmail: user email
#' @param FR : TRUE or FALSE, based on if FR advice is requested used to generate the markdown doc
#' @param IC : TRUE or FALSE, based on if IC advice is requested used to generate the markdown doc
#' @param PP : TRUE or FALSE, based on if PP advice is requested used to generate the markdown doc
#' @param SP : TRUE or FALSE, based on if SP advice is requested used to generate the markdown doc
#' @param FRrecom : TRUE or FALSE, based on if FR advice is requested used to bundel together the pdf generated
#' @param ICrecom : TRUE or FALSE, based on if FR advice is requested used to bundel together the pdf generated
#' @param country : TRUE or FALSE, based on if FR advice is requested used to bundel together the pdf generated
#' @param PPrecom : TRUE or FALSE, based on if FR advice is requested used to bundel together the pdf generated
#' @param SPrecom : TRUE or FALSE, based on if FR advice is requested used to bundel together the pdf generated
#' @export
sendEmailReport <- function(userEmail, FR, IC, PP, SP, FRrecom, ICrecom, country, PPrecom, SPrecom){

  if(FR == TRUE & IC == FALSE & FRrecom == TRUE & country == "NG"){
    if(file.exists("fertilizer_advice_VFT.pdf"))
    webshot::rmdshot('FR_markdown_VFT.Rmd', file=paste0('fertilizer_advice_', userPhoneNr,".pdf"), delay = 3)
  }

  if(FR == TRUE & IC == FALSE & FRrecom == TRUE & country == "TZ"){
    if(file.exists("fertilizer_advice_swa.pdf"))
    webshot::rmdshot('FR_markdown_swa.Rmd', file=paste0('fertilizer_advice_swa_', userPhoneNr,".pdf"), delay = 3)
  }

  if(IC == TRUE & FR == FALSE & country == "NG" & ICrecom == TRUE){
    if(file.exists("intercrop_advice_VFT.pdf"))
    webshot::rmdshot('IC_markdown_VFT.Rmd', file=paste0('intercrop_advice__', userPhoneNr,".pdf"), delay = 3)
  }

  if(IC == TRUE & FR == FALSE & country == "TZ" & ICrecom == TRUE){
    if(file.exists("CIS_VFT.pdf"))
    webshot::rmdshot('CIS_markdown_swa.Rmd', file=paste0('CIS_advice_', userPhoneNr,".pdf"), delay = 3)
  }

  if(PP == TRUE & PPrecom == TRUE & country == "NG"){
    if(file.exists("PP_advice_VFT.pdf"))
    webshot::rmdshot('PP_markdownVFT.Rmd', file=paste0('PP_advice_', userPhoneNr,".pdf"), delay = 3)
  }

  if(PP == TRUE & PPrecom == TRUE & country == "TZ"){
    if(file.exists("PP_advice_swa.pdf"))
    webshot::rmdshot('PP_markdown_swa.Rmd', file=paste0('PP_advice_swa_', userPhoneNr,".pdf"), delay = 3)
  }

  if(SP == TRUE & SPrecom == TRUE & country == "NG"){
    if(file.exists("SP_advice_VFT.pdf"))
    webshot::rmdshot('SP_markdownVFT.Rmd', file=paste0('SP_advice_', userPhoneNr,".pdf"), delay = 3)
    if (file.exists("spgg.png")) file.remove("spgg.png")
  }


  if(SP == TRUE & SPrecom == TRUE & country == "TZ"){
    if(file.exists("SP_advice_swa.pdf"))
    webshot::rmdshot('SP_markdown_swa.Rmd', file=paste0('SP_advice_swa_', userPhoneNr,".pdf"), delay = 3)
    if (file.exists("spgg.png")) file.remove("spgg.png")
  }

  listofPDFs <- NULL
  if(FR == TRUE & FRrecom == TRUE & country == "NG"){listofPDFs <- c(listofPDFs, paste("fertilizer_advice_", userPhoneNr,".pdf"))}

  if(FR == TRUE & FRrecom == TRUE & country == "TZ"){listofPDFs <- c(listofPDFs, paste("fertilizer_advice_swa_", userPhoneNr,".pdf"))}

  if(PP == TRUE & PPrecom == TRUE & country == "NG") {listofPDFs <- c(listofPDFs, paste("PP_advice_", userPhoneNr,".pdf"))}

  if(PP == TRUE & PPrecom == TRUE & country == "TZ") {listofPDFs <- c(listofPDFs, paste("PP_advice_swa_", userPhoneNr,".pdf"))}

  if(SP == TRUE & SPrecom == TRUE & country == "NG") {listofPDFs <- c(listofPDFs, paste("SP_advice__", userPhoneNr,".pdf"))}

  if(SP == TRUE & SPrecom == TRUE & country == "TZ") {listofPDFs <- c(listofPDFs, paste("SP_advice_swa_", userPhoneNr,".pdf"))}

  if(IC == TRUE & country == "NG" & ICrecom == TRUE) {listofPDFs <- c(listofPDFs, paste("intercrop_advice_", userPhoneNr,".pdf"))}

  if(IC == TRUE & country == "TZ" & ICrecom == TRUE) {listofPDFs <- c(listofPDFs, paste("CIS__advice", userPhoneNr,".pdf"))}

  if(!is.null(listofPDFs)){
    send.mail(from =  "AKILIMO@cgiar.org",
              to = as.character(userEmail),
              subject = "AKILIMO recommendation",
              body = "Please find attached the recommendation. \n Best Regards, \n AKILIMO",
              authenticate = TRUE,
              attach.files = dput(as.character(listofPDFs)),
              smtp = list(host.name = "smtp.office365.com", port = 587,
                          user.name = "AKILIMO@cgiar.org", passwd = "Pass03$Imo", tls = TRUE))
    if (file.exists(listofPDFs)) file.remove(listofPDFs)
  }


}

#' @name : fertilizerAdviseTable
#' @title  :   function to generate the components of the markdown doc
#' @param FR TRUE or FALSE, based on if FR advice is requested
#' @param IC TRUE or FALSE, based on if SP advice is requested
#' @param country NG or TZ
#' @param areaUnits ha or acre
#' @export
fertilizerAdviseTable <- function(FR, IC, country, areaUnits){

  suppressWarnings(if (file.exists("datall1.csv")) file.remove("datall1.csv"))
  suppressWarnings(if (file.exists("datall2.csv")) file.remove("datall2.csv"))
  suppressWarnings(if (file.exists("datall3.csv")) file.remove("datall3.csv"))
  suppressWarnings(if (file.exists("datall4.csv")) file.remove("datall4.csv"))
  suppressWarnings(if (file.exists("datall5.csv")) file.remove("datall5.csv"))
  suppressWarnings(if (file.exists("datall6.csv")) file.remove("datall6.csv"))


  if(FR == TRUE & IC == FALSE) {
    acairm <- read.csv("FR_MarkDownText.csv")
  }else if(IC == TRUE & FR == FALSE){
    if(country == "TZ"){
      acairm <- read.csv("CIS_MarkDownText.csv")
    }else if (country == "NG"){
      acairm <- read.csv("IC_MarkDownText.csv")
    }
  }else{
    return("FR and IC can not be true together")
  }


  acairm$currency <- ifelse(acairm$country == "NG", "NGN", "TZS")

  ## Loop
  Nrfert <- length(grep("fertilizer", colnames(acairm)))
  if(Nrfert > 0){
    for(j in 1:Nrfert){
      colNames <- c(paste(c("fertilizer", "bags", "cost", "total_cost","kgs", "unit", "costPerBag"), j, sep=""))
      colNames <- c(colNames, c("currency", "field_area", "unit_field"))
      dat <- acairm[, colNames]
      dat$bag <- dat[,paste("bags", j, sep="")]


      if(dat[,1] == "Urea"){
        fertColCode <- "green"
        dat[,1] <- "Urea"
      }else if(dat[,1] == "NPK15_15_15"){
        fertColCode <- "blue"
        dat[,1] <- "NPK15:15:15"
      }else if(dat[,1] == "NPK20_10_10"){
        fertColCode <- "yellow"
        dat[,1] <- "NPK20:10:10"
      }else if(dat[,1] == "NPK17_17_17"){
        fertColCode <- "purple"
        dat[,1] <- "NPK17:17:17"
      }else if (dat[, 1] == "NPK20_12_16") {
        fertColCode <- "royal"
        dat[, 1] <- "NPK20:12:16+2Mg"
      }else {
        fertColCode <- "orange"
      }



      if (dat$bag==0.5){
        dat$rep <- sprintf(paste('![](net/',fertColCode,'/half.png)', sep=""))
      }else if (dat$bag==1){
        dat$rep <- sprintf(paste('![](net/',fertColCode,'/1.png)', sep=""))
      }else if (dat$bag==1.5){
        dat$rep <- sprintf(paste('![](net/',fertColCode,'/1_5.png)', sep=""))
      }else if (dat$bag==2){
        dat$rep <- sprintf(paste('![](net/',fertColCode,'/2.png)', sep=""))
      }else if (dat$bag==2.5){
        dat$rep <- sprintf(paste('![](net/',fertColCode,'/2_5.png)', sep=""))
      }else if (dat$bag==3){
        dat$rep <- sprintf(paste('![](net/',fertColCode,'/3.png)', sep=""))
      }else if (dat$bag==3.5){
        dat$rep <- sprintf(paste('![](net/',fertColCode,'/3_5.png)', sep=""))
      }else if (dat$bag==4){
        dat$rep <- sprintf(paste('![](net/',fertColCode,'/4.png)', sep=""))
      }else if (dat$bag==4.5){
        dat$rep <- sprintf(paste('![](net/',fertColCode,'/4_5.png)', sep=""))
      }else if (dat$bag==5){
        dat$rep <- sprintf(paste('![](net/',fertColCode,'/5.png)', sep=""))
      }else if (dat$bag==5.5){
        dat$rep <- sprintf(paste('![](net/',fertColCode,'/5_5.png)', sep=""))
      }else if (dat$bag==6){
        dat$rep <- sprintf(paste('![](net/',fertColCode,'/6.png)', sep=""))
      }else if (dat$bag==6.5){
        dat$rep <- sprintf(paste('![](net/',fertColCode,'/6_5.png)', sep=""))
      }else if (dat$bag==7){
        dat$rep <- sprintf(paste('![](net/',fertColCode,'/7.png)', sep=""))
      }else if (dat$bag==7.5){
        dat$rep <- sprintf(paste('![](net/',fertColCode,'/7_5.png)', sep=""))
      }else if (dat$bag==8){
        dat$rep <- sprintf(paste('![](net/',fertColCode,'/8.png)', sep=""))
      }else if (dat$bag==8.5){
        dat$rep <- sprintf(paste('![](net/',fertColCode,'/8_5.png)', sep=""))
      }else if (dat$bag==9){
        dat$rep <- sprintf(paste('![](net/',fertColCode,'/9.png)', sep=""))
      }else if (dat$bag==9.5){
        dat$rep <- sprintf(paste('![](net/',fertColCode,'/9_5.png)', sep=""))
      }else if (dat$bag==10){
        dat$rep <- sprintf(paste('![](net/',fertColCode,'/10.png)', sep=""))
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

      fn <- paste("datall", j, ".csv", sep="")
      write.csv(dat, fn, row.names = FALSE)

    }

  }

  if(min(acairm$sum_total, acairm$revenue) == acairm$sum_total  ){
    ratioFertCost <- 1
    ratioTotalSale <-  round(acairm$totalSalePrice/acairm$sum_total, digits=0)
    ratioRevenue <-  round(acairm$revenue/acairm$sum_total, digits=0)
  }else{
    ratioRevenue <- 1
    ratioFertCost <- round(acairm$sum_total/acairm$revenue, digits=0)
    ratioTotalSale <-  round(acairm$totalSalePrice/acairm$revenue, digits=0)

  }



  acairm$revenue <- formatC(acairm$revenue, format="f", big.mark=",", digits=0)
  acairm$totalSalePrice <- formatC(acairm$totalSalePrice, format="f", big.mark=",", digits=0)
  acairm$sum_total <- formatC(acairm$sum_total, format="f", big.mark=",", digits=0)
  # acairm$sum_total <- formatC(signif(acairm$sum_total, digits=3), format="f", big.mark=",", digits=0)


  totalCostmoney <- data.frame(title=paste( acairm$sum_total, acairm$currency, sep=" "))
  totalSalemoney <- data.frame(title=paste( acairm$totalSalePrice, acairm$currency, sep=" "))
  totalRevenuemoney <- data.frame(title=paste( acairm$revenue, acairm$currency, sep=" "))

  if (ratioFertCost==1){
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture1.png)')
  } else if (ratioFertCost==2){
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture2.png)')
  }else if (ratioFertCost==3){
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture3.png)')
  }else if (ratioFertCost==4){
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture4.png)')
  }else if (ratioFertCost==5){
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture5.png)')
  }else if (ratioFertCost==6){
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture6.png)')
  }else if (ratioFertCost==7){
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture7.png)')
  }else if (ratioFertCost==8){
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture8.png)')
  }else if (ratioFertCost==9){
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture9.png)')
  }else if (ratioFertCost==10){
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture10.png)')
  }
  write.csv(totalCostmoney, "totalCostmoney.csv", row.names = FALSE)

  if (ratioTotalSale==1){
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture1.png)')
  } else if (ratioTotalSale==2){
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture2.png)')
  }else if (ratioTotalSale==3){
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture3.png)')
  }else if (ratioTotalSale==4){
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture4.png)')
  }else if (ratioTotalSale==5){
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture5.png)')
  }else if (ratioTotalSale==6){
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture6.png)')
  }else if (ratioTotalSale==7){
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture7.png)')
  }else if (ratioTotalSale==8){
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture8.png)')
  }else if (ratioTotalSale==9){
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture9.png)')
  }else if (ratioTotalSale==10){
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture10.png)')
  }
  write.csv(totalSalemoney, "totalSalemoney.csv", row.names = FALSE)


  if (ratioRevenue==1){
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture1.png)')
  } else if (ratioRevenue==2){
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture2.png)')
  }else if (ratioRevenue==3){
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture3.png)')
  }else if (ratioRevenue==4){
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture4.png)')
  }else if (ratioRevenue==5){
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture5.png)')
  }else if (ratioRevenue==6){
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture6.png)')
  }else if (ratioRevenue==7){
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture7.png)')
  }else if (ratioRevenue==8){
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture8.png)')
  }else if (ratioRevenue==9){
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture9.png)')
  }else if (ratioRevenue==10){
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture10.png)')
  }
  write.csv(totalRevenuemoney, "totalRevenuemoney.csv", row.names = FALSE)


  #return(acairm)
}



#' @name : Rfmodel_Wrapper
#' @title : RF model works only if the factr levels are exactly identical to the data used to develp the model,
#' @param FCY : farmers current yield
#' @param country : NG or TZ
#' @param lat : latitude
#' @param lon : lngitude
#'  @export
Rfmodel_Wrapper <- function(FCY, country, lat, lon){

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
  ISRIC_SoilData <- unique(ISRIC_SoilData[ISRIC_SoilData$lat == lat & ISRIC_SoilData$long == lon, ])

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

  GIS_soilINS_modData2 <- factoring[factoring$use == "train", ]
  GIS_soilINS_modData2 <- subset(GIS_soilINS_modData2, select=-c(use))

  ISRIC_SoilData <- factoring[factoring$use == "Valid", ]
  ISRIC_SoilData <- subset(ISRIC_SoilData, select=-c(use))

  ### Data partioning
  set.seed(444)
  ind <- sample(2, nrow(GIS_soilINS_modData2), replace=TRUE, prob=c(0.7, 0.3))## where conrtol yield is used as a covariate
  trainData <- GIS_soilINS_modData2[ind==1, ]
  testData <- GIS_soilINS_modData2[ind==2, ]

  Ndata_Train  <- subset(trainData, select=-c(soilP, soilK))
  Pdata_Train  <- subset(trainData, select=-c(soilN, soilK))
  Kdata_Train  <- subset(trainData, select=-c(soilN, soilP))

  Ndata_Valid  <- subset(testData, select=-c(soilP, soilK))
  Pdata_Valid  <- subset(testData, select=-c(soilN, soilK))
  Kdata_Valid  <- subset(testData, select=-c(soilN, soilP))

  custom <- caret::trainControl(method="oob", number=10)
  ##########################################################################
  ## Random Forest soilN:
  ##########################################################################
  set.seed(444)
  RF_N1 <- randomForest::randomForest(log(soilN) ~ ., subset(Ndata_Train, select = -c(CON)), importance=TRUE, ntree=1000)

  ##########################################################################
  ## Random Forest "soilP"
  ##########################################################################
  set.seed(773)
  RF_P1 <- randomForest::randomForest(log(soilP) ~ ., subset(Pdata_Train, select=-c(CON)),importance=TRUE, ntree=1000)

  ##########################################################################
  ## Random Forest soilK" R sq. 0.60 if control is used, 0.29 otherwise
  ##########################################################################
  set.seed(773)
  RF_K1 <- randomForest::randomForest(log(soilK) ~ ., subset(Kdata_Train, select=-c(CON)), importance=TRUE, ntree=1000)

  ##########################################################################
  ## use the random forest model and get the soil NPK estimates for the whole area
  ##########################################################################
  ISRIC_SoilData$soilN <- 0
  ISRIC_SoilData$soilP <- 0
  ISRIC_SoilData$soilK <- 0


  ISRIC_SoilData <- ISRIC_SoilData[, c("soilN", "soilP", "soilK", "exchK", "olsenP", "Clay_5","Clay_15","Clay_30","percentSOM_5","percentSOM_15","percentSOM_30",
                                       "pH_5","pH_15","pH_30", "silt_5", "silt_15", "silt_30","BD_5", "BD_15", "BD_30", "CEC_5","CEC_15","CEC_30", "percentSOC_5",
                                       "percentSOC_15", "percentSOC_30", "FC_5", "FC_15", "FC_30", "wp_5", "wp_15", "wp_30", "sws_5", "sws_15", "sws_30",
                                       "TotalN", "Mn","B", "Ca","Fe", "Cu","Al", "Mg", "Na","ncluster","country", "CON", "CONclass")]
  ISRIC_SoilData <- subset(ISRIC_SoilData, select = -(CON))

  ISRIC_SoilData$country <- as.factor(ISRIC_SoilData$country)
  ISRIC_SoilData$ncluster <- as.factor(ISRIC_SoilData$ncluster)

  ISRIC_SoilData$soilN <-  exp(predict(RF_N1, ISRIC_SoilData))
  ISRIC_SoilData$soilP <-  exp(predict(RF_P1, ISRIC_SoilData))
  ISRIC_SoilData$soilK <-  exp(predict(RF_K1, ISRIC_SoilData))

  ISRIC_SoilData$rec_N <- 0.5
  ISRIC_SoilData$rec_P <- 0.15
  ISRIC_SoilData$rec_K <- 0.5
  ISRIC_SoilData$rel_N <- 1
  ISRIC_SoilData$rel_P <- ISRIC_SoilData$soilP / ISRIC_SoilData$soilN
  ISRIC_SoilData$rel_K <- ISRIC_SoilData$soilK / ISRIC_SoilData$soilN
  ISRIC_SoilData$lat <- lat
  ISRIC_SoilData$long <- lon
  ISRIC_SoilData$location <- paste(ISRIC_SoilData$lat, ISRIC_SoilData$long, sep="_")
  ISRIC_SoilData$Zone <- country
  ISRIC_SoilData <- ISRIC_SoilData[, c("location","lat", "long", "soilN","soilP","soilK", "Zone","rec_N", "rec_P", "rec_K", "rel_N","rel_P","rel_K")]
  return(ISRIC_SoilData)

}


#' @name : QUEFTS_WLY_CY
#' @title  The soil NPK as obtained from randdom forest model
#' @param SoilData : isric data
#' @param country : NG or TZ
#' @param wlyd a data frae wit Wwater limited yield, current yield and location#'
#' @return
#' @author ACAI
#' @export
QUEFTS_WLY_CY <- function(SoilData=SoilData, country=country, wlyd=wlyd){
  wlyd$long <- wlyd$lon
  wly_plDate <- wlyd[,  c("lat", "long", "water_limited_yield")]

  Quefts_Input_Data_wly <- merge(SoilData, wly_plDate, by=c("lat", "long"))
  crop_param <- cbind(NUE(HI=0.55), data.frame(rN=0, rP=0, rK=0, max_yield=Quefts_Input_Data_wly$water_limited_yield, tolerance=0.01))

  ## 1. get soil nutrient supply
  Queft_Input_Data_Var <- cbind(Quefts_Input_Data_wly, crop_param)
  supply <- getsupply(Queft_Input_Data_Var) ## to get yield at zero input level


  ## 2. Current yield:
  actualUptake <- merge(supply, plyr::ddply(supply, .(lat, long), actual_uptake_tool), by=c("lat","long"))
  minmax_Yield <-  merge(actualUptake, plyr::ddply(actualUptake,.(lat, long), max_min_yields_tools), by=c("lat","long"))
  Current_Yield <- ddply(minmax_Yield,.(lat, long), final_yield_tools)## yield at zero input
  colnames(Current_Yield) <- c("lat", "long", "CurrentYield")
  Yield_Fertilizer <- merge(wly_plDate, Current_Yield, by=c("lat", "long"))
  Yield_Fertilizer$CurrentYield <- ifelse(Yield_Fertilizer$CurrentYield > Yield_Fertilizer$water_limited_yield,
                                          as.character(as.numeric(Yield_Fertilizer$water_limited_yield)), as.numeric(Yield_Fertilizer$CurrentYield))
  return(Yield_Fertilizer$CurrentYield)
}


#' @name : getsupply
#' @title :
#' @param dss data frame with location, and QUEFTS crop parameters
#' @author ACAI
#' @export
getsupply <- function(dss){
  supply <- data.frame(lat=dss$lat, long=dss$long, rel_N=dss$rel_N, rel_P=dss$rel_P, rel_K=dss$rel_K, SN=dss$soilN, SP=dss$soilP, SK=dss$soilK, water_limited_yield = dss$water_limited_yield,
                       aN=dss$aN, dN=dss$dN, aP=dss$aP, dP=dss$dP, aK=dss$aK, dK=dss$dK, rN=dss$rN, rP=dss$rP, rK=dss$rK, max_yield=dss$max_yield,  tolerance=dss$tolerance,
                       WLY = dss$water_limited_yield)
}



#######################################################################
## FR & SP
#######################################################################
#' @name : getFRrecommendations
#' @title : the main fertilizer recommendation function
#' @param areaHa is area of land in ha#'
#' @param country should be NG or TZ
#' @param lat : latitude
#' @param lon : longitude
#' @param pd planting date in the xth day of the year format
#' @param pw planting week
#' @param HD harvest date
#' @param had difference between HD and PD
#' @param maxInv max investment
#' @param fertilizers a data frame with type, NPK contnet and price
#' @param rootUP root price
#' @param FCY  based on user input five values based on user input are passed, the app converts the value per ha so it is always per ha that comes
#' @param return a data frame with lat,lon, plDate,N, P, K, WLY, CurrentY, TargetY, TC, NR, harvestDate and rates of fertilizer (if any)
#' @export
getFRrecommendations <- function(lat, lon, pd, pw, HD, had, maxInv, fertilizers, rootUP, areaHa, country, FCY){

  ########### getting CY
  latr <- as.factor(floor(lat*10)/10 + ifelse(lat-(floor(lat*10)/10) < 0.05, 0.025, 0.075))
  lonr <- as.factor(floor(lon*10)/10 + ifelse(lat-(floor(lon*10)/10) < 0.05, 0.025, 0.075))
  lat2 <- as.numeric(levels(latr))
  lon2 <- as.numeric(levels(lonr))

  latlon <- paste(lat2, lon2, sep="_")

  if(country == "NG"){
    WLY_365 <- readRDS("Nigeria_WLY_LINTUL_2020.RDS")
  }else{
    WLY_365 <- readRDS("Tanzania_WLY_LINTUL_2020.RDS")
  }


  pdates <- data.frame(wlyPD=unique(WLY_365$pl_Date))
  pdates$diff <- pd - pdates$wlyPD
  pdates$absdiff <- abs(pdates$diff)
  PD2 <- pdates[pdates$absdiff == min(abs(pdates$diff)), "wlyPD"]
  hdates <- data.frame(wlyHD = seq(214, 361, 7))
  hdates$diff <- abs(had - hdates$wlyHD)
  HD2 <- hdates[hdates$diff == min(abs(hdates$diff)), "wlyHD"]


  wlypd <- WLY_365[WLY_365$location == latlon & WLY_365$pl_Date == PD2, ]


  if(nrow(wlypd) == 0){

    if(country == "NG"){
            return("We do not have fertilizer recommendation for your location because your location is out of the recommendation domain AKILIMO is currently serving.")
              }else{
            return("Hatuna mapendekezo yoyote  kwa eneo lako kwa sababu eneo lako liko nje la eneo ambalo AKILIMO linafanya kazi kwa sasa")
           }

        }else{

    wlydata <-  wlypd[, c("lat", "long", "pl_Date", "location")]
    colnames(wlydata) <- c("lat", "lon", "pl_Date", "location")
    wlydata$water_limited_yield <-  wlypd[, colnames(wlypd) == HD2 ]

    wlydata$zone <- country
    wlydata$daysOnField <- had
    wlydata <- wlydata[,c("lat", "lon", "water_limited_yield", "location", "pl_Date", "zone", "daysOnField") ]


    SoilData <- Rfmodel_Wrapper(FCY=FCY, country=country, lat=lat2, lon = lon2)


    ## get CY
    wlydata$Current_Yield <- QUEFTS_WLY_CY(SoilData=SoilData, country=country, wlyd=wlydata)
    WLYData <- wlydata
    WLYData$weekNr <- pw

    #############################
    ## 1. get WLY, CY, fert recom and soil data
    WLY <- WLYData$water_limited_yield ## DM in kg/ha
    DCY <- WLYData$Current_Yield## DM in kg/ha


    ## 2. change investment from given areaHa to 1ha
    InvestHa <- (maxInv / areaHa)


    ## 3. optimize the fertilizer recommendation for maxInv in local currency and provide expected target yield in kg
    fert_optim <- run_Optim_NG2(rootUP=rootUP, QID=SoilData, fertilizer=fertilizers, invest=InvestHa, plDate=WLYData$pl_Date,
                                WLYData=WLYData, lat=lat, lon=lon, areaHa, HD=HD, DCY = DCY, WLY=WLY, country=country)

    if(fert_optim$NR == 0){ ## no fertilizer recommendation
      fertilizer_rates <- NULL
      return(list(rec=fert_optim, fertilizer_rates=fertilizer_rates))
    }else{
      fertinfo <- subset(fert_optim, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
      onlyFert <- subset(fert_optim, select = -c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))

      ## 4. remove ferilizer application < 25 kg/ha and re run the TY and NR calculation
      RecomperHa <- onlyFert/areaHa
      RecomperHa2 <- tidyr::gather(RecomperHa, type, rate)
      onlyFert2 <- droplevels(RecomperHa2[RecomperHa2$rate > 25, ])

      if(nrow(onlyFert2) == 0 ){ ## if all fertilizer recom < 25 kg/ha all will be set to 0
        fertinfo$N <- fertinfo$P <- fertinfo$K <- fertinfo$NR <- fertinfo$TC <- 0
        fertinfo$TargetY <- fertinfo$CurrentY
        fertilizer_rates <- NULL
        return(list(rec=fertinfo, fertilizer_rates=fertilizer_rates))
      }else if (ncol(onlyFert) == nrow(onlyFert2)){ ## if all fertilizer recom are >= 25 kg/ha they will be kept and only checked for NR >= 18% of invest
        Reset_fert_Cont <- fert_optim
        GPS_fertRecom <- NRabove18Cost(ds=Reset_fert_Cont)
        rec <- subset(GPS_fertRecom, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
        frates <- subset(GPS_fertRecom, select = -c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
        frates2 <- tidyr::gather(frates, type, rate)
        return(list(rec=rec, fertilizer_rates=frates2))

      }else{
        fert25 <- tidyr::spread(onlyFert2, type, rate) ## when some fertilizer recom are dropped b/c < 25 kg/ha, ty and NR should be recalculated
        fert_optim2 <- cbind(fertinfo, fert25)
        fertilizer <- fertilizers[fertilizers$type %in% onlyFert2$type, ]
        Reset_fert_Cont <- Rerun_25kgKa_try(rootUP=rootUP, rdd=fert_optim2, fertilizer=fertilizer, QID=SoilData, onlyFert=onlyFert2,
                                            country = country, WLY=WLY, DCY = DCY, HD=HD, areaHa=areaHa)
        if(Reset_fert_Cont$NR <= 0){ ## after rerunning after avoiding <25KG/ha fertilizers, if NR <=0
          fertilizer_rates <- NULL
          return(list(rec=Reset_fert_Cont, fertilizer_rates=fertilizer_rates))
        }else{
          GPS_fertRecom <- NRabove18Cost(ds=Reset_fert_Cont)
          rec <- subset(GPS_fertRecom, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
          frates <- subset(GPS_fertRecom, select = -c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
          frates2 <- tidyr::gather(frates, type, rate)
          return(list(rec=rec, fertilizer_rates=frates2))

        }
      }
    }
  }
}


#' @name : getFRrecText
#' @title : makes a text of the advice to be used in the R markdown report
#' @param ds out put of get FR recommendation function
#' @param country NG or TZ
#' @param fertilizers a data frame with type, NPK contnet and price
#' @param rootUP root price
#' @export
getFRrecText <- function(ds, country, fertilizers, rootUP){
  TRNS <- read.csv("translations_TEST.csv",  stringsAsFactors = FALSE)
  norecom_ng <- gsub(pattern = "\"",replacement = "",TRNS$norecom[1]); norecom_tz <- gsub(pattern = "\"",replacement = "",TRNS$norecom[2]);notapply_ng <- gsub(pattern = "\"",replacement = "",TRNS$notapply[1]); notapply_tz <- gsub(pattern = "\"",replacement = "",TRNS$notapply[2])

  rec <- ds$rec
  frate <- ds$fertilizer_rates
  if(is.null(rec)) {
    recom <- if(country == "NG") {
      paste(norecom_ng)
    }else{
      paste(norecom_tz)
    }
  }else{
    if(rec$TC == 0) {
      recom <- if(country == "NG") {
        paste(notapply_ng)
      }else{
        paste(notapply_tz)

      }
    }else{
      currency <- ifelse(country == "NG", "NGN", "TZS")
      fertilizerTypes <- frate$type
      fertilizerRates <- round(frate$rate, digits=0)

      bags <- round(fertilizerRates/50, digits=1)
      Bagsfull <- trunc(bags)
      bagshalf <- bags - floor(bags)
      bagshalf  <- ifelse(bagshalf  >= 0.25 & bagshalf <= 0.75, 0.5,  ifelse(bagshalf < 0.25, 0, 1) )
      bags <- Bagsfull + bagshalf


      sum_total = ds$rec$TC
      fertilizers_recom <- fertilizers[fertilizers$type %in% ds$fertilizer_rates$type, ]
      fertilizers_recom <- merge(fertilizers_recom, ds$fertilizer_rates, by='type')
      fertilizers_recom$rate <- round(fertilizers_recom$rate, digits = 0)
      fertilizers_recom$cost <- round(fertilizers_recom$rate, digits=0) *  fertilizers_recom$price
      sum_total <- sum(fertilizers_recom$cost)
      totalSalePrice <- round(ds$rec$TC + ds$rec$NR, digits = 0)
      revenue =  totalSalePrice -  sum_total


      fertilizers <- droplevels(fertilizers[fertilizers$type %in% frate$type, ])
      TC <- formatC(round(sum_total, digits=0), format="f", big.mark=",", digits=0)

      NR <- formatC(revenue, format="f", big.mark=",", digits=0)
      DY <- signif(rec$TargetY - rec$CurrentY, digits=2)

      werec_ng <- gsub(pattern = "\"",replacement = "",TRNS$werec[1]) ; werec_tz <- gsub(pattern = "\"",replacement = "",TRNS$werec[2])
      kgof_ng <- gsub(pattern = "\"",replacement = "",TRNS$kgof[1]);kgof_tz <- gsub(pattern = "\"",replacement = "",TRNS$kgof[2]);area_ng <- gsub(pattern = "\"",replacement = "",TRNS$area[1]);area_tz <- gsub(pattern = "\"",replacement = "",TRNS$area[2])
      willc_ng <- gsub(pattern = "\"",replacement = "",TRNS$willc[1]);willc_tz <- gsub(pattern = "\"",replacement = "",TRNS$willc[2]);extrap_ng <- gsub(pattern = "\"",replacement = "",TRNS$extrap[1]);extrap_tz <- gsub(pattern = "\"",replacement = "",TRNS$extrap[2])
      tonof_ng <- gsub(pattern = "\"",replacement = "",TRNS$tonof[1]);tonof_tz <- gsub(pattern = "\"",replacement = "",TRNS$tonof[2]);netincr_ng <- gsub(pattern = "\"",replacement = "",TRNS$netincr[1]) ; netincr_tz <- gsub(pattern = "\"",replacement = "",TRNS$netincr[2])
      of_tz <- gsub(pattern = "\"",replacement = "",TRNS$of[2]);

      recom <- if(country == "NG") {
                      paste0(werec_ng, "\n",
                      paste0(fertilizerRates, kgof_ng, fertilizerTypes, collapse="\n"), " ",
                      area_ng, "\n",
                      willc_ng, currency, " ", TC, ".\n",
                      extrap_ng, DY, tonof_ng,
                      netincr_ng, currency, " ", NR, ".")
      }else{
                      paste0(werec_tz, " ", "\n",
                      paste0(kgof_tz, fertilizerRates, of_tz, fertilizerTypes, collapse="\n"), " ",
                      area_tz, "\n",
                      willc_tz, currency, " ", TC, ".\n",
                      extrap_tz, " ", DY, tonof_tz,
                      netincr_tz, currency, " ", NR, ".")

      }
    }
  }

  return(recom)
}


#' @name : getSPrecText
#' @title : makes a text of the advice to be used in the R markdown report
#' @param ds out put of get SP recommendation function
#' @param country NG or TZ
#' @export
getSPrecText <- function(ds, country){
  TRNS <- read.csv("translations_TEST.csv",  stringsAsFactors = FALSE)
  norecom_ng <- gsub(pattern = "\"",replacement = "",TRNS$norecom[1]); norecom_tz <- gsub(pattern = "\"",replacement = "",TRNS$norecom[2]);notapply_ng <- gsub(pattern = "\"",replacement = "",TRNS$notapply[1]); notapply_tz <- gsub(pattern = "\"",replacement = "",TRNS$notapply[2])
  recrev_ng <- gsub(pattern = "\"",replacement = "",TRNS$recrev[1]) ; recrev_tz <- gsub(pattern = "\"",replacement = "",TRNS$recrev[2])
  hvsdate_ng <- gsub(pattern = "\"",replacement = "",TRNS$hvsdate[1]) ; hvsdate_tz <- gsub(pattern = "\"",replacement = "",TRNS$hvsdate[2]);nochange_ng <- gsub(pattern = "\"",replacement = "",TRNS$nochange[1]) ; nochange_tz <- gsub(pattern = "\"",replacement = "",TRNS$nochange[2])
  recPln_ng <- gsub(pattern = "\"",replacement = "",TRNS$recPln[1]) ; recPln_tz <- gsub(pattern = "\"",replacement = "",TRNS$recPln[2]);recHvs_ng <- gsub(pattern = "\"",replacement = "",TRNS$recHvs[1]); recHvs_tz <- gsub(pattern = "\"",replacement = "",TRNS$recHvs[2])
  wks_ng <- gsub(pattern = "\"",replacement = "",TRNS$wks[1]) ; wks_tz <- gsub(pattern = "\"",replacement = "",TRNS$wks[2]) ;recPlnP_ng <- gsub(pattern = "\"",replacement = "",TRNS$recPlnP[1]) ; recPlnP_tz <- gsub(pattern = "\"",replacement = "",TRNS$recPlnP[2])
  early_ng <- gsub(pattern = "\"",replacement = "",TRNS$early[1]); early_tz <- gsub(pattern = "\"",replacement = "",TRNS$early[2]);late_ng <- gsub(pattern = "\"",replacement = "",TRNS$late[1]) ; late_tz <- gsub(pattern = "\"",replacement = "",TRNS$late[2])
  recPhv_ng <- gsub(pattern = "\"",replacement = "",TRNS$recPhv[1]) ; recPhv_tz <- gsub(pattern = "\"",replacement = "",TRNS$recPhv[2]) ;


  if(is.null(ds)) {
    rec <- if(country == "NG") {
      norecom_ng
    }else{
      norecom_tz
    }
  }else{
    if(ds[1,]$CP) {
      rec <- if(country == "NG") {
        paste0(recrev_ng, " ( ", format(ds[1,]$PD, "%d %B %Y"), " ) ",
                    hvsdate_ng, " ( ", format(ds[1,]$HD, "%d %B %Y"), " ) ",
                    nochange_ng)
      }else{
        paste0(recrev_tz, " ( ", format(ds[1,]$PD, "%d %B %Y"), " ) ",
               hvsdate_tz, " ( ", format(ds[1,]$HD, "%d %B %Y"), " ) ",
               nochange_tz)
      }
    }else{
      if(ds[1,]$PD != ds[ds$CP==TRUE,]$PD){
        recP <- if(country == "NG") {
          paste0(recPln_ng, format(ds[1,]$PD, "%d %B %Y"), ", ",
                 abs(ds[1,]$rPWnr), " ", wks_ng, " ", ifelse(ds[1,]$rPWnr<0, early_ng, late_ng), " ", recPlnP_ng, "\n" )
        }else{
          paste0(recPln_tz, " ", format(ds[1,]$PD, "%d %B %Y"), ", ",
                 wks_tz, " ", abs(ds[1,]$rPWnr), " ",  " ", ifelse(ds[1,]$rPWnr<0, early_tz, late_tz), " ", recPlnP_tz, "\n" )
        }
      }else{
          recP <- NULL
        }
      if(ds[1,]$HD != ds[ds$CP==TRUE,]$HD){
        recH <- if(country == "NG") {
          paste0(recHvs_ng, format(ds[1,]$PD, "%d %B %Y"), ", ",
                       abs(ds[1,]$rPWnr), " ", wks_ng, " ", ifelse(ds[1,]$rPWnr<0, early_ng, late_ng), recPhv_ng, "\n")

        }else{
          paste0(recHvs_tz, " ", format(ds[1,]$PD, "%d %B %Y"), ", ", wks_tz, " ",
                 abs(ds[1,]$rPWnr),  " ", ifelse(ds[1,]$rPWnr<0, early_tz, late_tz), recPhv_tz, "\n")
        }
      }else{
        recH <- NULL
      }

      DP  <- signif(ds[1, ]$RP - ds[ds$CP==TRUE,]$RP, digits=2)
      currency <- ifelse(country == "NG", "NGN", "TZS")
      dGR <- formatC(signif(ds[1, ]$dGR, digits=3), format="f", big.mark=",", digits=0)
      rechange_ng <- gsub(pattern = "\"",replacement = "",TRNS$rechange[1]); rechange_tz <- gsub(pattern = "\"",replacement = "",TRNS$rechange[2])
      hvst_ng <- gsub(pattern = "\"",replacement = "",TRNS$hvst[1]);hvst_tz <- gsub(pattern = "\"",replacement = "",TRNS$hvst[2]);plnt_ng <- gsub(pattern = "\"",replacement = "",TRNS$plnt[1]); plnt_tz <- gsub(pattern = "\"",replacement = "",TRNS$plnt[2])
      recRatt1_ng <- gsub(pattern = "\"",replacement = "",TRNS$recRatt1[1]) ; recRatt1_tz <- gsub(pattern = "\"",replacement = "",TRNS$recRatt1[2]);recRatt2_ng <- gsub(pattern = "\"",replacement = "",TRNS$recRatt2[1]) ; recRatt2_tz <- gsub(pattern = "\"",replacement = "",TRNS$recRatt2[2])
      exp_ng <- gsub(pattern = "\"",replacement = "",TRNS$exp[1]) ; exp_tz <- gsub(pattern = "\"",replacement = "",TRNS$exp[2]);dec_ng <- gsub(pattern = "\"",replacement = "",TRNS$dec[1]) ; dec_tz <- gsub(pattern = "\"",replacement = "",TRNS$dec[2])
      inc_ng <- gsub(pattern = "\"",replacement = "",TRNS$inc[1]) ; inc_tz <- gsub(pattern = "\"",replacement = "",TRNS$inc[2]);root_ng <- gsub(pattern = "\"",replacement = "",TRNS$root[1]) ; root_tz <- gsub(pattern = "\"",replacement = "",TRNS$root[2])
      notot_ng <- gsub(pattern = "\"",replacement = "",TRNS$notot[1]) ; notot_tz <- gsub(pattern = "\"",replacement = "",TRNS$notot[2]) ;optim_ng <- gsub(pattern = "\"",replacement = "",TRNS$optim[1]) ; optim_tz <- gsub(pattern = "\"",replacement = "",TRNS$optim[2])
      ton_ng <- gsub(pattern = "\"",replacement = "",TRNS$ton[1]) ; ton_tz <- gsub(pattern = "\"",replacement = "",TRNS$ton[2]) ;but_ng <- gsub(pattern = "\"",replacement = "",TRNS$but[1]) ; but_tz <- gsub(pattern = "\"",replacement = "",TRNS$but[2])
      and_ng <- gsub(pattern = "\"",replacement = "",TRNS$and[1]) ; and_tz <- gsub(pattern = "\"",replacement = "",TRNS$and[2]) ;valinc_ng <- gsub(pattern = "\"",replacement = "",TRNS$valinc[1]) ; valinc_tz <- gsub(pattern = "\"",replacement = "",TRNS$valinc[2])

      if(DP == 0){

        if(dGR == 0){
          recR <- if(country == "NG") {
            paste0(rechange_ng,
                   ifelse(!is.null(recH), paste(hvst_ng), paste(plnt_ng)))
          }else{
            paste0(rechange_tz,
                   ifelse(!is.null(recH), paste(hvst_tz), paste(plnt_tz)))

          }
        }else{
          recR <- if(country == "NG") {
            paste0(recRatt1_ng, currency, " ", dGR, " ", recRatt2_ng)
          }else{
            paste0(recRatt1_tz, currency, " ", dGR, " ", recRatt2_tz)
          }
        }
      }else{
        if(dGR == 0){
          recR <- if(country == "NG") {
            paste0(exp_ng,
                   ifelse(DP<0, paste(dec_ng), paste(inc_ng)), root_ng, abs(DP), " ", ton_ng,
                         notot_ng,
                         ifelse(!is.null(recH), paste(hvst_ng), paste(plnt_ng)))
          }else{
            paste0(exp_tz,
                   ifelse(DP<0, paste(dec_tz), paste(inc_tz)), root_tz, " ", abs(DP), " ", ton_tz,
                   notot_tz,
                   ifelse(!is.null(recH), paste(hvst_tz), paste(plnt_tz)))

          }
        }else{

          recR <- if(country == "NG") {
                         paste0(exp_ng,
                         ifelse(DP<0, paste(dec_ng), paste(inc_ng)), root_ng, abs(DP), " ", ton_ng,
                         ifelse(DP<0, paste(but_ng), paste(and_ng)),
                         valinc_ng, currency, " ", dGR, ".")
        }else{

                 paste0(exp_tz,
                 ifelse(DP<0, paste(dec_tz), paste(inc_tz)), root_tz, " ", abs(DP), ton_tz,
                 ifelse(DP<0, paste(but_tz), paste(and_tz)),
                 valinc_tz, currency, " ", dGR, ".")
        }
      }

    }
      rec <- paste0(recP, recH, recR)
    }
  }
  return(rec)
}


#' @name : getPPrecText
#' @title : makes a text of the advice to be used in the R markdown report
#' @param ds out put of get PP recommendation function
#' @param country NG or TZ
#' @export
getPPrecText <- function(ds, country = c("NG", "TZ")){

  TRNS <- read.csv("translations_TEST.csv",  stringsAsFactors = FALSE)
  no_ng <- gsub(pattern = "\"",replacement = "",TRNS$no[1]) ; no_tz <- gsub(pattern = "\"",replacement = "",TRNS$no[2]) ; plo_ng <- gsub(pattern = "\"",replacement = "",TRNS$plo[1]) ; plo_tz <- gsub(pattern = "\"",replacement = "",TRNS$plo[2])
  ridg_ng <- gsub(pattern = "\"",replacement = "",TRNS$ridg[1]) ; ridg_tz <- gsub(pattern = "\"",replacement = "",TRNS$ridg[2]) ; decnet_ng <- gsub(pattern = "\"",replacement = "",TRNS$decnet[1]); decnet_tz <- gsub(pattern = "\"",replacement = "",TRNS$decnet[2])
  werec_ng <- gsub(pattern = "\"",replacement = "",TRNS$werec[1]) ; werec_tz <- gsub(pattern = "\"",replacement = "",TRNS$werec[2]) ;plofol_ng <- gsub(pattern = "\"",replacement = "",TRNS$plofol[1]) ; plofol_tz <- gsub(pattern = "\"",replacement = "",TRNS$plofol[2])
  noridg_ng <- gsub(pattern = "\"",replacement = "",TRNS$noridg[1]) ; noridg_tz <- gsub(pattern = "\"",replacement = "",TRNS$noridg[2]); thank_ng <- gsub(pattern = "\"",replacement = "",TRNS$thank[1]) ; thank_tz <- gsub(pattern = "\"",replacement = "",TRNS$thank[2])
  plodir_ng <- gsub(pattern = "\"",replacement = "",TRNS$noplo[1]) ;plodir_tz <- gsub(pattern = "\"",replacement = "",TRNS$noplo[2]);zerot_ng <- gsub(pattern = "\"",replacement = "",TRNS$zerot[1]) ; zerot_tz <- gsub(pattern = "\"",replacement = "",TRNS$zerot[2])
  changcost_ng <- gsub(pattern = "\"",replacement = "",TRNS$changcos[1]) ; changcost_tz <- gsub(pattern = "\"",replacement = "",TRNS$changcos[2]) ; this_ng <- gsub(pattern = "\"",replacement = "",TRNS$this[1]); this_tz <- gsub(pattern = "\"",replacement = "",TRNS$this[2])
  decr_ng <- gsub(pattern = "\"",replacement = "",TRNS$decr[1]); optim_ng <- gsub(pattern = "\"",replacement = "",TRNS$optim[1]) ; decr_tz <- gsub(pattern = "\"",replacement = "",TRNS$decr[2]);incr_ng <- gsub(pattern = "\"",replacement = "",TRNS$incr[1]);incr_tz <- gsub(pattern = "\"",replacement = "",TRNS$incr[2])
  costb_ng <- gsub(pattern = "\"",replacement = "",TRNS$costb[1]);costb_tz <- gsub(pattern = "\"",replacement = "",TRNS$costb[2]);rtprod_ng <- gsub(pattern = "\"",replacement = "",TRNS$rtprod[1]);rtprod_tz <- gsub(pattern = "\"",replacement = "",TRNS$rtprod[2])
  optim_ng <- gsub(pattern = "\"",replacement = "",TRNS$optim[1]) ;

  ds$method_ploughing <- as.character(ds$method_ploughing)
  ds$method_ridging   <- as.character(ds$method_ridging)

  if(ds[1,]$CP){

    #trans
    rec <- if(country == "NG") {
      paste0(optim_ng,
                  ifelse(ds[1, ]$method_ploughing == "N/A", paste(no_ng),  ds[1, ]$method_ploughing), paste(plo_ng),
                  ifelse(ds[1, ]$method_ridging == "N/A",   paste(no_ng),  ds[1, ]$method_ridging), paste(ridg_ng), "\n", " ",
                  decnet_ng)
    }else{
      paste0(optim_tz,
             ifelse(ds[1, ]$method_ploughing == "N/A", paste(no_tz),  ds[1, ]$method_ploughing), paste(plo_tz),
             ifelse(ds[1, ]$method_ridging == "N/A",   paste(no_tz),  ds[1, ]$method_ridging), paste(ridg_tz), "\n", " ",
             decnet_tz)

    }
  }else{

    #trans
    if(ds[1, ]$ploughing & ds[1, ]$ridging)   {recT <- if(country == "NG") {
      paste0(werec_ng, ds[1, ]$method_ploughing, plofol_ng, ds[1, ]$method_ridging, ridg_ng, "\n")
    }else{
      paste0(werec_tz, ds[1, ]$method_ploughing, plofol_tz, ds[1, ]$method_ridging, ridg_tz, "\n")

      }
    }

    if(ds[1, ]$ploughing & !ds[1, ]$ridging)  {recT <- if(country == "NG") {
      paste0(werec_ng, ds[1, ]$method_ploughing, noridg_ng, "\n")
    }else{
      paste0(werec_tz, ds[1, ]$method_ploughing, noridg_tz, "\n")
       }
    }


    print("error turry")
     if(!ds[1, ]$ploughing & ds[1, ]$ridging)  {recT <- if(country == "NG") {
       paste0(plodir_ng, ds[1, ]$method_ridging, ridg_ng, "\n")
     }else{

       paste0(plodir_tz, ds[1, ]$method_ridging, ridg_tz, "\n")
       }
     }

     if(!ds[1, ]$ploughing & !ds[1, ]$ridging) {recT <- if(country == "NG") {
       paste(zerot_ng, "\n")

     }else{
       paste(zerot_tz, "\n")

       }
     }
    currency <- ifelse(country == "NG", "NGN", "TZS")
    dTC <- formatC(signif(abs(ds[1, ]$dTC), digits=3), format="f", big.mark=",", digits=0)
    dNR <- formatC(signif(ds[1, ]$dNR, digits=3), format="f", big.mark=",", digits=0)
    dRP <- signif(ds[1, ]$dRP, digits=2)

    rtcurr_ng <- gsub(pattern = "\"",replacement = "",TRNS$rtcurr[1]);rtcurr_tz <- gsub(pattern = "\"",replacement = "",TRNS$rtcurr[2]);netinc_ng <- gsub(pattern = "\"",replacement = "",TRNS$netinc[1]);netinc_tz <- gsub(pattern = "\"",replacement = "",TRNS$netinc[2])
    rtwill_ng <- gsub(pattern = "\"",replacement = "",TRNS$rtwill[1]);rtwill_tz <- gsub(pattern = "\"",replacement = "",TRNS$rtwill[2]);by_ng <- gsub(pattern = "\"",replacement = "",TRNS$by[1]);by_tz <- gsub(pattern = "\"",replacement = "",TRNS$by[2])
    tonb_ng <- gsub(pattern = "\"",replacement = "",TRNS$tonb[1]);tonb_tz <- gsub(pattern = "\"",replacement = "",TRNS$tonb[2]);netno_ng <- gsub(pattern = "\"",replacement = "",TRNS$netno[1]);netno_tz <- gsub(pattern = "\"",replacement = "",TRNS$netno[2])
    incomp_ng <- gsub(pattern = "\"",replacement = "",TRNS$incomp[1]);incomp_tz <- gsub(pattern = "\"",replacement = "",TRNS$incomp[2]);recap_ng <- gsub(pattern = "\"",replacement = "",TRNS$recap[1]);recap_tz <- gsub(pattern = "\"",replacement = "",TRNS$recap[2])
    kgof_ng <- gsub(pattern = "\"",replacement = "",TRNS$kgof[1]);kgof_tz <- gsub(pattern = "\"",replacement = "",TRNS$kgof[2]);area_ng <- gsub(pattern = "\"",replacement = "",TRNS$area[1]);area_tz <- gsub(pattern = "\"",replacement = "",TRNS$area[2])
    willc_ng <- gsub(pattern = "\"",replacement = "",TRNS$willc[1]);willc_tz <- gsub(pattern = "\"",replacement = "",TRNS$willc[2]);extrap_ng <- gsub(pattern = "\"",replacement = "",TRNS$extrap[1]);extrap_tz <- gsub(pattern = "\"",replacement = "",TRNS$extrap[2])
    tonof_ng <- gsub(pattern = "\"",replacement = "",TRNS$tonof[1]);tonof_tz <- gsub(pattern = "\"",replacement = "",TRNS$tonof[2]);netincr_ng <- gsub(pattern = "\"",replacement = "",TRNS$netincr[1]) ; netincr_tz <- gsub(pattern = "\"",replacement = "",TRNS$netincr[2])

    if(dTC == 0){

      #trans
      recC <- if(country == "NG") {
        paste(changcost_ng)

      }else{
        paste(changcost_tz)

      }

    }else{

        #trans
      recC <- if(country == "NG") {
        paste0(this_ng, ifelse(ds[1, ]$dTC < 0, paste(decr_ng), paste(incr_ng)), costb_ng, currency, " ", dTC, ". ")

      }else{
        paste0(this_tz, ifelse(ds[1, ]$dTC < 0, paste(decr_tz), paste(incr_tz)), costb_tz, currency, " ", dTC, ". ")

      }

     }


    #trans

    if(dRP == 0 & dNR == 0) {recP <- if(country == "NG") {
      rtprod_ng
    }else{
      rtprod_tz
      }
    }
    if(dRP == 0 & dNR > 0)  {recP <- if(country == "NG") {
      paste0(rtcurr_ng, netinc_ng, currency, " ", dNR, ".")
    }else{
      paste0(rtcurr_tz, netinc_tz, currency, " ", dNR, ".")
      }
    }

    if(dRP != 0 & dNR == 0) {recP <- if(country == "NG") {
      paste0(rtwill_ng, ifelse(ds[1, ]$TC < 0, paste(decr_ng), paste(incr_ng)), by_ng, dRP, tonb_ng,
                                            netno_ng)
    }else{
      paste0(rtwill_tz, ifelse(ds[1, ]$TC < 0, paste(decr_tz), paste(incr_tz)), by_tz, dRP, tonb_tz,
             netno_tz)

      }
    }
    if(dRP != 0 & dNR != 0) {recP <- if(country == "NG") {
      paste0(rtwill_ng, ifelse(ds[1, ]$TC < 0, paste(decr_ng), paste(incr_ng)), by_ng, dRP, tonb_ng, ifelse(ds[1, ]$TC < 0, but_ng, and_ng),
                                            netinc_ng, currency, " ", dNR, incomp_ng)
    }else{

      paste0(rtwill_tz, ifelse(ds[1, ]$TC < 0, paste(decr_tz), paste(incr_tz)), by_tz, dRP, tonb_tz, ifelse(ds[1, ]$TC < 0, but_tz, and_tz),
             netinc_tz, currency, " ", dNR, incomp_tz)
      }
    }
    rec <- paste0(recT, recC, recP)

   }
  return(rec)
}


#' @name : getICrecText
#' @title : makes a text of the advice to be used in the R markdown report
#' @param ds out put of get IC recommendation function
#' @param maizePD maize product grain or cob
#' @export
getICrecText <- function(ds, maizePD){
  if(!ds[["rec"]]$rec_F){
    recF <- paste0("Fertilizer use is not recommended because ", ds[["rec"]]$reason_F)
  }else{
    dTC <- formatC(signif(ds[["rec"]]$dTC, digits=3), format="f", big.mark=",", digits=0)
    dNR <- formatC(signif(ds[["rec"]]$dNR, digits=3), format="f", big.mark=",", digits=0)
    dMP <- signif(ds[["rec"]]$dMP, digits=2)
    currency <- "NGN"
    ds[["fertilizer_rates"]]$rate <- round( ds[["fertilizer_rates"]]$rate, digits=0)

    if(maizePD == "grain"){
      #1 kg of grain ~ 7.64 cobs
      dMP <- round(dMP / 7.64, digits=0)
      recF <- paste0("We recommend applying\n",
                     paste0(ds[["fertilizer_rates"]]$rate, " kg of ", ds[["fertilizer_rates"]]$type, collapse="\n"), " ",
                     "\nfor the area of your field.\n",
                     "This will cost ", currency, " ", dTC, ". ",
                     "We expect an extra production of ", dMP, " kg of maize for the area of your field, ",
                     "and a net value increase of ", currency, " ", dNR, ".\n")
    }else{
      recF <- paste0("We recommend applying\n",
                     paste0(ds[["fertilizer_rates"]]$rate, " kg of ", ds[["fertilizer_rates"]]$type, collapse="\n"), " ",
                     "\nfor the area of your field.\n",
                     "This will cost ", currency, " ", dTC, ". ",
                     "We expect an extra production of ", dMP, " cobs for the area of your field, ",
                     "and a net value increase of ", currency, " ", dNR, ".\n")
    }
  }

  if(!is.null(ds[["rec"]]$reason_D)){
    recD <- ifelse(ds[["rec"]]$rec_D, "Plant your maize intercrop at high density: 1 m between rows and 25 cm within row (40,000 plants per hectare).",
                   paste0("Plant your maize intercrop at low density: 1 m between rows and 50 cm within row (20,000 plants per hectare) because ", ds[["rec"]]$reason_D, "."))
  }else{
    recD <- ifelse(ds[["rec"]]$rec_D, "Plant your maize intercrop at high density: 1 m between rows and 25 cm within row (40,000 plants per hectare).",
                   paste0("Plant your maize intercrop at low density: 1 m between rows and 50 cm within row (20,000 plants per hectare)."))
  }

  rec <- paste0(recF, recD)
  return(rec)
}


#' @name : FR_MarkdownText
#' @title : process the FR recom output to be used in the Markdown
#' @param rr recommendation text as obtained from the get recommendation functions
#' @param fertilizers : data frame with type, nutreint content and cost of fertilizers
#' @param userName : user name
#' @param country : NG or TZ
#' @param userPhoneNr phone number without country code
#' @param userField : if user has a anme for his field
#' @param area field size
#' @param areaUnits ha or acre
#' @param PD planting date
#' @param HD harvest date
#' @param email user email
#' @param lat latitude
#' @param lon longitude
#' @param rootUP root price
#' @param cassPD cassava product root, flour, gari, ...
#' @param cassUW cassava unit wt.
#' @param maxInv max investement
#' @param userPhoneCC user cuntry code for phone
#' @export
FR_MarkdownText <- function(rr, fertilizers,userName, country, userPhoneNr, userField, area, areaUnits, PD, HD, email, lat, lon,
                            rootUP, cassPD, cassUW, maxInv,userPhoneCC){
  #
  bags_total = round(rr$rec$TargetY, digits=1)
  totalSalePrice = rr$rec$TC + rr$rec$NR
  revenue= rr$rec$NR
  current_yield = rr$rec$CurrentY
  sum_total = rr$rec$TC

  currency <- ifelse(country == "NG", "NGN", "TZS")

  MarkDownTextD <- data.frame(name = userName, country = country, phone=userPhoneNr, field = userField, field_area = area,
                              unit_field = areaUnits, plant_date=PD, hvst_date=HD, current_yield=current_yield,
                              email = email, latitude =  lat, longitude = lon, userPhoneCC=userPhoneCC,
                              costcassava =rootUP, unitcassava = cassPD, maxinvest = maxInv,
                              sum_total = sum_total, bags_total = bags_total , product = cassPD,
                              totalSalePrice = totalSalePrice, revenue= revenue, currency = currency, cassUW = cassUW)

  MarkDownTextD$costcassava <- formatC(signif(MarkDownTextD$costcassava, digits=4), format="f", big.mark=",", digits=0)
  MarkDownTextD$maxinvest <- formatC(signif(MarkDownTextD$maxinvest, digits=4), format="f", big.mark=",", digits=0)

  write.csv(MarkDownTextD, "personalized_info.csv", row.names = FALSE)

  fertilizers_recom <- fertilizers[fertilizers$type %in% rr$fertilizer_rates$type, ]
  if(nrow(fertilizers_recom) > 0){
    fertilizers_recom <- merge(fertilizers_recom, rr$fertilizer_rates, by='type')
    fertilizers_recom$rate <- round(fertilizers_recom$rate, digits = 0)
    fertilizers_recom$bags <- round(fertilizers_recom$rate / fertilizers_recom$bagWeight, digits=1)
    fertilizers_recom$cost <- round(fertilizers_recom$rate, digits=0) *  fertilizers_recom$price
    #fertilizers_recom$cost <- (fertilizers_recom$rate *  fertilizers_recom$costPerBag) / fertilizers_recom$bagWeight
    Bagsfull <- trunc(fertilizers_recom$bags)
    bagshalf <- fertilizers_recom$bags - floor(fertilizers_recom$bags)
    bagshalf  <- ifelse(bagshalf  >= 0.25 & bagshalf <= 0.75, 0.5,  ifelse(bagshalf < 0.25, 0, 1) )
    fertilizers_recom$bags <- Bagsfull + bagshalf

    sum_total <- round(sum(fertilizers_recom$cost), digits=0)
    MarkDownTextD$sum_total <- sum_total
    MarkDownTextD$revenue <-  MarkDownTextD$totalSalePrice -  sum_total
    write.csv(MarkDownTextD, "personalized_info.csv", row.names = FALSE)


    ff <- NULL
    for(j in 1: nrow(fertilizers_recom)){
      dd <- data.frame(fertilizer = fertilizers_recom$type[j],
                       cost = fertilizers_recom$price[j],
                       costPerBag  = fertilizers_recom$costPerBag [j],
                       unit = paste(fertilizers_recom$bagWeight[j],"kg bag", sep=''),
                       kgs = fertilizers_recom$rate[j],
                       rep = NA,
                       bags = fertilizers_recom$bags[j],
                       total_cost = fertilizers_recom$cost[j])
      names(dd) <- paste(names(dd), j, sep="")
      if(j == 1){
        ff <- dd
      }else{
        ff <- cbind(ff, dd)
      }
    }
    MarkDownTextD <- cbind(MarkDownTextD, ff)
    write.csv(MarkDownTextD, "FR_MarkDownText.csv", row.names=FALSE)
  }
}

#' @name : IC_MarkdownText
#' @title : process the FR recom output to be used in the Markdown
#' @param rr recommendation text as obtained from the get recommendation functions
#' @param fertilizers : data frame with type, nutreint content and cost of fertilizers
#' @param userName : user name
#' @param country : NG or TZ
#' @param userPhoneNr phone number without country code
#' @param userField : if user has a anme for his field
#' @param area field size
#' @param areaUnits ha or acre
#' @param PD planting date
#' @param HD harvest date
#' @param email user email
#' @param lat latitude
#' @param lon longitude
#' @param rootUP root price
#' @param cassPD cassava product root, flour, gari, ...
#' @param maxInv max investement
#' @param userPhoneCC user cuntry code for phone
#' @param CMP : current maize height
#' @param maizeUW : mauze unit wt
#' @param maizePD : maize product
#' @param cassUW : cassava unit wt
#' @param maizeUP : maize unit price
#' @description  process the recom output as Markdown input
#' @export
IC_MarkdownText <- function(rr, fertilizers, userName, country,
                            userPhoneNr, userField, area, areaUnits,
                            PD, HD, email, lat, lon,
                            rootUP, cassPD, maxInv,userPhoneCC, CMP,
                            maizeUW,maizePD,cassUW,maizeUP){


  current_yield = rr$rec$dMP ## this is increase in maize yield
  totalSalePrice = rr$rec$dTC + rr$rec$dNR
  revenue= rr$rec$dNR
  sum_total = rr$rec$dTC
  currency <- ifelse(country == "NG", "NGN", "TZS")
  dMP <- rr$rec$dMP


  currency <- ifelse(country == "NG", "NGN", "TZS")

  MarkDownTextD <- data.frame(name = userName, country = country, phone=userPhoneNr, field = userField, field_area = area,
                              unit_field = areaUnits, plant_date=PD, hvst_date=HD, userPhoneCC=userPhoneCC,
                              email = email, latitude =  lat, longitude = lon,  product = cassPD, costcassava =rootUP, unitcassava = cassPD, maxinvest = maxInv,
                              currency = currency, maizeUP = maizeUP, maizeUW = maizeUW, maizePD = maizePD,
                              sum_total = sum_total, cassUW = cassUW, totalSalePrice = totalSalePrice, revenue= revenue,  dMP = dMP
                              )

  MarkDownTextD$maxinvest <- as.numeric(as.character(MarkDownTextD$maxinvest))
  MarkDownTextD$costcassava <- formatC(signif(MarkDownTextD$costcassava, digits=4), format="f", big.mark=",", digits=0)
  MarkDownTextD$maxinvest <- formatC(signif(MarkDownTextD$maxinvest, digits=4), format="f", big.mark=",", digits=0)

  if(CMP == 1){
    MarkDownTextD$CMP <- "About Knee height (~50 cm)"
  }else if(CMP == 2){
    MarkDownTextD$CMP <- "About chest height (~150 cm)"
  }else if(CMP == 3){
    MarkDownTextD$CMP <- "Larger than a person with yellowish leaves (~200 cm)"
  }else if(CMP == 4){
    MarkDownTextD$CMP <- "Larger than a person with green leaves (~200 cm)"
  }else if(CMP == 5){
    MarkDownTextD$CMP <- "Larger than a person with dark green leaves (~200 cm)"
  }

  if(MarkDownTextD$maizePD == "fresh_cob"){
    MarkDownTextD$unitproduct <- paste(MarkDownTextD$currency, " ", MarkDownTextD$maizeUP, " per ",
                                    MarkDownTextD$maizePD, ".", sep="")
  }else{
    MarkDownTextD$unitproduct <- paste(MarkDownTextD$currency, " ", MarkDownTextD$maizeUP, " per ", MarkDownTextD$maizeUW,
          " kg of grain.", sep="")
  }

  write.csv(MarkDownTextD, "personalized_info.csv", row.names = FALSE)

  fertilizers_recom <- fertilizers[fertilizers$type %in% rr$fertilizer_rates$type, ]
  if(nrow(fertilizers_recom) > 0){
    fertilizers_recom <- merge(fertilizers_recom, rr$fertilizer_rates, by='type')
    fertilizers_recom$rate <- round(fertilizers_recom$rate, digits = 0)
    fertilizers_recom$cost <- round(fertilizers_recom$rate, digits=0) *  fertilizers_recom$price
    fertilizers_recom$bags <- round(fertilizers_recom$rate / fertilizers_recom$bagWeight, digits=1)
    Bagsfull <- trunc(fertilizers_recom$bags)
    bagshalf <- fertilizers_recom$bags - floor(fertilizers_recom$bags)
    bagshalf  <- ifelse(bagshalf  >= 0.25 & bagshalf <= 0.75, 0.5,  ifelse(bagshalf < 0.25, 0, 1) )
    fertilizers_recom$bags <- Bagsfull + bagshalf


    MarkDownTextD$sum_total <- sum(fertilizers_recom$cost)
    MarkDownTextD$revenue =  MarkDownTextD$totalSalePrice -  MarkDownTextD$sum_total
    write.csv(MarkDownTextD, "personalized_info.csv", row.names = FALSE)


    ff <- NULL
    for(j in 1: nrow(fertilizers_recom)){
      dd <- data.frame(fertilizer = fertilizers_recom$type[j],
                       cost = fertilizers_recom$price[j],
                       costPerBag  = fertilizers_recom$costPerBag [j],
                       unit = paste(fertilizers_recom$bagWeight[j],"kg bag", sep=''),
                       kgs = fertilizers_recom$rate[j],
                       rep = NA,
                       bags = fertilizers_recom$bags[j],
                       total_cost = round(fertilizers_recom$rate[j], digits=0) * fertilizers_recom$price[j] )
      names(dd) <- paste(names(dd), j, sep="")
      if(j == 1){
        ff <- dd
      }else{
        ff <- cbind(ff, dd)
      }
    }

    MarkDownTextD <- cbind(MarkDownTextD, ff, rr$rec)
    write.csv(MarkDownTextD, "IC_MarkDownText.csv", row.names=FALSE)
  }
}


#' @name : CIS_MarkdownText
#' @title : process the CIS recom output to be used in the Markdown
#' @param rr recommendation text as obtained from the get recommendation functions
#' @param fertilizers : data frame with type, nutreint content and cost of fertilizers
#' @param userName : user name
#' @param country : NG or TZ
#' @param userPhoneNr phone number without country code
#' @param userField : if user has a anme for his field
#' @param area field size
#' @param areaUnits ha or acre
#' @param PD planting date
#' @param HD harvest date
#' @param email user email
#' @param lat latitude
#' @param lon longitude
#' @param rootUP root price
#' @param cassPD cassava product root, flour, gari, ...
#' @param cassUW cassave unit wt
#' @param maxInv max investement
#' @param userPhoneCC user cuntry code for phone
#' @param sweetPotatoUP : sweetpotato unit price
#' @param sweetPotatoPD : swwetpotato product
#' @param tuberUP : tuber unit price
#' @param sweetPotatoUW : sweet potato unit wt
#' @export
CIS_MarkdownText <- function(rr, fertilizers, userName, country,
                            userPhoneNr, userField, area, areaUnits,
                            PD, HD, email, lat, lon,
                            rootUP, cassPD, cassUW, maxInv,userPhoneCC,
                            sweetPotatoUP,sweetPotatoPD,tuberUP, sweetPotatoUW){

  #current_yield = rr$rec$dMP ## this is increase in maize yield
  totalSalePrice = rr$rec$dTC + rr$rec$dNR
  revenue= rr$rec$dNR
  sum_total = rr$rec$dTC
  currency <- ifelse(country == "NG", "NGN", "TZS")
  #dMP <- rr$rec$dMP

  currency <- ifelse(country == "NG", "NGN", "TZS")

  MarkDownTextD <- data.frame(name = userName, country = country, phone=userPhoneNr, field = userField, field_area = area,
                              unit_field = areaUnits, plant_date=PD, hvst_date=HD, userPhoneCC=userPhoneCC,
                              email = email, latitude =  lat, longitude = lon,  product = cassPD,
                              costcassava =rootUP, unitcassava = cassPD, maxinvest = maxInv, currency = currency,  sum_total = sum_total,
                              product = cassPD, totalSalePrice = totalSalePrice, revenue= revenue, currency = currency,cassUW = cassUW,
                              sweetPotatoUW = sweetPotatoUW,sweetPotatoUP = sweetPotatoUP,sweetPotatoPD = sweetPotatoPD,tuberUP = tuberUP)



  MarkDownTextD$costcassava <- formatC(signif(MarkDownTextD$costcassava, digits=4), format="f", big.mark=",", digits=0)
  MarkDownTextD$maxinvest <- formatC(signif(MarkDownTextD$maxinvest, digits=4), format="f", big.mark=",", digits=0)


  write.csv(MarkDownTextD, "personalized_info.csv", row.names = FALSE)

  fertilizers_recom <- fertilizers[fertilizers$type %in% rr$fertilizer_rates$type, ]
  if(nrow(fertilizers_recom) > 0){
    fertilizers_recom <- merge(fertilizers_recom, rr$fertilizer_rates, by='type')
    fertilizers_recom$rate <- round(fertilizers_recom$rate, digits = 0)
    fertilizers_recom$cost <- round(fertilizers_recom$rate, digits=0) *  fertilizers_recom$price
    fertilizers_recom$bags <- round(fertilizers_recom$rate / fertilizers_recom$bagWeight, digits=1)
    Bagsfull <- trunc(fertilizers_recom$bags)
    bagshalf <- fertilizers_recom$bags - floor(fertilizers_recom$bags)
    bagshalf  <- ifelse(bagshalf  >= 0.3 & bagshalf <= 0.65, 0.5,  ifelse(bagshalf < 0.3, 0, 1) )
    fertilizers_recom$bags <- Bagsfull + bagshalf

    MarkDownTextD$sum_total <- sum(fertilizers_recom$cost)
    MarkDownTextD$revenue =  MarkDownTextD$totalSalePrice -  MarkDownTextD$sum_total
    write.csv(MarkDownTextD, "personalized_info.csv", row.names = FALSE)


    ff <- NULL
    for(j in 1: nrow(fertilizers_recom)){
      dd <- data.frame(fertilizer = fertilizers_recom$type[j],
                       cost = fertilizers_recom$price[j],
                       costPerBag  = fertilizers_recom$costPerBag [j],
                       unit = paste(fertilizers_recom$bagWeight[j],"kg bag", sep=''),
                       kgs = round(fertilizers_recom$rate[j], digits=0),
                       rep = NA,
                       bags = fertilizers_recom$bags[j],
                       total_cost = round(fertilizers_recom$rate[j], digits=0) * fertilizers_recom$price[j] )


      names(dd) <- paste(names(dd), j, sep="")
      if(j == 1){
        ff <- dd
      }else{
        ff <- cbind(ff, dd)
      }
    }

    MarkDownTextD <- cbind(MarkDownTextD, ff, rr$rec)
    write.csv(MarkDownTextD, "CIS_MarkDownText.csv", row.names=FALSE)
  }
}


#' @name : PPSP_MarkdownText
#' @title : process the PP recom output to be used in the Markdown
#' @param rr recommendation text as obtained from the get recommendation functions
#' @param fname : file name for output
#' @param userName : user name
#' @param country : NG or TZ
#' @param userPhoneNr phone number without country code
#' @param userField : if user has a anme for his field
#' @param area field size
#' @param areaUnits ha or acre
#' @param PD planting date
#' @param HD harvest date
#' @param email user email
#' @param lat latitude
#' @param lon longitude
#' @param rootUP root price
#' @param cassPD cassava product root, flour, gari, ...
#' @param cassUW casasva unit wt
#' @param maxInv max investement for fertilizers
#'
#' @export
PPSP_MarkdownText <- function(rr, fname, userName=userName, country=country,
                              userPhoneNr=userPhoneNr, userField=userField, area=area, areaUnits=areaUnits,
                              PD =PD, HD=HD, email=email, lat=lat, lon=lon,
                              rootUP = rootUP, cassPD = cassPD, cassUW = cassUW, maxInv = maxInv){

  currency <- ifelse(country == "NG", "NGN", "TZS")

  MarkDownTextD <- data.frame(name = userName, country = country, phone=userPhoneNr, field = userField, field_area = area,
                              unit_field = areaUnits, plant_date=PD, hvst_date=HD,
                              email = email, latitude =  lat, longitude = lon,
                              costcassava =rootUP, unitcassava = cassPD,
                              maxinvest = maxInv, cassUW = cassUW, product = cassPD, currency = currency)

  write.csv(MarkDownTextD, "personalized_info.csv", row.names = FALSE)

  fname2 <- paste(fname, "_MarkDownText.csv", sep='')
  write.csv(MarkDownTextD, "PP_MarkDownText.csv", row.names=FALSE)
}


#' @name : getRecommendation for all use cases
#' @title : PP_MarkdownText
#' @description  process the PP recom output to be used in the Markdown
#' @param userName : user name
#' @param country : NG or TZ
#' @param userPhoneNr phone number without country code
#' @param userField : if user has a anme for his field
#' @param area field size
#' @param areaUnits ha or acre
#' @param PD planting date
#' @param HD harvest date
#' @param email user email
#' @param lat latitude
#' @param lon longitude
#' @param rootUP root price
#' @param cassPD cassava product root, flour, gari, ...
#' @param cassUW cassava unit wt
#' @param maxInv max investement
#' @param userPhoneCC user cuntry code for phone
#' @param ploughing : true or false
#' @param ridging ; true or false
#' @param method_ploughing : tractor or mannual
#' @param method_ridging : rtactor or mannual
#' @export
PP_MarkdownText <- function(userName, country, userPhoneNr, userField, area, areaUnits, PD, HD, email,lat, lon,rootUP,cassPD,cassUW,
                            maxInv, ploughing, ridging, method_ploughing, method_ridging, userPhoneCC){
  MarkDownTextD <- data.frame(name = userName, country = country, phone=userPhoneNr, field = userField, field_area = area,
                              unit_field = areaUnits, plant_date=PD, hvst_date=HD,
                              email = email, latitude =  lat, longitude = lon,
                              costcassava =rootUP, unitcassava = cassPD, cassUW = cassUW,
                              maxinvest = maxInv, product = cassPD, ploughing = ploughing,  ridging = ridging,
                              method_ploughing = method_ploughing,  method_ridging = method_ridging, userPhoneCC=userPhoneCC)



  write.csv(MarkDownTextD, "PP_MarkDownText.csv", row.names=FALSE)
}


#' @name : SP_MarkdownText
#' @title : process the SP recom output to be used in the Markdown
#' @param userName : user name
#' @param country : NG or TZ
#' @param userPhoneNr phone number without country code
#' @param userField : if user has a anme for his field
#' @param area field size
#' @param areaUnits ha or acre
#' @param PD planting date
#' @param HD harvest date
#' @param email user email
#' @param lat latitude
#' @param lon longitude
#' @param saleSF : saling cassava for factory true or false
#' @param nameSF ; name of cassava processing factroy
#' @param maxInv : max investment for fertilizers
#' @param ploughing : true or false
#' @param ridging : true or false
#' @param method_ploughing : tractor or mannual
#' @param method_ridging : tractor or mannual
#' @param userPhoneCC : user phone number country code
#' @param CMP : current maize height
#' @param riskAtt : reis attitude of user
#' @param PD_window :  planting window
#' @param HD_window : harvest window
#' @param cassPD : cassava product
#' @param cassUW : cassava unit wt
#' @param cassUP : cassava price
#' @param cassUP_m1 : cassava price one month from the harvest data in the future
#' @param cassUP_m2 : cassava price two months from the harvest data in the future
#' @param cassUP_p1 : cassava price one month from the harvest data in the past
#' @param cassUP_p2 : cassava price two months from the harvest data in the past
#' @export
SP_MarkdownText <- function(userName, country, userPhoneNr, userField, area, areaUnits, PD, HD, email,lat, lon,saleSF, nameSF,
                            maxInv, ploughing, ridging, method_ploughing, method_ridging, userPhoneCC, CMP,riskAtt,
                            PD_window, HD_window,cassPD,cassUW,cassUP,cassUP_m1, cassUP_m2, cassUP_p1,cassUP_p2){
  MarkDownTextD <- data.frame(name = userName, country = country, phone=userPhoneNr, field = userField, field_area = area,
                              unit_field = areaUnits, plant_date=PD, hvst_date=HD,
                              email = email, latitude =  lat, longitude = lon,
                              maxinvest = maxInv, saleSF = saleSF, nameSF = nameSF, CMP = CMP, riskAtt = riskAtt, PD, HD,
                              PD_window = PD_window,
                              HD_window = HD_window, cassPD = cassPD,
                              cassUW = cassUW, cassUP = cassUP, cassUP_m1 = cassUP_m1, cassUP_m2 = cassUP_m2,
                              cassUP_p1 = cassUP_p1, cassUP_p2 = cassUP_p2, userPhoneCC=userPhoneCC )
  write.csv(MarkDownTextD, "SP_MarkDownText.csv", row.names=FALSE)
}



#' @name : Function to obtain recommendations on cassava-sweet potato intercropping.
#' @title :  Function to obtain recommendations on cassava-sweet potato intercropping.
#             Returns (i) a 1-row dataframe cost-benefit parameters (extra yield, cost and net revenue, and whether to apply
#             fertilizer and whether to intercrop, and why (not)) , and (ii) a data.frame with types of fertilizer and rates to apply (zeros included).
#' @param areaHa ha or acre
#' @param FCY : farmers current yield
#' @param tuberUP : tuber price
#' @param rootUP : root price
#' @param fertilizers : data frame with type, nutreint content and cost of fertilizers
#' @param riskAtt : rist attitude of user
#' @return:     list of 2 dataframes: (i) cost benefit analysis for most profitable system, and (ii) fertilizer rates to apply.
#' @export
getCISrecommendations <- function(areaHa = 1,
                                  FCY = 11,
                                  tuberUP,
                                  rootUP,
                                  fertilizers,
                                  riskAtt = c(0, 1, 2)){



  #calculating expected yield increase from fertilizer
  FSY <- 0.7 * FCY #expected yield of a sweet potato monocrop
  FSY <- FSY * areaHa #extra sweet potato production for the area of the field TODO check with Pieter
  GR_MC <- FCY * rootUP #gross revenue of cassava monocrop
  GR_IC <- GR_MC * 0.6 + 0.8 * FSY * tuberUP #gross revenue of cassava-sweet potato intercrop
  rec_IC <- GR_MC < GR_IC

  if(rec_IC){
    #calculating fertilizer requirement
    E <- t(data.matrix(fertilizers[,2:4]))
    #F <- c(68, 19.6, 56.8) #ideally 2 bags of urea + 6 bags of NPK15:15:15
    F <- c(68, 33.2, 60) #ideally 2 bags of urea + 8 bags of NPK17:17:17
    G <- diag(nrow(fertilizers))
    H <- rep(0, nrow(fertilizers))
    Cost <- fertilizers$price

    #calculating fertilizer recommendation and total cost of fertilizer
    FR <- limSolve::linp(E, F, G, H, Cost)$X
    FR[FR<25] <- 0 #dropping all rates less than 25 kg/ha
    FR <- FR * areaHa #adjusting to field area

    #calculating total cost
    dTC <- c(FR %*% fertilizers$price)
   # dGR <- ifelse(FCY > 20, 0, FCY * 0.6 * 0.4 * rootUP + FSY * 0.8 * 0.2 * tuberUP) #gross revenue increase: 40% yield increase in cassava + 20% yield increase in sweet potato, but not in fields with yields above 20 t/ha

    #gross revenue increase: 40% yield increase in cassava + 20% yield increase in sweet potato, but not in fields with yields above 20 t/ha in yield classes 1-3, 20% in cassava and 10% in sweet potato in yield class 4, and 0 in yield class 5
    dGR <- ifelse(FCY > 30, 0, ifelse(FCY > 20, FCY * 0.6 * 0.2 * rootUP + FSY * 0.8 * 0.1 * tuberUP, FCY * 0.6 * 0.4 * rootUP + FSY * 0.8 * 0.2 * tuberUP))



    #evaluating if a solution was found
    if(dTC==0){
      dGR <- 0

      #trans


      reason_F <- "Mbolea sahihi haipatikani"
      rec_F <- FALSE
    }else{

      #trans
      reason_F <- "Matumizi ya mbolea haipendekezwi"
      rec_F <- TRUE
    }
  }else{
    dTC <- 0
    FR <- 0
    dGR <- 0

    #trans
    reason_F <- "Kilimo mchanganyiko haupendekezwi.Panda muhogo peke yake."
    rec_F <- FALSE
  }

  #net revenue increase from fertilizer
  dNR <- dGR - dTC

  if(dTC > 0){
    #minimal required net revenue increase from fertilizer needed (taking into account risk attitude of user)
    dNRmin <- dTC * ifelse(riskAtt == 0, 1.8, ifelse(riskAtt == 1, 1, 0.2))

    #check profitability of fertilizer use
    if(dNR > dNRmin){
      rec_F <- TRUE

      #trans
      reason_F <- "Matumizi ya mbolea inapendekezwa"
    }else{
      dTC <- 0
      dGR <- 0
      dNR <- 0
      FR <- FR * 0
      rec_F <- FALSE

      #trans
      reason_F <- "Matumizi ya mbolea haina faida ya kutosha"
    }
  }

  #output
  rec <- data.frame(rec_IC=rec_IC, #boolean indicating whether intercropping is recommended (more profitable than monocropping cassava)
                    rec_F=rec_F, #TRUE or FALSE indicating if fertilizer application is recommended
                    dNR=dNR, #net revenue increase from fertilizer use (in local currency)
                    dTC=dTC, #extra cost for fertilizer use (in local currency)
                    reason_F=reason_F #reason why fertilizer application is not recommended
  )

  fertilizer_rates <- data.frame(type=fertilizers$type, rate=FR) #fertilizer rates to apply
  fertilizer_rates <- fertilizer_rates[fertilizer_rates$rate > 0,]

  return(list(rec=rec, fertilizer_rates=fertilizer_rates))

}

#' @name : getCISrecText
#' @title : process the recom output as Markdown input
#' @param ds : get recommendation output
#' @export
getCISrecText <- function(ds){

  if(!ds[["rec"]]$rec_IC){

    # recIC <- "Intercropping is not recommended. Growing a cassava monocrop will give you a higher profit.\n"
    # recF  <- "If you consider investing in fertilizer, please use our Fertilizer Recommendations Tool to obtain fertilizer advice for a cassava monocrop."

    recIC <- "Kilimo mchanganyiko haipendekezwi. Kupanda muhogo peke yake utakupatia faida ya juu.\n"
    recF  <- "Kama ungependa kuwekeza katika mbolea, tafadhali tumia chombo chetu cha mapandekezo ya mbolea ili kupata ushauri wa kupanda muhogo bila mseto."


  }else{

    recIC <- "Tunapendekeza kilimo mseto wa muhogo na viazi vitamu. Hii itakupatia faida ya juu na mapato ya haraka kutoka kwenye viazi vitamu."
    #recIC <- "This will generate a higher profit overall, and also give you access to early income from sweet potato.\n"

    if(!ds[["rec"]]$rec_F){

      #recF <- paste0("Fertilizer use is not recommended because ", ds[["rec"]]$reason_F, ".\n")
      recF <- paste0("Haishauriwi kutumia  mbolea kwa sababu ", ds[["rec"]]$reason_F, ".\n")

    }else{

      dTC <- formatC(signif(ds[["rec"]]$dTC, digits=3), format="f", big.mark=",", digits=0)
      dNR <- formatC(signif(ds[["rec"]]$dNR, digits=3), format="f", big.mark=",", digits=0)
      #currency <- ifelse(country == "NG", "NGN", "TZS")
      currency <- "TZS"

      #recF <- paste0("We recommend applying\n",
                     # paste0(round(ds[["fertilizer_rates"]]$rate), " kg of ", ds[["fertilizer_rates"]]$type, collapse="\n"),
                     # "\nfor the area of your field.\n",
                     # "This will cost ", currency, " ", dTC, ". ",
                     # "We expect a net value increase of ", currency, " ", dNR, " for the area of your field.")


      recF <- paste0("Tunapendekeza utumie\n",
         paste0("kilo ", round(ds[["fertilizer_rates"]]$rate), " ya ", ds[["fertilizer_rates"]]$type, collapse="\n"), " ",
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

  return(rec)

}





#' @name: getRecommendation
#' @title : Function with all inputs required by the DST app, and calling use case-specific wrapper functions to calculated recommendations on IC, FR, PP and SP.
#'             This function also calls on functions to send recommendation reports by SMS and/or email.Wrapper function to calculate recommendations across all use cases.
#' @param: country    : Character, c("NG", "TZ")
#' @param: lat        : Numeric, Latitude in decimal degrees
#' @param: lon        : Numeric, Longitude in decimal degrees
#' @param: area  : Numeric,  value for area of the field, if the user does not give this value, it will be set at 1
#' @param: areaUnits  : Character, c("acre", "ha" or "m2")
#' @param: IC         : Logical, c(TRUE, FALSE), indicating if an intercrop (TRUE) or a monocrop (FALSE) is grown
#' @param: intercrop  : Character, c(NA, "maize", "sweetpotato"), Intercrop species grown, either maize or sweetpotato, or NA if monocrop, shold be in
#' @param: FR         : Logical, c(NA, TRUE, FALSE), indicating if fertilizer recommendations are requested, NA if IC == TRUE and country == "TZ"
#' @param: PP         : Logical, c(NA, TRUE, FALSE), indicating if planting practice recommendations are requested, NA if IC == TRUE or country == "TZ"
#' @param: SPP        : Logical, c(NA, TRUE, FALSE), indicating if scheduled planting - advice on planting date is requested, NA if IC == TRUE
#' @param: SPH        : Logical, c(NA, TRUE, FALSE), indicating if scheduled planting - advice on harvest date is requested, NA if IC == TRUE
#' @param: PD         : Character, Planting date (date format)
#' @param: HD         : Character, Harvest data (date format)
#' @param: PD_window  : planting day window, it should be c(0,1,2), zero if the user has one fixed PD, 1 if PD can be +- 1 month and 2 if PD can be +-2 months
#' @param: HD_window  : Harvest day window, it should be c(0,1,2), zero if the user has one fixed HD, 1 if HD can be +- 1 month and 2 if HD can be +-2 months
#' @param: fallowType  : Categorical: c("bush", "broad_leaves", "grass", "none"), type of fallow prior to land clearing
#' @param: fallowHeight: Categorical: c(NA, 100, 150, 200), height of the fallow prior to clearing
#' @param: fallowGreen : Logical: c(TRUE, FALSE), indicating if the fallow is lush, fresh and green (TRUE) or withered, dry or dead (FALSE)
#' @param: problemWeeds: Logical:  c(TRUE, FALSE), indicating the presence of any problem weeds that need to be controlled by herbicide (Tithonia, Imperata,...)
#' @param: tractor_plough: Logical, indicating if the user has access to a tractor with plough, NA if PP != TRUE
#' @param: tractor_harrow: Logical, indicating if the user has access to a tractor with harrow, NA if PP != TRUE
#' @param: tractor_ridger: Logical, indicating if the user has access to a tractor with ridger, NA if PP != TRUE
#' @param: cost_LMO_areaBasis: Categorical: c("areaUnit", "areaField"), indicating if the area basis for the cost of land operations is for 1 areaUnits, or for the entire field
#' @param: cost_tractor_ploughing: Cost for a ploughing operation by tractor c() for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField), NA if tractor_plough == FALSE
#' @param: cost_tractor_harrowing: Cost for a rarrowing operation by tractor for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField), NA if tractor_harrow == FALSE
#' @param: cost_tractor_ridging: Cost for a ridging operation by tractor for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField), NA if tractor_ridger == FALSE
#' @param: cost_manual_ploughing: Cost for a manual ploughing operation for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField)
#' @param: cost_manual_harrowing: Cost for a manual harrowing operation for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField)
#' @param: cost_manual_ridging: Cost for a manual ridging operation for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField)
#' @param: cost_weeding1: Cost for the first weeding operation for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField)
#' @param: cost_weeding2: Cost for the second and subsequent weeding operations for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField)
#' @param: ploughing   : Logical, c(NA, TRUE, FALSE), indicating if the user conducts a ploughing operation in current practice, NA if PP != TRUE
#' @param: harrowing   : Logical, c(NA, TRUE, FALSE), indicating if the user conducts a harrowing operation in current practice, NA if PP != TRUE
#' @param: ridging     : Logical, c(NA, TRUE, FALSE), indicating if the user ridges his/her field in current practice (NA, TRUE, FALSE), NA if PP != TRUE
#' @param: method_ploughing: Categorical: c("manual", "tractor", "N/A"), method of ploughing currently applied by the farmer. Note: "N/A" = not applicable becasue nPlough=0
#' @param: method_harrowing: Categorical: c("manual", "tractor", "N/A"), method of harrowing currently applied by the farmer. Note: "N/A" = not applicable becasue nHarrow=0
#' @param: method_ridging: Categorical: c("manual", "tractor", "N/A"), method of ridging currently applied by the farmer. Note: "N/A" = not applicable becasue ridges=FALSE
#' @param: FCY        : Farmer-reported current yield, in tonnes FM per ha (optional, default value = 11)
#' @param: CMP        : c(1,2,3,4,5),  Current maize performance, score on a scale of 1 (very yellow and stunted) .. 5 (tall and dark green), NA if IC != TRUE and FR != TRUE, or NA if the user does not know (NA = default)
#' @param: saleSF     : Logical, c(NA, TRUE, FALSE), indicating if the user is selling roots to a registered starch factory at factory-fixed prices
#' @param: nameSF     : c(NA, "AlliedAtlanticDistilleries", "MatnaStarch", "PsaltryMarketers", "PsaltryOutgrowers",	"Greentech", "ThaiFarm", "FJS"), Name of starch factory where roots will be sold, NA if saleSF = FALSE
#' @param: cassPD     : c("roots", "chips", "flour", "gari"), Type of cassava produce sold
#' @param: cassUW 	: c(1, 50, 100, 1000), Unit weight at which cassava produce is sold, in kg, common measures are 1 (per kg), 50 (per 50kg bag), 100 (per 100kg bag) and 1000 (per tonne).
#' @param: cassUP 	: Price of 1 cassava produce unit (fresh wt) in local currency, can be NA if user does not know and then 12000 NGN and ... TZS is used as default
#' @param: cassUP_m1: Price of 1 cassava produce unit (fresh wt) 1 month in the future, can be NA if users does not know;
#' @param: cassUP_m2: Price of 1 cassava produce unit (fresh wt) 2 month in the future, can be NA if users does not know;
#' @param: cassUP_p1: Price of 1 cassava produce unit (fresh wt) 1 month before, can be NA if users does not know
#' @param: cassUP_p2: Price of 1 cassava produce unit (fresh wt) 2 month before, can be NA if users does not know
#' @param: sweetPotatoPD: Type of sweet potato produce sold (tubers, flour), NA if IC != TRUE and country == "TZ"
#' @param: sweetPotatoUW: Unit weight at which sweet potato produce is sold, in kg; common measures are 1 (per kg), 50 (per 50kg bag), 100 (per 100kg bag), NA if IC != TRUE and country == "TZ"; can be NA if user does not know.
#' @param: sweetPotatoUP: Price of 1 sweet potato produce unit in local currency, NA if IC != TRUE and country == "TZ"; can be NA if user does not know.
#' @param: maizePD    : Type of maize produce sold (fresh cobs, dry cobs, grain), NA if IC != TRUE and country == "NG"
#' @param: maizeUW    : c(NA, 1, 50, 100), Unit weight at which maize produce is sold, in kg, common measures are 1 (per kg), 50 (per 50kg bag), 100 (per 100kg bag), NA if IC != TRUE and country == "NG" or NA if maizePD == "grain", can be NA if user does not know.
#' @param: maizeUP    : Price of 1 maize produce unit (or cob, if maizePC == TRUE) in local currency, NA if IC != TRUE and country == "NG", can be NA if user does not know.
#' @param: maxInv     : Maximal investment in fertilizer, for the area of the field in local currency, NA if FR != TRUE, default = NA (if user does not wish to set an investment ceiling)
#' @param: SMS        : Logical, c(TRUE, FALSE), indicating if recommendations must be sent by SMS to the user
#' @param: email      : Logical, c(TRUE, FALSE), indicating if recommendations must be sent by email to the user
#' @param: userPhoneCC: Country code of the phone number of the user requesting the recommendations (to send recommendations by SMS), default = NA (if user does not wish to receive recommendations by SMS), example 234 for Nigeria
#' @param: userPhoneNr: Phone number of the user requesting the recommendations, without the initial zero (to send recommendations by SMS), default = NA (if user does not wish to receive recommendations by SMS), excludes the initial zero, stored as numerical (e.g., 789123456)
#' @param: userName   : Name of the user requesting the recommendations (to be included in the email report), default = NA (if user does not wish to receive recommendations by email)
#' @param: userEmail  : Email address of the user requesting the recommendations (to be included in the email report), default = NA (if user does not wish to receive recommendations by email)
#' @param: userField  : Name or desciption of the field (to be included in the email report, and aid the user to recall for which field recommendations were requested), default = NA (if user does not wish to receive recommendations by email)
#' @param: riskAtt = c(0, 1, 2): Risk attitude of the farmer, with 0 being very risk-averse (low income farmers who cannot afford to loose on investment), 1 = risk-neutral and 2 = risk-loving (higher income farmers willing to take their chances for higher net returns)
#' @param: ureaavailable = TRUE 	: is urea available in the market
#' @param: ureaCostperBag = NA	: The cost of  abg of urea
#' @param: ureaBagWt = 50			: The weight of the bag of urea
#' @param: MOPavailable = TRUE
#' @param: MOPCostperBag = NA
#' @param: MOPBagWt = 50
#' @param: DAPavailable = TRUE
#' @param: DAPCostperBag = NA
#' @param: DAPBagWt = 50
#' @param: NPK201010available = TRUE
#' @param: NPK201010CostperBag = NA
#' @param: NPK201010BagWt =50
#' @param: NPK151515available = TRUE
#' @param: NPK151515CostperBag = NA
#' @param: NPK151515BagWt =50
#' @param: TSPavailable = TRUE
#' @param: TSPCostperBag = NA
#' @param: TSPBagWt = 50
#' @param: NPK171717available = TRUE
#' @param: NPK171717CostperBag = NA
#' @param: NPK171717BagWt = 50
#' @param: Nafakaavailable = FALSE
#' @param: NafakaCostperBag = NA
#' @param: NafakaBagWt = 50
#' @param: CANavailable = FALSE
#' @param: CANCostperBag = NA
#' @param: CANBagWt = 50
#' @param: SSPavailable = FALSE
#' @param: SSPCostperBag = NA
#' @param: SSPBagWt = 50
#' @param: newFert1name = NA		:this is for new fertilizer and if defined the N_cont, P2O5, K2O, (all in percent like 20, 15, ..)and CostperBag & BagWt have to be provided. if any of these input is missing, OtherFertilizers should be NULL
#' @param: newFert1N_cont=NA		: N content of the new fertilizer
#' @param: newFert1P2O5=NA	    : P2O5 of the new fertilizer
#' @param: newFertK2O = NA		: K2O of the new fertilizer
#' @param: newFertCostperBag=NA		: cost per bag,
#' @param: newFert1BagWt=NA			: The weight of the bag with new fertilizer
#' @param: newFert2name = NA
#' @param: newFert2N_cont=NA
#' @param: newFert2P2O5=NA
#' @param: newFert2K2O = NA
#' @param: newFert2CostperBag=NA
#' @param: newFert2BagWt=NA
#' @param: newFert3name = NA
#' @param: newFert3N_cont=NA
#' @param: newFert3P2O5=NA
#' @param: newFert3K2O = NA
#' @param: newFert3CostperBag=NA
#' @param: newFert3BagWt=NA
#' @param: newFert4name = NA
#' @param: newFert4N_cont=NA
#' @param: newFert4P2O5=NA
#' @param: newFert4K2O = NA
#' @param: newFert4CostperBag	=NA
#' @param: newFert4BagWt=NA
#' @param: newFert5name = NA
#' @param: newFert5N_cont=NA
#' @param: newFert5P2O5=NA
#' @param: newFert5K2O = NA
#' @param: newFert5CostperBag=NA
#' @param:  newFert5BagWt=NA
#' @returns:     Vector of with recommendation texts to display.
#' @export
getRecommendation <- function(country = c("NG", "TZ"),
                              lat,
                              lon,
                              area,
                              areaUnits = c("acre", "ha", "m2"),
                              IC = c(TRUE, FALSE),
                              intercrop = c("maize", "sweetpotato"),
                              FR = c(TRUE, FALSE),
                              PP = c(TRUE, FALSE),
                              SPP = c(TRUE, FALSE),
                              SPH = c(TRUE, FALSE),
                              PD,
                              HD,
                              PD_window = c(0, 1, 2),
                              HD_window = c(0, 1, 2),
                              fallowType = c("bush", "broad_leaves", "grass", "none"),
                              fallowHeight = c(NA, 100, 150, 200),
                              fallowGreen = c(TRUE, FALSE),
                              problemWeeds = c(TRUE, FALSE),
                              tractor_plough = c(TRUE, FALSE),
                              tractor_harrow = c(TRUE, FALSE),
                              tractor_ridger = c(TRUE, FALSE),
                              cost_LMO_areaBasis = c("areaUnit", "areaField"),
                              cost_tractor_ploughing = NA,
                              cost_tractor_harrowing = NA,
                              cost_tractor_ridging = NA,
                              cost_manual_ploughing = NA,
                              cost_manual_harrowing = NA,
                              cost_manual_ridging = NA,
                              cost_weeding1 = NA,
                              cost_weeding2 = NA,
                              ploughing = c(TRUE, FALSE),
                              harrowing = c(TRUE, FALSE),
                              ridging = c(TRUE, FALSE),
                              method_ploughing = c("manual", "tractor", "N/A"),
                              method_harrowing = c("manual", "tractor", "N/A"),
                              method_ridging = c("manual", "tractor", "N/A"),
                              FCY,
                              CMP,
                              saleSF = c(TRUE, FALSE),
                              nameSF = c(NA, "AlliedAtlanticDistilleries", "MatnaStarch", "PsaltryMarketers", "PsaltryOutgrowers",	"Greentech", "ThaiFarm", "FJS"),
                              cassPD = c("roots", "chips", "flour", "gari"),
                              cassUW = c(1, 50, 100, 1000),
                              cassUP,
                              cassUP_m1,
                              cassUP_m2,
                              cassUP_p1,
                              cassUP_p2,
                              maizePD =  c("fresh_cob", "grain"),
                              maizeUW = c(1, 50, 100),
                              maizeUP,
                              sweetPotatoPD = c("tubers", "flour"),
                              sweetPotatoUW = NA,
                              sweetPotatoUP = NA,
                              maxInv = NA,
                              SMS = c(TRUE, FALSE),
                              email = c(TRUE, FALSE),
                              userPhoneCC,
                              userPhoneNr,
                              userName,
                              userEmail,
                              userField,
                              riskAtt = c(0, 1, 2),
                              NPK201216available = c(TRUE, FALSE), NPK201216CostperBag = NA, NPK201216BagWt = 50,
                              ureaavailable = c(TRUE, FALSE), ureaCostperBag = NA, ureaBagWt =50,
                              MOPavailable = c(TRUE, FALSE), MOPCostperBag = NA, MOPBagWt =50,
                              DAPavailable = c(TRUE, FALSE), DAPCostperBag = NA, DAPBagWt =50,
                              NPK201010available = c(TRUE, FALSE), NPK201010CostperBag = NA, NPK201010BagWt = 50,
                              NPK151515available = c(TRUE, FALSE), NPK151515CostperBag = NA, NPK151515BagWt =50,
                              TSPavailable = c(TRUE, FALSE), TSPCostperBag = NA, TSPBagWt =50,
                              NPK171717available = c(TRUE, FALSE), NPK171717CostperBag = NA, NPK171717BagWt = 50,
                              Nafakaavailable = c(FALSE, FALSE), NafakaCostperBag = NA, NafakaBagWt =50,
                              CANavailable = c(TRUE, FALSE), CANCostperBag = NA, CANBagWt =50,
                              SSPavailable = c(TRUE, FALSE), SSPCostperBag = NA, SSPBagWt = 50,
                              newFert1name = c(TRUE, FALSE) ,newFert1N_cont = NA,newFert1P2O5 = NA, newFert1K2O = NA,newFert1CostperBag = NA,newFert1BagWt = NA,
                              newFert2name = c(TRUE, FALSE),newFert2N_cont = NA,newFert2P2O5 = NA, newFert2K2O = NA,newFert2CostperBag = NA,newFert2BagWt = NA,
                              newFert3name = c(TRUE, FALSE), newFert3N_cont = NA, newFert3P2O5 = NA, newFert3K2O = NA, newFert3CostperBag = NA, newFert3BagWt = NA,
                              newFert4name = c(TRUE, FALSE), newFert4N_cont = NA, newFert4P2O5 = NA, newFert4K2O = NA, newFert4CostperBag = NA, newFert4BagWt = NA,
                              newFert5name = c(TRUE, FALSE), newFert5N_cont = NA, newFert5P2O5 = NA, newFert5K2O = NA, newFert5CostperBag = NA, newFert5BagWt = NA)
{


  fertilizers <-  fertilizerFunc(ureaavailable=ureaavailable, ureaCostperBag=ureaCostperBag,ureaBagWt=ureaBagWt,
                                 MOPavailable=MOPavailable, MOPCostperBag=MOPCostperBag, MOPBagWt=MOPBagWt,
                                 DAPavailable=DAPavailable, DAPCostperBag=DAPCostperBag, DAPBagWt=DAPBagWt,
                                 NPK201010available=NPK201010available, NPK201010CostperBag=NPK201010CostperBag, NPK201010BagWt=NPK201010BagWt,
                                 NPK151515available=NPK151515available, NPK151515CostperBag=NPK151515CostperBag, NPK151515BagWt=NPK151515BagWt,
                                 TSPavailable=TSPavailable, TSPCostperBag=TSPCostperBag, TSPBagWt=TSPBagWt,
                                 NPK171717available=NPK171717available, NPK171717CostperBag=NPK171717CostperBag, NPK171717BagWt=NPK171717BagWt,
                                 NPK201216available= NPK201216available, NPK201216CostperBag =NPK201216CostperBag, NPK201216BagWt=NPK201216BagWt,
                                 CANavailable=CANavailable, CANCostperBag=CANCostperBag, CANBagWt=CANBagWt,
                                 SSPavailable=SSPavailable, SSPCostperBag=SSPCostperBag, SSPBagWt=SSPBagWt,
                                 YaraMila_UNIKavailable=YaraMila_UNIKavailable, YaraMila_UNIKCostperBag=YaraMila_UNIKCostperBag, YaraMila_UNIKBagWt=YaraMila_UNIKBagWt,
                                 newFert1name=newFert1name, newFert1N_cont=newFert1N_cont, newFert1P2O5=newFert1P2O5,
                                 newFert1K2O=newFert1K2O, newFert1CostperBag=newFert1CostperBag, newFert1BagWt=newFert1BagWt,
                                 newFert2name=newFert2name, newFert2N_cont=newFert2N_cont, newFert2P2O5=newFert2P2O5,
                                 newFert2K2O=newFert2K2O, newFert2CostperBag=newFert2CostperBag, newFert2BagWt=newFert2BagWt,
                                 newFert3name=newFert3name, newFert3N_cont=newFert3N_cont, newFert3P2O5=newFert3P2O5,
                                 newFert3K2O=newFert3K2O, newFert3CostperBag=newFert3CostperBag, newFert3BagWt=newFert3BagWt,
                                 newFert4name=newFert4name, newFert4N_cont=newFert4N_cont, newFert4P2O5=newFert4P2O5,
                                 newFert4K2O=newFert4K2O, newFert4CostperBag=newFert4CostperBag, newFert4BagWt=newFert4BagWt,
                                 newFert5name=newFert5name, newFert5N_cont=newFert5N_cont, newFert5P2O5=newFert5P2O5,
                                 newFert5K2O=newFert5K2O, newFert5CostperBag=newFert5CostperBag, newFert5BagWt=newFert5BagWt, country=country)



  if(nameSF == "NA") nameSF <- NA
  if(userName == "NA") userName <- "AKILIMO farmer"
  if(userField == "NA") userField <- paste(round(lat, digits = 3), round(lon, digits = 3), sep="_")

  if(method_ploughing == "NA") method_ploughing <- "N/A"
  if(method_ridging == "NA") method_ridging <- "N/A"


  if(sweetPotatoUW == 0) sweetPotatoUW <- 1000
  if(cost_manual_ploughing == 0) cost_manual_ploughing <- NA
  if(cost_manual_harrowing == 0) cost_manual_harrowing <- NA

  if(cost_manual_ridging == 0) cost_manual_ridging <- NA
  if(cost_tractor_ploughing == 0) cost_tractor_ploughing <- NA
  if(cost_tractor_harrowing == 0) cost_tractor_harrowing <- NA
  if(cost_tractor_ridging == 0) cost_tractor_ridging <- NA

  if(cost_weeding1 == 0) cost_weeding1 <- NA
  if(cost_weeding2 == 0) cost_weeding2 <- NA
  if(maizeUW == 0) maizeUW <- NA
  if(maxInv == 0) maxInv <- NA
  if(fallowHeight == 0) fallowHeight <- NA


  PD <- as.Date(PD, format = "%Y-%m-%d")
  HD <- as.Date(HD, format = "%Y-%m-%d")

  rootConv <- data.frame(cassPD = c("roots", "chips", "flour", "gari"), conversion = c(1, 3, 3.2, 3.5))


  if(saleSF){
    SF <- read.csv("starchPrices.csv")
    SF <- SF[SF$starchFactory == nameSF,]
    cassUP <- max(SF$price)
    cassUW <- 1000
  }else{
    if(cassUP == 0 & cassPD=="roots" & country=="NG"){cassUP = 12000; cassUW = 1000}
    if(cassUP == 0 & cassPD=="chips" & country=="NG"){cassUP = 36000; cassUW = 1000}
    if(cassUP == 0 & cassPD=="flour" & country=="NG"){cassUP = 38400; cassUW = 1000}
    if(cassUP == 0 & cassPD=="gari"  & country=="NG"){cassUP = 42000; cassUW = 1000}

    if(cassUP == 0 & cassPD=="roots" & country=="TZ"){cassUP = 180000; cassUW = 1000}
    if(cassUP == 0 & cassPD=="chips" & country=="TZ"){cassUP = 540000; cassUW = 1000}
    if(cassUP == 0 & cassPD=="flour" & country=="TZ"){cassUP = 576000; cassUW = 1000}
    if(cassUP == 0 & cassPD=="gari"  & country=="TZ"){cassUP = 630000; cassUW = 1000}

  }


  rootUP <- cassUP / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000

  rootUP_m1 <- cassUP_m1 / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000
  rootUP_m2 <- cassUP_m2 / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000
  rootUP_p1 <- cassUP_p1 / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000
  rootUP_p2 <- cassUP_p2 / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000

  # calculating cobUP based on maizeUP, maizeUW and conversion from grain to cobs if maizePD == "grain"
  if(maizeUP == 0 & maizePD == "fresh_cob") maizeUP <- 50 #default price for 1 large fresh cob

  if(maizeUP == 0  & maizePD == "grain") {
    maizeUP <- 230 #default price for 1 kg of maize grain
    maizeUW <- 1
  }

  cobUP <- ifelse(maizePD == "fresh_cob", maizeUP, maizeUP / maizeUW / 7.64) #1 kg of grain ~ 7.64 cobs

  # calculating tuberUP based on sweetPotatoUP, sweetPotatoUW and conversion factor for sweetPotato product sold
  tuberConv <- data.frame(sweetPotatoPD = c("tubers", "flour"), conversion = c(1, 3.2))
  if(sweetPotatoUP == 0 & sweetPotatoPD=="tubers" & country=="TZ"){sweetPotatoUP = 120000; sweetPotatoUW = 1000}
  if(sweetPotatoUP == 0 & sweetPotatoPD=="flour"  & country=="TZ"){sweetPotatoUP = 384000; sweetPotatoUW = 1000}

  tuberUP <- sweetPotatoUP / sweetPotatoUW / tuberConv[tuberConv$sweetPotatoPD==sweetPotatoPD,]$conversion * 1000

  # calculating the field area
  areaHa <- area / ifelse(areaUnits=="ha", 1, ifelse(areaUnits=="acre", 2.47105, 10000))


  # create dataframe with cost of land management operations
  costLMO <- data.frame(operation = c(rep(c("ploughing", "harrowing", "ridging"),2), "weeding1", "weeding2"),
                        method = c(rep("manual", 3), rep("tractor", 3), NA, NA),
                        cost = c(cost_manual_ploughing, cost_manual_harrowing, cost_manual_ridging, cost_tractor_ploughing, cost_tractor_harrowing, cost_tractor_ridging, cost_weeding1, cost_weeding2),
                        area = ifelse(cost_LMO_areaBasis=="areaField", areaHa, ifelse(areaUnits=="acre", 0.404686, ifelse(areaUnits=="ha", 1, 0.0001))))

  costLMO_MD <- costLMO
  costLMO$costHa <- costLMO$cost / costLMO$area
  costLMO <- subset(costLMO, select=-c(area, cost))


  # add default values for LMO operations if missing
  if(country == "NG"){
    if(is.na(cost_manual_ploughing))                   costLMO[costLMO$operation=="ploughing" & costLMO$method=="manual" ,]$costHa <- 17000 * 2.47105
    if(is.na(cost_manual_harrowing))                   costLMO[costLMO$operation=="harrowing" & costLMO$method=="manual" ,]$costHa <- 15000 * 2.47105
    if(is.na(cost_manual_ridging))                     costLMO[costLMO$operation=="ridging"   & costLMO$method=="manual" ,]$costHa <- 12000 * 2.47105
    if(is.na(cost_tractor_ploughing) & tractor_plough) costLMO[costLMO$operation=="ploughing" & costLMO$method=="tractor" ,]$costHa <- 6000 * 2.47105
    if(is.na(cost_tractor_harrowing) & tractor_harrow) costLMO[costLMO$operation=="harrowing" & costLMO$method=="tractor" ,]$costHa <- 6000 * 2.47105
    if(is.na(cost_tractor_ridging)   & tractor_ridger) costLMO[costLMO$operation=="ridging"   & costLMO$method=="tractor" ,]$costHa <- 6000 * 2.47105
    if(is.na(cost_weeding1))                           costLMO[costLMO$operation=="weeding1", ]$costHa                             <- 30000 * 2.47105
    if(is.na(cost_weeding2))                           costLMO[costLMO$operation=="weeding2", ]$costHa                             <- 30000 * 2.47105

  }else{
    if(is.na(cost_manual_ploughing))                   costLMO[costLMO$operation=="ploughing" & costLMO$method=="manual" ,]$costHa <- 175000 * 2.47105
    if(is.na(cost_manual_harrowing))                   costLMO[costLMO$operation=="harrowing" & costLMO$method=="manual" ,]$costHa <- 150000 * 2.47105
    if(is.na(cost_manual_ridging))                     costLMO[costLMO$operation=="ridging"   & costLMO$method=="manual" ,]$costHa <- 225000 * 2.47105
    if(is.na(cost_tractor_ploughing) & tractor_plough) costLMO[costLMO$operation=="ploughing" & costLMO$method=="tractor" ,]$costHa <- 150000 * 2.47105
    if(is.na(cost_tractor_harrowing) & tractor_harrow) costLMO[costLMO$operation=="harrowing" & costLMO$method=="tractor" ,]$costHa <- 100000 * 2.47105
    if(is.na(cost_tractor_ridging)   & tractor_ridger) costLMO[costLMO$operation=="ridging"   & costLMO$method=="tractor" ,]$costHa <- 115000 * 2.47105
    if(is.na(cost_weeding1))                           costLMO[costLMO$operation=="weeding1", ]$costHa                             <- 60000 * 2.47105
    if(is.na(cost_weeding2))                           costLMO[costLMO$operation=="weeding2", ]$costHa                             <- 45000 * 2.47105


  }


  if(!is.na(cost_manual_ploughing)|!is.na(cost_manual_harrowing)|!is.na(cost_manual_ridging)|!is.na(cost_tractor_ploughing)|
     !is.na(cost_tractor_harrowing)|!is.na(cost_tractor_ridging)|!is.na(cost_weeding1)|!is.na(cost_weeding2)){
    costLMO_MD$area <- paste(costLMO_MD$area, areaUnits, sep="")
    write.csv(costLMO_MD, "costLMO.csv", row.names = FALSE)
  }else{
    costLMO_MD <- costLMO
    names(costLMO_MD) <- c("operation", "method", "cost")
    costLMO_MD$area <- "1ha"
    costLMO_MD$cost <- formatC(signif(costLMO_MD$cost, digits=3), format="f", big.mark=",", digits=0)
    write.csv(costLMO_MD, "costLMO.csv", row.names = FALSE)

  }


  pd <- as.numeric(strftime(PD, format = "%j"))
  pw <- as.numeric(strftime(PD, format = "%W"))
  hd <- as.numeric(strftime(HD, format = "%j"))
  hw <- as.numeric(strftime(HD, format = "%W"))
  had <- as.numeric(as.Date(HD) - as.Date(PD))
  haw <- round(had / 7)


  # generate list with requested recommendations
  res <- list()
  recText <- list()

  res[["PP"]] <- NULL
  res[["SP"]] <- NULL

  FRrecom <- NULL
  ICrecom <- NULL
  PPrecom <- FALSE
  SPrecom <- NULL


  TRNS <- read.csv("translations_TEST.csv",  stringsAsFactors = FALSE)
  recloc_ng <- gsub(pattern = "\"",replacement = "",TRNS$recloc[1]) ; 	recloc_tz <- gsub(pattern = "\"",replacement = "",TRNS$recloc[2]) ; frnotrec_ng <- gsub(pattern = "\"",replacement = "",TRNS$frnotrec[1]) ;
  frnotrec_tz <- gsub(pattern = "\"",replacement = "",TRNS$frnotrec[2]) ; spinfo_ng <- gsub(pattern = "\"",replacement = "",TRNS$spinfo[1]) ; spinfo_tz <- gsub(pattern = "\"",replacement = "",TRNS$spinfo[2])


  if(FR == TRUE)   {res[["FR"]] <- getFRrecommendations(lat = lat,
                                                        lon = lon,
                                                        pd = pd,
                                                        pw = pw,
                                                        HD=HD,
                                                        had=had,
                                                        maxInv = maxInv,
                                                        fertilizers=fertilizers,
                                                        rootUP = rootUP,
                                                        areaHa=areaHa,
                                                        country=country,
                                                        FCY=FCY)

  if(all(res$FR == "We do not have fertilizer recommendation for your location because your location is out of the recommendation domain AKILIMO is currently serving.") & country == "NG"){
    FRrecom <- FALSE
    recText[["FR"]] <- res$FR
  }else if (all(res$FR == "Hatuna mapendekezo yoyote  kwa eneo lako kwa sababu eneo lako liko nje la eneo ambalo AKILIMO linafanya kazi kwa sasa") & country == "TZ"){
    FRrecom <- FALSE
    recText[["FR"]] <- res$FR
  }else{
    if(res[["FR"]]$rec$NR > 0){
      FRrecom <- TRUE
      recText[["FR"]] <- getFRrecText(ds = res$FR , country = country, fertilizers=fertilizers, rootUP=rootUP)
      write.csv(recText$FR, 'FR_recText.csv', row.names = FALSE)
      FR_MarkdownText(rr=res$FR, fertilizers=fertilizers, userName=userName, country=country,
                      userPhoneNr=userPhoneNr, userField=userField, area=area, areaUnits=areaUnits,
                      PD =PD, HD=HD, email=email, lat=lat, lon=lon, userPhoneCC=userPhoneCC,
                      rootUP = rootUP, cassPD = cassPD, cassUW = cassUW, maxInv = maxInv)
      fertilizerAdviseTable(FR=TRUE, IC=FALSE, country=country, areaUnits=areaUnits)
    }else{
      FRrecom <- FALSE
      if(country == "NG") {
        recText[["FR"]] <-frnotrec_ng
      }else{
        recText[["FR"]] <-frnotrec_tz
      }
    }
  }
  }

  ### if urea or NPK 151515 is not available, no recommendation is possible.TODO
  if(IC == TRUE & country == "NG")                        {res[["IC"]] <- getICrecommendations(areaHa = areaHa,
                                                                                               CMP = CMP,
                                                                                               cobUP = cobUP,
                                                                                               fertilizers = fertilizers,
                                                                                               riskAtt = riskAtt)
  if (nrow(res$IC[[2]]) > 0){
    ICrecom <- TRUE
    recText[["IC"]] <- getICrecText(ds = res$IC, maizePD)
    write.csv(recText$IC, 'IC_recText.csv', row.names = FALSE)
    IC_MarkdownText(rr = res$IC, fertilizers=fertilizers, userName=userName, country=country,
                    userPhoneNr=userPhoneNr, userField=userField, area=area, areaUnits=areaUnits,
                    PD =PD, HD=HD, email=email, lat=lat, lon=lon, userPhoneCC=userPhoneCC,maizeUW=maizeUW,
                    maizePD=maizePD,cassUW = cassUW, rootUP = rootUP, cassPD = cassPD, maxInv = maxInv, CMP=CMP, maizeUP=maizeUP)
    fertilizerAdviseTable(FR=FALSE, IC=TRUE, country = "NG", areaUnits=areaUnits)
  }else{
    ICrecom <- FALSE
    recText[["IC"]] <- res[["IC"]]$rec$reason_F

  }
  }


  ### if NPK 171717  is not available, no recommendation is possible.
  if(IC == TRUE & country == "TZ")                        {res[["IC"]] <- getCISrecommendations(areaHa = areaHa,
                                                                                                FCY = FCY,
                                                                                                tuberUP = tuberUP,
                                                                                                rootUP=rootUP,
                                                                                                fertilizers = fertilizers,
                                                                                                riskAtt = riskAtt)
  if (nrow(res$IC[[2]]) > 0){
    ICrecom <- TRUE
    recText[["IC"]] <- getCISrecText(ds = res$IC)
    write.csv(recText$IC, 'CIS_recText.csv', row.names = FALSE)
    CIS_MarkdownText(rr = res$IC, fertilizers=fertilizers, userName=userName, country=country,
                     userPhoneNr=userPhoneNr, userField=userField, area=area, areaUnits=areaUnits,
                     PD =PD, HD=HD, email=email, lat=lat, lon=lon, userPhoneCC=userPhoneCC,
                     sweetPotatoUP =sweetPotatoUP, sweetPotatoPD=sweetPotatoPD, sweetPotatoUW=sweetPotatoUW,
                     rootUP = rootUP, cassUW = cassUW, cassPD = cassPD, maxInv = maxInv, tuberUP=tuberUP)

    fertilizerAdviseTable(FR=FALSE, IC=TRUE, country = "TZ", areaUnits=areaUnits)
  }else{
    ICrecom <- FALSE
    recText[["IC"]] <- res[["IC"]]$rec$reason_F
  }
  }


  if(PP == TRUE)                        {res[["PP"]] <- getPPrecommendations(areaHa = areaHa,
                                                                             costLMO = costLMO,
                                                                             ploughing = ploughing,
                                                                             ridging = ridging,
                                                                             method_ploughing = method_ploughing,
                                                                             method_ridging = method_ridging,
                                                                             FCY = FCY,
                                                                             rootUP = rootUP)

  recText[["PP"]] <- getPPrecText(ds = res$PP , country = country)
  write.csv(res$PP, 'PP_rec.csv', row.names = FALSE)
  write.csv(recText$PP, 'PP_recText.csv', row.names = FALSE)
  PP_MarkdownText(userName, country, userPhoneNr, userField, area, areaUnits, PD, HD, email,lat, lon,rootUP,cassPD,cassUW,
                  maxInv, ploughing, ridging, method_ploughing, method_ridging, userPhoneCC)

  }


  if(SPP == TRUE | SPH == TRUE)         {if(PD_window == 0 & HD_window ==0){
    SPrecom <- FALSE
    recText[["SP"]] <- if(country == "NG"){
      "AKILIMO provides advice for schedule planting if only at least your planting or harvest time or both are flexible. Please provide this information and you will be advised when the best time is for your location."

    }else{
      "AKILIMO hutoa ushauri wa upandaji wa ratiba ikiwa angalau wakati wako wa upandaji au wakati wa kuvuna au zote mbili zinabadilika. Tafadhali toa habari hii na utashauriwa wakati mzuri wa kupanda na kuvuna kwa eneo lako"
    }
  }else{
    res[["SP"]] <- getSPrecommendations(areaHa = areaHa,
                                        country = country,
                                        lat = lat,
                                        lon = lon,
                                        PD = PD,
                                        HD = HD,
                                        PD_window = PD_window,
                                        HD_window = HD_window,
                                        saleSF = saleSF,
                                        nameSF = nameSF,
                                        FCY = FCY,
                                        rootUP = rootUP,
                                        rootUP_m1 = rootUP_m1,
                                        rootUP_m2 = rootUP_m2,
                                        rootUP_p1 = rootUP_p1,
                                        rootUP_p2 = rootUP_p2)

    if(!is.data.frame(res[["SP"]])){
      SPrecom <- FALSE
      recText[["SP"]] <- res$SP
    }else{
      SPrecom <- TRUE
      recText[["SP"]] <- getSPrecText(ds = res$SP , country = country)
      write.csv(recText$SP, 'SP_recText.csv', row.names = FALSE)
      SP_MarkdownText(userName, country, userPhoneNr, userField, area, areaUnits, PD, HD, email,lat, lon,saleSF, nameSF,
                      maxInv, ploughing, ridging, method_ploughing, method_ridging, userPhoneCC, CMP,riskAtt,
                      PD_window, HD_window,cassPD,cassUW,cassUP,cassUP_m1, cassUP_m2, cassUP_p1,cassUP_p2)
    }
  }
  }



  if(is.null(FRrecom)){
    FRrecom <- FALSE
  }
  if(is.null(ICrecom)){
    ICrecom <- FALSE
  }
  if(!is.null(res[["PP"]])){
    PPrecom <- TRUE
  }
  if(is.null(SPrecom)){
    SPrecom <- FALSE
  }

  if(SPP == TRUE | SPH == TRUE){
    SP <- TRUE
  }else{
    SP <- FALSE
  }


  if(email == TRUE)                     {sendEmailReport(userEmail=userEmail, FR=FR, IC=IC, PP=PP, SP=SP, country=country, FRrecom=FRrecom, ICrecom=ICrecom, PPrecom=PPrecom, SPrecom=SPrecom)}


  return(recText)

}


# setwd("/home/akilimo/projects/akilimo_recommendation/ScriptPackage/AKILIMOFun")
#
# Tanzania_WLY_LINTUL_2020_Server <- readRDS("Tanzania_WLY_LINTUL_2020_Server.RDS")
# Nigeria_WLY_LINTUL_2020_Server <- readRDS("Nigeria_WLY_LINTUL_2020_Server.RDS")
# ISRIC_SoilData_2020 <- readRDS("ISRIC_SoilData_2020.RDS")
# Nigeria_WLY_LINTUL_2020 <- readRDS("Nigeria_WLY_LINTUL_2020.RDS")
# Tanzania_WLY_LINTUL_2020 <- readRDS("Tanzania_WLY_LINTUL_2020.RDS")
# SoilData_fcy1 <- readRDS("SoilData_fcy1.RDS")
#
# fd2 <- read.csv("fd2.csv")
# NOT_GIS_CON_2020 <- read.csv("NOT_GIS_CON_2020.csv")
# translations_TEST <- read.csv("translations_TEST.csv")
# starchPrices <- read.csv("starchPrices.csv")

#
# if (!dir.exists('extdata')) {dir.create('extdata')}

