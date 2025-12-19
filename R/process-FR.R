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
  
	tr <- get_data("TRNS")

	rec <- ds$rec
	frate <- ds$fertilizer_rates

	ci <- ifelse(country %in% c("NG", "GH", "BI"), 1, 
			ifelse(country == "TZ", 2, 3)) #RW = 3

	if (is.null(rec)) {
		tr$norecom[ci]
	} else if (rec$TC == 0) {
        tr$notapply[ci]

      #TODO: This does not provide details on the reasons why we do not recommend to apply fertilizer.
      #This might either be due to
      #1. unfavourable price ratios (root price over fertilizer price is too low),
      #2 low yield potential (unfavourable planting / harvest date and low WLY),
      #3. high soil fertility and low response (high FCY or high indigenous nutrient supply).

    } else {

	
      currency <- get_currency(country)

      fertilizerTypes <- frate$type
      fertilizerRates <- round(frate$rate, digits = 0)

      bags <- round(fertilizerRates / 50, digits = 1)  # 50 hard coded
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
      revenue <- round(revenue, -2)

      fertilizers <- droplevels(fertilizers[fertilizers$type %in% frate$type,])
      TC <- formatC(round(sum_total, digits = 0), format = "f", big.mark = ",", digits = 0)


      NR <- formatC(revenue, format = "f", big.mark = ",", digits = 0)
      DY <- signif(rec$TargetY - rec$CurrentY, digits = 2)

		add_more <- function(x, i) {
             paste0(x, tr$area[i], "\n",
               tr$willc[i], currency, " ", TC, ".\n",
               tr$extrap[i], DY, tr$tonof[1],
               tr$netincr[i], currency, " ", NR, ".")
		}

      recom <- if (ci == 1) {
				add_more(paste0(tr$werec[1], "\n", fertilizerRates, tr$kgof[1], 
					fertilizerTypes, collapse = "\n"), ci)
			} else {
				add_more(paste0(tr$werec[2], " ", "\n", tr$kgof[2], 
					fertilizerRates, tr$of[2], fertilizerTypes, collapse = "\n"), ci)
			}


      #TODO: This only provides the minimal information to return to the user. We may consider adding following information:
      #1. Split regime - how should this fertilizer application be distributed over time?
      #2. Best application method - furrow or full ring application.
      #3. Possible better alternative fertilizers...
      #4. Importance of good agronomic practices
      #5. Possible issues with the input data - very high fertilizer prices or very low root price, very low or very high FCY, very low or very high WY,...
  }

	gsub("[ ]+", " ", recom)
}



### see if profit is > (0.18 * total cost) + total cost
## if not set the recommnedation to zero
NRabove18Cost <- function(ds, riskAtt) {

  # Minimal required net revenue increase from fertilizer needed (taking into account risk attitude of user)
  dNRmin <- switch(as.character(riskAtt), "0" = 1.8, "1" = 1, "0.2")

  # Check if the net revenue is below the threshold
  #print("handling this one again")
  #print(paste("ds$TC:", class(ds$TC), ", ", ds$TC))
  #print(paste("dNRmin:", class(dNRmin), ", ", dNRmin))
  #print("after debuging")
  # Remove any non-numeric characters before conversion
  dNRmin <- gsub("[^0-9.-]", "", dNRmin)
  dNRmin <- as.numeric(dNRmin)
  if (ds$NR < ds$TC * dNRmin) {
    fertRecom <- subset(ds, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
    fertRecom$N <- 0
    fertRecom$P <- 0
    fertRecom$K <- 0
    fertRecom$TC <- 0
    fertRecom$NR <- 0
    fertRecom$TargetY <- fertRecom$CurrentY

    # dropped selction harvestData as it is not available in the dataFrame
    onlyFert <- subset(ds, select = -c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
    onlyFert[] <- 0

    fertRecom <- cbind(fertRecom, onlyFert)
    ds <- fertRecom
  }

  row.names(ds) <- NULL
  return(ds)
}



#######################################################################
## FR
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

round5min <- function(x) {
	round(floor(x * 10) / 10 + ifelse(x - (floor(x * 10) / 10) < 0.05, 0.025, 0.075), 3)
}

getFRrecommendations <- function(lat, lon, pd, pw, HD, had, maxInv, fertilizers, rootUP, areaHa, country, FCY, riskAtt) {

	lat2 <- round5min(lat)
	lon2 <- round5min(lon)
#	latlon <- paste(lat2, lon2, sep = "_")

  ## get WLY:get PDand HD to the closest daes fr which we have WLY
	WLY_365 <- get_data("WLY_365", country=country, lon=lon2, lat=lat2)
	
	#wlyPD <- unique(WLY_365$pl_Date)
	wlyPD <- seq(1, 365, 7)
	PD2 <- wlyPD[which.min(abs(pd - wlyPD))]
	wlyHD <- seq(214, 455, 7) # need to check the logic here 
	HD2 <- wlyHD[which.min(abs(had - wlyHD))]

##WLY_15M[WLY_15M$long == lonr & WLY_15M$lat == latr, ]
	wlypd <- WLY_365[WLY_365$pl_Date == PD2,] 

  #  wlypd <- WLY_365[WLY_365$lon==lon2 & WLY_365$lat == lat2 & WLY_365$pl_Date == PD2, ]
	if (nrow(wlypd) == 0) {
		if (country %in% c("NG", "GH", "BI", "RW")) {
			rec <- "We do not have fertilizer recommendation for your location because your location is out of the recommendation domain AKILIMO is currently serving."
    #} else if (country == "RW") {
	#	return("kinyarwanda here")
		} else {
			rec <- "Hatuna mapendekezo yoyote  kwa eneo lako kwa sababu eneo lako liko nje la eneo ambalo AKILIMO linafanya kazi kwa sasa"
		}
		return(list(message = rec, fertilizer_rates = NA, failed=TRUE))  
	} else {

		WLYdata <- wlypd[, c("lat", "lon", "pl_Date", HD2)]
		colnames(WLYdata) <- c("lat", "lon", "pl_Date", "water_limited_yield")
		WLYdata$zone <- country
		WLYdata$daysOnField <- had
		WLYdata <- WLYdata[, c("lat", "lon", "water_limited_yield", "pl_Date", "zone", "daysOnField")]

    ## get soil NPK
    if (country %in% c("NG", "TZ")) {
		# SoilData <- Rfmodel_Wrapper(FCY = FCY, country = country, lat = lat2, lon = lon2)
		SoilData <- Rfmodel_values(FCY=FCY, lat=lat2, lon=lon2)
    } else {
		SoilData <- get_data("soil_NPK", country, FCY, lon=lon2, lat=lat2)
    }

    ## get CY
    #WLYdata$Current_Yield <- QUEFTS_no_fertilizer(soil=SoilData, country=country, wlyd=WLYdata$water_limited_yield)
	Qinw <- data.frame(SoilData, WLY=WLYdata$water_limited_yield, water_limited_yield=WLYdata$water_limited_yield)
	WLYdata$Current_Yield <- QUEFTS(Qinw, c(0,0,0), HI=.55)

#	SoilData$WLY <- SoilData$water_limited_yield <- WLYdata$water_limited_yield
#	WLYdata$Current_Yield <- QUEFTS(SoilData, c(0,0,0), HI=0.52)
    WLYdata$weekNr <- pw

    #############################
    ## 1. get WLY, CY, fert recom and soil data
    WLY <- WLYdata$water_limited_yield ## DM in kg/ha
    DCY <- WLYdata$Current_Yield ## DM in kg/ha


    ## 2. change investment from given areaHa to 1ha
    InvestHa <- (maxInv / areaHa)


    ## 3. optimize the fertilizer recommendation for maxInv in local currency and provide expected target yield in kg
    fert_optim <- run_Optim_NG2(rootUP = rootUP, QID = SoilData, fertilizer = fertilizers, 
			invest = InvestHa, plDate = WLYdata$pl_Date, WLYData = WLYdata, 
			lat = lat, lon = lon, areaHa=areaHa, HD = HD, DCY = DCY, WLY = WLY, country = country)

    if (fert_optim$NR == 0) { ## no fertilizer recommendation
      fertilizer_rates <- NULL
      return(list(recommendations = fert_optim, fertilizer_rates = fertilizer_rates))
    } else {
      fertinfo <- subset(fert_optim, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
      onlyFert <- subset(fert_optim, select = -c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))

      ## 4. remove ferilizer application < 25 kg/ha and re run the TY and NR calculation
      RecomperHa <- onlyFert / areaHa
      RecomperHa2 <- tidyr::gather(RecomperHa, type, rate)
      onlyFert2 <- droplevels(RecomperHa2[RecomperHa2$rate > 25,])

      if (nrow(onlyFert2) == 0) { ## if all fertilizer recom < 25 kg/ha all will be set to 0
        fertinfo$N <- fertinfo$P <- fertinfo$K <- fertinfo$NR <- fertinfo$TC <- 0
        fertinfo$TargetY <- fertinfo$CurrentY
        fertilizer_rates <- NULL
        return(list(recommendations = fertinfo, fertilizer_rates = fertilizer_rates))
      } else if (ncol(onlyFert) == nrow(onlyFert2)) { ## if all fertilizer recom are >= 25 kg/ha they will be kept and only checked for NR >= 18% of invest
        Reset_fert_Cont <- fert_optim
        GPS_fertRecom <- NRabove18Cost(ds = Reset_fert_Cont, riskAtt = riskAtt)
        rec <- subset(GPS_fertRecom, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
        frates <- subset(GPS_fertRecom, select = -c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
        frates2 <- tidyr::gather(frates, type, rate)
        return(list(recommendations = rec, fertilizer_rates = frates2))

      } else {
        fert25 <- tidyr::spread(onlyFert2, type, rate) ## when some fertilizer recom are dropped b/c < 25 kg/ha, ty and NR should be recalculated
        fert_optim2 <- cbind(fertinfo, fert25)
        fertilizer <- fertilizers[fertilizers$type %in% onlyFert2$type,]
        Reset_fert_Cont <- Rerun_25kgKa_try(rootUP = rootUP, rdd = fert_optim2, fertilizer = fertilizer, QID = SoilData, onlyFert = onlyFert2,
                                            country = country, WLY = WLY, DCY = DCY, HD = HD, areaHa = areaHa)
        if (Reset_fert_Cont$NR <= 0) { ## after rerunning after avoiding <25KG/ha fertilizers, if NR <=0
          fertilizer_rates <- NULL
          return(list(recommendations = Reset_fert_Cont, fertilizer_rates = fertilizer_rates))
        } else {
          print("The elesae happens here you know")
          GPS_fertRecom <- NRabove18Cost(ds = Reset_fert_Cont, riskAtt = riskAtt)
          rec <- subset(GPS_fertRecom, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
          frates <- subset(GPS_fertRecom, select = -c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
          frates2 <- tidyr::gather(frates, type, rate)
          return(list(recommendations = rec, fertilizer_rates = frates2))

        }
      }
    }
  }
}


process <- function(...) {
	return(list(...))
}


process_FR <- function(lat, lon, pd, pw, HD, had, maxInv, fertilizers, rootUP, areaHa, country, FCY, 
				riskAtt, user, userField, area, areaUnits, PD, cassPD, cassUW) {

	tr <- get_data("TRNS")

	response <- getFRrecommendations(
		lat = lat, lon = lon, pd = pd, pw = pw, HD = HD, had = had, maxInv = maxInv,
		fertilizers = fertilizers, rootUP = rootUP, areaHa = areaHa, country = country,
		FCY = FCY, riskAtt = riskAtt
	)

	FRrecom <- FALSE
	if (isTRUE(response$failed)) {
	#no_fr_recommendation_countries <- c("NG", "GH", "TZ", "RW")
    #if (country %in% no_fr_recommendation_countries) {
		recText <- response$rec
    #} # else ? {}
	} else if (response$rec$NR > 0) {	
		FRrecom <- TRUE
		recText <- getFRrecText(ds = response, country=country, fertilizers=fertilizers, rootUP=rootUP)
		write.csv(recText, './temp/FR_recText.csv', row.names = FALSE)

		FR_MarkdownText(
			rr = response, fertilizers = fertilizers, user = user,
			country = country, userField = userField, area = area, areaUnits = areaUnits, PD = PD, HD = HD, 
			lat = lat, lon = lon, rootUP = rootUP, cassPD = cassPD, cassUW = cassUW, maxInv = maxInv
		)

		fertilizerAdviseTable(FR = TRUE, IC = FALSE, country = country, areaUnits = areaUnits)
	} else {
		recText <- switch(
			country,
			"NG" = tr$frnotrec[1],
			"GH" = tr$frnotrec[1],
			"RW" = tr$frnotrec[2],
			"TZ" = tr$frnotrec[3],
			"No recommendation available"
		)
	}

	list(recom = FRrecom, data=c(response, message = recText, rec_type="FR"))
}
