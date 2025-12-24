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
			ifelse(country == "TZ", 2, 
			ifelse(country == "RW", 3, NA)))

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
      fertilizerRates <- round(frate$rate)
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
      revenue <- totalSalePrice - sum_total
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
				add_more(paste0(tr$werec[1], "\n", paste0(fertilizerRates, tr$kgof[1], 
					fertilizerTypes, collapse = "\n")), ci)
			} else {
				add_more(paste0(tr$werec[2], "\n", paste0(tr$kgof[2], 
					fertilizerRates, tr$of[2], fertilizerTypes, collapse = "\n")), ci)
			}
		
      #TODO: This only provides the minimal information to return to the user. We may consider adding following information:
      #1. Split regime - how should this fertilizer application be distributed over time?
      #2. Best application method - furrow or full ring application.
      #3. Possible better alternative fertilizers...
      #4. Importance of good agronomic practices
      #5. Possible issues with the input data - very high fertilizer prices or very low root price, very low or very high FCY, very low or very high WY,...
  }

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
rerun_25kgha <- function(rootUP, rdd, fertilizer, QID, onlyFert25, country, WLY = WLY, DCY = DCY, HD = HD, areaHa=areaHa) {
	
    fertilizer <- fertilizer[fertilizer$type %in% names(onlyFert25),]
	fert25 <- onlyFert25[match(names(onlyFert25), fertilizer$type)]
	fert <- unlist(fert25)
	
	rec <- c(sum(fert * fertilizer$N_cont), 
			sum(fert * fertilizer$P_cont),
			sum(fert * fertilizer$K_cont))

	QID$WLY <- WLY
	TY <- QUEFTS(QID, rec) #dry wt yield in kg/ha

	rdd$CurrentY <- getRFY(HD = HD, RDY = DCY, country = country) * areaHa / 1000
	rdd$TargetY <- getRFY(HD = HD, RDY = TY, country = country) * areaHa / 1000
	rdd$TC <- round(sum(fertilizer$price * fert) * areaHa, -2)

	nr <- round((rdd$TargetY - rdd$CurrentY) * rootUP, -2)
	rdd$NR <- nr - rdd$TC

#	rdd <- subset(rdd, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
	if (rdd$NR <= 0 | rdd$TargetY <= rdd$CurrentY) {
		rdd$N <- rdd$P <- rdd$K <- rdd$TC <- rdd$NR <- 0
		rdd$TargetY <- rdd$CurrentY
	} else {
		## NPK rate for user land size
		NPK_user <- rec * areaHa
		rdd$N <- NPK_user[1]
		rdd$P <- NPK_user[2]
		rdd$K <- NPK_user[3]
	}

	return(rdd)
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
    fertRecom$N <- fertRecom$P <- fertRecom$K <- 0
    fertRecom$TC <- 0
    fertRecom$NR <- 0
    fertRecom$TargetY <- fertRecom$CurrentY

    # dropped selction harvestData as it is not available in the dataFrame
    onlyFert <- subset(ds, select = -c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
    onlyFert[] <- 0

    ds <- cbind(fertRecom, onlyFert)
	row.names(ds) <- NULL
  }

  ds
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


getFRrecommendations <- function(lat, lon, HD, PD, maxInv, fertilizers, rootUP, areaHa, country, FCY, riskAtt) {

	go_there <- function(x) {
		# as.numeric to get rid of the names
		data.frame(type=names(x), rate=as.numeric(unlist(x)))
	}

	# Calculate planting and harvest dates/weeks
	pd <- as.numeric(strftime(PD, format = "%j"))  # Planting day of year
	pw <- as.numeric(strftime(PD, format = "%W"))  # Planting week of year
	#hd <- as.numeric(strftime(HD, format = "%j"))  # Harvest day of year
	#hw <- as.numeric(strftime(HD, format = "%W"))  # Harvest week of year
	had <- as.numeric(difftime(HD, PD, units = "days"))  # Age in days
	#haw <- round(had / 7)                                # Age in weeks

  ## get WLY:get PDand HD to the closest daes fr which we have WLY
	WLY_365 <- get_data("WLY_365", country=country, lon=lon, lat=lat)
	
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
		SoilData <- get_data("RF_soil", FCY=FCY, lat=lat, lon=lon)
    } else {
		SoilData <- get_data("soil_NPK", country, FCY, lon=lon, lat=lat)
    }

    ## get CY
    #WLYdata$Current_Yield <- QUEFTS_no_fertilizer(soil=SoilData, country=country, wlyd=WLYdata$water_limited_yield)
	Qinw <- data.frame(SoilData, WLY=WLYdata$water_limited_yield)
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
		fertilizer_rates <- NULL  # c(0,0,0) ?
		return(list(recommendations = fert_optim, fertilizer_rates = fertilizer_rates))
    } else {
      fertinfo <- subset(fert_optim, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
      onlyFert <- subset(fert_optim, select = -c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))

      ## 4. remove ferilizer application < 25 kg/ha and re run the TY and NR calculation
      recom_ha <- onlyFert / areaHa
	  above25 <- recom_ha > 25

      if (!any(above25)) { 
## if all fertilizer recom < 25 kg/ha all will be set to 0
        fertinfo$N <- fertinfo$P <- fertinfo$K <- fertinfo$NR <- fertinfo$TC <- 0
        fertinfo$TargetY <- fertinfo$CurrentY
        return(list(recommendations=fertinfo, fertilizer_rates=NULL))
      } else if (all(above25)) { 
## all fertilizer recom are >= 25 kg/ha. Check for NR >= 18% of investment
		fertRecom <- NRabove18Cost(ds = fert_optim, riskAtt = riskAtt)
        rec <- subset(fertRecom, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
        frates <- subset(fertRecom, select = -c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
        return(list(recommendations=rec, fertilizer_rates=go_there(frates)))
      } else {
## Fertilizers < 25 kg/ha are dropped. ty and NR are recalculated
## RH: conceptually it would be better to optimize again?

		fert25 <- recom_ha[, above25]
		onlyFert25 <- onlyFert[, above25]
        rdd <- cbind(fertinfo, onlyFert25)
        fert25rec <- rerun_25kgha(rootUP = rootUP, rdd=rdd, 
				fertilizer = fertilizers, QID = SoilData, onlyFert25 = fert25, 
				country = country, WLY = WLY, DCY = DCY, HD = HD, areaHa = areaHa)

        if (fert25rec$NR <= 0) { 
			return(list(recommendations = fert25rec, fertilizer_rates = NULL))
        } else {
#          print("The else happens here")
          fertRecom <- NRabove18Cost(ds = fert25rec, riskAtt = riskAtt)
          rec <- subset(fertRecom, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
          return(list(recommendations = rec, fertilizer_rates = go_there(onlyFert25), note="below 25kg only"))
        }
      }
    }
  }
}


process <- function(...) {
	return(list(...))
}


process_FR <- function(lat, lon, HD, maxInv, fertilizers, rootUP, areaHa, country, FCY, 
				riskAtt, user, userField, area, areaUnits, PD, cassPD, cassUW) {

	tr <- get_data("TRNS")

	response <- getFRrecommendations(
		lat = lat, lon = lon, HD = HD, PD=PD, maxInv = maxInv,
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
			"TZ" = tr$frnotrec[2],
			"RW" = tr$frnotrec[3],
			"No recommendation available"
		)
	}

	c(rec_type="FR", message=recText, response)
}
