#' Title makes the text shown for FR recom
#'
#' @param ds output of getFRrecommendations
#' @param country
#'
#' @return  the advice as text in the right language to show in the app
#' @export
#'
#' @examples
getFRrecText <- function(ds, country, lang) {

	if (is.null(ds$data)) {	# out of geographic scope
		recText <- tr("fr_out_of_domain", lang)
	} else if (ds$data$NR <= 0) { # not profitable
		recText <- tr("fr_not_profitable", lang)
	} else if (ds$data$TC == 0) { # profitable, but do not apply
        recText <- tr("fr_no_fert", lang)
      # NOTE: recommendation text does not explain the reason for non-application.
      # Possible causes: unfavourable price ratio, low yield potential, or high
      # indigenous nutrient supply. Adding cause-specific text requires extending
      # this branch with additional data from the QUEFTS output.
    } else {
		frate <- ds$fertilizer_rates

		currency <- get_currency(country)
		TC <- formatC(ds$data$TC, format = "f", big.mark = ",", digits = 0)

		NR <- formatC(ds$data$NR, format = "f", big.mark = ",", digits = 0)
		DY <- formatC(signif(ds$data$TargetY - ds$data$CurrentY, digits = 2),
		              format = "f", big.mark = ",", digits = 0)
		frate$rate_fmt <- formatC(round(frate$rate), format = "f", big.mark = ",", digits = 0)

		add_more <- function(x) {
            paste0(x, tr("suffix_field_area", lang), "\n",
               tr("cost_amount", lang, currency = currency, amount = TC), "\n",
               tr("fr_impact", lang, dy = DY, currency = currency, nr = NR))
			}

		rate_lines <- paste0(
			tr("fert_rate_line", lang, rate = frate$rate_fmt, fertilizer = frate$type),
			collapse = "\n"
		)
		recText <- add_more(paste0(tr("fr_apply_prefix", lang), "\n", rate_lines))
		
      # NOTE: recommendation text is minimal. Future enhancements could include:
      # split application regime, best application method (furrow vs full ring),
      # alternative fertilizers, agronomic practice guidance, and input sanity
      # warnings (extreme prices, FCY, or WY values).
	}

	recText
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
	rdd$NR <- round((rdd$TargetY - rdd$CurrentY) * rootUP, -2) - rdd$TC

	if ((rdd$NR <= 0) | (rdd$TargetY <= rdd$CurrentY)) {
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
  dNRmin <- min_nr_multiplier(riskAtt)

  if (ds$NR < ds$TC * dNRmin) {
    # Define once to avoid duplicating the column list across the two subset() calls.
    base_cols <- c("lat", "lon", "plDate", "N", "P", "K", "WLY", "CurrentY", "TargetY", "TC", "NR")

    fertRecom <- ds[, base_cols]
    fertRecom$N <- fertRecom$P <- fertRecom$K <- 0
    fertRecom$TC <- 0
    fertRecom$NR <- 0
    fertRecom$TargetY <- fertRecom$CurrentY

    # dropped selction harvestData as it is not available in the dataFrame
    onlyFert <- ds[, setdiff(names(ds), base_cols), drop = FALSE]
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

	pd  <- as.numeric(strftime(PD, format = "%j"))  # Planting day of year
	pw  <- as.numeric(strftime(PD, format = "%W"))  # Planting week of year
	had <- as.numeric(difftime(HD, PD, units = "days"))  # Age in days

	WLY_365 <- get_data("WLY_365", country=country, lon=lon, lat=lat)

	wlyPD <- seq(1, 365, 7)
	PD2   <- wlyPD[which.min(abs(pd - wlyPD))]
	wlyHD <- seq(214, 455, 7)
	HD2   <- wlyHD[which.min(abs(had - wlyHD))]

	wlypd <- WLY_365[WLY_365$pl_Date == PD2,]
	if (nrow(wlypd) == 0) {
		return(list(data = NULL, fertilizer_rates = NULL))
	}

	# Guard: HD2 must exist as a column — harvest age outside the model's
	# supported range (235–452 days) maps to a value not present in the data.
	if (!as.character(HD2) %in% names(wlypd)) {
		return(list(data = NULL, fertilizer_rates = NULL))
	}

	WLYdata <- wlypd[, c("lat", "lon", HD2, "pl_Date")]
	colnames(WLYdata)[3] <- "WLY"
	WLYdata$daysOnField <- had
    WLYdata$weekNr <- pw

	## get soil NPK
	SoilData <- get_data("soil_NPK", country, FCY, lon=lon, lat=lat)

## get CY
	Qinw <- data.frame(SoilData, WLY=WLYdata$WLY)
	WLYdata$Current_Yield <- QUEFTS(Qinw, c(0,0,0), HI=.55)

    InvestHa <- (maxInv / areaHa)

## 3. optimize the fertilizer recommendation for maxInv in local currency and provide expected target yield in kg
    fert_optim <- run_Optim_NG2(rootUP = rootUP, QID = SoilData, fertilizer = fertilizers,
			invest = InvestHa, WLYData = WLYdata,
			lat = lat, lon = lon, areaHa = areaHa, HD = HD)


    if (fert_optim$NR == 0) { ## no fertilizer recommendation
		return(list(data = fert_optim, fertilizer_rates = NULL))
    } 
	
	vn <- c("lat", "lon", "plDate", "N", "P", "K", "WLY", "CurrentY", "TargetY", "TC", "NR")
	fertinfo <- fert_optim[vn]
	onlyFert <- fert_optim[!(names(fert_optim) %in% vn)]

## 4. remove ferilizer application < 25 kg/ha and re run the TY and NR calculation
	recom_ha <- onlyFert / areaHa
	above25 <- recom_ha > 25

	if (!any(above25)) { 
## if all fertilizer recom < 25 kg/ha all will be set to 0
		fertinfo$N <- fertinfo$P <- fertinfo$K <- fertinfo$NR <- fertinfo$TC <- 0
		fertinfo$TargetY <- fertinfo$CurrentY
		return(list(data=fertinfo, fertilizer_rates=NULL))
	} else if (all(above25)) { 
## all fertilizer recom are >= 25 kg/ha. Check for NR >= 18% of investment
		fertRecom <- NRabove18Cost(ds = fert_optim, riskAtt = riskAtt)
		rec <- fertRecom[vn]
		frates <- fertRecom[!(names(fertRecom) %in% vn)]
		return(list(data=rec, fertilizer_rates=go_there(frates)))
	} else {
## Fertilizers < 25 kg/ha are dropped. ty and NR are recalculated
## RH: conceptually it would be better to optimize again?
		fert25 <- recom_ha[, above25]
		onlyFert25 <- onlyFert[, above25]
        rdd <- cbind(fertinfo, onlyFert25)
        fert25rec <- rerun_25kgha(rootUP = rootUP, rdd=rdd, fertilizer = fertilizers, 
				QID = SoilData, onlyFert25 = fert25, country = country, 
				WLY=WLYdata$WLY, DCY=WLYdata$Current_Yield, HD = HD, areaHa = areaHa)

        if (fert25rec$NR <= 0) { 
			return(list(data = fert25rec, fertilizer_rates = NULL))
        } else {
			fertRecom <- NRabove18Cost(ds = fert25rec, riskAtt = riskAtt)
			rec <- fertRecom[vn]
			return(list(data = rec, fertilizer_rates = go_there(onlyFert25), note="below 25kg only"))
		}
	}
}



process_FR <- function(lat, lon, HD, maxInv, fertilizers, rootUP, areaHa, country, lang, FCY,
				riskAtt, user, userField, area, areaUnits, PD, cassPD, cassUW) {

	response <- getFRrecommendations(
		lat = lat, lon = lon, HD = HD, PD=PD, maxInv = maxInv,
		fertilizers = fertilizers, rootUP = rootUP, areaHa = areaHa, country = country,
		FCY = FCY, riskAtt = riskAtt
	)

	recText <- getFRrecText(ds=response, country=country, lang=lang)

	c(rec_type="FR", recommendation=recText, response, list(fertilizers=fertilizers))
}
