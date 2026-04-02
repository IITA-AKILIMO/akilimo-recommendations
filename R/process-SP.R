
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
getSPrecText <- function(ds, country, lang, PD, HD) {

  if (is.null(ds)) {
    rec <- tr("norecom", lang)
  } else {
    if (ds[1,]$CP) {
        rec <- paste0(tr("recrev", lang, pd_date = format(ds[1,]$PD, "%d %B %Y")),
                      tr("hvsdate", lang, hd_date = format(ds[1,]$HD, "%d %B %Y")),
                      tr("nochange", lang))

      # NOTE: "no change" text gives no reason. Possible causes: unfavourable prices at
      # other dates, low starch at later harvest (starch factory sales), or unattractive
      # yields at earlier dates. Could also add cropping practice impact and pest/disease
      # risk guidance.

    } else {

      if (ds[1,]$PD != ds[ds$CP == TRUE,]$PD) {
        recP <- paste0(tr("recPln", lang,
                          date      = format(ds[1,]$PD, "%d %B %Y"),
                          weeks     = abs(ds[1,]$rPWnr),
                          direction = ifelse(ds[1,]$rPWnr < 0, tr("early", lang), tr("late", lang))), "\n")
      } else {
        recP <- tr("recPopt", lang, date = format(ds[1,]$PD, "%d %B %Y"))
      }


      if (ds[1,]$HD != ds[ds$CP == TRUE,]$HD) {
        recH <- paste0(tr("recHvs", lang,
                          date      = format(ds[1,]$HD, "%d %B %Y"),
                          weeks     = abs(ds[1,]$rHWnr),
                          direction = ifelse(ds[1,]$rHWnr < 0, tr("early", lang), tr("late", lang))), "\n")
      } else {
        recH <- tr("recHopt", lang, date = format(ds[1,]$HD, "%d %B %Y"))
      }

      DP <- signif(ds[1,]$RP - ds[ds$CP == TRUE,]$RP, digits = 2)
      currency <- get_currency(country)
      dGR <- formatC(signif(ds[1,]$dGR, digits = 3), format = "f", big.mark = ",", digits = 0)	  

      if (DP == 0) {
        if (dGR == 0) {
            recR <- tr("rechange", lang,
                       action = ifelse(!is.null(recH), tr("hvst", lang), tr("plnt", lang)))
        } else {
            recR <- tr("recRatt1", lang, currency = currency, amount = dGR)
        }
      } else {
        direction_str <- ifelse(DP < 0, tr("dec", lang), tr("inc", lang))
        action_str    <- ifelse(!is.null(recH), tr("hvst", lang), tr("plnt", lang))
        if (dGR == 0) {
            recR <- tr("recRyieldOnly", lang,
                       direction = direction_str, amount = abs(DP), action = action_str)
        } else {
            recR <- tr("recRfull", lang,
                       direction = direction_str,
                       amount    = abs(DP),
                       conj      = ifelse(DP < 0, tr("but", lang), tr("and", lang)),
                       currency  = currency,
                       value     = dGR)
        }
      }

      rec <- paste0(recP, recH, recR)


      # NOTE: recommendation text is minimal. Future enhancements could include:
      # 1. Risks of harvesting later (especially in CBSD-affected areas)
      # 2. Importance of using the right varieties
      # 3. Reasons underlying recommendations (driven by yield, price, or both)
      # 4. Implications on agronomic practices, requirements for ridging, fertilizer application
      # 5. Possible issues with the input data — especially unrealistic prices
    }
  }
  rec
}



## schedule planting and harvest dates is based on searching highest return on investemnt within 1- or 2-months window around the user intended plant/harvest dates;
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
getSPrecommendations <- function(areaHa, country, lat, lon,
			PD, HD, PD_window, HD_window, saleSF, nameSF, FCY, 
			rootUP, rootUP_m1, rootUP_m2, rootUP_p1, rootUP_p2) {

  #rounding lat and lon to centroid of 5x5km pixel
  #latr <- as.factor(floor(lat * 10) / 10 + ifelse(lat - (floor(lat * 10) / 10) < 0.05, 0.025, 0.075))
  #lonr <- as.factor(floor(lon * 10) / 10 + ifelse(lat - (floor(lon * 10) / 10) < 0.05, 0.025, 0.075))
  #                                                --- note the error 
  #latr <- as.numeric(levels(latr))
  #lonr <- as.numeric(levels(lonr))
 
  SoilData <- get_data("soil_NPK-4", lon=lon, lat=lat)
  
	# it takes more than 10 seconds to read this, need to use a better apporach
#	WLY <- get_data("WLY_15M", country)
#	latlon <- paste(latr, lonr, sep = "_")
	##WLY_15M[WLY_15M$long == lonr & WLY_15M$lat == latr, ]
#	WLY <- WLY[WLY$location == latlon,] 

	WLY <- get_data("WLY_15M_ncdf", country, lon=lon, lat=lat)

	if ((nrow(SoilData) == 0) || isTRUE(is.na(SoilData$soilN)) || (NROW(WLY) == 0)) {
		return(NULL)
	}
   
	WLY <- merge(WLY, data.frame(daysOnField = seq(235, 455, 7), haw = 34:65), by = "daysOnField")

	## these could all be precomputed
	for (k in 1:nrow(WLY)) {
#		Qinw <- data.frame(SoilData, WLY=WLY$water_limited_yield[k])
		Qinw <- data.frame(SoilData, WLY=WLY$WLY[k]) #, water_limited_yield=WLY$WLY[k])
		WLY$Current_Yield[k] <- QUEFTS(Qinw, c(0,0,0), HI=.55) # in kg/ha dry
	}

#    yld <- unique(data.frame(plw = WLY$PlweekNr, haw = WLY$haw, CY = WLY$Current_Yield, WY = WLY$water_limited_yield)) ## is still dry weight

    yld <- unique(data.frame(plw = WLY$PlweekNr, haw = WLY$haw, CY = WLY$Current_Yield, WY = WLY$WLY)) ## is still dry weight

    yld$CY <- round(yld$CY / 1000)
    yld$WY <- round(yld$WY / 1000)

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
      # NOTE: hardcoded to "NG" — TZ dry-to-fresh conversion in getRFY() produces
      # unreliable values and has not yet been validated for Tanzania.
      ds$RFCY[k] <- getRFY(HD = ds$HD[k], RDY = ds$CY[k], country = "NG")
      ds$RFWY[k] <- getRFY(HD = ds$HD[k], RDY = ds$WY[k], country = "NG")
    }

    #scaling predicted yield based on farmer-reported current yield relative to modelled CY and WY:
    ds$RY <- (ds$RFWY - ds$RFCY) / (13.5 - 1.5) / 2.5 * (FCY - 1.5 * 2.5) + ds$RFCY
    ds$RP <- ds$RY * areaHa

    #If selling to a starch factory: rootUP is determined by starch content of roots
    if (saleSF) {

      ds$SC <- 0.75 * ds$WY / ds$RFWY * 100
	  SF <- get_data("starch_prices")
      SF <- SF[SF$starchFactory == nameSF,]
      price <- NULL
      for (i in 1:nrow(ds)) {
        price <- c(price, max(SF[SF$minStarch < ds[i,"SC"], "price"]))
      }
      ds$rootUP <- price
      ds <- subset(ds, select = -SC)
      #If not selling to a starch factory, user needs to specify rootUP across the harvest window
    } else {
      dp <- data.frame(rHWnr = c(-8, -4, 0, 4, 8),
                       rootUP = c(rootUP_m2, rootUP_m1, rootUP, rootUP_p1, rootUP_p2))
      dpm <- suppressWarnings(loess(rootUP ~ rHWnr, data = dp))
      dpp <- data.frame(rHWnr = seq((-4 * HD_window), (4 * HD_window), by = 2),
                        rootUP = predict(dpm, data.frame(rHWnr = seq((-4 * HD_window), (4 * HD_window), by = 2))))
      ds <- merge(ds, dpp)
    }

    ds$GR <- ds$RP * ds$rootUP
    ds$CP <- (ds$rPWnr == 0) & (ds$rHWnr == 0)
	if (!any(ds$CP)) {
	    # the combination was not available in the yield data...
		message("this situation needs to be avoided")
		return(NULL)
	} else {
		ds$dGR <- ds$GR - ds[ds$CP, "GR"]
		#sort by decreasing GR, increasing HWnr, decreasing PWnr
		#recommendation is highest GR, earliest harvesting, latest planting combination
		ds <- ds[order(-ds$dGR, ds$rHWnr, -ds$rPWnr),]
	}
    write.csv(ds, tp("SP_rec.csv"), row.names = FALSE)
	ds
}


process_SP <- function(
  SPP, SPH, country, lang, PD_window, HD_window, areaHa, lat, lon, PD, HD, saleSF, nameSF, FCY,
  rootUP, rootUP_m1, rootUP_m2, rootUP_p1, rootUP_p2, user, userField,
  area, areaUnits, maxInv, ploughing, ridging, method_ploughing, method_ridging, CMP, riskAtt,
  cassPD, cassUW, cassUP, cassUP_m1, cassUP_m2, cassUP_p1, cassUP_p2) {


	success <- FALSE
	res <- NULL
	if ((PD_window == 0) && (HD_window == 0)) {
		recText <- tr("spinfo", lang)
	} else if ((HD - PD) <= 30) {
		recText <- "Planting date should be at least 1 month after planting date."
	} else {

		res <- getSPrecommendations(
			areaHa = areaHa, country = country, lat = lat, lon = lon, PD = PD, HD = HD, 
			PD_window = PD_window, HD_window = HD_window, saleSF = saleSF, nameSF = nameSF,
			FCY = FCY, rootUP = rootUP, rootUP_m1 = rootUP_m1, rootUP_m2 = rootUP_m2, rootUP_p1 = rootUP_p1,
			rootUP_p2 = rootUP_p2)

		if (!is.data.frame(res)) {
			recText <- tr("recloc", lang)
		} else {
			success <- TRUE

			recText <- getSPrecText(ds = res, country = country, lang = lang, PD = PD, HD = HD)
		}
	}

#	list(recom=success, data=list(recommendations=res, fertilizer_rates=NULL, 
#							message=recText, rec_type="SP"))

	list(rec_type="SP", recommendation=recText, data=res)

#  return(list(SPrecom = SPrecom, plumberRes = res, recText = recText))
#  return(list(SPrecom = SPrecom, plumberRes = list(rec=res, dummy="dummy to prevent bug in test 10"), recText = recText)) 
}
