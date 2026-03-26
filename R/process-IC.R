# process_IC.R


#SHORT DEF:   Function to obtain recommendations on cassava-maize intercropping.
#RETURNS:     list of 2 dataframes: (i) cost benefit analysis for most profitable system, and (ii) fertilizer rates to apply.
#DESCRIPTION: Function to obtain recommendations on cassava-maize intercropping.
#             Returns (i) a 1-row dataframe cost-benefit parameters (extra yield, cost and net revenue, and whether to apply
#             fertilizer and to plant maize at high density, and why (not)) , and (ii) a data.frame with types of fertilizer and rates to apply (zeros included).
#INPUT:       See Cassava Crop Manager function for details
getICrecommendations <- function(areaHa, CMP, cobUP, fertilizers, riskAtt = c(0, 1, 2)) {

  #calculating expected yield increase from fertilizer
  maizeY <- data.frame(CMP = 1:5, dY = c(0, 6500, 4000, 2500, 0))

  dMY <- maizeY[maizeY$CMP == CMP, "dY"]
  dMP <- dMY * areaHa #extra maize production for the area of the field

  #extra gross revenue from fertilizer
  dGR <- dMP * cobUP

  if ((dGR == 0 || nrow(fertilizers) == 0)) {
    reason_F <- ifelse(CMP == 1, "of low soil fertility", "of high soil fertility")
    dTC <- 0
    FRATE <- 0
  } else {
    #calculating fertilizer requirement
    E <- t(data.matrix(fertilizers[, c("N_cont", "P_cont", "K_cont")]))
    F <- c(91, 21, 37.5) #ideally 2 bags of urea + 6 bags of NPK15:15:15
    G <- diag(nrow(fertilizers))
    H <- rep(0, nrow(fertilizers))
    Cost <- fertilizers$price

    #calculating fertilizer recommendation and total cost of fertilizer
    FRATE <- limSolve::linp(E, F, G, H, Cost)$X
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
  } else {
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
  reason_D <- ifelse(rec_F == TRUE, "fertilizer use is recommended", 
		ifelse(CMP == 5, "of high soil fertility", NA))


  #output
  if (!is.na(reason_D)) {
    rec <- data.frame(dMP = dMP, #extra maize production expected (in nr of cobs)
                      dNR = dNR, #net revenue increase from fertilizer use (in local currency)
                      dTC = dTC, #extra cost for fertilizer use (in local currency)
                      rec_F = rec_F, #TRUE or FALSE indicating if fertilizer application is recommended
                      rec_D = rec_D, #TRUE or FALSE indicating if high density maize planting is recommended
                      reason_F = reason_F, #reason why fertilizer application is not recommended
                      reason_D = reason_D)  #reason why high maize density is recommended


  } else {
    rec <- data.frame(dMP = dMP, #extra maize production expected (in nr of cobs)
                      dNR = dNR, #net revenue increase from fertilizer use (in local currency)
                      dTC = dTC, #extra cost for fertilizer use (in local currency)
                      rec_F = rec_F, #TRUE or FALSE indicating if fertilizer application is recommended
                      rec_D = rec_D, #TRUE or FALSE indicating if high density maize planting is recommended
                      reason_F = reason_F) #reason why fertilizer application is not recommended
  }
	
	if (nrow(fertilizers) > 0) {
		fertilizer_rates <- data.frame(type = fertilizers$type, rate = FRATE) #fertilizer rates to apply
		fertilizer_rates <- fertilizer_rates[fertilizer_rates$rate > 0,]
	} else {
		fertilizer_rates <- data.frame(type = "", rate = 0)[0,]
	}
	
  if (CMP == 1) {
    rec$reason_F <- "Your soil is very poor. You need to improve soil fertility before considering investing in fertilizer. You should apply compost or manure, or fallow for at least 2 years. Plant maize at low density (20,000 plants per hectare) and saw the seeds at 50 cm within rows."
  } else if (CMP == 5) {
    rec$reason_F <- "Your soil is very fertile. It is likely that your maize yield will not improve much after fertilizer application. Plant maize at high density (40,000 plants per hectare) and sow the seeds at 25 cm within rows."
  }

  list(data=rec, fertilizer_rates=fertilizer_rates)
}


#' @param ds is output of getICrecommendations
#' @param maizePD mmaize product
getICrecText <- function(x, maizePD) {

	ds <- x[["data"]]
	fs <- x[["fertilizer_rates"]]
  if (!ds$rec_F) {
    #trans
    recF <- paste0("Fertilizer use is not recommended because ", ds$reason_F)

  } else {
    dTC <- formatC(signif(ds$dTC, digits = 3), format = "f", big.mark = ",", digits = 0)
    dNR <- formatC(signif(ds$dNR, digits = 3), format = "f", big.mark = ",", digits = 0)
    dMP <- signif(ds$dMP, digits = 2)
    currency <- "NGN"
    fs$rate <- round(fs$rate)

    if (maizePD == "grain") {
      #1 kg of grain ~ 7.64 cobs
      dMP <- round(dMP / 7.64, digits = 0)

      #trans
      recF <- paste0("We recommend applying\n",
                     paste0(fs$rate, " kg of ", fs$type, collapse = "\n"), " ",
                     "\nfor the area of your field.\n",
                     "This will cost ", currency, " ", dTC, ". ",
                     "We expect an extra production of ", dMP, " kg of maize for the area of your field, ",
                     "and a net value increase of ", currency, " ", dNR, ".\n")

    } else {
      #trans
      recF <- paste0("We recommend applying\n",
                     paste0(fs$rate, " kg of ", fs$type, collapse = "\n"), " ",
                     "\nfor the area of your field.\n",
                     "This will cost ", currency, " ", dTC, ". ",
                     "We expect an extra production of ", dMP, " cobs for the area of your field, ",
                     "and a net value increase of ", currency, " ", dNR, ".\n")
    }
  }

  if (!is.null(ds$reason_D)) {

    #trans
    recD <- ifelse(ds$rec_D, "Plant your maize intercrop at high density: 1 m between rows and 25 cm within row (40,000 plants per hectare).",
                   paste0("Plant your maize intercrop at low density: 1 m between rows and 50 cm within row (20,000 plants per hectare) because ", ds$reason_D, "."))
  } else {

    #trans
    recD <- ifelse(ds$rec_D, "Plant your maize intercrop at high density: 1 m between rows and 25 cm within row (40,000 plants per hectare).",
                   paste0("Plant your maize intercrop at low density: 1 m between rows and 50 cm within row (20,000 plants per hectare)."))
  }

  paste0(recF, recD)

  # NOTE: recommendation text is minimal. Known gaps:
  # 1. No variety guidance (maize should mature in ≤95 days; cassava variety not mentioned).
  # 2. Fertilizer effect on cassava yield is not reflected in cost-benefit figures.
  # 3. No explanation when fertilizer or high density is not recommended.
  # 4. No sanity check on unrealistic input prices.
  # 5. KNOWN BUG: maize output is always reported in number of cobs, even when the
  #    user selected grain as the product type. Fix requires updating getICrecommendations()
  #    to return grain-equivalent output when maizePD == "grain".


}


# Process recommendations for Nigeria (NG)
process_IC_NG <- function(
	IC, country, areaHa, CMP, cobUP, fertilizers, riskAtt, maizePD, user, userField, area, areaUnits,
	PD, HD, lat, lon, maizeUW, cassUW, saleSF, nameSF, rootUP, cassPD, maxInv, maizeUP) {
	
	
	CMP <- min(5, max(1, as.numeric(CMP)))
	
  # Generate IC recommendations
  res <- getICrecommendations(areaHa = areaHa, CMP = CMP, cobUP = cobUP, 
							fertilizers = fertilizers, riskAtt = riskAtt)

  if (NROW(res$fertilizer_rates) > 0) {
    recText <- getICrecText(res, maizePD)

    write.csv(res, './temp/IC_rec.csv', row.names = FALSE)
    write.csv(recText, './temp/IC_recText.csv', row.names = FALSE)

    IC_MarkdownText(rr = res, fertilizers = fertilizers, user = user, country = country, 
			userField = userField, area = area, areaUnits = areaUnits, PD = PD, HD = HD, 
			lat = lat, lon = lon, maizeUW = maizeUW, maizePD = maizePD, cassUW = cassUW,
			saleSF = saleSF, nameSF = nameSF, rootUP = rootUP, cassPD = cassPD, maxInv = maxInv,
			CMP = CMP, maizeUP = maizeUP, riskAtt = riskAtt)

	fertilizerAdviseTable(FR = FALSE, IC = TRUE, country = country, areaUnits = areaUnits)
    ICrecom <- TRUE
  } else {
    recText <- res$data$reason_F
    ICrecom <- FALSE
  }

	c(list(rec_type="IC", recommendation=recText) , res)

}

# Process recommendations for Tanzania (TZ)
process_IC_TZ <- function(IC, country, areaHa, FCY, tuberUP, rootUP, fertilizers, riskAtt, 
			user, userField, area, areaUnits, PD, HD, lat, lon, sweetPotatoUP, sweetPotatoPD, 
			sweetPotatoUW, cassUW, cassPD, maxInv) {

  # Generate CIS recommendations
  res <- getCISrecommendations(areaHa = areaHa, FCY = FCY, tuberUP = tuberUP, rootUP = rootUP,
					fertilizers = fertilizers, riskAtt = riskAtt)

  if (NROW(res$fertilizer_rates) > 0) {
    recText <- getCISrecText(res)

    write.csv(recText, './temp/CIS_recText.csv', row.names = FALSE)

    CIS_MarkdownText(rr = res, fertilizers = fertilizers,
      user = user, country = country, userField = userField, area = area,
      areaUnits = areaUnits, PD = PD, HD = HD, lat = lat,lon = lon,
      sweetPotatoUP = sweetPotatoUP, sweetPotatoPD = sweetPotatoPD,
      sweetPotatoUW = sweetPotatoUW, rootUP = rootUP, cassUW = cassUW,
      cassPD = cassPD, maxInv = maxInv, tuberUP = tuberUP)

	  fertilizerAdviseTable(FR = FALSE, IC = TRUE, country = "TZ", areaUnits = areaUnits)

	  ICrecom <- TRUE
  } else {
    recText <- res$data$reason_F
    ICrecom <- FALSE
  }

  #return(list(ICrecom = ICrecom, plumberRes = res, recText = recText))
  #list(recom = ICrecom, data=c(res, message=recText, rec_type="IC"))

	c(list(rec_type="IC", recommendation=recText) , res)
  
}



#SHORT DEF:   Function to obtain recommendations on cassava-sweet potato intercropping.
#RETURNS:     list of 2 dataframes: (i) cost benefit analysis for most profitable system, and (ii) fertilizer rates to apply.
#DESCRIPTION: Function to obtain recommendations on cassava-sweet potato intercropping.
#             Returns (i) a 1-row dataframe cost-benefit parameters (extra yield, cost and net revenue, and whether to apply
#             fertilizer and whether to intercrop, and why (not)) , and (ii) a data.frame with types of fertilizer and rates to apply (zeros included).
#INPUT:       See Cassava Crop Manager function for details
getCISrecommendations <- function(areaHa = 1, FCY = 11,
                                  tuberUP, rootUP, fertilizers, riskAtt = c(0, 1, 2)) {

  #calculating expected yield increase from fertilizer
  FSY <- 0.7 * FCY #expected yield of a sweet potato monocrop
  FSY <- FSY * areaHa #extra sweet potato production for the area of the field TODO check with Pieter
  GR_MC <- FCY * rootUP #gross revenue of cassava monocrop
  # GR_IC <- GR_MC * 0.6 + 0.8 * FSY * tuberUP #gross revenue of cassava-sweet potato intercrop
  GR_IC <- GR_MC * 0.8 + 0.6 * FSY * tuberUP
  rec_IC <- GR_MC < GR_IC

  if (rec_IC) {
    #calculating fertilizer requirement
    E <- t(data.matrix(fertilizers[, c("N_cont", "P_cont", "K_cont")]))
    #F <- c(68, 19.6, 56.8) #ideally 2 bags of urea + 6 bags of NPK15:15:15
    F <- c(68, 33.2, 60) #ideally 2 bags of urea + 8 bags of NPK17:17:17
    G <- diag(nrow(fertilizers))
    H <- rep(0, nrow(fertilizers))
    Cost <- fertilizers$price

    #calculating fertilizer recommendation and total cost of fertilizer
    FR <- limSolve::linp(E, F, G, H, Cost)$X
    FR[FR < 25] <- 0 #dropping all rates less than 25 kg/ha
    FR <- FR * areaHa #adjusting to field area

    #calculating total cost
    dTC <- c(FR %*% fertilizers$price)
    # dGR <- ifelse(FCY > 20, 0, FCY * 0.6 * 0.4 * rootUP + FSY * 0.8 * 0.2 * tuberUP) #gross revenue increase: 40% yield increase in cassava + 20% yield increase in sweet potato, but not in fields with yields above 20 t/ha

    #gross revenue increase: 40% yield increase in cassava + 20% yield increase in sweet potato, but not in fields with yields above 20 t/ha in yield classes 1-3, 20% in cassava and 10% in sweet potato in yield class 4, and 0 in yield class 5
    # dGR <- ifelse(FCY > 30, 0, ifelse(FCY > 20, FCY * 0.6 * 0.2 * rootUP + FSY * 0.8 * 0.1 * tuberUP, FCY * 0.6 * 0.4 * rootUP + FSY * 0.8 * 0.2 * tuberUP))
    dGR <- ifelse(FCY > 30, 0, ifelse(FCY > 20, FCY * 0.8 * 0.2 * rootUP + FSY * 0.6 * 0.1 * tuberUP, 
				FCY * 0.8 * 0.4 * rootUP + FSY * 0.6 * 0.2 * tuberUP))  #NEW CALCULATION

    #evaluating if a solution was found
    if (dTC == 0) {
      dGR <- 0
      #trans
      reason_F <- "Mbolea sahihi haipatikani."
      rec_F <- FALSE
    } else { #trans
      reason_F <- "Tunakushauri usitumie mbolea kwa sababu itakuongezea gharama hatimae utapata hasara."
      rec_F <- TRUE
    }
  } else {
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
      reason_F <- "Matumizi ya mbolea inapendekezwa."
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

  if (nrow(fertilizers) > 0) {
	fertilizer_rates <- data.frame(type = fertilizers$type, rate = FR) #fertilizer rates to apply
	fertilizer_rates <- fertilizer_rates[fertilizer_rates$rate > 0,]
  } else {
	fertilizer_rates <- data.frame(type = "", rate = 0)[0,]
  }
  list(data=rec, fertilizer_rates=fertilizer_rates)

}



#' @param ds is output of getCISrecommendations
#' @param country
#'
#' @return the advice as text to print in app
getCISrecText <- function(d) {

	ds <- d[["data"]]

  if (!ds$rec_IC) {
    # recIC <- "Intercropping is not recommended. Growing a cassava monocrop will give you a higher profit.\n"
    # recF  <- "If you consider investing in fertilizer, please use our Fertilizer Recommendations Tool to obtain fertilizer advice for a cassava monocrop."

    recIC <- "Kilimo mchanganyiko haipendekezwi. Kupanda muhogo peke yake utakupatia faida ya juu.\n"
    recF <- "Kama ungependa kuwekeza katika mbolea, tafadhali tumia chombo chetu cha mapandekezo ya mbolea ili kupata ushauri wa kupanda muhogo bila mseto."
  } else {

    recIC <- "Tunapendekeza kuchanganya muhogo na viazi vitamu (kilimo mseto). Hii itakupatia faida ya juu na mapato ya haraka kutoka kwenye viazi vitamu."
    #recIC <- "This will generate a higher profit overall, and also give you access to early income from sweet potato.\n"
    if (!ds$rec_F) {
      #recF <- paste0("Fertilizer use is not recommended because ", ds$reason_F, ".\n")
      recF <- paste0("Haishauriwi kutumia  mbolea kwa sababu ", ds$reason_F, ".\n")
    } else {
      dTC <- formatC(signif(ds$dTC, digits = 3), format = "f", big.mark = ",", digits = 0)
      dNR <- formatC(signif(ds$dNR, digits = 3), format = "f", big.mark = ",", digits = 0)
      currency <- "TZS"

      #recF <- paste0("We recommend applying\n",
      # paste0(round(ds[["fertilizer_rates"]]$rate), " kg of ", ds[["fertilizer_rates"]]$type, collapse="\n"),
      # "\nfor the area of your field.\n",
      # "This will cost ", currency, " ", dTC, ". ",
      # "We expect a net value increase of ", currency, " ", dNR, " for the area of your field.")

      recF <- paste0("Tunapendekeza utumie\n",
                     paste0("kilo ", round(d[["fertilizer_rates"]]$rate), " ya ", ds[["fertilizer_rates"]]$type, collapse = "\n"), " ",
                     "\nkatika eneo la shamba lako.\n",
                     "Hii itagharimu shilingi ", currency, " ", dTC, ". ",
                     "Tunatarajia jumla ya ongezeko la thamani kwa ", currency, " ", dNR, " katika eneo la shamba lako.")
    }

  }

  paste0(recIC, recF)

  # NOTE: recommendation text is minimal. Known gaps:
  # 1. No variety guidance (sweet potato: Mayai; cassava: Kizimbani).
  # 2. Yield increase figures are intentionally omitted — data too weak to justify.
  # 3. No explanation when fertilizer or intercropping is not recommended.
  # 4. No sanity check on unrealistic input prices.

}

