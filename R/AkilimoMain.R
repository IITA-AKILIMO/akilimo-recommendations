
process_json_value <- function(field_name, body, default_value = "NA") {
  if (!is.null(body[[field_name]])) {
    value <- body[[field_name]]
    if (!is.null(value)) {
      return(value)
    }
  }
  return(default_value)
}


get_cassUPUW <- function(cassUP, cassUW, cassPD, country, saleSF, nameSF) {

    if (saleSF) {
		SF <- read.csv("./data/input/starchPrices.csv")
		SF <- SF[SF$starchFactory == nameSF,]
		cassUP <- max(SF$price)
		cassUW <- 1000
    } else if (cassUP == 0) {
		if (country == "NG") {
			  if (cassPD == "roots") { cassUP <- 12000; cassUW <- 1000 }
			  if (cassPD == "chips") { cassUP <- 36000; cassUW <- 1000 }
			  if (cassPD == "flour") { cassUP <- 38400; cassUW <- 1000 }
			  if (cassPD == "gari") { cassUP <- 42000; cassUW <- 1000 }
		} else if (country == "TZ") {
			  if (cassPD == "roots") { cassUP <- 180000; cassUW <- 1000 }
			  if (cassPD == "chips") { cassUP <- 540000; cassUW <- 1000 }
			  if (cassPD == "flour") { cassUP <- 576000; cassUW <- 1000 }
			  if (cassPD == "gari") { cassUP <- 630000; cassUW <- 1000 }
		} else if (country == "GH") {
			  if (cassPD == "roots") { cassUP <- 450; cassUW <- 1000 }
			  if (cassPD == "chips") { cassUP <- 450; cassUW <- 1000 }
			  if (cassPD == "flour") { cassUP <- 450; cassUW <- 1000 }
			  if (cassPD == "gari") { cassUP <- 450; cassUW <- 1000 }
		} else if (country == "RW") {
			  if (cassPD == "roots") { cassUP <- 75000; cassUW <- 1000 }
			  if (cassPD == "chips") { cassUP <- 75000; cassUW <- 1000 }
			  if (cassPD == "flour") { cassUP <- 75000; cassUW <- 1000 }
			  if (cassPD == "gari") { cassUP <- 75000; cassUW <- 1000 }
		} else if (country == "BU") {
			  if (cassPD == "roots") { cassUP <- 700000; cassUW <- 1000 }
			  if (cassPD == "chips") { cassUP <- 700000; cassUW <- 1000 }
			  if (cassPD == "flour") { cassUP <- 700000; cassUW <- 1000 }
			  if (cassPD == "gari") { cassUP <- 700000; cassUW <- 1000 }
		} else {
			# error
		}
	}
	c(cassUP, cassUW )
}

get_user <- function(body) {
	list(
		send_SMS = process_json_value("SMS", body, default_value = FALSE),
		send_email = process_json_value("email", body, default_value = FALSE),
		PhoneCC = process_json_value("userPhoneCC", body),
		PhoneNr = process_json_value("userPhoneNr", body),
		Name = process_json_value("userName", body),
		Email = process_json_value("userEmail", body)
	)
}


run_akilimo <- function(json) {

	dir.create("temp", FALSE, FALSE)

    # Parse JSON body
    body <- tryCatch(jsonlite::fromJSON(json), error = function(e) NULL)

    # extract parameters from the JSON payload
    country <- process_json_value("country", body)
    lat <- process_json_value("lat", body)
    lon <- process_json_value("lon", body)
    area <- process_json_value("area", body)
    areaUnits <- process_json_value("areaUnits", body)

    IC <- process_json_value("IC", body, default_value = FALSE)
    # not used?
	#intercrop <- process_json_value("intercrop", body, default_value = FALSE)
    FR <- process_json_value("FR", body, default_value = FALSE)
    PP <- process_json_value("PP", body, default_value = FALSE)
    SPP <- process_json_value("SPP", body, default_value = FALSE)
    SPH <- process_json_value("SPH", body, default_value = FALSE)
    PD <- process_json_value("PD", body, default_value = 0)
    HD <- process_json_value("HD", body, default_value = 0)

    PD_window <- process_json_value("PD_window", body, default_value = 0)
    HD_window <- process_json_value("HD_window", body, default_value = 0)
	cost_LMO_areaBasis <- process_json_value("cost_LMO_areaBasis", body, default_value = "areaUnit")
    FCY <- process_json_value("FCY", body)
    CMP <- process_json_value("CMP", body)
    saleSF <- process_json_value("saleSF", body, default_value = FALSE)
    nameSF <- process_json_value("nameSF", body, default_value = NA)
    cassPD <- process_json_value("cassPD", body, default_value = "roots")
    cassUW <- process_json_value("cassUW", body, default_value = 1000)
    cassUP <- process_json_value("cassUP", body)
    cassUP_m1 <- process_json_value("cassUP_m1", body)
    cassUP_m2 <- process_json_value("cassUP_m2", body)
    cassUP_p1 <- process_json_value("cassUP_p1", body)
    cassUP_p2 <- process_json_value("cassUP_p2", body)
    maxInv <- process_json_value("maxInv", body, default_value = NA)

    user <- get_user(body)
    
	userField <- process_json_value("userField", body)
    
	riskAtt <- process_json_value("riskAtt", body, default_value = 0)

    if (country == "BI") {
		country <- "BU" #use non standard country code for Burundi
    }

	message(paste0("Country: ", country, ", Planting: ", PD, ", Harvesting: ", HD))
    #riskAtt <- 0

    #fertilizers <- get_fertilizers(body, country)
	# use new function
    fertilizers <- get_fertilizers2(body, country)


    if (maxInv == 0) maxInv <- NA

    PD <- as.Date(PD, format = "%Y-%m-%d")
    HD <- as.Date(HD, format = "%Y-%m-%d")

    ## if cassava is to be sold to a processing factory, there should be a default price by factry and product
    # calculating rootUP based on cassUP, cassUW and conversion factor for cassava product sold
    rootConv <- data.frame(cassPD = c("roots", "chips", "flour", "gari"), conversion = c(1, 3, 3.2, 3.5))


	UPUW <- get_cassUPUW(cassUP, cassUW, cassPD, country, saleSF, nameSF)
	cassUP <- UPUW[1]
	cassUW <- UPUW[2]		
	
    # Extract conversion factor once
    conversion_factor <- rootConv[rootConv$cassPD == cassPD, "conversion"]

    # Calculate rootUP values using the same denominator
    denominator <- cassUW * conversion_factor / 1000
    # Compute each rootUP variant
    rootUP <- cassUP / denominator
    rootUP_m1 <- cassUP_m1 / denominator
    rootUP_m2 <- cassUP_m2 / denominator
    rootUP_p1 <- cassUP_p1 / denominator
    rootUP_p2 <- cassUP_p2 / denominator

    # Define unit conversion factors to hectares
    unit_factors <- c(ha=1, acre=2.47105, are=100, m2=10000)

    # Fallback to 10000 (i.e., square meters) if unit is unknown or missing
    conversion_factor <- unit_factors[[areaUnits]]
    if (is.null(conversion_factor)) conversion_factor <- 10000

    # Calculate area in hectares
    areaHa <- area / conversion_factor

    # Determine area basis for cost calculation
    area_basis <- switch(cost_LMO_areaBasis, "areaField" = areaHa, 
			"acre" = 0.404686, "ha" = 1, 0.0001)  # fallback default (likely m²)
    
    ### dates and weeks
    #pd         : Character, Planting date, in format of the ith day of the year (as.numeric(strftime(PD, format = "%j")))
    #pw         : planting week of the year = as.numeric(format(PD, format = "%W"))
    #hd         : harvest day of the year = as.numeric(strftime(HD, format = "%j"))
    #hw         : harvest week of the year = as.numeric(format(HD, format = "%W"))
    #had        : age of the crop at harvest in days since planting = as.numeric(HD - PD), number of days the crop was on the field
    #haw        : age of the crop at harvest in weeks since planting = round(had / 7), number of weeks the crop was on the field

    # Ensure PD and HD are Date objects
    PD <- as.Date(PD)
    HD <- as.Date(HD)

    # Calculate planting and harvest dates/weeks
    pd <- as.numeric(strftime(PD, format = "%j"))  # Planting day of year
    pw <- as.numeric(strftime(PD, format = "%W"))  # Planting week of year
    hd <- as.numeric(strftime(HD, format = "%j"))  # Harvest day of year
    hw <- as.numeric(strftime(HD, format = "%W"))  # Harvest week of year

    # Calculate crop age at harvest
    had <- as.numeric(difftime(HD, PD, units = "days"))  # Age in days
    haw <- round(had / 7)                                # Age in weeks

    # generate list with requested recommendations
    recText <- list(FR = NULL, PP = NULL, IC = NULL, SP = NULL)
    plumberRes <- list(FR = NULL, PP = NULL, SP = NULL)

    FRrecom <- NULL
    ICrecom <- NULL
    PPrecom <- FALSE
    SPrecom <- NULL

    # Read the CSV file
    TRNS <- read.csv("./data/input/translations_TEST.csv", stringsAsFactors = FALSE)

    # Define a function to clean the data
    clean_data <- function(column, index) {
      gsub(pattern = "\"", replacement = "", column[index])
    }

    # Clean the required variables
    #recloc_ng <- clean_data(TRNS$recloc, 1)
    #recloc_tz <- clean_data(TRNS$recloc, 2)
    #recloc_rw <- clean_data(TRNS$recloc, 3)
    #spinfo_ng <- clean_data(TRNS$spinfo, 1)
    #spinfo_rw <- clean_data(TRNS$spinfo, 3)
    #spinfo_tz <- clean_data(TRNS$spinfo, 2)

    selected_key <- NULL

    if (FR) {
		frnotrec_ng <- clean_data(TRNS$frnotrec, 1)
		frnotrec_tz <- clean_data(TRNS$frnotrec, 2)
		frnotrec_rw <- clean_data(TRNS$frnotrec, 3)


		resFr <- process_FR(
			FR, lat, lon, pd, pw, HD, had, maxInv, fertilizers, rootUP, areaHa, country, FCY, riskAtt,
			user, userField, area, areaUnits, PD, 
			cassPD, cassUW, recText, plumberRes, frnotrec_ng, frnotrec_tz, frnotrec_rw
		)

		FRrecom <- resFr$FRrecom
		recText <- resFr$recText
		plumberRes <- resFr$plumberRes
		selected_key <- 'FR'
    }

    if (IC) {
	
		sweetPotatoPD <- process_json_value("sweetPotatoPD", body, default_value = "tubers")
		sweetPotatoUW <- process_json_value("sweetPotatoUW", body, default_value = NA)
		sweetPotatoUP <- process_json_value("sweetPotatoUP", body, default_value = NA)
		maizePD <- process_json_value("maizePD", body, default_value = "fresh_cob")
		maizeUW <- process_json_value("maizeUW", body, default_value = NA)
		maizeUP <- process_json_value("maizeUP", body)
		if (sweetPotatoUW == 0) sweetPotatoUW <- 1000 ## if it is not given default is a ton
		if (maizeUW == 0) maizeUW <- NA

		# Set default price and weight if maizeUP is zero
		if (maizeUP == 0) {
		  if (maizePD == "fresh_cob") {
			maizeUP <- 50    # Default price for 1 large fresh cob
			maizeUW <- 1
		  } else if (maizePD == "grain") {
			maizeUP <- 230   # Default price for 1 kg of maize grain
			maizeUW <- 1
		  }
		}

		# Ensure maizeUW is numeric if using grain
		if (maizePD == "grain") {
		  maizeUW <- as.numeric(as.character(maizeUW))
		}

		# Calculate cobUP
		cobUP <- ifelse (maizePD == "fresh_cob", maizeUP, maizeUP / maizeUW / 7.64)  # 1 kg of grain ~ 7.64 cobs
	   
		# Conversion factors for sweetPotato products
		tuberConv <- data.frame(
		  sweetPotatoPD = c("tubers", "flour"), # sweetpotato "tubers"? ouch.
		  conversion = c(1, 3.2)
		)

		# Set default price and weight for Tanzania if price is missing
		if (sweetPotatoUP == 0 && country == "TZ") {
		  if (sweetPotatoPD == "tubers") {
			sweetPotatoUP <- 120000
			sweetPotatoUW <- 1000
		  } else if (sweetPotatoPD == "flour") {
			sweetPotatoUP <- 384000
			sweetPotatoUW <- 1000
		  }
		}

		# Get the conversion factor
		conversion_factor <- tuberConv[tuberConv$sweetPotatoPD == sweetPotatoPD, "conversion"]

		# Compute tuberUP
		tuberUP <- sweetPotatoUP / sweetPotatoUW / conversion_factor * 1000

		
      if (country == "NG") {
        resIC <- process_IC_NG(
          IC = IC, country = country, areaHa = areaHa, CMP = CMP, cobUP = cobUP, fertilizers = fertilizers,
          riskAtt = riskAtt, maizePD = maizePD, user = user, userField = userField,
          area = area, areaUnits = areaUnits, PD = PD, HD = HD, lat = lat, lon = lon,
		  maizeUW = maizeUW, cassUW = cassUW, saleSF = saleSF, nameSF = nameSF,
          rootUP = rootUP, cassPD = cassPD, maxInv = maxInv, maizeUP = maizeUP, res = plumberRes, recText = recText
        )
      }

      if (country == "TZ") {
        resIC <- process_IC_TZ(
          IC = IC, country = country, areaHa = areaHa, FCY = FCY, tuberUP = tuberUP, rootUP = rootUP,
          fertilizers = fertilizers, riskAtt = riskAtt, user = user, 
		  userField = userField, area = area, areaUnits = areaUnits,
          PD = PD, HD = HD, lat = lat, lon = lon, sweetPotatoUP = sweetPotatoUP, sweetPotatoPD = sweetPotatoPD,
          sweetPotatoUW = sweetPotatoUW, cassUW = cassUW, cassPD = cassPD, maxInv = maxInv,
          res = plumberRes, recText_input = recText
        )
      }

      ICrecom <- resIC$ICrecom
      plumberRes <- resIC$res
      recText <- resIC$recText
      selected_key <- 'IC'
    }

    if (PP) {

		tractor_plough <- process_json_value("tractor_plough", body, default_value = FALSE)
		tractor_harrow <- process_json_value("tractor_harrow", body, default_value = FALSE)
		tractor_ridger <- process_json_value("tractor_ridger", body, default_value = FALSE)
		cost_tractor_ploughing <- process_json_value("cost_tractor_ploughing", body, default_value = NA)
		cost_tractor_harrowing <- process_json_value("cost_tractor_harrowing", body, default_value = NA)
		cost_tractor_ridging <- process_json_value("cost_tractor_ridging", body, default_value = NA)
		cost_manual_ploughing <- process_json_value("cost_manual_ploughing", body, default_value = NA)
		cost_manual_harrowing <- process_json_value("cost_manual_harrowing", body, default_value = NA)
		cost_manual_ridging <- process_json_value("cost_manual_ridging", body, default_value = NA)
		cost_weeding1 <- process_json_value("cost_weeding1", body, default_value = NA)
		cost_weeding2 <- process_json_value("cost_weeding2", body, default_value = NA)
		ploughing <- process_json_value("ploughing", body, default_value = FALSE)
		harrowing <- process_json_value("harrowing", body, default_value = FALSE)
		ridging <- process_json_value("ridging", body, default_value = FALSE)
		method_ploughing <- process_json_value("method_ploughing", body)
		method_harrowing <- process_json_value("method_harrowing", body)
		method_ridging <- process_json_value("method_ridging", body)
		if (method_ploughing == "NA") method_ploughing <- "N/A"
		if (method_ridging == "NA") method_ridging <- "N/A"
		if (cost_manual_ploughing == 0) cost_manual_ploughing <- NA
		if (cost_manual_harrowing == 0) cost_manual_harrowing <- NA
		if (cost_manual_ridging == 0) cost_manual_ridging <- NA
		if (cost_tractor_ploughing == 0) cost_tractor_ploughing <- NA
		if (cost_tractor_harrowing == 0) cost_tractor_harrowing <- NA
		if (cost_tractor_ridging == 0) cost_tractor_ridging <- NA

		if (cost_weeding1 == 0) cost_weeding1 <- NA
		if (cost_weeding2 == 0) cost_weeding2 <- NA
		if (fallowHeight == 0) fallowHeight <- NA

		# create dataframe with cost of land management operations
		costLMO <- data.frame(operation = c(rep(c("ploughing", "harrowing", "ridging"), 2), "weeding1", "weeding2"),
							  method = c(rep("manual", 3), rep("tractor", 3), NA, NA),
							  cost = c(cost_manual_ploughing, cost_manual_harrowing, cost_manual_ridging, cost_tractor_ploughing, cost_tractor_harrowing, cost_tractor_ridging, cost_weeding1, cost_weeding2), area = area_basis)

		costLMO_MD <- costLMO
		costLMO$costHa <- costLMO$cost / costLMO$area
		costLMO <- subset(costLMO, select = -c(area, cost))

		# add default values for LMO operations if missing
		if (country == "NG") {
		  if (is.na(cost_manual_ploughing)) {
			costLMO[costLMO$operation == "ploughing" & costLMO$method == "manual",]$costHa <- 17000 * 2.47105
		  }
		  if (is.na(cost_manual_harrowing)) {
			costLMO[costLMO$operation == "harrowing" & costLMO$method == "manual",]$costHa <- 15000 * 2.47105
		  }  
		  if (is.na(cost_manual_ridging)) {
			costLMO[costLMO$operation == "ridging" & costLMO$method == "manual",]$costHa <- 12000 * 2.47105
		  }
		  if (is.na(cost_tractor_ploughing) & tractor_plough) {
			costLMO[costLMO$operation == "ploughing" & costLMO$method == "tractor",]$costHa <- 6000 * 2.47105
		  }
		  if (is.na(cost_tractor_harrowing) & tractor_harrow) {
			costLMO[costLMO$operation == "harrowing" & costLMO$method == "tractor",]$costHa <- 6000 * 2.47105
		  } 
		  if (is.na(cost_tractor_ridging) & tractor_ridger) {
			costLMO[costLMO$operation == "ridging" & costLMO$method == "tractor",]$costHa <- 6000 * 2.47105
		  }
		  if (is.na(cost_weeding1)) {
			costLMO[costLMO$operation == "weeding1",]$costHa <- 30000 * 2.47105
		  }
		  if (is.na(cost_weeding2)) {
			costLMO[costLMO$operation == "weeding2",]$costHa <- 30000 * 2.47105
		  }
		}else if (country == "TZ") {
		  if (is.na(cost_manual_ploughing)) {
			costLMO[costLMO$operation == "ploughing" & costLMO$method == "manual",]$costHa <- 175000 * 2.47105
		  }
		  if (is.na(cost_manual_harrowing)) {
			costLMO[costLMO$operation == "harrowing" & costLMO$method == "manual",]$costHa <- 150000 * 2.47105
		  }
		  if (is.na(cost_manual_ridging)) {
			costLMO[costLMO$operation == "ridging" & costLMO$method == "manual",]$costHa <- 225000 * 2.47105
		  }
		  if (is.na(cost_tractor_ploughing) & tractor_plough) {
			costLMO[costLMO$operation == "ploughing" & costLMO$method == "tractor",]$costHa <- 150000 * 2.47105
		  }
		  if (is.na(cost_tractor_harrowing) & tractor_harrow) {
			costLMO[costLMO$operation == "harrowing" & costLMO$method == "tractor",]$costHa <- 100000 * 2.47105
		  }
		  if (is.na(cost_tractor_ridging) & tractor_ridger) {
			costLMO[costLMO$operation == "ridging" & costLMO$method == "tractor",]$costHa <- 115000 * 2.47105
		  }
		  if (is.na(cost_weeding1)) {
			costLMO[costLMO$operation == "weeding1",]$costHa <- 60000 * 2.47105
		  }
		  if (is.na(cost_weeding2)) {
			costLMO[costLMO$operation == "weeding2",]$costHa <- 45000 * 2.47105
		  }
		}

		if (any(!is.na(c(cost_manual_ploughing, cost_manual_harrowing, cost_manual_ridging,
				cost_tractor_ploughing, cost_tractor_harrowing, cost_tractor_ridging,
				cost_weeding1, cost_weeding2)))) {
		  costLMO_MD$area <- paste(costLMO_MD$area, areaUnits, sep = "")
		  write.csv(./temp/costLMO_MD, "costLMO.csv", row.names = FALSE)
		} else {
		  costLMO_MD <- costLMO
		  names(costLMO_MD) <- c("operation", "method", "cost")
		  costLMO_MD$area <- "1ha"
		  costLMO_MD$cost <- formatC(signif(costLMO_MD$cost, digits = 3), format = "f", big.mark = ",", digits = 0)
		  write.csv(./temp/costLMO_MD, "costLMO.csv", row.names = FALSE)

		}
	
      resPP <- process_PP(
        PP = PP, country = country, areaHa = areaHa, costLMO = costLMO,
        ploughing = ploughing, ridging = ridging,
        method_ploughing = method_ploughing, method_ridging = method_ridging,
        FCY = FCY, rootUP = rootUP, riskAtt = riskAtt,
        user = user, userField = userField, area = area, areaUnits = areaUnits,
        PD = PD, HD = HD, lat = lat, lon = lon,
        cassPD = cassPD, cassUW = cassUW, maxInv = maxInv,
        res = plumberRes, recText = recText
      )

      PPrecom <- resPP$PPrecom
      recText <- resPP$recText
      plumberRes <- resPP$plumberRes
      selected_key <- 'PP'
    }

    if (SPP || SPH) {
	
      resSP <- process_SP(
        SPP = SPP, SPH = SPH, PD_window = PD_window, HD_window = HD_window,
        areaHa = areaHa, country = country, lat = lat, lon = lon, PD = PD, HD = HD,
        saleSF = saleSF, nameSF = nameSF, FCY = FCY,
        rootUP = rootUP, rootUP_m1 = rootUP_m1, rootUP_m2 = rootUP_m2,
        rootUP_p1 = rootUP_p1, rootUP_p2 = rootUP_p2,
        user = user, userField = userField,
        area = area, areaUnits = areaUnits, maxInv = maxInv,
        ploughing = ploughing, ridging = ridging, method_ploughing = method_ploughing,
        method_ridging = method_ridging,  CMP = CMP, riskAtt = riskAtt,
        cassPD = cassPD, cassUW = cassUW, cassUP = cassUP,
        cassUP_m1 = cassUP_m1, cassUP_m2 = cassUP_m2, cassUP_p1 = cassUP_p1, cassUP_p2 = cassUP_p2,
        res = plumberRes, recText = recText
      )

      SPrecom <- resSP$SPRecom
      recText <- resSP$recText
      plumberRes <- resSP$plumberRes
      selected_key <- 'SP'
    }

# for getWMrecommendations 
#		fallowType <- process_json_value("fallowType", body, default_value = "none")
#		fallowHeight <- process_json_value("fallowHeight", body, default_value = NA)
#		fallowGreen <- process_json_value("fallowGreen", body, default_value = FALSE)
#		problemWeeds <- process_json_value("problemWeeds", body, default_value = FALSE)


    #=============================================================================
    result <- list(
      res = plumberRes,
      recText = recText
    )


    request_token <- process_json_value("request_token", body)

    if (is.null(selected_key)) {
      res$status <- 404
      data <- list(
        request_token = jsonlite::unbox(request_token),
        message = jsonlite::unbox("No valid recommendation found")
      )
      list(status = jsonlite::unbox("error"), data = data)
    }

    # Extract data
    recommendations <- result$res[[selected_key]]$rec
    if (is.null(recommendations) || length(recommendations) == 0) {
      recommendations <- result$res[[selected_key]]
    }
    fertilizer_rates <- result$res[[selected_key]]$fertilizer_rates
    text <- result$recText[[selected_key]]


    data <- list(
      request_token = jsonlite::unbox(request_token),
      recommendations = recommendations,
      fertilizer_rates = fertilizer_rates,
      recommendation = jsonlite::unbox(text),
      rec_type = jsonlite::unbox(selected_key)  # optional: tells you whether it's FR, SP, IC, PP, etc.
    )

    list(status = jsonlite::unbox("success"), data = data)
}  
