
run_akilimo <- function(json) {
	
	aki_version <- "20251222"
	dir.create("temp", FALSE, FALSE)
    body <- try(jsonlite::fromJSON(json))
	if (inherits(body, "try-error")) {
		return(list(status = jsonlite::unbox("404 - bad request"), data=NULL))
    }

    # extract parameters from the JSON payload
    country <- from_json("country", body)
    lat <- from_json("lat", body)
    lon <- from_json("lon", body)
    area <- from_json("area", body)
    areaUnits <- from_json("areaUnits", body)

    # not used?
	#intercrop <- from_json("intercrop", body, default_value = FALSE)
    IC <- from_json("IC", body, default_value = FALSE)
    FR <- from_json("FR", body, default_value = FALSE)
    PP <- from_json("PP", body, default_value = FALSE)
    SPP <- from_json("SPP", body, default_value = FALSE)
    SPH <- from_json("SPH", body, default_value = FALSE)

    PD <- from_json("PD", body, default_value = 0)
    HD <- from_json("HD", body, default_value = 0)
    PD_window <- from_json("PD_window", body, default_value = 0)
    HD_window <- from_json("HD_window", body, default_value = 0)

    FCY <- from_json("FCY", body)
    CMP <- from_json("CMP", body)
    saleSF <- from_json("saleSF", body, default_value = FALSE)
    nameSF <- from_json("nameSF", body, default_value = NA)
    cassPD <- from_json("cassPD", body, default_value = "roots")
    cassUW <- from_json("cassUW", body, default_value = 1000)
    cassUP <- from_json("cassUP", body)
    maxInv <- from_json("maxInv", body, default_value = NA)
    if (!isTRUE(maxInv > 0)) maxInv <- NA

    user <- get_user(body)    
	userField <- from_json("userField", body)
	riskAtt <- from_json("riskAtt", body, default_value = 0)

#    if (country == "BI") {
#		country <- "BU" #use non standard country code for Burundi
#    }

	selected_key <- unique(c("FR", "PP", "IC", "SP", "SP")[c(FR, PP, IC, SPP, SPH)])
	message(paste0(selected_key, ": ", country, ", planting: ", PD, ", harvest: ", HD))
    #riskAtt <- 0

    PD <- as.Date(PD, format = "%Y-%m-%d")
    HD <- as.Date(HD, format = "%Y-%m-%d")

    ## if cassava is to be sold to a processing factory, there should be a default price by factry and product
    # calculating rootUP based on cassUP, cassUW and conversion factor for cassava product sold
    rootConv <- data.frame(cassPD = c("roots", "chips", "flour", "gari"), conversion = c(1, 3, 3.2, 3.5))
    # Extract conversion factor once
    conversion_factor1 <- rootConv[rootConv$cassPD == cassPD, "conversion"]

	UPUW <- get_cassUPUW(cassUP, cassUW, cassPD, country, saleSF, nameSF)
	cassUP <- UPUW[1]
	cassUW <- UPUW[2]		

    # Calculate rootUP values using the same denominator 
    cass_denominator <- cassUW * conversion_factor1 / 1000
    # Compute each rootUP variant
    rootUP <- cassUP / cass_denominator

    # Define unit conversion factors to hectares
	areaUnits[areaUnits == "ekari"] <- "acre"
	areaUnits[areaUnits == "hekta"] <- "ha"
	
    unit_factors <- c(ha=1, acre=2.47105, are=100, m2=10000, string=1000)

    # Fallback to 10000 (i.e., square meters) if unit is unknown or missing
    conversion_factor2 <- unit_factors[[areaUnits]]
    if (is.null(conversion_factor2)) conversion_factor2 <- 10000
    # Calculate area in hectares
    areaHa <- area / conversion_factor2

    # Ensure PD and HD are Date objects
    PD <- as.Date(PD)
    HD <- as.Date(HD)

#	result <- list()
    if (FR) {

		fertilizers <- get_fertilizers2(body, country)

		result <- process_FR(lat=lat, lon=lon, HD=HD, maxInv=maxInv, 
				fertilizers=fertilizers, rootUP=rootUP, areaHa=areaHa, country=country, 
				FCY=FCY, riskAtt=riskAtt, user=user, userField=userField, area=area, 
				areaUnits=areaUnits, PD=PD, cassPD=cassPD, cassUW=cassUW)

    } else if (IC) {
	
		fertilizers <- get_fertilizers2(body, country)

		if (country == "NG") {
			maizePD <- from_json("maizePD", body, default_value = "fresh_cob")
			maizeUW <- from_json("maizeUW", body, default_value = NA)
			# Ensure maizeUW is numeric if using grain
			if (maizePD == "grain") {
				maizeUW <- as.numeric(as.character(maizeUW))
			}
			if (maizeUW == 0) maizeUW <- NA

			maizeUP <- from_json("maizeUP", body)
			# Set default price and weight if maizeUP is zero
			if (maizeUP == 0) {
				maizeUW <- 1
				if (maizePD == "fresh_cob") {
					maizeUP <- 50    # Default price for 1 large fresh cob
				} else if (maizePD == "grain") {
					maizeUP <- 230   # Default price for 1 kg of maize grain
				}
			}
			# Calculate cobUP  # 1 kg of grain ~ 7.64 cobs
			cobUP <- ifelse (maizePD == "fresh_cob", maizeUP, maizeUP / maizeUW / 7.64) 

			result <- process_IC_NG(
			  IC = IC, country = country, areaHa = areaHa, CMP = CMP, cobUP = cobUP, fertilizers = fertilizers,
			  riskAtt = riskAtt, maizePD = maizePD, user = user, userField = userField,
			  area = area, areaUnits = areaUnits, PD = PD, HD = HD, lat = lat, lon = lon,
			  maizeUW = maizeUW, cassUW = cassUW, saleSF = saleSF, nameSF = nameSF,
			  rootUP = rootUP, cassPD = cassPD, maxInv = maxInv, maizeUP = maizeUP
			)
			
		} else if (country == "TZ") {

			sweetPotatoPD <- from_json("sweetPotatoPD", body, default_value = "tubers")
			sweetPotatoUW <- from_json("sweetPotatoUW", body, default_value = NA)
			sweetPotatoUP <- from_json("sweetPotatoUP", body, default_value = NA)
			if (sweetPotatoUW == 0) sweetPotatoUW <- 1000 ## if it is not given default is a ton
		   
			# Conversion factors for sweetPotato products
			tuberConv <- data.frame(
				sweetPotatoPD = c("tubers", "flour"), # sweetpotato "tubers"? ouch.
				conversion = c(1, 3.2)
			)
			# Get the conversion factor
			conversion_factor3 <- tuberConv[tuberConv$sweetPotatoPD == sweetPotatoPD, "conversion"]
			# Compute tuberUP
			tuberUP <- sweetPotatoUP / sweetPotatoUW / conversion_factor3 * 1000

			# Set default price and weight for Tanzania if price is missing
			if (sweetPotatoUP == 0 && country == "TZ") {
				sweetPotatoUW <- 1000
				if (sweetPotatoPD == "tubers") {
					sweetPotatoUP <- 120000
				} else if (sweetPotatoPD == "flour") {
					sweetPotatoUP <- 384000
				}
			}

			result <- process_IC_TZ(
			  IC = IC, country = country, areaHa = areaHa, FCY = FCY, tuberUP = tuberUP, rootUP = rootUP,
			  fertilizers = fertilizers, riskAtt = riskAtt, user = user, 
			  userField = userField, area = area, areaUnits = areaUnits,
			  PD = PD, HD = HD, lat = lat, lon = lon, sweetPotatoUP = sweetPotatoUP, 
			  sweetPotatoPD = sweetPotatoPD, sweetPotatoUW = sweetPotatoUW, cassUW = cassUW, 
			  cassPD = cassPD, maxInv = maxInv
			)
		}
		
	} else if (PP) {

		ploughing <- from_json("ploughing", body, default_value = FALSE)
		harrowing <- from_json("harrowing", body, default_value = FALSE)
		ridging <- from_json("ridging", body, default_value = FALSE)
		method_ploughing <- from_json("method_ploughing", body)
		method_harrowing <- from_json("method_harrowing", body)
		method_ridging <- from_json("method_ridging", body)

		costLMO <- get_costLMO(body, country, areaHa, areaUnits, ploughing, harrowing, ridging, method_ploughing, method_harrowing, method_ridging)
		result <- process_PP(
			PP = PP, country = country, areaHa = areaHa, costLMO = costLMO,
			ploughing = ploughing, ridging = ridging,
			method_ploughing = method_ploughing, method_ridging = method_ridging,
			FCY = FCY, rootUP = rootUP, riskAtt = riskAtt,
			user = user, userField = userField, area = area, areaUnits = areaUnits,
			PD = PD, HD = HD, lat = lat, lon = lon,
			cassPD = cassPD, cassUW = cassUW, maxInv = maxInv
		)

    } else if (SPP || SPH) {
	
		cassUP_m1 <- from_json("cassUP_m1", body)
		cassUP_m2 <- from_json("cassUP_m2", body)
		cassUP_p1 <- from_json("cassUP_p1", body)
		cassUP_p2 <- from_json("cassUP_p2", body)
		rootUP_m1 <- cassUP_m1 / cass_denominator
		rootUP_m2 <- cassUP_m2 / cass_denominator
		rootUP_p1 <- cassUP_p1 / cass_denominator
		rootUP_p2 <- cassUP_p2 / cass_denominator

		result <- process_SP(
			SPP = SPP, SPH = SPH, PD_window = PD_window, HD_window = HD_window,	areaHa = areaHa,
			country = country, lat = lat, lon = lon, PD = PD, HD = HD, saleSF = saleSF, nameSF = nameSF,
			FCY = FCY, rootUP = rootUP, rootUP_m1 = rootUP_m1, rootUP_m2 = rootUP_m2, 
			rootUP_p1 = rootUP_p1, rootUP_p2 = rootUP_p2, user = user, userField = userField,
			area = area, areaUnits = areaUnits, maxInv = maxInv, ploughing = ploughing, ridging = ridging, method_ploughing = method_ploughing,	method_ridging = method_ridging, CMP = CMP, 
			riskAtt = riskAtt, cassPD = cassPD, cassUW = cassUW, cassUP = cassUP,
			cassUP_m1 = cassUP_m1, cassUP_m2 = cassUP_m2, cassUP_p1 = cassUP_p1, cassUP_p2 = cassUP_p2
		)
    }


    request_token <- jsonlite::unbox(from_json("request_token", body))

    if (is.null(selected_key)) {
		res$status <- 404
		data <- list(
			request_token = request_token,
			message = jsonlite::unbox("No valid recommendation found")
		)
		return(list(status = jsonlite::unbox("error"), data = data))
    }

	result$data <- c(request_token = request_token, result$data)
	# it would be clearer to use "message" instead of recommendation
	#r$data$message <- jsonlite::unbox(r$data$message)
	result$data$recommendation <- jsonlite::unbox(result$data$message)
	result$recom <- result$data$message <- NULL

	result$data$rec_type <- jsonlite::unbox(result$data$rec_type)
    list(status=jsonlite::unbox("success"), version=jsonlite::unbox(aki_version), data=result$data)

}  



from_json <- function(field_name, body, default_value = "NA") {
  if (!is.null(body[[field_name]])) {
    value <- body[[field_name]]
    if (!is.null(value)) {
      return(value)
    }
  }
  return(default_value)
}



get_user <- function(body) {
	list(
		send_SMS = from_json("SMS", body, default_value = FALSE),
		send_email = from_json("email", body, default_value = FALSE),
		PhoneCC = from_json("userPhoneCC", body),
		PhoneNr = from_json("userPhoneNr", body),
		Name = from_json("userName", body),
		Email = from_json("userEmail", body)
	)
}



get_cassUPUW <- function(cassUP, cassUW, cassPD, country, saleSF, nameSF) {

    if (saleSF) {
		SF <- get_data("starch_prices")
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
		} else if (country == "BI") {
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


get_costLMO <- function(body, country, areaHa, areaUnits, ploughing, harrowing, ridging, method_ploughing, method_harrowing, method_ridging) {

    # Determine area basis for cost calculation
	cost_LMO_areaBasis <- from_json("cost_LMO_areaBasis", body, default_value = "areaUnit")
    area_basis <- switch(cost_LMO_areaBasis, "areaField" = areaHa, 
			"acre" = 0.404686, "ha" = 1, 0.0001)  # fallback default (likely m²)

# for getWMrecommendations 
#		fallowType <- from_json("fallowType", body, default_value = "none")
		fallowHeight <- from_json("fallowHeight", body, default_value = NA)
#		fallowGreen <- from_json("fallowGreen", body, default_value = FALSE)
#		problemWeeds <- from_json("problemWeeds", body, default_value = FALSE)


		tractor_plough <- from_json("tractor_plough", body, default_value = FALSE)
		tractor_harrow <- from_json("tractor_harrow", body, default_value = FALSE)
		tractor_ridger <- from_json("tractor_ridger", body, default_value = FALSE)
		cost_tractor_ploughing <- from_json("cost_tractor_ploughing", body, default_value = NA)
		cost_tractor_harrowing <- from_json("cost_tractor_harrowing", body, default_value = NA)
		cost_tractor_ridging <- from_json("cost_tractor_ridging", body, default_value = NA)
		cost_manual_ploughing <- from_json("cost_manual_ploughing", body, default_value = NA)
		cost_manual_harrowing <- from_json("cost_manual_harrowing", body, default_value = NA)
		cost_manual_ridging <- from_json("cost_manual_ridging", body, default_value = NA)
		cost_weeding1 <- from_json("cost_weeding1", body, default_value = NA)
		cost_weeding2 <- from_json("cost_weeding2", body, default_value = NA)
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
		costLMO <- data.frame(
				operation = c(rep(c("ploughing", "harrowing", "ridging"), 2), "weeding1", "weeding2"),
				method = c(rep("manual", 3), rep("tractor", 3), NA, NA),
				cost = c(cost_manual_ploughing, cost_manual_harrowing, cost_manual_ridging, cost_tractor_ploughing, cost_tractor_harrowing, cost_tractor_ridging, cost_weeding1, cost_weeding2), area = area_basis)

		costLMO_MD <- costLMO
		costLMO$costHa <- costLMO$cost / costLMO$area
		costLMO <- subset(costLMO, select = -c(area, cost))

		# add default values for LMO operations if missing
		man <- costLMO$method == "manual"
		tract <- costLMO$method == "tractor"

		if (country == "NG") {
		  if (is.na(cost_manual_ploughing)) {
			costLMO[costLMO$operation == "ploughing" & man, "costHa"] <- 17000 * 2.47105
		  }
		  if (is.na(cost_manual_harrowing)) {
			costLMO[costLMO$operation == "harrowing" & man, "costHa"] <- 15000 * 2.47105
		  }  
		  if (is.na(cost_manual_ridging)) {
			costLMO[costLMO$operation == "ridging" & man, "costHa"] <- 12000 * 2.47105
		  }
		  if (is.na(cost_tractor_ploughing) & tractor_plough) {
			costLMO[costLMO$operation == "ploughing" & tract, "costHa"] <- 6000 * 2.47105
		  }
		  if (is.na(cost_tractor_harrowing) & tractor_harrow) {
			costLMO[costLMO$operation == "harrowing" & tract, "costHa"] <- 6000 * 2.47105
		  } 
		  if (is.na(cost_tractor_ridging) & tractor_ridger) {
			costLMO[costLMO$operation == "ridging" & tract, "costHa"] <- 6000 * 2.47105
		  }
		  if (is.na(cost_weeding1)) {
			costLMO[costLMO$operation == "weeding1", "costHa"] <- 30000 * 2.47105
		  }
		  if (is.na(cost_weeding2)) {
			costLMO[costLMO$operation == "weeding2", "costHa"] <- 30000 * 2.47105
		  }
		} else if (country == "TZ") {
		  if (is.na(cost_manual_ploughing)) {
			costLMO[costLMO$operation == "ploughing" & man, "costHa"] <- 175000 * 2.47105
		  }
		  if (is.na(cost_manual_harrowing)) {
			costLMO[costLMO$operation == "harrowing" & man, "costHa"] <- 150000 * 2.47105
		  }
		  if (is.na(cost_manual_ridging)) {
			costLMO[costLMO$operation == "ridging" & man, "costHa"] <- 225000 * 2.47105
		  }
		  if (is.na(cost_tractor_ploughing) & tractor_plough) {
			costLMO[costLMO$operation == "ploughing" & tract, "costHa"] <- 150000 * 2.47105
		  }
		  if (is.na(cost_tractor_harrowing) & tractor_harrow) {
			costLMO[costLMO$operation == "harrowing" & tract, "costHa"] <- 100000 * 2.47105
		  }
		  if (is.na(cost_tractor_ridging) & tractor_ridger) {
			costLMO[costLMO$operation == "ridging" & tract, "costHa"] <- 115000 * 2.47105
		  }
		  if (is.na(cost_weeding1)) {
			costLMO[costLMO$operation == "weeding1", "costHa"] <- 60000 * 2.47105
		  }
		  if (is.na(cost_weeding2)) {
			costLMO[costLMO$operation == "weeding2", "costHa"] <- 45000 * 2.47105
		  }
		}

		if (any(!is.na(c(cost_manual_ploughing, cost_manual_harrowing, cost_manual_ridging,
				cost_tractor_ploughing, cost_tractor_harrowing, cost_tractor_ridging,
				cost_weeding1, cost_weeding2)))) {
		  costLMO_MD$area <- paste(costLMO_MD$area, areaUnits, sep = "")
		  write.csv(costLMO_MD, "./temp/costLMO.csv", row.names = FALSE)
		} else {
		  costLMO_MD <- costLMO
		  names(costLMO_MD) <- c("operation", "method", "cost")
		  costLMO_MD$area <- "1ha"
		  costLMO_MD$cost <- formatC(signif(costLMO_MD$cost, digits = 3), format = "f", big.mark = ",", digits = 0)
		  write.csv(costLMO_MD, "./temp/costLMO.csv", row.names = FALSE)
		}
		costLMO
}

