
bad_request <- function(msg) {
    list(status = jsonlite::unbox("400 - bad request"),
         data   = list(message = jsonlite::unbox(msg)))
}

validate_request <- function(body) {
    VALID_COUNTRIES <- c("NG", "TZ", "RW", "GH", "BI")
    VALID_AREA_UNITS <- c("ha", "acre", "are", "m2")

    country   <- body[["country"]]
    lat       <- body[["lat"]]
    lon       <- body[["lon"]]
    area      <- body[["area"]]
    areaUnits <- body[["areaUnits"]]
    if (!is.null(areaUnits)) areaUnits <- tolower(areaUnits)
    flags <- as.logical(c(body[["FR"]], body[["IC"]], body[["PP"]], body[["SPP"]], body[["SPH"]]))

    if (is.null(country)   || nchar(trimws(country)) == 0)
        return("Missing required field: country")
    if (!country %in% VALID_COUNTRIES)
        return(paste("Invalid country:", country, "— must be one of:", paste(VALID_COUNTRIES, collapse = ", ")))
    if (is.null(lat) || !is.numeric(lat) || lat < -90  || lat > 90)
        return("Invalid or missing lat — must be numeric between -90 and 90")
    if (is.null(lon) || !is.numeric(lon) || lon < -180 || lon > 180)
        return("Invalid or missing lon — must be numeric between -180 and 180")
    if (is.null(area) || !is.numeric(area) || area <= 0)
        return("Invalid or missing area — must be a positive number")
    if (is.null(areaUnits) || !areaUnits %in% VALID_AREA_UNITS)
        return(paste("Invalid or missing areaUnits — must be one of:", paste(VALID_AREA_UNITS, collapse = ", ")))
    if (!any(flags))
        return("At least one recommendation flag must be TRUE (FR, IC, PP, SPP, or SPH)")

    FCY <- body[["FCY"]]
    if (!is.null(FCY) && (!is.numeric(FCY) || FCY < 0 || FCY > 100))
        return("Invalid FCY — must be a number between 0 and 100 t/ha")

    for (date_field in c("PD", "HD")) {
        val <- body[[date_field]]
        if (!is.null(val) && !is.na(val) && nchar(as.character(val)) > 0) {
            if (is.na(as.Date(as.character(val), format = "%Y-%m-%d")))
                return(paste(date_field, "must be a valid date in YYYY-MM-DD format"))
        }
    }

    NULL  # no error
}


setup_temp_dir <- function() {
    dir.create("temp", FALSE, FALSE)
    # Clean up per-request subdirectories older than 1 hour
    subdirs <- list.dirs("temp", full.names = TRUE, recursive = FALSE)
    old <- subdirs[file.info(subdirs)$mtime < Sys.time() - 3600]
    for (d in old) unlink(d, recursive = TRUE)
    # Create an isolated directory for this request
    req_id  <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_",
                      paste0(as.hexmode(sample.int(.Machine$integer.max, 2)), collapse = ""))
    req_dir <- file.path("temp", req_id)
    dir.create(req_dir)
    req_dir
}


# Extract and normalise all common request parameters into a named list.
parse_request <- function(body) {
    country   <- from_json("country",   body)
    lat       <- from_json("lat",       body)
    lon       <- from_json("lon",       body)
    area      <- from_json("area",      body)
    areaUnits <- tolower(from_json("areaUnits", body))

    IC  <- from_json("IC",  body, default_value = FALSE)
    FR  <- from_json("FR",  body, default_value = FALSE)
    PP  <- from_json("PP",  body, default_value = FALSE)
    SPP <- from_json("SPP", body, default_value = FALSE)
    SPH <- from_json("SPH", body, default_value = FALSE)

    PD        <- as.Date(from_json("PD", body, default_value = 0), format = "%Y-%m-%d")
    HD        <- as.Date(from_json("HD", body, default_value = 0), format = "%Y-%m-%d")
    PD_window <- from_json("PD_window", body, default_value = 0)
    HD_window <- from_json("HD_window", body, default_value = 0)

    FCY    <- from_json("FCY",    body)
    CMP    <- from_json("CMP",    body)
    saleSF <- from_json("saleSF", body, default_value = FALSE)
    nameSF <- from_json("nameSF", body, default_value = NA)
    cassPD <- from_json("cassPD", body, default_value = "roots")
    cassUW <- as.numeric(from_json("cassUW", body, default_value = 1000))
    cassUP <- as.numeric(from_json("cassUP", body, default_value = 0))
    maxInv <- from_json("maxInv", body, default_value = NA)
    if (!isTRUE(maxInv > 0)) maxInv <- NA

    user      <- get_user(body)
    userField <- from_json("userField", body)
    riskAtt   <- from_json("riskAtt",   body, default_value = 0)

    lang_raw <- from_json("lang", body, default_value = "en")
    lang <- if (lang_raw %in% c("en", "sw")) lang_raw else "en"

    flag_to_key  <- c(FR = "FR", PP = "PP", IC = "IC", SPP = "SP", SPH = "SP")
    active_flags <- c(FR = FR, PP = PP, IC = IC, SPP = SPP, SPH = SPH)
    selected_key <- unique(flag_to_key[active_flags])

    UPUW   <- get_cassUPUW(cassUP, cassUW, cassPD, country, saleSF, nameSF)
    cassUP <- UPUW[1]
    cassUW <- UPUW[2]
    if (!is.numeric(cassUW) || cassUW <= 0) {
        warning("cassUW is zero or invalid; defaulting to 1000 kg")
        cassUW <- 1000
    }

    rootConv          <- data.frame(cassPD = c("roots", "chips", "flour", "gari"), conversion = c(1, 3, 3.2, 3.5))
    conversion_factor <- rootConv[rootConv$cassPD == cassPD, "conversion"]
    cass_denominator  <- cassUW * conversion_factor / 1000
    rootUP            <- cassUP / cass_denominator

    areaUnits[areaUnits == "ekari"] <- "acre"
    areaUnits[areaUnits == "hekta"] <- "ha"
    unit_factors <- c(ha = 1, acre = 2.47105, are = 100, m2 = 10000, string = 1000)
    areaHa <- area / unit_factors[[areaUnits]]

    list(
        country = country, lang = lang, lat = lat, lon = lon, area = area, areaUnits = areaUnits,
        IC = IC, FR = FR, PP = PP, SPP = SPP, SPH = SPH,
        PD = PD, HD = HD, PD_window = PD_window, HD_window = HD_window,
        FCY = FCY, CMP = CMP, saleSF = saleSF, nameSF = nameSF,
        cassPD = cassPD, cassUW = cassUW, cassUP = cassUP, maxInv = maxInv,
        user = user, userField = userField, riskAtt = riskAtt,
        selected_key = selected_key, areaHa = areaHa,
        rootUP = rootUP, cass_denominator = cass_denominator
    )
}


# Route to the appropriate processor based on the active recommendation flag.
dispatch_recommendations <- function(p, body) {

    if (p$FR) {

        fertilizers <- get_fertilizers2(body, p$country)
        process_FR(
            lat = p$lat, lon = p$lon, HD = p$HD, maxInv = p$maxInv,
            fertilizers = fertilizers, rootUP = p$rootUP, areaHa = p$areaHa,
            country = p$country, lang = p$lang, FCY = p$FCY, riskAtt = p$riskAtt,
            user = p$user, userField = p$userField, area = p$area,
            areaUnits = p$areaUnits, PD = p$PD, cassPD = p$cassPD, cassUW = p$cassUW
        )

    } else if (p$IC) {

        fertilizers <- get_fertilizers2(body, p$country)

        if (p$country == "NG") {

            maizePD <- from_json("maizePD", body, default_value = "fresh_cob")
            maizeUW <- from_json("maizeUW", body, default_value = NA)
            if (maizePD == "grain") maizeUW <- as.numeric(as.character(maizeUW))
            if (!is.na(maizeUW) && maizeUW == 0) maizeUW <- NA

            maizeUP <- as.numeric(from_json("maizeUP", body, default_value = 0))
            if (!is.na(maizeUP) && maizeUP == 0) {
                maizeUW <- 1
                maizeUP <- if (maizePD == "fresh_cob") 50 else 230
            }
            cobUP <- ifelse(maizePD == "fresh_cob", maizeUP, maizeUP / maizeUW / 7.64)

            process_IC_NG(
                IC = p$IC, country = p$country, lang = p$lang, areaHa = p$areaHa, CMP = p$CMP,
                cobUP = cobUP, fertilizers = fertilizers, riskAtt = p$riskAtt,
                maizePD = maizePD, user = p$user, userField = p$userField,
                area = p$area, areaUnits = p$areaUnits, PD = p$PD, HD = p$HD,
                lat = p$lat, lon = p$lon, maizeUW = maizeUW, cassUW = p$cassUW,
                saleSF = p$saleSF, nameSF = p$nameSF, rootUP = p$rootUP,
                cassPD = p$cassPD, maxInv = p$maxInv, maizeUP = maizeUP
            )

        } else if (p$country %in% c("RW", "GH", "BI")) {
            return(bad_request(paste(
                "Intercropping (IC) recommendations are not yet available for country:", p$country,
                "— supported countries are NG and TZ"
            )))

        } else if (p$country == "TZ") {

            sweetPotatoPD <- from_json("sweetPotatoPD", body, default_value = "tubers")
            sweetPotatoUW <- from_json("sweetPotatoUW", body, default_value = NA)
            sweetPotatoUP <- from_json("sweetPotatoUP", body, default_value = NA)
            if (!is.na(sweetPotatoUW) && sweetPotatoUW == 0) sweetPotatoUW <- 1000

            # Apply price defaults BEFORE computing tuberUP to avoid dividing by zero price
            if (is.na(sweetPotatoUP) || sweetPotatoUP == 0) {
                sweetPotatoUW <- 1000
                sweetPotatoUP <- if (sweetPotatoPD == "tubers") 120000 else 384000
            }

            tuberConv <- data.frame(
                sweetPotatoPD = c("tubers", "flour"),
                conversion    = c(1, 3.2)
            )
            conversion_factor3 <- tuberConv[tuberConv$sweetPotatoPD == sweetPotatoPD, "conversion"]
            tuberUP <- sweetPotatoUP / sweetPotatoUW / conversion_factor3 * 1000

            process_IC_TZ(
                IC = p$IC, country = p$country, lang = p$lang, areaHa = p$areaHa, FCY = p$FCY,
                tuberUP = tuberUP, rootUP = p$rootUP, fertilizers = fertilizers,
                riskAtt = p$riskAtt, user = p$user, userField = p$userField,
                area = p$area, areaUnits = p$areaUnits, PD = p$PD, HD = p$HD,
                lat = p$lat, lon = p$lon, sweetPotatoUP = sweetPotatoUP,
                sweetPotatoPD = sweetPotatoPD, sweetPotatoUW = sweetPotatoUW,
                cassUW = p$cassUW, cassPD = p$cassPD, maxInv = p$maxInv
            )
        }

    } else if (p$PP) {

        ploughing        <- from_json("ploughing",        body, default_value = FALSE)
        harrowing        <- from_json("harrowing",        body, default_value = FALSE)
        ridging          <- from_json("ridging",          body, default_value = FALSE)
        method_ploughing <- from_json("method_ploughing", body)
        method_harrowing <- from_json("method_harrowing", body)
        method_ridging   <- from_json("method_ridging",   body)
        if (method_ploughing == "NA") method_ploughing <- "N/A"
        method_ridging <- ifelse(method_ridging == "NA", "N/A", tolower(method_ridging))

        costLMO <- get_costLMO(body, p$country, p$areaHa, p$areaUnits,
                               ploughing, harrowing, ridging,
                               method_ploughing, method_harrowing, method_ridging)

        process_PP(
            PP = p$PP, country = p$country, lang = p$lang, areaHa = p$areaHa, costLMO = costLMO,
            ploughing = ploughing, ridging = ridging,
            method_ploughing = method_ploughing, method_ridging = method_ridging,
            FCY = p$FCY, rootUP = p$rootUP, riskAtt = p$riskAtt,
            user = p$user, userField = p$userField, area = p$area,
            areaUnits = p$areaUnits, PD = p$PD, HD = p$HD, lat = p$lat, lon = p$lon,
            cassPD = p$cassPD, cassUW = p$cassUW, maxInv = p$maxInv
        )

    } else if (p$SPP || p$SPH) {

        cassUP_m1 <- from_json("cassUP_m1", body)
        cassUP_m2 <- from_json("cassUP_m2", body)
        cassUP_p1 <- from_json("cassUP_p1", body)
        cassUP_p2 <- from_json("cassUP_p2", body)
        rootUP_m1 <- cassUP_m1 / p$cass_denominator
        rootUP_m2 <- cassUP_m2 / p$cass_denominator
        rootUP_p1 <- cassUP_p1 / p$cass_denominator
        rootUP_p2 <- cassUP_p2 / p$cass_denominator

        ploughing        <- from_json("ploughing",        body, default_value = FALSE)
        harrowing        <- from_json("harrowing",        body, default_value = FALSE)
        ridging          <- from_json("ridging",          body, default_value = FALSE)
        method_ploughing <- from_json("method_ploughing", body)
        method_harrowing <- from_json("method_harrowing", body)
        method_ridging   <- from_json("method_ridging",   body)

        process_SP(
            SPP = p$SPP, SPH = p$SPH, PD_window = p$PD_window, HD_window = p$HD_window,
            areaHa = p$areaHa, country = p$country, lang = p$lang, lat = p$lat, lon = p$lon,
            PD = p$PD, HD = p$HD, saleSF = p$saleSF, nameSF = p$nameSF,
            FCY = p$FCY, rootUP = p$rootUP, rootUP_m1 = rootUP_m1, rootUP_m2 = rootUP_m2,
            rootUP_p1 = rootUP_p1, rootUP_p2 = rootUP_p2,
            user = p$user, userField = p$userField, area = p$area, areaUnits = p$areaUnits,
            maxInv = p$maxInv, ploughing = ploughing, ridging = ridging,
            method_ploughing = method_ploughing, method_ridging = method_ridging,
            CMP = p$CMP, riskAtt = p$riskAtt, cassPD = p$cassPD, cassUW = p$cassUW,
            cassUP = p$cassUP, cassUP_m1 = cassUP_m1, cassUP_m2 = cassUP_m2,
            cassUP_p1 = cassUP_p1, cassUP_p2 = cassUP_p2
        )
    }
}


build_response <- function(result, aki_version) {
    if (is.null(result)) return(bad_request("No valid recommendation found"))
    result$recommendation <- jsonlite::unbox(gsub("[ ]+", " ", result$recommendation))
    result$rec_type        <- jsonlite::unbox(result$rec_type)
    c(list(status = jsonlite::unbox("success"), version = jsonlite::unbox(aki_version)), result)
}


run_akilimo <- function(json) {

    aki_version <- "20251228"
    set_temp_dir(setup_temp_dir())

    body <- try(jsonlite::fromJSON(json))
    if (inherits(body, "try-error")) return(bad_request("Malformed JSON body"))

    err <- validate_request(body)
    if (!is.null(err)) return(bad_request(err))

    params <- parse_request(body)

    message(paste0(params$selected_key, ": ", params$country,
                   ", planting: ", params$PD, ", harvest: ", params$HD))

    result <- dispatch_recommendations(params, body)
    build_response(result, aki_version)
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
		# Default cassava prices (per 1000 kg) by country and product type
		default_cass_prices <- list(
			NG = c(roots = 12000,  chips = 36000,  flour = 38400,  gari = 42000),
			TZ = c(roots = 180000, chips = 540000,  flour = 576000, gari = 630000),
			GH = c(roots = 450,    chips = 450,     flour = 450,    gari = 450),
			RW = c(roots = 75000,  chips = 75000,   flour = 75000,  gari = 75000),
			BI = c(roots = 700000, chips = 700000,  flour = 700000, gari = 700000)
		)
		country_prices <- default_cass_prices[[country]]
		if (!is.null(country_prices) && !is.null(country_prices[[cassPD]])) {
			cassUP <- country_prices[[cassPD]]
			cassUW <- 1000
		} else {
			warning(paste("No default cassava price for country:", country, "product:", cassPD))
		}
	}
	c(cassUP, cassUW )
}


get_costLMO <- function(body, country, areaHa, areaUnits, ploughing, harrowing, ridging, method_ploughing, method_harrowing, method_ridging) {

    # Determine area basis for cost calculation
	cost_LMO_areaBasis <- from_json("cost_LMO_areaBasis", body, default_value = "areaUnit")
    area_basis <- switch(cost_LMO_areaBasis,
			"areaUnit" = areaHa,
			"acre" = 0.404686, "ha" = 1, 0.0001)  # fallback default (likely m²)

		fallowHeight <- from_json("fallowHeight", body, default_value = NA)

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
		if (!is.na(cost_manual_ploughing)  && cost_manual_ploughing  == 0) cost_manual_ploughing  <- NA
		if (!is.na(cost_manual_harrowing)  && cost_manual_harrowing  == 0) cost_manual_harrowing  <- NA
		if (!is.na(cost_manual_ridging)    && cost_manual_ridging    == 0) cost_manual_ridging    <- NA
		if (!is.na(cost_tractor_ploughing) && cost_tractor_ploughing == 0) cost_tractor_ploughing <- NA
		if (!is.na(cost_tractor_harrowing) && cost_tractor_harrowing == 0) cost_tractor_harrowing <- NA
		if (!is.na(cost_tractor_ridging)   && cost_tractor_ridging   == 0) cost_tractor_ridging   <- NA

		if (!is.na(cost_weeding1)  && cost_weeding1  == 0) cost_weeding1  <- NA
		if (!is.na(cost_weeding2)  && cost_weeding2  == 0) cost_weeding2  <- NA
		if (!is.na(fallowHeight)   && fallowHeight   == 0) fallowHeight   <- NA

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

		# Conversion factor: cost per acre → cost per hectare
		ACRES_PER_HA <- 2.47105

		# Default land management operation costs (per acre) by country
		default_lmo_costs <- list(
			NG = list(
				manual_ploughing  = 17000, manual_harrowing  = 15000, manual_ridging  = 12000,
				tractor_ploughing =  6000, tractor_harrowing =  6000, tractor_ridging =  6000,
				weeding1          = 30000, weeding2          = 30000
			),
			TZ = list(
				manual_ploughing  = 175000, manual_harrowing  = 150000, manual_ridging  = 225000,
				tractor_ploughing = 150000, tractor_harrowing = 100000, tractor_ridging = 115000,
				weeding1          =  60000, weeding2          =  45000
			)
		)

		lmo <- default_lmo_costs[[country]]
		if (!is.null(lmo)) {
		  if (is.na(cost_manual_ploughing))
			costLMO[costLMO$operation == "ploughing" & man,   "costHa"] <- lmo$manual_ploughing  * ACRES_PER_HA
		  if (is.na(cost_manual_harrowing))
			costLMO[costLMO$operation == "harrowing" & man,   "costHa"] <- lmo$manual_harrowing  * ACRES_PER_HA
		  if (is.na(cost_manual_ridging))
			costLMO[costLMO$operation == "ridging"   & man,   "costHa"] <- lmo$manual_ridging    * ACRES_PER_HA
		  if (is.na(cost_tractor_ploughing) & tractor_plough)
			costLMO[costLMO$operation == "ploughing" & tract, "costHa"] <- lmo$tractor_ploughing * ACRES_PER_HA
		  if (is.na(cost_tractor_harrowing) & tractor_harrow)
			costLMO[costLMO$operation == "harrowing" & tract, "costHa"] <- lmo$tractor_harrowing * ACRES_PER_HA
		  if (is.na(cost_tractor_ridging)   & tractor_ridger)
			costLMO[costLMO$operation == "ridging"   & tract, "costHa"] <- lmo$tractor_ridging   * ACRES_PER_HA
		  if (is.na(cost_weeding1))
			costLMO[costLMO$operation == "weeding1",          "costHa"] <- lmo$weeding1          * ACRES_PER_HA
		  if (is.na(cost_weeding2))
			costLMO[costLMO$operation == "weeding2",          "costHa"] <- lmo$weeding2          * ACRES_PER_HA
		}

		if (any(!is.na(c(cost_manual_ploughing, cost_manual_harrowing, cost_manual_ridging,
				cost_tractor_ploughing, cost_tractor_harrowing, cost_tractor_ridging,
				cost_weeding1, cost_weeding2)))) {
		  costLMO_MD$area <- paste(costLMO_MD$area, areaUnits, sep = "")
		  write.csv(costLMO_MD, tp("costLMO.csv"), row.names = FALSE)
		} else {
		  costLMO_MD <- costLMO
		  names(costLMO_MD) <- c("operation", "method", "cost")
		  costLMO_MD$area <- "1ha"
		  costLMO_MD$cost <- formatC(signif(costLMO_MD$cost, digits = 3), format = "f", big.mark = ",", digits = 0)
		  write.csv(costLMO_MD, tp("costLMO.csv"), row.names = FALSE)
		}
		costLMO
}
