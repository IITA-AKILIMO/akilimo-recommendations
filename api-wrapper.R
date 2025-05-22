#* Compute recommendations based on input parameters
#* @plumber
#* Accepts a JSON payload with user and agronomic data,
#* and returns a JSON response with recommendation results.
#*
#* @tag Recommendation
#* @param req Internal request object (injected by plumber)
#* @param res Internal response object (used to customize status or headers)
#* @post /compute
#* @serializer json
function(req, res) {
  tryCatch({
    # Parse JSON body
    json_input <- req$postBody
    body <- tryCatch(jsonlite::fromJSON(json_input), error = function(e) NULL)

    # Now extract parameters from the JSON payload
    country <- process_json_value("country", body)
    lat <- process_json_value("lat", body)
    lon <- process_json_value("lon", body)
    area <- process_json_value("area", body)
    areaUnits <- process_json_value("areaUnits", body)

    IC <- process_json_value("IC", body, default_value = FALSE)
    intercrop <- process_json_value("intercrop", body, default_value = FALSE)
    FR <- process_json_value("FR", body, default_value = FALSE)
    PP <- process_json_value("PP", body, default_value = FALSE)
    SPP <- process_json_value("SPP", body, default_value = FALSE)
    SPH <- process_json_value("SPH", body, default_value = FALSE)

    PD <- process_json_value("PD", body, default_value = 0)
    HD <- process_json_value("HD", body, default_value = 0)

    PD_window <- process_json_value("PD_window", body, default_value = 0)
    HD_window <- process_json_value("HD_window", body, default_value = 0)
    fallowType <- process_json_value("fallowType", body, default_value = "none")
    fallowHeight <- process_json_value("fallowHeight", body, default_value = NA)
    fallowGreen <- process_json_value("fallowGreen", body, default_value = FALSE)
    problemWeeds <- process_json_value("problemWeeds", body, default_value = FALSE)
    tractor_plough <- process_json_value("tractor_plough", body, default_value = FALSE)
    tractor_harrow <- process_json_value("tractor_harrow", body, default_value = FALSE)
    tractor_ridger <- process_json_value("tractor_ridger", body, default_value = FALSE)
    cost_LMO_areaBasis <- process_json_value("cost_LMO_areaBasis", body, default_value = "areaUnit")
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
    sweetPotatoPD <- process_json_value("sweetPotatoPD", body, default_value = "tubers")
    sweetPotatoUW <- process_json_value("sweetPotatoUW", body, default_value = NA)
    sweetPotatoUP <- process_json_value("sweetPotatoUP", body, default_value = NA)
    maizePD <- process_json_value("maizePD", body, default_value = "fresh_cob")
    maizeUW <- process_json_value("maizeUW", body, default_value = NA)
    maizeUP <- process_json_value("maizeUP", body)
    maxInv <- process_json_value("maxInv", body, default_value = NA)

    SMS <- process_json_value("SMS", body, default_value = FALSE)
    email <- process_json_value("email", body, default_value = FALSE)
    request_token <- process_json_value("request_token", body)
    userPhoneCC <- process_json_value("userPhoneCC", body)
    userPhoneNr <- process_json_value("userPhoneNr", body)
    userName <- process_json_value("userName", body)
    userEmail <- process_json_value("userEmail", body)
    userField <- process_json_value("userField", body)
    riskAtt <- process_json_value("riskAtt", body, default_value = 0)


    # UREA
    ureaavailable <- process_json_value("ureaavailable", body)
    ureaCostperBag <- process_json_value("ureaCostperBag", body)
    ureaBagWt <- process_json_value("ureaBagWt", body, default_value = 50)

    # MOP
    MOPavailable <- process_json_value("MOPavailable", body)
    MOPCostperBag <- process_json_value("MOPCostperBag", body)
    MOPBagWt <- process_json_value("MOPBagWt", body, default_value = 50)

    # DAP
    DAPavailable <- process_json_value("DAPavailable", body)
    DAPCostperBag <- process_json_value("DAPCostperBag", body)
    DAPBagWt <- process_json_value("DAPBagWt", body, default_value = 50)

    # NPK 20-10-10
    NPK201010available <- process_json_value("NPK201010available", body)
    NPK201010CostperBag <- process_json_value("NPK201010CostperBag", body)
    NPK201010BagWt <- process_json_value("NPK201010BagWt", body, default_value = 50)

    # NPK 20-12-16
    NPK201216available <- process_json_value("NPK201216available", body)
    NPK201216CostperBag <- process_json_value("NPK201216CostperBag", body)
    NPK201216BagWt <- process_json_value("NPK201216BagWt", body, default_value = 50)

    # NPK 15-15-15
    NPK151515available <- process_json_value("NPK151515available", body)
    NPK151515CostperBag <- process_json_value("NPK151515CostperBag", body)
    NPK151515BagWt <- process_json_value("NPK151515BagWt", body, default_value = 50)

    # TSP
    TSPavailable <- process_json_value("TSPavailable", body)
    TSPCostperBag <- process_json_value("TSPCostperBag", body)
    TSPBagWt <- process_json_value("TSPBagWt", body, default_value = 50)

    # NPK 17-17-17
    NPK171717available <- process_json_value("NPK171717available", body)
    NPK171717CostperBag <- process_json_value("NPK171717CostperBag", body)
    NPK171717BagWt <- process_json_value("NPK171717BagWt", body, default_value = 50)

    # Nafaka
    Nafakaavailable <- process_json_value("Nafakaavailable", body)
    NafakaCostperBag <- process_json_value("NafakaCostperBag", body)
    NafakaBagWt <- process_json_value("NafakaBagWt", body, default_value = 50)

    # CAN
    CANavailable <- process_json_value("CANavailable", body)
    CANCostperBag <- process_json_value("CANCostperBag", body)
    CANBagWt <- process_json_value("CANBagWt", body, default_value = 50)

    # SSP
    SSPavailable <- process_json_value("SSPavailable", body)
    SSPCostperBag <- process_json_value("SSPCostperBag", body)
    SSPBagWt <- process_json_value("SSPBagWt", body, default_value = 50)

    # NPK 11-22-21
    NPK112221available <- process_json_value("NPK112221available", body)
    NPK112221CostperBag <- process_json_value("NPK112221CostperBag", body)
    NPK112221BagWt <- process_json_value("NPK112221BagWt", body, default_value = 50)

    # NPK 25-10-10
    NPK251010available <- process_json_value("NPK251010available", body)
    NPK251010CostperBag <- process_json_value("NPK251010CostperBag", body)
    NPK251010BagWt <- process_json_value("NPK251010BagWt", body, default_value = 50)

    # NPK 20-12-16
    NPK201216available <- process_json_value("NPK201216available", body)
    NPK201216CostperBag <- process_json_value("NPK201216CostperBag", body)
    NPK201216BagWt <- process_json_value("NPK201216BagWt", body, default_value = 50)

    # NPK 15-20-20
    NPK152020available <- process_json_value("NPK152020available", body)
    NPK152020CostperBag <- process_json_value("NPK152020CostperBag", body)
    NPK152020BagWt <- process_json_value("NPK152020BagWt", body, default_value = 50)

    # NPK 23-10-5
    NPK23105available <- process_json_value("NPK23105available", body)
    NPK23105CostperBag <- process_json_value("NPK23105CostperBag", body)
    NPK23105BagWt <- process_json_value("NPK23105BagWt", body, default_value = 50)

    # NPK 12-30-17
    NPK123017available <- process_json_value("NPK123017available", body)
    NPK123017CostperBag <- process_json_value("NPK123017CostperBag", body)
    NPK123017BagWt <- process_json_value("NPK123017BagWt", body, default_value = 50)

    # FOMI IMBURA
    FOMIIMBURAavailable <- process_json_value("FOMIIMBURAavailable", body)
    FOMIIMBURACostperBag <- process_json_value("FOMIIMBURACostperBag", body)
    FOMIIMBURABagWt <- process_json_value("FOMIIMBURABagWt", body, default_value = 50)

    # FOMI BAGARA
    FOMIBAGARAavailable <- process_json_value("FOMIBAGARAavailable", body)
    FOMIBAGARACostperBag <- process_json_value("FOMIBAGARACostperBag", body)
    FOMIBAGARABagWt <- process_json_value("FOMIBAGARABagWt", body, default_value = 50)

    # FOMI TOTAHAZA
    FOMITOTAHAZAavailable <- process_json_value("FOMITOTAHAZAavailable", body)
    FOMITOTAHAZACostperBag <- process_json_value("FOMITOTAHAZACostperBag", body)
    FOMITOTAHAZABagWt <- process_json_value("FOMITOTAHAZABagWt", body, default_value = 50)

    # newFert1
    newFert1name <- process_json_value("newFert1name", body)
    newFert1N_cont <- process_json_value("newFert1N_cont", body)
    newFert1P2O5 <- process_json_value("newFert1P2O5", body)
    newFert1K2O <- process_json_value("newFert1K2O", body)
    newFertCostperBag <- process_json_value("newFertCostperBag", body)
    newFert1BagWt <- process_json_value("newFert1BagWt", body, default_value = 50)

    # newFert2
    newFert2name <- process_json_value("newFert2name", body)
    newFert2N_cont <- process_json_value("newFert2N_cont", body)
    newFert2P2O5 <- process_json_value("newFert2P2O5", body)
    newFert2K2O <- process_json_value("newFert2K2O", body)
    newFert2CostperBag <- process_json_value("newFert2CostperBag", body)
    newFert2BagWt <- process_json_value("newFert2BagWt", body, default_value = 50)

    # newFert3
    newFert3name <- process_json_value("newFert3name", body)
    newFert3N_cont <- process_json_value("newFert3N_cont", body)
    newFert3P2O5 <- process_json_value("newFert3P2O5", body)
    newFert3K2O <- process_json_value("newFert3K2O", body)
    newFert3CostperBag <- process_json_value("newFert3CostperBag", body)
    newFert3BagWt <- process_json_value("newFert3BagWt", body, default_value = 50)

    # newFert4
    newFert4name <- process_json_value("newFert4name", body)
    newFert4N_cont <- process_json_value("newFert4N_cont", body)
    newFert4P2O5 <- process_json_value("newFert4P2O5", body)
    newFert4K2O <- process_json_value("newFert4K2O", body)
    newFert4CostperBag <- process_json_value("newFert4CostperBag", body)
    newFert4BagWt <- process_json_value("newFert4BagWt", body, default_value = 50)

    # newFert5
    newFert5name <- process_json_value("newFert5name", body)
    newFert5N_cont <- process_json_value("newFert5N_cont", body)
    newFert5P2O5 <- process_json_value("newFert5P2O5", body)
    newFert5K2O <- process_json_value("newFert5K2O", body)
    newFert5CostperBag <- process_json_value("newFert5CostperBag", body)
    newFert5BagWt <- process_json_value("newFert5BagWt", body, default_value = 50)


    #now call the funtion to do the computations
    print(paste("Starting plumbR version 4 script processing ---", country))

    print(paste("Planting dated PD and HD", PD, HD))

    #riskAtt <- 0
    if (country == "BI") {
      country <- "BU" #use non standard counttry code for Burundi
    }

    ##call the fertilizerFunc in the AkilimoFunctions file
    fertilizers <- fertilizerFunc(ureaavailable = ureaavailable, ureaCostperBag = ureaCostperBag, ureaBagWt = ureaBagWt,
                                  MOPavailable = MOPavailable, MOPCostperBag = MOPCostperBag, MOPBagWt = MOPBagWt,
                                  DAPavailable = DAPavailable, DAPCostperBag = DAPCostperBag, DAPBagWt = DAPBagWt,
                                  NPK201010available = NPK201010available, NPK201010CostperBag = NPK201010CostperBag, NPK201010BagWt = NPK201010BagWt,
                                  NPK151515available = NPK151515available, NPK151515CostperBag = NPK151515CostperBag, NPK151515BagWt = NPK151515BagWt,
                                  TSPavailable = TSPavailable, TSPCostperBag = TSPCostperBag, TSPBagWt = TSPBagWt,
                                  NPK171717available = NPK171717available, NPK171717CostperBag = NPK171717CostperBag, NPK171717BagWt = NPK171717BagWt,
                                  NPK201216available = NPK201216available, NPK201216CostperBag = NPK201216CostperBag, NPK201216BagWt = NPK201216BagWt,
                                  CANavailable = CANavailable, CANCostperBag = CANCostperBag, CANBagWt = CANBagWt,
                                  SSPavailable = SSPavailable, SSPCostperBag = SSPCostperBag, SSPBagWt = SSPBagWt,
                                  NPK112221available = NPK112221available, NPK112221CostperBag = NPK112221CostperBag, NPK112221BagWt = NPK112221BagWt,
                                  NPK251010available = NPK251010available, NPK251010CostperBag = NPK251010CostperBag, NPK251010BagWt = NPK251010BagWt,
                                  NPK152020available = NPK152020available, NPK152020CostperBag = NPK152020CostperBag, NPK152020BagWt = NPK152020BagWt,
                                  NPK23105available = NPK23105available, NPK23105CostperBag = NPK23105CostperBag, NPK23105BagWt = NPK23105BagWt,
                                  NPK123017available = NPK123017available, NPK123017CostperBag = NPK123017CostperBag, NPK123017BagWt = NPK123017BagWt,

                                  FOMIIMBURAavailable = FOMIIMBURAavailable, FOMIIMBURACostperBag = FOMIIMBURACostperBag, FOMIIMBURABagWt = FOMIIMBURABagWt,
                                  FOMIBAGARAavailable = FOMIBAGARAavailable, FOMIBAGARACostperBag = FOMIBAGARACostperBag, FOMIBAGARABagWt = FOMIBAGARABagWt,
                                  FOMITOTAHAZAavailable = FOMITOTAHAZAavailable, FOMITOTAHAZACostperBag = FOMITOTAHAZACostperBag, FOMITOTAHAZABagWt = FOMITOTAHAZABagWt,

                                  newFert1name = newFert1name, newFert1N_cont = newFert1N_cont, newFert1P2O5 = newFert1P2O5,
                                  newFert1K2O = newFert1K2O, newFert1CostperBag = newFert1CostperBag, newFert1BagWt = newFert1BagWt,
                                  newFert2name = newFert2name, newFert2N_cont = newFert2N_cont, newFert2P2O5 = newFert2P2O5,
                                  newFert2K2O = newFert2K2O, newFert2CostperBag = newFert2CostperBag, newFert2BagWt = newFert2BagWt,
                                  newFert3name = newFert3name, newFert3N_cont = newFert3N_cont, newFert3P2O5 = newFert3P2O5,
                                  newFert3K2O = newFert3K2O, newFert3CostperBag = newFert3CostperBag, newFert3BagWt = newFert3BagWt,
                                  newFert4name = newFert4name, newFert4N_cont = newFert4N_cont, newFert4P2O5 = newFert4P2O5,
                                  newFert4K2O = newFert4K2O, newFert4CostperBag = newFert4CostperBag, newFert4BagWt = newFert4BagWt,
                                  newFert5name = newFert5name, newFert5N_cont = newFert5N_cont, newFert5P2O5 = newFert5P2O5,
                                  newFert5K2O = newFert5K2O, newFert5CostperBag = newFert5CostperBag, newFert5BagWt = newFert5BagWt, country = country)

    print("Finished processing fertilizers")
    print("==============================================")
    print("Prcessing the other variables for recommendations")

    if (method_ploughing == "NA") method_ploughing <- "N/A"
    if (method_ridging == "NA") method_ridging <- "N/A"


    if (sweetPotatoUW == 0) sweetPotatoUW <- 1000 ## if it is not given default is a ton

    if (cost_manual_ploughing == 0) cost_manual_ploughing <- NA
    if (cost_manual_harrowing == 0) cost_manual_harrowing <- NA


    if (cost_manual_ridging == 0) cost_manual_ridging <- NA
    if (cost_tractor_ploughing == 0) cost_tractor_ploughing <- NA
    if (cost_tractor_harrowing == 0) cost_tractor_harrowing <- NA
    if (cost_tractor_ridging == 0) cost_tractor_ridging <- NA

    if (cost_weeding1 == 0) cost_weeding1 <- NA
    if (cost_weeding2 == 0) cost_weeding2 <- NA
    if (maizeUW == 0) maizeUW <- NA
    if (maxInv == 0) maxInv <- NA
    if (fallowHeight == 0) fallowHeight <- NA

    PD <- as.Date(PD, format = "%Y-%m-%d")
    HD <- as.Date(HD, format = "%Y-%m-%d")

    ## if cassava is to be sold to a processing factory, there should be a default price by factry and product
    # calculating rootUP based on cassUP, cassUW and conversion factor for cassava product sold
    rootConv <- data.frame(cassPD = c("roots", "chips", "flour", "gari"), conversion = c(1, 3, 3.2, 3.5))


    if (saleSF) {
      SF <- read.csv("starchPrices.csv")
      SF <- SF[SF$starchFactory == nameSF,]
      cassUP <- max(SF$price)
      cassUW <- 1000
    }else {
      if (cassUP == 0 & cassPD == "roots" & country == "NG") { cassUP <- 12000; cassUW <- 1000 }
      if (cassUP == 0 & cassPD == "chips" & country == "NG") { cassUP <- 36000; cassUW <- 1000 }
      if (cassUP == 0 & cassPD == "flour" & country == "NG") { cassUP <- 38400; cassUW <- 1000 }
      if (cassUP == 0 & cassPD == "gari" & country == "NG") { cassUP <- 42000; cassUW <- 1000 }

      if (cassUP == 0 & cassPD == "roots" & country == "TZ") { cassUP <- 180000; cassUW <- 1000 }
      if (cassUP == 0 & cassPD == "chips" & country == "TZ") { cassUP <- 540000; cassUW <- 1000 }
      if (cassUP == 0 & cassPD == "flour" & country == "TZ") { cassUP <- 576000; cassUW <- 1000 }
      if (cassUP == 0 & cassPD == "gari" & country == "TZ") { cassUP <- 630000; cassUW <- 1000 }

      if (cassUP == 0 & cassPD == "roots" & country == "GH") { cassUP <- 450; cassUW <- 1000 }
      if (cassUP == 0 & cassPD == "chips" & country == "GH") { cassUP <- 450; cassUW <- 1000 }
      if (cassUP == 0 & cassPD == "flour" & country == "GH") { cassUP <- 450; cassUW <- 1000 }
      if (cassUP == 0 & cassPD == "gari" & country == "GH") { cassUP <- 450; cassUW <- 1000 }

      if (cassUP == 0 & cassPD == "roots" & country == "RW") { cassUP <- 75000; cassUW <- 1000 }
      if (cassUP == 0 & cassPD == "chips" & country == "RW") { cassUP <- 75000; cassUW <- 1000 }
      if (cassUP == 0 & cassPD == "flour" & country == "RW") { cassUP <- 75000; cassUW <- 1000 }
      if (cassUP == 0 & cassPD == "gari" & country == "RW") { cassUP <- 75000; cassUW <- 1000 }

      if (cassUP == 0 & cassPD == "roots" & country == "BU") { cassUP <- 700000; cassUW <- 1000 }
      if (cassUP == 0 & cassPD == "chips" & country == "BU") { cassUP <- 700000; cassUW <- 1000 }
      if (cassUP == 0 & cassPD == "flour" & country == "BU") { cassUP <- 700000; cassUW <- 1000 }
      if (cassUP == 0 & cassPD == "gari" & country == "BU") { cassUP <- 700000; cassUW <- 1000 }

    }

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
    cobUP <- if (maizePD == "fresh_cob") {
      maizeUP
    } else {
      maizeUP / maizeUW / 7.64  # 1 kg of grain ~ 7.64 cobs
    }

    # Conversion factors for sweetPotato products
    tuberConv <- data.frame(
      sweetPotatoPD = c("tubers", "flour"),
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

    # Define unit conversion factors to hectares
    unit_factors <- c(
      ha = 1,
      acre = 2.47105,
      are = 100,
      m2 = 10000
    )

    # Fallback to 10000 (i.e., square meters) if unit is unknown or missing
    conversion_factor <- unit_factors[[areaUnits]]
    if (is.null(conversion_factor)) conversion_factor <- 10000

    # Calculate area in hectares
    areaHa <- area / conversion_factor

    # Determine area basis for cost calculation
    area_basis <- switch(
      cost_LMO_areaBasis,
      "areaField" = areaHa,
      "acre" = 0.404686,
      "ha" = 1,
      0.0001  # fallback default (likely m²)
    )

    # create dataframe with cost of land management operations
    costLMO <- data.frame(operation = c(rep(c("ploughing", "harrowing", "ridging"), 2), "weeding1", "weeding2"),
                          method = c(rep("manual", 3), rep("tractor", 3), NA, NA),
                          cost = c(cost_manual_ploughing, cost_manual_harrowing, cost_manual_ridging, cost_tractor_ploughing, cost_tractor_harrowing, cost_tractor_ridging, cost_weeding1, cost_weeding2),
                          area = area_basis)

    costLMO_MD <- costLMO
    costLMO$costHa <- costLMO$cost / costLMO$area
    costLMO <- subset(costLMO, select = -c(area, cost))


    # add default values for LMO operations if missing
    if (country == "NG") {
      if (is.na(cost_manual_ploughing))                   costLMO[costLMO$operation == "ploughing" & costLMO$method == "manual",]$costHa <- 17000 * 2.47105
      if (is.na(cost_manual_harrowing))                   costLMO[costLMO$operation == "harrowing" & costLMO$method == "manual",]$costHa <- 15000 * 2.47105
      if (is.na(cost_manual_ridging))                     costLMO[costLMO$operation == "ridging" & costLMO$method == "manual",]$costHa <- 12000 * 2.47105
      if (is.na(cost_tractor_ploughing) & tractor_plough) costLMO[costLMO$operation == "ploughing" & costLMO$method == "tractor",]$costHa <- 6000 * 2.47105
      if (is.na(cost_tractor_harrowing) & tractor_harrow) costLMO[costLMO$operation == "harrowing" & costLMO$method == "tractor",]$costHa <- 6000 * 2.47105
      if (is.na(cost_tractor_ridging) & tractor_ridger) costLMO[costLMO$operation == "ridging" & costLMO$method == "tractor",]$costHa <- 6000 * 2.47105
      if (is.na(cost_weeding1))                           costLMO[costLMO$operation == "weeding1",]$costHa <- 30000 * 2.47105
      if (is.na(cost_weeding2))                           costLMO[costLMO$operation == "weeding2",]$costHa <- 30000 * 2.47105

    }else if (country == "TZ") {
      if (is.na(cost_manual_ploughing))                   costLMO[costLMO$operation == "ploughing" & costLMO$method == "manual",]$costHa <- 175000 * 2.47105
      if (is.na(cost_manual_harrowing))                   costLMO[costLMO$operation == "harrowing" & costLMO$method == "manual",]$costHa <- 150000 * 2.47105
      if (is.na(cost_manual_ridging))                     costLMO[costLMO$operation == "ridging" & costLMO$method == "manual",]$costHa <- 225000 * 2.47105
      if (is.na(cost_tractor_ploughing) & tractor_plough) costLMO[costLMO$operation == "ploughing" & costLMO$method == "tractor",]$costHa <- 150000 * 2.47105
      if (is.na(cost_tractor_harrowing) & tractor_harrow) costLMO[costLMO$operation == "harrowing" & costLMO$method == "tractor",]$costHa <- 100000 * 2.47105
      if (is.na(cost_tractor_ridging) & tractor_ridger) costLMO[costLMO$operation == "ridging" & costLMO$method == "tractor",]$costHa <- 115000 * 2.47105
      if (is.na(cost_weeding1))                           costLMO[costLMO$operation == "weeding1",]$costHa <- 60000 * 2.47105
      if (is.na(cost_weeding2))                           costLMO[costLMO$operation == "weeding2",]$costHa <- 45000 * 2.47105


    }


    if (!is.na(cost_manual_ploughing) |
      !is.na(cost_manual_harrowing) |
      !is.na(cost_manual_ridging) |
      !is.na(cost_tractor_ploughing) |
      !is.na(cost_tractor_harrowing) |
      !is.na(cost_tractor_ridging) |
      !is.na(cost_weeding1) |
      !is.na(cost_weeding2)) {
      costLMO_MD$area <- paste(costLMO_MD$area, areaUnits, sep = "")
      write.csv(costLMO_MD, "costLMO.csv", row.names = FALSE)
    }else {
      costLMO_MD <- costLMO
      names(costLMO_MD) <- c("operation", "method", "cost")
      costLMO_MD$area <- "1ha"
      costLMO_MD$cost <- formatC(signif(costLMO_MD$cost, digits = 3), format = "f", big.mark = ",", digits = 0)
      write.csv(costLMO_MD, "costLMO.csv", row.names = FALSE)

    }


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
    TRNS <- read.csv("translations_TEST.csv", stringsAsFactors = FALSE)

    # Define a function to clean the data
    clean_data <- function(column, index) {
      gsub(pattern = "\"", replacement = "", column[index])
    }

    # Clean the required variables
    recloc_ng <- clean_data(TRNS$recloc, 1)
    recloc_tz <- clean_data(TRNS$recloc, 2)
    recloc_rw <- clean_data(TRNS$recloc, 3)

    frnotrec_ng <- clean_data(TRNS$frnotrec, 1)
    frnotrec_tz <- clean_data(TRNS$frnotrec, 2)
    frnotrec_rw <- clean_data(TRNS$frnotrec, 3)

    spinfo_ng <- clean_data(TRNS$spinfo, 1)
    spinfo_rw <- clean_data(TRNS$spinfo, 3)
    spinfo_tz <- clean_data(TRNS$spinfo, 2)

    print("Begin processing")
    # Define countries with the same logic

    selected_key <- NULL

    if (FR) {
      resFr <- process_FR(
        FR, lat, lon, pd, pw, HD, had, maxInv, fertilizers, rootUP, areaHa, country, FCY, riskAtt,
        userName, userPhoneNr, userField, area, areaUnits, PD, email, userPhoneCC,
        cassPD, cassUW, recText, plumberRes, frnotrec_ng, frnotrec_tz, frnotrec_rw
      )

      FRrecom <- resFr$FRrecom
      recText <- resFr$recText
      plumberRes <- resFr$plumberRes
      selected_key <- 'FR'
    }

    if (IC) {
      if (country == "NG") {
        resIC <- process_IC_NG(
          IC = IC, country = country, areaHa = areaHa, CMP = CMP, cobUP = cobUP, fertilizers = fertilizers,
          riskAtt = riskAtt, maizePD = maizePD, userName = userName, userPhoneNr = userPhoneNr, userField = userField,
          area = area, areaUnits = areaUnits, PD = PD, HD = HD, email = email, lat = lat, lon = lon,
          userPhoneCC = userPhoneCC, maizeUW = maizeUW, cassUW = cassUW, saleSF = saleSF, nameSF = nameSF,
          rootUP = rootUP, cassPD = cassPD, maxInv = maxInv, maizeUP = maizeUP, res = plumberRes, recText = recText
        )
      }

      if (country == "TZ") {
        resIC <- process_IC_TZ(
          IC = IC, country = country, areaHa = areaHa, FCY = FCY, tuberUP = tuberUP, rootUP = rootUP,
          fertilizers = fertilizers, riskAtt = riskAtt, userName = userName, userPhoneNr = userPhoneNr,
          userPhoneCC = userPhoneCC, email = email, userField = userField, area = area, areaUnits = areaUnits,
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
      resPP <- process_PP(
        PP = PP, country = country,
        areaHa = areaHa, costLMO = costLMO,
        ploughing = ploughing, ridging = ridging,
        method_ploughing = method_ploughing, method_ridging = method_ridging,
        FCY = FCY, rootUP = rootUP, riskAtt = riskAtt,
        userName = userName, userPhoneNr = userPhoneNr, userPhoneCC = userPhoneCC,
        userField = userField, area = area, areaUnits = areaUnits,
        PD = PD, HD = HD, email = email, lat = lat, lon = lon,
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
        userName = userName, userPhoneNr = userPhoneNr, userField = userField,
        area = area, areaUnits = areaUnits, email = email, maxInv = maxInv,
        ploughing = ploughing, ridging = ridging, method_ploughing = method_ploughing,
        method_ridging = method_ridging, userPhoneCC = userPhoneCC,
        CMP = CMP, riskAtt = riskAtt,
        cassPD = cassPD, cassUW = cassUW, cassUP = cassUP,
        cassUP_m1 = cassUP_m1, cassUP_m2 = cassUP_m2,
        cassUP_p1 = cassUP_p1, cassUP_p2 = cassUP_p2,
        res = plumberRes, recText = recText
      )

      SPrecom <- resSP$SPRecom
      recText <- resSP$recText
      plumberRes <- resSP$plumberRes
      selected_key <- 'SP'
    }


    #=============================================================================
    result <- list(
      res = plumberRes,
      recText = recText
    )


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
  }, error = function(e) {
    res$status <- 500
    print(e)
    data <- list(
      request_token = jsonlite::unbox(request_token),
      message = jsonlite::unbox(e$message),
      traces = jsonlite::unbox(capture.output(e))
    )
    list(status = jsonlite::unbox("error"), data = data)
  })
}

process_json_value <- function(field_name, body, default_value = "NA") {
  if (!is.null(body[[field_name]])) {
    value <- body[[field_name]]
    if (!is.null(value)) {
      return(value)
    }
  }
  return(default_value)
}


  
