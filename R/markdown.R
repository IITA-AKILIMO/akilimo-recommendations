
### R markdown



get_markdown_text <- function(FR, IC, country) {
	if (FR & !IC) {
		acairm <- read.csv("./temp/FR_MarkDownText.csv")
	} else if (IC & !FR) {
		if (country == "TZ") {
			acairm <- read.csv("./temp/CIS_MarkDownText.csv")
		} else if (country == "NG") {
			acairm <- read.csv("./temp/IC_MarkDownText.csv")
		}
	} else {
		stop("FR and IC can not both be TRUE")
	}
	acairm$currency <- ifelse(acairm$country == "NG", "NGN",
                        ifelse(acairm$country == "TZ", "TZS", 
						ifelse(acairm$country == "BU", "BIF", "GHS")))
	acairm
}


#'function to put data used in the markdown .rmd. It selects the number of bags of fertilizerm the color, the money stubs for cost, sale and profit
#'It reads the FR_MarkdownText, SP_MarkdownText, IC_MarkdownText, BPP_MarkdownText saved output which brings together the user info and based on
#' the recommendation the fertilizer types, amount, bag colur, cost and profit
fertilizerAdviseTable <- function(FR, IC, country, areaUnits) {
 
	suppressWarnings(file.remove(paste0("./temp/datall", 1:6, ".csv")))

	acairm <- get_markdown_text(FR, IC, country)

  ## Loop
  Nrfert <- length(grep("fertilizer", colnames(acairm)))
  if (Nrfert > 0) {
    for (j in 1:Nrfert) {
      colNames <- c(paste(c("fertilizer", "bags", "cost", "total_cost", "kgs", "unit", "costPerBag"), j, sep = ""))
      colNames <- c(colNames, c("currency", "field_area", "unit_field"))
      dat <- acairm[, colNames]
      dat$bag <- dat[, paste("bags", j, sep = "")]

      #TO included code for NPK1520202 to read from orange bags then set grey bags other
      fertColCode <- "green"
      if (dat[, 1] == "Urea") {
        fertColCode <- "green"
        dat[, 1] <- "Urea"
      }else if (dat[, 1] == "NPK15_15_15") {
        fertColCode <- "blue"
        dat[, 1] <- "NPK15:15:15"
      }else if (dat[, 1] == "NPK20_10_10") {
        fertColCode <- "yellow"
        dat[, 1] <- "NPK20:10:10"
      }else if (dat[, 1] == "NPK17_17_17") {
        fertColCode <- "purple"
        dat[, 1] <- "NPK17:17:17"
      }else if (dat[, 1] == "NPK20_12_16") {
        fertColCode <- "royal"
        dat[, 1] <- "NPK20:12:16+2Mg"
      }else if (dat[, 1] == "NPK152020") {
        fertColCode <- "orange"
        dat[, 1] <- "NPK15:20:20"
      }else if (dat[, 1] == "FOMI_TOTAHAZA") {
        fertColCode <- "red"
        dat[, 1] <- "FOMI-TOTAHAZA"
      }else if (dat[, 1] == "FOMI_IMBURA") {
        fertColCode <- "redMG"
        dat[, 1] <- "FOMI-IMBURA"
      }else if (dat[, 1] == "FOMI_BAGARA") {
        fertColCode <- "grey"
        dat[, 1] <- "FOMI-BAGARA"
      }

      if (dat$bag == 0.5) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/half.png)', sep = ""))
      } else {
		dat$rep <- paste0('![](net/', fertColCode, "/", gsub(".", "_", dat$bag), ".png)")
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

      fn <- paste("./temp/datall", j, ".csv", sep = "")
      write.csv(dat, fn, row.names = FALSE)

    }

  }

  if (min(acairm$sum_total, acairm$revenue) == acairm$sum_total) {
    ratioFertCost <- 1
    ratioTotalSale <- round(acairm$totalSalePrice / acairm$sum_total)
    ratioRevenue <- round(acairm$revenue / acairm$sum_total)
  }else {
    ratioRevenue <- 1
    ratioFertCost <- round(acairm$sum_total / acairm$revenue)
    ratioTotalSale <- round(acairm$totalSalePrice / acairm$revenue)
  }

  acairm$revenue <- formatC(acairm$revenue, format = "f", big.mark = ",", digits = 0)
  acairm$totalSalePrice <- formatC(acairm$totalSalePrice, format = "f", big.mark = ",", digits = 0)
  acairm$sum_total <- formatC(acairm$sum_total, format = "f", big.mark = ",", digits = 0)
  # acairm$sum_total <- formatC(signif(acairm$sum_total, digits=3), format="f", big.mark=",", digits=0)

  totalCostmoney <- data.frame(title = paste(acairm$sum_total, acairm$currency))
  totalSalemoney <- data.frame(title = paste(acairm$totalSalePrice, acairm$currency))
  totalRevenuemoney <- data.frame(title = paste(acairm$revenue, acairm$currency))

	totalCostmoney$moneypack <- paste0('![](net/cash/Picture', ratioFertCost, '.png)')
	write.csv(totalCostmoney, "./temp/totalCostmoney.csv", row.names = FALSE)
    totalSalemoney$moneypack <- paste0('![](net/cash/Picture', ratioTotalSale, '.png)')
	write.csv(totalSalemoney, "./temp/totalSalemoney.csv", row.names = FALSE)
    totalRevenuemoney$moneypack <- paste0('![](net/cash/Picture', ratioRevenue, '.png)')
	write.csv(totalRevenuemoney, "./temp/totalRevenuemoney.csv", row.names = FALSE)

  #return(acairm)
}

#'  @param fertilizers: data frame with type, N_cont, P_cont, K_cont, price. Price is per kg of fertilizer
#'  @param NG_CY_Fertdata: data frame with lat,long,fert_N,fert_P,fert_K, water_limited_yield, CurrentYield,location, pl_Date,zone, harvestDay, harvestmonth, daysOnField
#'  @param SoilData: data frame with lat,long,soilN,soilP,soilK,rec_N, rec_P, rec_K, rel_N,rel_P,rel_K
#'  @param rootUP: a price of 1 tonne of cassava in freshwt. It is used as freshwt price, after QUEFTS give drywt root yield (kg/ha) it is converted to freshwt in tonne/ha and this price is then used
#'  @param areaHa is area of land in ha
#'  @return a data frame with ...

## never reached?

getFRrecomMarkdown <- function(lat, lon, PD, HD, maxInv, fertilizers, GPS_fertRecom,
                               area, areaUnits, user, cassPD, userField, cassUP) {

  is.even <- function(x) x %% 2 == 0
  rootConv <- data.frame(cassPD = c("roots", "chips", "flour", "gari"),
                         conversion = c(1, 3, 3.2, 3.5))

  GPS_fertRecom2 <- suppressWarnings(data.frame(lapply(GPS_fertRecom, function(x) as.numeric(as.character(x)))))
  fertilizer <- fertilizers[fertilizers$type %in% colnames(GPS_fertRecom),]
  onlyFert <- subset(GPS_fertRecom, select = -c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
  onlyFert <- tidyr::gather(onlyFert, type, amountKg)
  fertilizer_amount <- merge(fertilizer, onlyFert, by = "type")
  fertilizer_amount$cost <- fertilizer_amount$price * 50
  fertilizer_amount$Nrbags25 <- signif(fertilizer_amount$amountKg / 25, digits = 0) ##
  fertilizer_amount$bags50Kg <- fertilizer_amount$Nrbags25 / 2
  fertilizer_amount$total_cost <- fertilizer_amount$price *
    fertilizer_amount$bags50Kg *
    50

  fertilizer_amount <- fertilizer_amount[, c('type', 'cost', 'amountKg', 'bags50Kg', 'total_cost')]
  FA <- NULL
  for (k in 1:nrow(fertilizer_amount)) {
    fat <- fertilizer_amount[k,]
    colnames(fat) <- paste(c('fertilizer', 'cost', 'kgs', 'bags', 'total_cost'), k, sep = "")
    if (k == 1) {
      FA <- fat
    }else {
      FA <- cbind(FA, fat)
    }
  }

  phone <- paste(user$PhoneCC, user$PhoneNr, sep = "")
  field_area <- paste(area, areaUnits, sep = " ")
  sum_total <- round(sum(fertilizer_amount$total_cost), digits = 0)
  unitcassava <- paste(cassUW, "kg bag of ", cassPD, sep = "")
  bags_totalroot <- round(GPS_fertRecom2$TargetY - GPS_fertRecom2$CurrentY, digits = 0) ## root yield increase in tonnes
  product <- cassPD
  if (cassPD != "roots") {
    bags_totalproduct <- round(bags_totalroot / rootConv[rootConv$cassPD == cassPD,]$conversion, digits = 0)
  }else {
    bags_totalproduct <- bags_totalroot
  }
  totalSalePrice <- round(GPS_fertRecom2$NR + GPS_fertRecom$TC, digits = 0)
  revenue <- round(GPS_fertRecom2$NR, digits = 0)
  current_yield <- paste(round(GPS_fertRecom2$CurrentY, digits = 0), " tonnes per ", field_area, sep = "")


  T_csv <- data.frame(name = user$Name, phone = phone, field = userField, field_area = field_area, 
		unit_field = areaUnits, plant_date = PD, hvst_date = HD, product,
        current_yield, email = user$Email, latitude = lat, longitude = lon,
        costcassava = cassUP, unitcassava = unitcassava, maxinvest = maxInv,
        sum_total, bags_total = bags_totalroot, unit = "Kg", totalSalePrice = totalSalePrice, revenue)

  T_csv <- cbind(T_csv, FA)

  fnc <- "./temp/MarkDownTextD.csv"
  if (file.exists(fnc)) file.remove(fnc)
  write.csv(T_csv, fnc, row.names = FALSE)


  fText <- c()
  for (k in 1:nrow(fertilizer_amount)) {
    fc <- fertilizer_amount[k,]
    text1 <- paste(round(fc$amountKg, digits = 0), ' kg of ', fc$type, sep = "")
    fText <- c(fText, text1)
  }

  recText <- paste("Hello ", T_csv$name, "! If you plant cassava on ", PD, " and harvest on ", HD, ", we recommend you apply ", paste(fText, collapse = ", "),
                   " on your field of ", T_csv$field_area, ". This investment will cost you ", sum_total, " ", T_csv$currency, ". ",
                   "We expect you will increase your cassava", product, "production by ", bags_totalproduct, " and your net returns by ",
                   revenue, " ", T_csv$currency, ". Thank you for using our services!", sep = "")
  return(recText)
}



## process the recom output as Markdown input
FR_MarkdownText <- function(rr, fertilizers, user, country, userField,
			area, areaUnits, PD, HD, lat, lon, rootUP, cassPD, cassUW, maxInv) {
  #
  bags_total = round(rr$rec$TargetY, digits = 1)
  totalSalePrice = rr$rec$TC + rr$rec$NR
  revenue = rr$rec$NR
  current_yield = rr$rec$CurrentY
  sum_total = rr$rec$TC

  currency <- ifelse(country == "NG", "NGN", ifelse(country == "RW", "RWF", ifelse(country == "GH", "GHS", ifelse(country == "BU", "BIF", "TZS"))))

  MarkDownTextD <- data.frame(name = user$Name, country = country, phone = user$PhoneNr, field = userField, 
		field_area = area, unit_field = areaUnits, plant_date = PD, hvst_date = HD, current_yield = current_yield,    email = user$Email, latitude = lat, longitude = lon, userPhoneCC = user$PhoneCC,
        costcassava = rootUP, unitcassava = cassPD, maxinvest = maxInv,
        sum_total = sum_total, bags_total = bags_total, product = cassPD,
        totalSalePrice = totalSalePrice, revenue = revenue, currency = currency, cassUW = cassUW)

  MarkDownTextD$costcassava <- formatC(signif(MarkDownTextD$costcassava, digits = 4), format = "f", big.mark = ",", digits = 0)
  MarkDownTextD$maxinvest <- formatC(signif(MarkDownTextD$maxinvest, digits = 4), format = "f", big.mark = ",", digits = 0)

  filename <- paste("./temp/personalized_info", user$PhoneNr, sep = "_")
  filename <- paste0(filename, ".csv")
  write.csv(MarkDownTextD, filename, row.names = FALSE)

  fertilizers_recom <- fertilizers[fertilizers$type %in% rr$fertilizer_rates$type,]
  if (nrow(fertilizers_recom) > 0) {
    fertilizers_recom <- merge(fertilizers_recom, rr$fertilizer_rates, by = 'type')
    fertilizers_recom$rate <- round(fertilizers_recom$rate, digits = 0)
    fertilizers_recom$bags <- round(fertilizers_recom$rate / fertilizers_recom$bagWeight, digits = 1)
    fertilizers_recom$cost <- round(fertilizers_recom$rate, digits = 0) * fertilizers_recom$price
    #fertilizers_recom$cost <- (fertilizers_recom$rate *  fertilizers_recom$costPerBag) / fertilizers_recom$bagWeight
    Bagsfull <- trunc(fertilizers_recom$bags)
    bagshalf <- fertilizers_recom$bags - floor(fertilizers_recom$bags)
    bagshalf <- ifelse(bagshalf >= 0.25 & bagshalf <= 0.75, 0.5, ifelse(bagshalf < 0.25, 0, 1))
    fertilizers_recom$bags <- Bagsfull + bagshalf

    sum_total <- round(sum(fertilizers_recom$cost), digits = 0)
    MarkDownTextD$sum_total <- sum_total
    MarkDownTextD$revenue <- MarkDownTextD$totalSalePrice - sum_total

    write.csv(MarkDownTextD, filename, row.names = FALSE)


    ff <- NULL
    for (j in 1:nrow(fertilizers_recom)) {
      dd <- data.frame(fertilizer = fertilizers_recom$type[j],
                       cost = fertilizers_recom$price[j],
                       costPerBag = fertilizers_recom$costPerBag[j],
                       unit = paste(fertilizers_recom$bagWeight[j], "kg bag", sep = ''),
                       kgs = fertilizers_recom$rate[j],
                       rep = NA,
                       bags = fertilizers_recom$bags[j],
                       total_cost = fertilizers_recom$cost[j])
      names(dd) <- paste(names(dd), j, sep = "")
      if (j == 1) {
        ff <- dd
      }else {
        ff <- cbind(ff, dd)
      }
    }
    MarkDownTextD <- cbind(MarkDownTextD, ff)
    write.csv(MarkDownTextD, "./temp/FR_MarkDownText.csv", row.names = FALSE)
  }
}


IC_MarkdownText <- function(rr, fertilizers, user, country, userField,
          area, areaUnits, PD, HD, lat, lon, rootUP, cassPD, maxInv, CMP,
          maizeUW, maizePD, cassUW, maizeUP, nameSF, saleSF, riskAtt) {


  current_yield = rr$rec$dMP ## this is increase in maize yield
  totalSalePrice = rr$rec$dTC + rr$rec$dNR
  revenue = rr$rec$dNR
  sum_total = rr$rec$dTC
  currency <- ifelse(country == "NG", "NGN", ifelse(country == "RW", "RWF", ifelse(country == "GH", "GHS", "TZS")))
  dMP <- rr$rec$dMP


  currency <- ifelse(country == "NG", "NGN", ifelse(country == "RW", "RWF", ifelse(country == "GH", "GHS", "TZS")))

  message(paste("Processing IC_MarkdownText  with risk attitutde", riskAtt))
  MarkDownTextD <- data.frame(name = user$Name, country = country, phone = user$PhoneNr, 
		field = userField, field_area = area,
        unit_field = areaUnits, plant_date = PD, hvst_date = HD, userPhoneCC = user$PhoneCC,
        email = user$Email, latitude = lat, longitude = lon, product = cassPD, costcassava = rootUP, 
		unitcassava = cassPD, maxinvest = maxInv,
        currency = currency, maizeUP = maizeUP, maizeUW = maizeUW, maizePD = maizePD,
        sum_total = sum_total, cassUW = cassUW, totalSalePrice = totalSalePrice, revenue = revenue, dMP = dMP,
        saleSF = saleSF, nameSF = nameSF, CMP = CMP, riskAtt = riskAtt
  )

  MarkDownTextD$maxinvest <- as.numeric(as.character(MarkDownTextD$maxinvest))
  MarkDownTextD$costcassava <- formatC(signif(MarkDownTextD$costcassava, digits = 4), format = "f", big.mark = ",", digits = 0)
  MarkDownTextD$maxinvest <- formatC(signif(MarkDownTextD$maxinvest, digits = 4), format = "f", big.mark = ",", digits = 0)

  if (CMP == 1) {
    MarkDownTextD$CMP <- "About Knee height (~50 cm)"
  }else if (CMP == 2) {
    MarkDownTextD$CMP <- "About chest height (~150 cm)"
  }else if (CMP == 3) {
    MarkDownTextD$CMP <- "Larger than a person with yellowish leaves (~200 cm)"
  }else if (CMP == 4) {
    MarkDownTextD$CMP <- "Larger than a person with green leaves (~200 cm)"
  }else if (CMP == 5) {
    MarkDownTextD$CMP <- "Larger than a person with dark green leaves (~200 cm)"
  }

  if (MarkDownTextD$maizePD == "fresh_cob") {
    MarkDownTextD$unitproduct <- paste(MarkDownTextD$currency, " ", MarkDownTextD$maizeUP, " per ",
                                       MarkDownTextD$maizePD, ".", sep = "")
  }else {
    MarkDownTextD$unitproduct <- paste(MarkDownTextD$currency, " ", MarkDownTextD$maizeUP, " per ", MarkDownTextD$maizeUW,
                                       " kg of grain.", sep = "")
  }

  filename <- paste("./temp/personalized_info", user$PhoneNr, sep = "_")
  filename <- paste0(filename, ".csv")
  write.csv(MarkDownTextD, filename, row.names = FALSE)

  fertilizers_recom <- fertilizers[fertilizers$type %in% rr$fertilizer_rates$type,]
  if (nrow(fertilizers_recom) > 0) {
    fertilizers_recom <- merge(fertilizers_recom, rr$fertilizer_rates, by = 'type')
    fertilizers_recom$rate <- round(fertilizers_recom$rate, digits = 0)
    fertilizers_recom$cost <- round(fertilizers_recom$rate, digits = 0) * fertilizers_recom$price
    fertilizers_recom$bags <- round(fertilizers_recom$rate / fertilizers_recom$bagWeight, digits = 1)
    Bagsfull <- trunc(fertilizers_recom$bags)
    bagshalf <- fertilizers_recom$bags - floor(fertilizers_recom$bags)
    bagshalf <- ifelse(bagshalf >= 0.25 & bagshalf <= 0.75, 0.5, ifelse(bagshalf < 0.25, 0, 1))
    fertilizers_recom$bags <- Bagsfull + bagshalf


    MarkDownTextD$sum_total <- sum(fertilizers_recom$cost)
    MarkDownTextD$revenue = MarkDownTextD$totalSalePrice - MarkDownTextD$sum_total
    write.csv(MarkDownTextD, filename, row.names = FALSE)


    ff <- NULL
    for (j in 1:nrow(fertilizers_recom)) {
      dd <- data.frame(fertilizer = fertilizers_recom$type[j],
                       cost = fertilizers_recom$price[j],
                       costPerBag = fertilizers_recom$costPerBag[j],
                       unit = paste(fertilizers_recom$bagWeight[j], "kg bag", sep = ''),
                       kgs = fertilizers_recom$rate[j],
                       rep = NA,
                       bags = fertilizers_recom$bags[j],
                       total_cost = round(fertilizers_recom$rate[j], digits = 0) * fertilizers_recom$price[j])
      names(dd) <- paste(names(dd), j, sep = "")
      if (j == 1) {
        ff <- dd
      }else {
        ff <- cbind(ff, dd)
      }
    }

    MarkDownTextD <- cbind(MarkDownTextD, ff, rr$rec)
    write.csv(MarkDownTextD, "./temp/IC_MarkDownText.csv", row.names = FALSE)
  }
}


CIS_MarkdownText <- function(rr, fertilizers, user, country, userField, area, areaUnits,
                             PD, HD, lat, lon, rootUP, cassPD, cassUW, maxInv,
                             sweetPotatoUP, sweetPotatoPD, tuberUP, sweetPotatoUW) {

  #current_yield = rr$rec$dMP ## this is increase in maize yield
  totalSalePrice = rr$rec$dTC + rr$rec$dNR
  revenue = rr$rec$dNR
  sum_total = rr$rec$dTC
  currency <- ifelse(country == "NG", "NGN", ifelse(country == "RW", "RWF", ifelse(country == "GH", "GHS", "TZS")))
  #dMP <- rr$rec$dMP

  currency <- ifelse(country == "NG", "NGN", ifelse(country == "RW", "RWF", ifelse(country == "GH", "GHS", "TZS")))

  MarkDownTextD <- data.frame(name = user$Name, country = country, phone = user$PhoneNr, 
		field = userField, field_area = area,
		unit_field = areaUnits, plant_date = PD, hvst_date = HD, userPhoneCC = user$PhoneCC,
        email = user$Email, latitude = lat, longitude = lon, product = cassPD,
        costcassava = rootUP, unitcassava = cassPD, maxinvest = maxInv, currency = currency, sum_total = sum_total,
        product = cassPD, totalSalePrice = totalSalePrice, revenue = revenue, currency = currency, cassUW = cassUW,
        sweetPotatoUW = sweetPotatoUW, sweetPotatoUP = sweetPotatoUP, sweetPotatoPD = sweetPotatoPD, tuberUP = tuberUP)


  MarkDownTextD$costcassava <- formatC(signif(MarkDownTextD$costcassava, digits = 4), format = "f", big.mark = ",", digits = 0)
  MarkDownTextD$maxinvest <- formatC(signif(MarkDownTextD$maxinvest, digits = 4), format = "f", big.mark = ",", digits = 0)


  filename <- paste("./temp/personalized_info", user$PhoneNr, sep = "_")
  filename <- paste0(filename, ".csv")
  write.csv(MarkDownTextD, filename, row.names = FALSE)

  fertilizers_recom <- fertilizers[fertilizers$type %in% rr$fertilizer_rates$type,]
  if (nrow(fertilizers_recom) > 0) {
    fertilizers_recom <- merge(fertilizers_recom, rr$fertilizer_rates, by = 'type')
    fertilizers_recom$rate <- round(fertilizers_recom$rate, digits = 0)
    fertilizers_recom$cost <- round(fertilizers_recom$rate, digits = 0) * fertilizers_recom$price
    fertilizers_recom$bags <- round(fertilizers_recom$rate / fertilizers_recom$bagWeight, digits = 1)
    Bagsfull <- trunc(fertilizers_recom$bags)
    bagshalf <- fertilizers_recom$bags - floor(fertilizers_recom$bags)
    bagshalf <- ifelse(bagshalf >= 0.3 & bagshalf <= 0.65, 0.5, ifelse(bagshalf < 0.3, 0, 1))
    fertilizers_recom$bags <- Bagsfull + bagshalf

    MarkDownTextD$sum_total <- sum(fertilizers_recom$cost)
    MarkDownTextD$revenue = MarkDownTextD$totalSalePrice - MarkDownTextD$sum_total
    write.csv(MarkDownTextD, filename, row.names = FALSE)


    ff <- NULL
    for (j in 1:nrow(fertilizers_recom)) {
      dd <- data.frame(fertilizer = fertilizers_recom$type[j],
                       cost = fertilizers_recom$price[j],
                       costPerBag = fertilizers_recom$costPerBag[j],
                       unit = paste(fertilizers_recom$bagWeight[j], "kg bag", sep = ''),
                       kgs = round(fertilizers_recom$rate[j], digits = 0),
                       rep = NA,
                       bags = fertilizers_recom$bags[j],
                       total_cost = round(fertilizers_recom$rate[j], digits = 0) * fertilizers_recom$price[j])


      names(dd) <- paste(names(dd), j, sep = "")
      if (j == 1) {
        ff <- dd
      }else {
        ff <- cbind(ff, dd)
      }
    }

    MarkDownTextD <- cbind(MarkDownTextD, ff, rr$rec)
    write.csv(MarkDownTextD, "./temp/CIS_MarkDownText.csv", row.names = FALSE)
  }
}


PPSP_MarkdownText <- function(rr, fname, user, country, userField, area, areaUnits,
                              PD, HD, lat, lon, rootUP, cassPD, cassUW, maxInv) {

  currency <- ifelse(country == "NG", "NGN", ifelse(country == "RW", "RWF", ifelse(country == "GH", "GHS", "TZS")))

  MarkDownTextD <- data.frame(name = user$Name, country = country, phone = user$PhoneNr, 
		field = userField, field_area = area, unit_field = areaUnits, plant_date = PD, hvst_date = HD,
        email = user$Email, latitude = lat, longitude = lon, costcassava = rootUP, unitcassava = cassPD,
        maxinvest = maxInv, cassUW = cassUW, product = cassPD, currency = currency)

  filename <- paste("./temp/personalized_info", user$PhoneNr, sep = "_")
  filename <- paste0(filename, ".csv")
  write.csv(MarkDownTextD, filename, row.names = FALSE)

  fname2 <- paste(fname, "_MarkDownText.csv", sep = '')
  write.csv(MarkDownTextD, "PP_MarkDownText.csv", row.names = FALSE)
}


PP_MarkdownText <- function(user, country, userField, area, areaUnits, PD, HD, lat, lon, rootUP,
		cassPD, cassUW, maxInv, ploughing, ridging, method_ploughing, method_ridging) {

  MarkDownTextD <- data.frame(
		name = user$Name, country = country, phone = user$PhoneNr, field = userField, field_area = area,
        unit_field = areaUnits, plant_date = PD, hvst_date = HD,
        email = user$Email, latitude = lat, longitude = lon,
        costcassava = rootUP, unitcassava = cassPD, cassUW = cassUW,
        maxinvest = maxInv, product = cassPD, ploughing = ploughing, ridging = ridging,
        method_ploughing = method_ploughing, method_ridging = method_ridging, userPhoneCC = user$PhoneCC)


	write.csv(MarkDownTextD, "./temp/PP_MarkDownText.csv", row.names = FALSE)
}


### create a data frame from user info which iwll be used within teh markdown file to create the file for the pdf document to be shared via email.
SP_MarkdownText <- function(user, country, userField, area, areaUnits, PD, HD, lat, lon, saleSF, nameSF,
                            maxInv, ploughing, ridging, method_ploughing, method_ridging, CMP, riskAtt,
                            PD_window, HD_window, cassPD, cassUW, cassUP, cassUP_m1, cassUP_m2, cassUP_p1, cassUP_p2) {

  MarkDownTextD <- data.frame(name = user$Name, country = country, phone = user$PhoneNr, 
		field = userField, field_area = area,
        unit_field = areaUnits, plant_date = PD, hvst_date = HD,
        email = user$Email, latitude = lat, longitude = lon,
        maxinvest = maxInv, saleSF = saleSF, nameSF = nameSF, CMP = CMP, riskAtt = riskAtt, PD, HD,
        PD_window = PD_window,
        HD_window = HD_window, cassPD = cassPD,
        cassUW = cassUW, cassUP = cassUP, cassUP_m1 = cassUP_m1, cassUP_m2 = cassUP_m2,
        cassUP_p1 = cassUP_p1, cassUP_p2 = cassUP_p2, userPhoneCC = user$PhoneCC)
		
  write.csv(MarkDownTextD, "./temp/SP_MarkDownText.csv", row.names = FALSE)
}
