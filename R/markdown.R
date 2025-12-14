
#####################################################################################################################
## R markdown
#####################################################################################################################



#'function to put data used in the markdown .rmd. It selects the number of bags of fertilizerm the color, the money stubs for cost, sale and profit
#'It reads the FR_MarkdownText, SP_MarkdownText, IC_MarkdownText, BPP_MarkdownText saved output which brings together the user info and based on
#' the recommendation the fertilizer types, amount, bag colur, cost and profit
fertilizerAdviseTable <- function(FR, IC, country, areaUnits) {


  suppressWarnings(if (file.exists("datall1.csv")) file.remove("datall1.csv"))
  suppressWarnings(if (file.exists("datall2.csv")) file.remove("datall2.csv"))
  suppressWarnings(if (file.exists("datall3.csv")) file.remove("datall3.csv"))
  suppressWarnings(if (file.exists("datall4.csv")) file.remove("datall4.csv"))
  suppressWarnings(if (file.exists("datall5.csv")) file.remove("datall5.csv"))
  suppressWarnings(if (file.exists("datall6.csv")) file.remove("datall6.csv"))


  if (FR == TRUE & IC == FALSE) {
    acairm <- read.csv("./temp/FR_MarkDownText.csv")
  }else if (IC == TRUE & FR == FALSE) {
    if (country == "TZ") {
      acairm <- read.csv("./temp/CIS_MarkDownText.csv")
    }else if (country == "NG") {
      acairm <- read.csv("./temp/IC_MarkDownText.csv")
    }
  }else {
    return("FR and IC can not be true together")
  }


  acairm$currency <- ifelse(acairm$country == "NG", "NGN",
                            ifelse(acairm$country == "TZ", "TZS", ifelse(acairm$country == "BU", "BIF", "GHS")))

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
      }else if (dat$bag == 1) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/1.png)', sep = ""))
      }else if (dat$bag == 1.5) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/1_5.png)', sep = ""))
      }else if (dat$bag == 2) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/2.png)', sep = ""))
      }else if (dat$bag == 2.5) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/2_5.png)', sep = ""))
      }else if (dat$bag == 3) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/3.png)', sep = ""))
      }else if (dat$bag == 3.5) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/3_5.png)', sep = ""))
      }else if (dat$bag == 4) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/4.png)', sep = ""))
      }else if (dat$bag == 4.5) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/4_5.png)', sep = ""))
      }else if (dat$bag == 5) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/5.png)', sep = ""))
      }else if (dat$bag == 5.5) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/5_5.png)', sep = ""))
      }else if (dat$bag == 6) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/6.png)', sep = ""))
      }else if (dat$bag == 6.5) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/6_5.png)', sep = ""))
      }else if (dat$bag == 7) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/7.png)', sep = ""))
      }else if (dat$bag == 7.5) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/7_5.png)', sep = ""))
      }else if (dat$bag == 8) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/8.png)', sep = ""))
      }else if (dat$bag == 8.5) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/8_5.png)', sep = ""))
      }else if (dat$bag == 9) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/9.png)', sep = ""))
      }else if (dat$bag == 9.5) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/9_5.png)', sep = ""))
      }else if (dat$bag == 10) {
        dat$rep <- sprintf(paste('![](net/', fertColCode, '/10.png)', sep = ""))
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
    ratioTotalSale <- round(acairm$totalSalePrice / acairm$sum_total, digits = 0)
    ratioRevenue <- round(acairm$revenue / acairm$sum_total, digits = 0)
  }else {
    ratioRevenue <- 1
    ratioFertCost <- round(acairm$sum_total / acairm$revenue, digits = 0)
    ratioTotalSale <- round(acairm$totalSalePrice / acairm$revenue, digits = 0)

  }

  acairm$revenue <- formatC(acairm$revenue, format = "f", big.mark = ",", digits = 0)
  acairm$totalSalePrice <- formatC(acairm$totalSalePrice, format = "f", big.mark = ",", digits = 0)
  acairm$sum_total <- formatC(acairm$sum_total, format = "f", big.mark = ",", digits = 0)
  # acairm$sum_total <- formatC(signif(acairm$sum_total, digits=3), format="f", big.mark=",", digits=0)


  totalCostmoney <- data.frame(title = paste(acairm$sum_total, acairm$currency, sep = " "))
  totalSalemoney <- data.frame(title = paste(acairm$totalSalePrice, acairm$currency, sep = " "))
  totalRevenuemoney <- data.frame(title = paste(acairm$revenue, acairm$currency, sep = " "))

  if (ratioFertCost == 1) {
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture1.png)')
  } else if (ratioFertCost == 2) {
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture2.png)')
  }else if (ratioFertCost == 3) {
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture3.png)')
  }else if (ratioFertCost == 4) {
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture4.png)')
  }else if (ratioFertCost == 5) {
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture5.png)')
  }else if (ratioFertCost == 6) {
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture6.png)')
  }else if (ratioFertCost == 7) {
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture7.png)')
  }else if (ratioFertCost == 8) {
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture8.png)')
  }else if (ratioFertCost == 9) {
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture9.png)')
  }else if (ratioFertCost == 10) {
    totalCostmoney$moneypack <- sprintf('![](net/cash/Picture10.png)')
  }
  write.csv(totalCostmoney, "./temp/totalCostmoney.csv", row.names = FALSE)

  if (ratioTotalSale == 1) {
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture1.png)')
  } else if (ratioTotalSale == 2) {
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture2.png)')
  }else if (ratioTotalSale == 3) {
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture3.png)')
  }else if (ratioTotalSale == 4) {
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture4.png)')
  }else if (ratioTotalSale == 5) {
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture5.png)')
  }else if (ratioTotalSale == 6) {
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture6.png)')
  }else if (ratioTotalSale == 7) {
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture7.png)')
  }else if (ratioTotalSale == 8) {
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture8.png)')
  }else if (ratioTotalSale == 9) {
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture9.png)')
  }else if (ratioTotalSale == 10) {
    totalSalemoney$moneypack <- sprintf('![](net/cash/Picture10.png)')
  }
  write.csv(totalSalemoney, "./temp/totalSalemoney.csv", row.names = FALSE)


  if (ratioRevenue == 1) {
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture1.png)')
  } else if (ratioRevenue == 2) {
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture2.png)')
  }else if (ratioRevenue == 3) {
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture3.png)')
  }else if (ratioRevenue == 4) {
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture4.png)')
  }else if (ratioRevenue == 5) {
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture5.png)')
  }else if (ratioRevenue == 6) {
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture6.png)')
  }else if (ratioRevenue == 7) {
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture7.png)')
  }else if (ratioRevenue == 8) {
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture8.png)')
  }else if (ratioRevenue == 9) {
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture9.png)')
  }else if (ratioRevenue == 10) {
    totalRevenuemoney$moneypack <- sprintf('![](net/cash/Picture10.png)')
  }
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


PP_MarkdownText <- function(user, country, userField, area, areaUnits, PD, HD, lat, lon, rootUP, cassPD, cassUW,
                            maxInv, ploughing, ridging, method_ploughing, method_ridging) {

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

  fertilizer_rates <- data.frame(type = fertilizers$type, rate = FR) #fertilizer rates to apply
  fertilizer_rates <- fertilizer_rates[fertilizer_rates$rate > 0,]

  return(list(rec = rec, fertilizer_rates = fertilizer_rates))

}

