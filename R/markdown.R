
### R markdown

# Sanitise a user-supplied value before embedding it in a file path.
# Keeps only digits, letters, hyphens and plus signs (covers phone numbers
# like +234789123456) and strips any path traversal characters.
safe_filename_part <- function(x) {
    gsub("[^A-Za-z0-9+\\-]", "", as.character(x))
}

# ---------------------------------------------------------------------------
# Fertilizer display: bag colour and label, keyed by internal type name
# ---------------------------------------------------------------------------
FERT_COLOUR <- c(
    Urea          = "green",
    NPK15_15_15   = "blue",
    NPK20_10_10   = "yellow",
    NPK17_17_17   = "purple",
    NPK20_12_16   = "royal",
    NPK152020     = "orange",
    FOMI_TOTAHAZA = "red",
    FOMI_IMBURA   = "redMG",
    FOMI_BAGARA   = "grey"
)

FERT_LABEL <- c(
    Urea          = "Urea",
    NPK15_15_15   = "NPK15:15:15",
    NPK20_10_10   = "NPK20:10:10",
    NPK17_17_17   = "NPK17:17:17",
    NPK20_12_16   = "NPK20:12:16+2Mg",
    NPK152020     = "NPK15:20:20",
    FOMI_TOTAHAZA = "FOMI-TOTAHAZA",
    FOMI_IMBURA   = "FOMI-IMBURA",
    FOMI_BAGARA   = "FOMI-BAGARA"
)

# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------

# Round raw bag count to nearest whole or half bag.
round_bags <- function(raw, half_lo = 0.25, half_hi = 0.75) {
    full <- trunc(raw)
    frac <- raw - floor(raw)
    half <- ifelse(frac >= half_lo & frac <= half_hi, 0.5,
                   ifelse(frac < half_lo, 0, 1))
    full + half
}

# Merge fertilizer list with recommendation rates, compute cost and bags.
calc_fertilizer_recom <- function(fertilizers, rr, half_lo = 0.25, half_hi = 0.75) {
    fr <- fertilizers[fertilizers$type %in% rr$fertilizer_rates$type, ]
    if (nrow(fr) == 0) return(fr)
    fr <- merge(fr, rr$fertilizer_rates, by = "type")
    fr$rate      <- round(fr$rate, digits = 0)
    fr$cost      <- fr$rate * fr$price
    fr$bags      <- round_bags(fr$rate / fr$bagWeight, half_lo, half_hi)
    fr
}

# Pivot fertilizer rows into wide columns (fertilizer1, bags1, cost1, …).
pivot_fertilizers_wide <- function(fr) {
    ff <- NULL
    for (j in seq_len(nrow(fr))) {
        dd <- data.frame(
            fertilizer  = fr$type[j],
            cost        = fr$price[j],
            costPerBag  = fr$costPerBag[j],
            unit        = paste0(fr$bagWeight[j], "kg bag"),
            kgs         = fr$rate[j],
            rep         = NA,
            bags        = fr$bags[j],
            total_cost  = fr$cost[j]
        )
        names(dd) <- paste0(names(dd), j)
        ff <- if (is.null(ff)) dd else cbind(ff, dd)
    }
    ff
}


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
    acairm$currency <- get_currency(acairm$country)
    acairm
}


#' Builds datall*.csv files consumed by Rmd templates.
fertilizerAdviseTable <- function(FR, IC, country, areaUnits) {

    suppressWarnings(file.remove(paste0("./temp/datall", 1:6, ".csv")))

    acairm <- get_markdown_text(FR, IC, country)

    Nrfert <- length(grep("fertilizer", colnames(acairm)))
    if (Nrfert > 0) {
        for (j in 1:Nrfert) {
            colNames <- c(paste(c("fertilizer", "bags", "cost", "total_cost", "kgs", "unit", "costPerBag"), j, sep = ""),
                          "currency", "field_area", "unit_field")
            dat      <- acairm[, colNames]
            dat$bag  <- dat[, paste0("bags", j)]

            fert_type    <- dat[, 1]
            fertColCode  <- FERT_COLOUR[[fert_type]]
            if (is.null(fertColCode)) fertColCode <- "green"
            if (!is.null(FERT_LABEL[[fert_type]])) dat[, 1] <- FERT_LABEL[[fert_type]]

            dat$rep <- if (dat$bag == 0.5) {
                sprintf("![](net/%s/half.png)", fertColCode)
            } else {
                paste0("![](net/", fertColCode, "/", gsub(".", "_", dat$bag, fixed = TRUE), ".png)")
            }

            write.csv(dat, paste0("./temp/datall", j, ".csv"), row.names = FALSE)
        }
    }

    if (min(acairm$sum_total, acairm$revenue) == acairm$sum_total) {
        ratioFertCost  <- 1
        ratioTotalSale <- round(acairm$totalSalePrice / acairm$sum_total)
        ratioRevenue   <- round(acairm$revenue        / acairm$sum_total)
    } else {
        ratioRevenue   <- 1
        ratioFertCost  <- round(acairm$sum_total      / acairm$revenue)
        ratioTotalSale <- round(acairm$totalSalePrice / acairm$revenue)
    }

    acairm$revenue       <- formatC(acairm$revenue,       format = "f", big.mark = ",", digits = 0)
    acairm$totalSalePrice <- formatC(acairm$totalSalePrice, format = "f", big.mark = ",", digits = 0)
    acairm$sum_total     <- formatC(acairm$sum_total,     format = "f", big.mark = ",", digits = 0)

    totalCostmoney    <- data.frame(title = paste(acairm$sum_total,      acairm$currency))
    totalSalemoney    <- data.frame(title = paste(acairm$totalSalePrice,  acairm$currency))
    totalRevenuemoney <- data.frame(title = paste(acairm$revenue,         acairm$currency))

    totalCostmoney$moneypack    <- paste0("![](net/cash/Picture", ratioFertCost,  ".png)")
    totalSalemoney$moneypack    <- paste0("![](net/cash/Picture", ratioTotalSale, ".png)")
    totalRevenuemoney$moneypack <- paste0("![](net/cash/Picture", ratioRevenue,   ".png)")

    write.csv(totalCostmoney,    "./temp/totalCostmoney.csv",    row.names = FALSE)
    write.csv(totalSalemoney,    "./temp/totalSalemoney.csv",    row.names = FALSE)
    write.csv(totalRevenuemoney, "./temp/totalRevenuemoney.csv", row.names = FALSE)
}


## process the recom output as Markdown input
FR_MarkdownText <- function(rr, fertilizers, user, country, userField,
            area, areaUnits, PD, HD, lat, lon, rootUP, cassPD, cassUW, maxInv) {

    bags_total     <- round(rr$data$TargetY, digits = 1)
    totalSalePrice <- rr$data$TC + rr$data$NR
    revenue        <- rr$data$NR
    current_yield  <- rr$data$CurrentY
    sum_total      <- rr$data$TC
    currency       <- get_currency(country)

    MarkDownTextD <- data.frame(
        name = user$Name, country = country, phone = user$PhoneNr,
        field = userField, field_area = area, unit_field = areaUnits,
        plant_date = PD, hvst_date = HD, current_yield = current_yield,
        email = user$Email, latitude = lat, longitude = lon,
        userPhoneCC = user$PhoneCC, costcassava = rootUP, unitcassava = cassPD,
        maxinvest = maxInv, sum_total = sum_total, bags_total = bags_total,
        product = cassPD, totalSalePrice = totalSalePrice, revenue = revenue,
        currency = currency, cassUW = cassUW
    )
    MarkDownTextD$costcassava <- formatC(signif(MarkDownTextD$costcassava, digits = 4), format = "f", big.mark = ",", digits = 0)
    MarkDownTextD$maxinvest   <- formatC(signif(MarkDownTextD$maxinvest,   digits = 4), format = "f", big.mark = ",", digits = 0)

    filename <- file.path("temp", paste0("personalized_info_", safe_filename_part(user$PhoneNr), ".csv"))
    write.csv(MarkDownTextD, filename, row.names = FALSE)

    fr <- calc_fertilizer_recom(fertilizers, rr)
    if (nrow(fr) > 0) {
        MarkDownTextD$sum_total <- round(sum(fr$cost), digits = 0)
        MarkDownTextD$revenue   <- MarkDownTextD$totalSalePrice - MarkDownTextD$sum_total
        write.csv(MarkDownTextD, filename, row.names = FALSE)

        MarkDownTextD <- cbind(MarkDownTextD, pivot_fertilizers_wide(fr))
        write.csv(MarkDownTextD, "./temp/FR_MarkDownText.csv", row.names = FALSE)
    }
}


IC_MarkdownText <- function(rr, fertilizers, user, country, userField,
          area, areaUnits, PD, HD, lat, lon, rootUP, cassPD, maxInv, CMP,
          maizeUW, maizePD, cassUW, maizeUP, nameSF, saleSF, riskAtt) {

    current_yield  <- rr$data$dMP
    totalSalePrice <- rr$data$dTC + rr$data$dNR
    revenue        <- rr$data$dNR
    sum_total      <- rr$data$dTC
    currency       <- get_currency(country)
    dMP            <- rr$data$dMP

    message(paste("Processing IC_MarkdownText with risk attitude", riskAtt))

    MarkDownTextD <- data.frame(
        name = user$Name, country = country, phone = user$PhoneNr,
        field = userField, field_area = area, unit_field = areaUnits,
        plant_date = PD, hvst_date = HD, userPhoneCC = user$PhoneCC,
        email = user$Email, latitude = lat, longitude = lon,
        product = cassPD, costcassava = rootUP, unitcassava = cassPD,
        maxinvest = maxInv, currency = currency, maizeUP = maizeUP,
        maizeUW = maizeUW, maizePD = maizePD, sum_total = sum_total,
        cassUW = cassUW, totalSalePrice = totalSalePrice,
        revenue = revenue, dMP = dMP, saleSF = saleSF, nameSF = nameSF,
        CMP = CMP, riskAtt = riskAtt
    )

    MarkDownTextD$maxinvest   <- as.numeric(as.character(MarkDownTextD$maxinvest))
    MarkDownTextD$costcassava <- formatC(signif(MarkDownTextD$costcassava, digits = 4), format = "f", big.mark = ",", digits = 0)
    MarkDownTextD$maxinvest   <- formatC(signif(MarkDownTextD$maxinvest,   digits = 4), format = "f", big.mark = ",", digits = 0)

    cmp_labels <- c(
        "1" = "About Knee height (~50 cm)",
        "2" = "About chest height (~150 cm)",
        "3" = "Larger than a person with yellowish leaves (~200 cm)",
        "4" = "Larger than a person with green leaves (~200 cm)",
        "5" = "Larger than a person with dark green leaves (~200 cm)"
    )
    if (!is.null(cmp_labels[[as.character(CMP)]])) {
        MarkDownTextD$CMP <- cmp_labels[[as.character(CMP)]]
    }

    MarkDownTextD$unitproduct <- if (MarkDownTextD$maizePD == "fresh_cob") {
        paste0(MarkDownTextD$currency, " ", MarkDownTextD$maizeUP, " per ", MarkDownTextD$maizePD, ".")
    } else {
        paste0(MarkDownTextD$currency, " ", MarkDownTextD$maizeUP, " per ", MarkDownTextD$maizeUW, " kg of grain.")
    }

    filename <- file.path("temp", paste0("personalized_info_", safe_filename_part(user$PhoneNr), ".csv"))
    write.csv(MarkDownTextD, filename, row.names = FALSE)

    fr <- calc_fertilizer_recom(fertilizers, rr)
    if (nrow(fr) > 0) {
        MarkDownTextD$sum_total <- sum(fr$cost)
        MarkDownTextD$revenue   <- MarkDownTextD$totalSalePrice - MarkDownTextD$sum_total
        write.csv(MarkDownTextD, filename, row.names = FALSE)

        MarkDownTextD <- cbind(MarkDownTextD, pivot_fertilizers_wide(fr), rr$data)
        write.csv(MarkDownTextD, "./temp/IC_MarkDownText.csv", row.names = FALSE)
    }
}


CIS_MarkdownText <- function(rr, fertilizers, user, country, userField, area, areaUnits,
                             PD, HD, lat, lon, rootUP, cassPD, cassUW, maxInv,
                             sweetPotatoUP, sweetPotatoPD, tuberUP, sweetPotatoUW) {

    totalSalePrice <- rr$data$dTC + rr$data$dNR
    revenue        <- rr$data$dNR
    sum_total      <- rr$data$dTC
    currency       <- get_currency(country)

    MarkDownTextD <- data.frame(
        name = user$Name, country = country, phone = user$PhoneNr,
        field = userField, field_area = area, unit_field = areaUnits,
        plant_date = PD, hvst_date = HD, userPhoneCC = user$PhoneCC,
        email = user$Email, latitude = lat, longitude = lon,
        product = cassPD, costcassava = rootUP, unitcassava = cassPD,
        maxinvest = maxInv, currency = currency, sum_total = sum_total,
        totalSalePrice = totalSalePrice, revenue = revenue, cassUW = cassUW,
        sweetPotatoUW = sweetPotatoUW, sweetPotatoUP = sweetPotatoUP,
        sweetPotatoPD = sweetPotatoPD, tuberUP = tuberUP
    )

    MarkDownTextD$costcassava <- formatC(signif(MarkDownTextD$costcassava, digits = 4), format = "f", big.mark = ",", digits = 0)
    MarkDownTextD$maxinvest   <- formatC(signif(MarkDownTextD$maxinvest,   digits = 4), format = "f", big.mark = ",", digits = 0)

    filename <- file.path("temp", paste0("personalized_info_", safe_filename_part(user$PhoneNr), ".csv"))
    write.csv(MarkDownTextD, filename, row.names = FALSE)

    # CIS uses slightly wider half-bag band (0.3–0.65)
    fr <- calc_fertilizer_recom(fertilizers, rr, half_lo = 0.3, half_hi = 0.65)
    if (nrow(fr) > 0) {
        MarkDownTextD$sum_total <- sum(fr$cost)
        MarkDownTextD$revenue   <- MarkDownTextD$totalSalePrice - MarkDownTextD$sum_total
        write.csv(MarkDownTextD, filename, row.names = FALSE)

        MarkDownTextD <- cbind(MarkDownTextD, pivot_fertilizers_wide(fr), rr$data)
        write.csv(MarkDownTextD, "./temp/CIS_MarkDownText.csv", row.names = FALSE)
    }
}


PPSP_MarkdownText <- function(rr, fname, user, country, userField, area, areaUnits,
                              PD, HD, lat, lon, rootUP, cassPD, cassUW, maxInv) {
    currency <- get_currency(country)
    MarkDownTextD <- data.frame(
        name = user$Name, country = country, phone = user$PhoneNr,
        field = userField, field_area = area, unit_field = areaUnits,
        plant_date = PD, hvst_date = HD, email = user$Email,
        latitude = lat, longitude = lon, costcassava = rootUP,
        unitcassava = cassPD, maxinvest = maxInv, cassUW = cassUW,
        product = cassPD, currency = currency
    )
    filename <- file.path("temp", paste0("personalized_info_", safe_filename_part(user$PhoneNr), ".csv"))
    write.csv(MarkDownTextD, filename, row.names = FALSE)
    write.csv(MarkDownTextD, "PP_MarkDownText.csv", row.names = FALSE)
}


PP_MarkdownText <- function(user, country, userField, area, areaUnits, PD, HD, lat, lon, rootUP,
        cassPD, cassUW, maxInv, ploughing, ridging, method_ploughing, method_ridging) {
    MarkDownTextD <- data.frame(
        name = user$Name, country = country, phone = user$PhoneNr,
        field = userField, field_area = area, unit_field = areaUnits,
        plant_date = PD, hvst_date = HD, email = user$Email,
        latitude = lat, longitude = lon, costcassava = rootUP,
        unitcassava = cassPD, cassUW = cassUW, maxinvest = maxInv,
        product = cassPD, ploughing = ploughing, ridging = ridging,
        method_ploughing = method_ploughing, method_ridging = method_ridging,
        userPhoneCC = user$PhoneCC
    )
    write.csv(MarkDownTextD, "./temp/PP_MarkDownText.csv", row.names = FALSE)
}


### create a data frame from user info which will be used within the markdown file
SP_MarkdownText <- function(user, country, userField, area, areaUnits, PD, HD, lat, lon, saleSF, nameSF,
                            maxInv, ploughing, ridging, method_ploughing, method_ridging, CMP, riskAtt,
                            PD_window, HD_window, cassPD, cassUW, cassUP,
                            cassUP_m1, cassUP_m2, cassUP_p1, cassUP_p2) {
    MarkDownTextD <- data.frame(
        name = user$Name, country = country, phone = user$PhoneNr,
        field = userField, field_area = area, unit_field = areaUnits,
        plant_date = PD, hvst_date = HD, email = user$Email,
        latitude = lat, longitude = lon, maxinvest = maxInv,
        saleSF = saleSF, nameSF = nameSF, CMP = CMP, riskAtt = riskAtt,
        PD = PD, HD = HD, PD_window = PD_window, HD_window = HD_window,
        cassPD = cassPD, cassUW = cassUW, cassUP = cassUP,
        cassUP_m1 = cassUP_m1, cassUP_m2 = cassUP_m2,
        cassUP_p1 = cassUP_p1, cassUP_p2 = cassUP_p2,
        userPhoneCC = user$PhoneCC
    )
    write.csv(MarkDownTextD, "./temp/SP_MarkDownText.csv", row.names = FALSE)
}
