
#SHORT DEF:   Function to obtain tillage recommendations (step 4 of 6 steps).
#RETURNS:     dataframe with cost benefit for various combinations of ploughing and ridging.
#DESCRIPTION: Function to obtain recommendations on ploughing and ridging. Returns a dataframe with all possible combinations of
#             ploughing (none, manual, tractor) and ridging (none, manual, tractor), ordered by decreasing net returns and increasing
#             tillage intensity (riding then ploughing)
#INPUT:       See Cassava Crop Manager function for details
getPPrecommendations <- function(areaHa,
                                 costLMO,
                                 ploughing, #select one
                                 ridging, #select one,
                                 method_ploughing, #select one
                                 method_ridging, #select one
                                 FCY,
                                 rootUP,
                                 riskAtt) {
  #creating ploughing and ridging scenarios


  ds <- expand.grid(method_ploughing = c("N/A", "manual", "tractor"), method_ridging = c("N/A", "manual", "tractor"))
  ds$ploughing <- ifelse(ds$method_ploughing == "N/A", FALSE, TRUE)
  ds$ridging <- ifelse(ds$method_ridging == "N/A", FALSE, TRUE)
  ds$cost_ploughing <- ifelse(ds$method_ploughing == "N/A", 0,
                              ifelse(ds$method_ploughing == "manual",
                                     costLMO[costLMO$operation == "ploughing" & costLMO$method == "manual",]$costHa,
                                     costLMO[costLMO$operation == "ploughing" & costLMO$method == "tractor",]$costHa))
  ds$cost_ridging <- ifelse(ds$method_ridging == "N/A", 0,
                            ifelse(ds$method_ridging == "manual",
                                   costLMO[costLMO$operation == "ridging" & costLMO$method == "manual",]$costHa,
                                   costLMO[costLMO$operation == "ridging" & costLMO$method == "tractor",]$costHa))
  ds <- na.omit(ds)
  #adding cost saving for weeding
  ds$cost_weeding <- ifelse(ds$ridging, -costLMO[costLMO$operation == "weeding1",]$costHa, 0)

  #adding expected yields
  yd <- expand.grid(ploughing = c(FALSE, TRUE), ridging = c(TRUE, FALSE), YL = c("low", "high"))
  yd$RY <- c(rep(10, 4), 20, 25, 15, 22)
  yd <- yd[yd$YL == ifelse(FCY < 12.5, "low", "high"),]
  ds <- merge(ds, yd)
  ds$RP <- ds$RY * areaHa

  #calculating total cost, gross and net revenue
  ds$TC <- (ds$cost_ploughing +
    ds$cost_ridging +
    ds$cost_weeding) * areaHa
  ds$GR <- ds$RP * rootUP
  ds$NR <- ds$GR - ds$TC

  #minimal required net revenue increase from fertilizer needed (taking into account risk attitude of user)
  # ds$dNRmin <- ds$TC * ifelse(riskAtt == 0, 2.8, ifelse(riskAtt == 1, 2, 1.2))
  ds$dNRmin <- ds$TC * ifelse(riskAtt == 0, 1.8, ifelse(riskAtt == 1, 1, 0.2))
  ds <- droplevels(ds[ds$NR > ds$dNRmin,])

  ds <- subset(ds, select = -c(cost_ploughing, cost_ridging, cost_weeding, YL, RY))
  ds <- ds[order(-ds$NR, ds$ridging, ds$ploughing),] #order by decreasing net revenue, increasing ridging and increasing ploughing so that recommendation is first row

  #comparing to current practice
  # Create a logical column 'CP' based on conditions
  ds$CP <- with(ds, ploughing == ploughing &
    method_ploughing == method_ploughing &
    ridging == ridging &
    method_ridging == method_ridging)

  # Calculate the differences only for rows where 'CP' is TRUE
  if (any(ds$CP)) {
    cp_values <- ds[ds$CP,]

    ds$dTC <- ds$TC - cp_values$TC
    ds$dRP <- ds$RP - cp_values$RP
    ds$dGR <- ds$GR - cp_values$GR
    ds$dNR <- ds$NR - cp_values$NR
  } else {
    # Handle case where no rows match the 'CP' condition
    ds$dTC <- ds$TC
    ds$dRP <- ds$RP
    ds$dGR <- ds$GR
    ds$dNR <- ds$NR
  }

  return(ds)

}


#' Title
#'
#' @param ds is output of getPPrecommendations
#' @param country
#'
#' @return the advice as text to print in app
#' @export
#'
#' @examples
getPPrecText <- function(ds, country = c("NG", "TZ", "RW")) {

  TRNS <- read.csv("./data/input/translations_TEST.csv", stringsAsFactors = FALSE)
  no_ng <- gsub(pattern = "\"", replacement = "", TRNS$no[1])
  no_tz <- gsub(pattern = "\"", replacement = "", TRNS$no[2])
  no_rw <- gsub(pattern = "\"", replacement = "", TRNS$no[3])
  plo_ng <- gsub(pattern = "\"", replacement = "", TRNS$plo[1])
  plo_tz <- gsub(pattern = "\"", replacement = "", TRNS$plo[2])
  plo_rw <- gsub(pattern = "\"", replacement = "", TRNS$plo[3])
  ridg_ng <- gsub(pattern = "\"", replacement = "", TRNS$ridg[1])
  ridg_tz <- gsub(pattern = "\"", replacement = "", TRNS$ridg[2])
  ridg_rw <- gsub(pattern = "\"", replacement = "", TRNS$ridg[3])
  decnet_ng <- gsub(pattern = "\"", replacement = "", TRNS$decnet[1])
  decnet_tz <- gsub(pattern = "\"", replacement = "", TRNS$decnet[2])
  decnet_rw <- gsub(pattern = "\"", replacement = "", TRNS$decnet[3])
  werec_ng <- gsub(pattern = "\"", replacement = "", TRNS$werec[1])
  werec_tz <- gsub(pattern = "\"", replacement = "", TRNS$werec[2])
  werec_rw <- gsub(pattern = "\"", replacement = "", TRNS$werec[3])
  plofol_ng <- gsub(pattern = "\"", replacement = "", TRNS$plofol[1])
  plofol_tz <- gsub(pattern = "\"", replacement = "", TRNS$plofol[2])
  plofol_rw <- gsub(pattern = "\"", replacement = "", TRNS$plofol[3])
  noridg_ng <- gsub(pattern = "\"", replacement = "", TRNS$noridg[1])
  noridg_tz <- gsub(pattern = "\"", replacement = "", TRNS$noridg[2])
  noridg_rw <- gsub(pattern = "\"", replacement = "", TRNS$noridg[3])
  thank_ng <- gsub(pattern = "\"", replacement = "", TRNS$thank[1])
  thank_tz <- gsub(pattern = "\"", replacement = "", TRNS$thank[2])
  thank_rw <- gsub(pattern = "\"", replacement = "", TRNS$thank[3])
  plodir_ng <- gsub(pattern = "\"", replacement = "", TRNS$noplo[1])
  plodir_tz <- gsub(pattern = "\"", replacement = "", TRNS$noplo[2])
  plodir_rw <- gsub(pattern = "\"", replacement = "", TRNS$noplo[3])
  zerot_ng <- gsub(pattern = "\"", replacement = "", TRNS$zerot[1])
  zerot_tz <- gsub(pattern = "\"", replacement = "", TRNS$zerot[2])
  zerot_rw <- gsub(pattern = "\"", replacement = "", TRNS$zerot[3])
  changcost_ng <- gsub(pattern = "\"", replacement = "", TRNS$changcos[1])
  changcost_tz <- gsub(pattern = "\"", replacement = "", TRNS$changcos[2])
  changcost_rw <- gsub(pattern = "\"", replacement = "", TRNS$changcos[3])
  this_ng <- gsub(pattern = "\"", replacement = "", TRNS$this[1])
  this_tz <- gsub(pattern = "\"", replacement = "", TRNS$this[2])
  this_rw <- gsub(pattern = "\"", replacement = "", TRNS$this[3])
  decr_ng <- gsub(pattern = "\"", replacement = "", TRNS$decr[1])
  decr_tz <- gsub(pattern = "\"", replacement = "", TRNS$decr[2])
  decr_rw <- gsub(pattern = "\"", replacement = "", TRNS$decr[3])
  incr_ng <- gsub(pattern = "\"", replacement = "", TRNS$incr[1])
  incr_tz <- gsub(pattern = "\"", replacement = "", TRNS$incr[2])
  incr_rw <- gsub(pattern = "\"", replacement = "", TRNS$incr[3])
  costb_ng <- gsub(pattern = "\"", replacement = "", TRNS$costb[1])
  costb_tz <- gsub(pattern = "\"", replacement = "", TRNS$costb[2])
  costb_rw <- gsub(pattern = "\"", replacement = "", TRNS$costb[3])
  rtprod_ng <- gsub(pattern = "\"", replacement = "", TRNS$rtprod[1])
  rtprod_tz <- gsub(pattern = "\"", replacement = "", TRNS$rtprod[2])
  rtprod_rw <- gsub(pattern = "\"", replacement = "", TRNS$rtprod[3])
  optim_ng <- gsub(pattern = "\"", replacement = "", TRNS$optim[1])
  optim_tz <- gsub(pattern = "\"", replacement = "", TRNS$optim[2])
  optim_rw <- gsub(pattern = "\"", replacement = "", TRNS$optim[3])
  plofol2_tz <- gsub(pattern = "\"", replacement = "", TRNS$plofol2[2])

  ds$method_ploughing <- as.character(ds$method_ploughing)
  ds$method_ridging <- as.character(ds$method_ridging)

  ds$method_ploughing <- ifelse(country == "TZ" & ds$method_ploughing == "tractor", "kwa trekta", ds$method_ploughing)
  ds$method_ploughing <- ifelse(country == "TZ" & ds$method_ploughing == "manual", "kwa jembe la mkono", ds$method_ploughing)
  ds$method_ridging <- ifelse(country == "TZ" & ds$method_ridging == "tractor", "kwa trekta", ds$method_ridging)
  ds$method_ridging <- ifelse(country == "TZ" & ds$method_ridging == "manual", "kwa jembe la mkono", ds$method_ridging)

  if (ds[1,]$CP) {
    rec <- if (country == "NG") {
      paste0(optim_ng,
             ifelse(ds[1,]$method_ploughing == "N/A", paste(no_ng), ds[1,]$method_ploughing), paste(plo_ng),
             ifelse(ds[1,]$method_ridging == "N/A", paste(no_ng), ds[1,]$method_ridging), paste(ridg_ng), "\n", " ",
             decnet_ng)
    } else if (country == "TZ") {
      paste0(optim_tz,
             ifelse(ds[1,]$method_ploughing == "N/A", paste(no_tz), ds[1,]$method_ploughing), paste(plo_tz),
             ifelse(ds[1,]$method_ridging == "N/A", paste(no_tz), ds[1,]$method_ridging), paste(ridg_tz), "\n", " ",
             decnet_tz)
    } else {
      paste0(optim_rw,
             ifelse(ds[1,]$method_ploughing == "N/A", paste(no_rw), ds[1,]$method_ploughing), paste(plo_rw),
             ifelse(ds[1,]$method_ridging == "N/A", paste(no_rw), ds[1,]$method_ridging), paste(ridg_rw), "\n", " ",
             decnet_rw)
    }
  } else {
    if (ds[1,]$ploughing & ds[1,]$ridging) {
      recT <- if (country == "NG") {
        paste0(werec_ng, ds[1,]$method_ploughing, plofol_ng, ds[1,]$method_ridging, ridg_ng, "\n")
      } else if (country == "TZ") {
        paste0(werec_tz, plofol_tz, ds[1,]$method_ploughing, plofol2_tz, ds[1,]$method_ridging, ridg_tz, "\n")
      } else {
        paste0(werec_rw, ds[1,]$method_ploughing, plofol_rw, ds[1,]$method_ridging, ridg_rw, "\n")
      }
    }

    if (ds[1,]$ploughing & !ds[1,]$ridging) {
      recT <- if (country == "NG") {
        paste0(werec_ng, ds[1,]$method_ploughing, noridg_ng, "\n")
      } else if (country == "TZ") {
        paste0(werec_tz, ds[1,]$method_ploughing, noridg_tz, "\n")
      } else {
        paste0(werec_rw, ds[1,]$method_ploughing, noridg_rw, "\n")
      }
    }

    if (!ds[1,]$ploughing & ds[1,]$ridging) {
      recT <- if (country == "NG") {
        paste0(plodir_ng, ds[1,]$method_ridging, "\n")
      } else if (country == "TZ") {
        paste0(plodir_tz, ds[1,]$method_ridging, "\n")
      } else {
        paste0(plodir_rw, ds[1,]$method_ridging, "\n")
      }
    }

    if (!ds[1,]$ploughing & !ds[1,]$ridging) {
      recT <- if (country == "NG") {
        paste0(zerot_ng, "\n")
      } else if (country == "TZ") {
        paste0(zerot_tz, "\n")
      } else {
        paste0(zerot_rw, "\n")
      }
    }

    rcost <- if (ds[1,]$ploughing | ds[1,]$ridging) {
      if (country == "NG") {
        paste0(changcost_ng, this_ng, decr_ng, incr_ng, ds[1,]$cost, costb_ng, rtprod_ng, "\n")
      } else if (country == "TZ") {
        paste0(changcost_tz, this_tz, decr_tz, incr_tz, ds[1,]$cost, costb_tz, rtprod_tz, "\n")
      } else {
        paste0(changcost_rw, this_rw, decr_rw, incr_rw, ds[1,]$cost, costb_rw, rtprod_rw, "\n")
      }
    }
    rec <- paste(recT, rcost, thank_ng)
  }
  return(rec)
}


process_PP <- function( PP, country, areaHa, costLMO, ploughing, ridging,
		method_ploughing, method_ridging, FCY, rootUP, riskAtt, user,
		userField, area, areaUnits, PD, HD, lat, lon,
		cassPD, cassUW, maxInv, res, recText_input
) {
  recText <- recText_input
  message(paste("Processing PP for", country))

  # Generate PP recommendations
  res[["PP"]] <- getPPrecommendations(
    areaHa = areaHa,
    costLMO = costLMO,
    ploughing = ploughing,
    ridging = ridging,
    method_ploughing = method_ploughing,
    method_ridging = method_ridging,
    FCY = FCY,
    rootUP = rootUP,
    riskAtt = riskAtt
  )

  # Generate recommendation text
  recText[["PP"]] <- getPPrecText(ds = res$PP, country = country)

  # Write output files
  write.csv(res$PP, './temp/PP_rec.csv', row.names = FALSE)
  write.csv(recText$PP, './temp/PP_recText.csv', row.names = FALSE)

  # Generate markdown output
  PP_MarkdownText(
    user = user,
    country = country,
    userField = userField,
    area = area,
    areaUnits = areaUnits,
    PD = PD,
    HD = HD,
    lat = lat,
    lon = lon,
    rootUP = rootUP,
    cassPD = cassPD,
    cassUW = cassUW,
    maxInv = maxInv,
    ploughing = ploughing,
    ridging = ridging,
    method_ploughing = method_ploughing,
    method_ridging = method_ridging,
  )

  return(list(PPrecom = TRUE, plumberRes = res, recText = recText))
}
