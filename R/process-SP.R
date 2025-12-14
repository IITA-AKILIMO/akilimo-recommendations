

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
getSPrecText <- function(ds, country, PD, HD) {

  TRNS <- read.csv("./data/input/translations_TEST.csv", stringsAsFactors = FALSE)
  norecom_ng <- gsub(pattern = "\"", replacement = "", TRNS$norecom[1]); norecom_tz <- gsub(pattern = "\"", replacement = "", TRNS$norecom[2]); norecom_rw <- gsub(pattern = "\"", replacement = "", TRNS$norecom[3]);
  notapply_ng <- gsub(pattern = "\"", replacement = "", TRNS$notapply[1]); notapply_tz <- gsub(pattern = "\"", replacement = "", TRNS$notapply[2]); notapply_rw <- gsub(pattern = "\"", replacement = "", TRNS$notapply[3])
  recrev_ng <- gsub(pattern = "\"", replacement = "", TRNS$recrev[1]); recrev_tz <- gsub(pattern = "\"", replacement = "", TRNS$recrev[2]); recrev_rw <- gsub(pattern = "\"", replacement = "", TRNS$recrev[3])
  hvsdate_ng <- gsub(pattern = "\"", replacement = "", TRNS$hvsdate[1]); hvsdate_tz <- gsub(pattern = "\"", replacement = "", TRNS$hvsdate[2]); hvsdate_rw <- gsub(pattern = "\"", replacement = "", TRNS$hvsdate[3]);
  nochange_ng <- gsub(pattern = "\"", replacement = "", TRNS$nochange[1]); nochange_tz <- gsub(pattern = "\"", replacement = "", TRNS$nochange[2]); nochange_rw <- gsub(pattern = "\"", replacement = "", TRNS$nochange[3])
  recPln_ng <- gsub(pattern = "\"", replacement = "", TRNS$recPln[1]); recPln_tz <- gsub(pattern = "\"", replacement = "", TRNS$recPln[2]); recPln_rw <- gsub(pattern = "\"", replacement = "", TRNS$recPln[3]);
  recHvs_ng <- gsub(pattern = "\"", replacement = "", TRNS$recHvs[1]); recHvs_tz <- gsub(pattern = "\"", replacement = "", TRNS$recHvs[2]); recHvs_rw <- gsub(pattern = "\"", replacement = "", TRNS$recHvs[3])
  wks_ng <- gsub(pattern = "\"", replacement = "", TRNS$wks[1]); wks_tz <- gsub(pattern = "\"", replacement = "", TRNS$wks[2]); wks_rw <- gsub(pattern = "\"", replacement = "", TRNS$wks[3]);
  recPlnP_ng <- gsub(pattern = "\"", replacement = "", TRNS$recPlnP[1]); recPlnP_tz <- gsub(pattern = "\"", replacement = "", TRNS$recPlnP[2]); recPlnP_rw <- gsub(pattern = "\"", replacement = "", TRNS$recPlnP[3])
  early_ng <- gsub(pattern = "\"", replacement = "", TRNS$early[1]); early_tz <- gsub(pattern = "\"", replacement = "", TRNS$early[2]); early_rw <- gsub(pattern = "\"", replacement = "", TRNS$early[3]);
  late_ng <- gsub(pattern = "\"", replacement = "", TRNS$late[1]); late_tz <- gsub(pattern = "\"", replacement = "", TRNS$late[2]); late_rw <- gsub(pattern = "\"", replacement = "", TRNS$late[3])
  recPhv_ng <- gsub(pattern = "\"", replacement = "", TRNS$recPhv[1]); recPhv_tz <- gsub(pattern = "\"", replacement = "", TRNS$recPhv[2]); recPhv_rw <- gsub(pattern = "\"", replacement = "", TRNS$recPhv[3]);


  if (is.null(ds)) {

    rec <- if (country == "NG" | country == "GH") {
      norecom_ng
    }else if (country == "TZ") {
      norecom_tz
    }else {
      norecom_rw
    }

  }else {

    if (ds[1,]$CP) {
      if (country == "NG" | country == "GH") {
        rec <- paste0(recrev_ng, " ( ", format(ds[1,]$PD, "%d %B %Y"), " ) ",
                      hvsdate_ng, " ( ", format(ds[1,]$HD, "%d %B %Y"), " ) ",
                      nochange_ng)
      }else if (country == "TZ") {
        rec <- paste0(recrev_tz, " ( ", format(ds[1,]$PD, "%d %B %Y"), " ) ",
                      hvsdate_tz, " ( ", format(ds[1,]$HD, "%d %B %Y"), " ) ",
                      nochange_tz)
      }else {
        rec <- paste0(recrev_rw, " ( ", format(ds[1,]$PD, "%d %B %Y"), " ) ",
                      hvsdate_rw, " ( ", format(ds[1,]$HD, "%d %B %Y"), " ) ",
                      nochange_rw)
      }


      #TODO: This does not provide details on the reasons. This might either be due to
      #1. unfavourable price conditions at other planting/harvest dates,
      #2. low starch content at later harvest dates (if selling to as starch factory)
      #3. unattractive yields (at earlier harvest dates),
      #4. combination of both.
      #We may also want to include some information on the impact on cropping practices, requirement to ridge, risks of pest and disease issues,...

    }else {

      if (ds[1,]$PD != ds[ds$CP == TRUE,]$PD) {
        #trans
        if (country == "NG" | country == "GH") {
          recP <- paste0(recPln_ng, format(ds[1,]$PD, "%d %B %Y"), ", ",
                         abs(ds[1,]$rPWnr), " ", wks_ng, " ", ifelse(ds[1,]$rPWnr < 0, early_ng, late_ng), " ", recPlnP_ng, "\n")

        }else if (country == "TZ") {
          recP <- paste0(recPln_tz, " ", format(ds[1,]$PD, "%d %B %Y"), ", ",
                         wks_tz, " ", abs(ds[1,]$rPWnr), " ", " ", ifelse(ds[1,]$rPWnr < 0, early_tz, late_tz), " ", recPlnP_tz, "\n")
        }else {
          recP <- paste0(recPln_rw, " ", format(ds[1,]$PD, "%d %B %Y"), ", ",
                         wks_rw, " ", abs(ds[1,]$rPWnr), " ", " ", ifelse(ds[1,]$rPWnr < 0, early_tz, late_rw), " ", recPlnP_rw, "\n")
        }

      }else {
        # recP <- NULL
        if (country == "NG" | country == "GH") {
          recP <- paste0("Your revenue will be highest at your proposed planting date, on ", format(ds[1,]$PD, "%d %B %Y"), ".", sep = "")
        }else if (country == "TZ") {
          recP <- paste0("Mapato yako yatakuwa makubwa zaidi ukipanda tarehe, ", format(ds[1,]$PD, "%d %B %Y"), ".", sep = "")
        }else {
          recP <- paste0("kinyarwanda, ", format(ds[1,]$PD, "%d %B %Y"), ".", sep = "")
        }

      }


      if (ds[1,]$HD != ds[ds$CP == TRUE,]$HD) {
        if (country == "NG" | country == "GH") {
          recH <- paste0(recHvs_ng, format(ds[1,]$PD, "%d %B %Y"), ", ",
                         abs(ds[1,]$rPWnr), " ", wks_ng, " ", ifelse(ds[1,]$rPWnr < 0, early_ng, late_ng), recPhv_ng, "\n")
        }else if (country == "TZ") {
          recH <- paste0(recHvs_tz, " ", format(ds[1,]$PD, "%d %B %Y"), ", ", wks_tz, " ",
                         abs(ds[1,]$rPWnr), " ", ifelse(ds[1,]$rPWnr < 0, early_tz, late_tz), recPhv_tz, "\n")
        }else {
          recH <- paste0(recHvs_rw, " ", format(ds[1,]$PD, "%d %B %Y"), ", ", wks_rw, " ",
                         abs(ds[1,]$rPWnr), " ", ifelse(ds[1,]$rPWnr < 0, early_rw, late_rw), recPhv_rw, "\n")
        }
      }else {
        recH <- NULL
        if (country == "NG" | country == "GH") {
          recH <- paste0("Your selected harvest date is optimal, harvest your cassava on ", format(ds[1,]$HD, "%d %B %Y"), ".", sep = " ")
        }else if (country == "TZ") {
          recH <- paste0("Tarehe uliyochagua ya kuvuna ni bora, vuna mihogo yako tarehe ", format(ds[1,]$HD, "%d %B %Y"), ".", sep = " ")
        }else {
          recH <- paste0("kinyarwanda ", format(ds[1,]$HD, "%d %B %Y"), ".", sep = " ")
        }
      }


      DP <- signif(ds[1,]$RP - ds[ds$CP == TRUE,]$RP, digits = 2)
      currency <- ifelse(country == "NG", "NGN", ifelse(country == "RW", "RWF", ifelse(country == "GH", "GHS", "TZS")))
      dGR <- formatC(signif(ds[1,]$dGR, digits = 3), format = "f", big.mark = ",", digits = 0)
      rechange_ng <- gsub(pattern = "\"", replacement = "", TRNS$rechange[1]); rechange_tz <- gsub(pattern = "\"", replacement = "", TRNS$rechange[2]); rechange_rw <- gsub(pattern = "\"", replacement = "", TRNS$rechange[3])
      hvst_ng <- gsub(pattern = "\"", replacement = "", TRNS$hvst[1]); hvst_tz <- gsub(pattern = "\"", replacement = "", TRNS$hvst[2]); hvst_rw <- gsub(pattern = "\"", replacement = "", TRNS$hvst[3])
      plnt_ng <- gsub(pattern = "\"", replacement = "", TRNS$plnt[1]); plnt_tz <- gsub(pattern = "\"", replacement = "", TRNS$plnt[2]); plnt_rw <- gsub(pattern = "\"", replacement = "", TRNS$plnt[3])
      recRatt1_ng <- gsub(pattern = "\"", replacement = "", TRNS$recRatt1[1]); recRatt1_tz <- gsub(pattern = "\"", replacement = "", TRNS$recRatt1[2]); recRatt1_rw <- gsub(pattern = "\"", replacement = "", TRNS$recRatt1[3])
      recRatt2_ng <- gsub(pattern = "\"", replacement = "", TRNS$recRatt2[1]); recRatt2_tz <- gsub(pattern = "\"", replacement = "", TRNS$recRatt2[2]); recRatt2_rw <- gsub(pattern = "\"", replacement = "", TRNS$recRatt2[3])
      exp_ng <- gsub(pattern = "\"", replacement = "", TRNS$exp[1]); exp_tz <- gsub(pattern = "\"", replacement = "", TRNS$exp[2]); exp_rw <- gsub(pattern = "\"", replacement = "", TRNS$exp[3])
      dec_ng <- gsub(pattern = "\"", replacement = "", TRNS$dec[1]); dec_tz <- gsub(pattern = "\"", replacement = "", TRNS$dec[2]); dec_rw <- gsub(pattern = "\"", replacement = "", TRNS$dec[3])
      inc_ng <- gsub(pattern = "\"", replacement = "", TRNS$inc[1]); inc_tz <- gsub(pattern = "\"", replacement = "", TRNS$inc[2]); inc_rw <- gsub(pattern = "\"", replacement = "", TRNS$inc[3])
      root_ng <- gsub(pattern = "\"", replacement = "", TRNS$root[1]); root_tz <- gsub(pattern = "\"", replacement = "", TRNS$root[2]); root_rw <- gsub(pattern = "\"", replacement = "", TRNS$root[3])
      notot_ng <- gsub(pattern = "\"", replacement = "", TRNS$notot[1]); notot_tz <- gsub(pattern = "\"", replacement = "", TRNS$notot[2]); notot_rw <- gsub(pattern = "\"", replacement = "", TRNS$notot[3])
      optim_ng <- gsub(pattern = "\"", replacement = "", TRNS$optim[1]); optim_tz <- gsub(pattern = "\"", replacement = "", TRNS$optim[2]); optim_rw <- gsub(pattern = "\"", replacement = "", TRNS$optim[3])
      ton_ng <- gsub(pattern = "\"", replacement = "", TRNS$ton[1]); ton_tz <- gsub(pattern = "\"", replacement = "", TRNS$ton[2]); ton_rw <- gsub(pattern = "\"", replacement = "", TRNS$ton[3])
      but_ng <- gsub(pattern = "\"", replacement = "", TRNS$but[1]); but_tz <- gsub(pattern = "\"", replacement = "", TRNS$but[2]); but_rw <- gsub(pattern = "\"", replacement = "", TRNS$but[3])
      and_ng <- gsub(pattern = "\"", replacement = "", TRNS$and[1]); and_tz <- gsub(pattern = "\"", replacement = "", TRNS$and[2]); and_rw <- gsub(pattern = "\"", replacement = "", TRNS$and[3])
      valinc_ng <- gsub(pattern = "\"", replacement = "", TRNS$valinc[1]); valinc_tz <- gsub(pattern = "\"", replacement = "", TRNS$valinc[2]); valinc_rw <- gsub(pattern = "\"", replacement = "", TRNS$valinc[3])


      if (DP == 0) {

        if (dGR == 0) {
          if (country == "NG" | country == "GH") {
            recR <- paste0(rechange_ng,
                           ifelse(!is.null(recH), paste(hvst_ng), paste(plnt_ng)))
          }else if (country == "TZ") {
            recR <- paste0(rechange_tz,
                           ifelse(!is.null(recH), paste(hvst_tz), paste(plnt_tz)))
          }else {
            recR <- paste0(rechange_rw,
                           ifelse(!is.null(recH), paste(hvst_rw), paste(plnt_rw)))
          }

        } else {
          if (country == "NG" | country == "GH") {
            recR <- paste0(recRatt1_ng, currency, " ", dGR, " ", recRatt2_ng)
          }else if (country == "TZ") {
            recR <- paste0(recRatt1_tz, currency, " ", dGR, " ", recRatt2_tz)
          }else {
            recR <- paste0(recRatt1_rw, currency, " ", dGR, " ", recRatt2_rw)
          }
        }
      } else {
        if (dGR == 0) {
          if (country == "NG" | country == "GH") {
            recR <- paste0(exp_ng,
                           ifelse(DP < 0, paste(dec_ng), paste(inc_ng)), root_ng, abs(DP), " ", ton_ng,
                           notot_ng,
                           ifelse(!is.null(recH), paste(hvst_ng), paste(plnt_ng)))
          }else if (country == "TZ") {
            recR <- paste0(exp_tz,
                           ifelse(DP < 0, paste(dec_tz), paste(inc_tz)), root_tz, " ", abs(DP), " ", ton_tz,
                           notot_tz,
                           ifelse(!is.null(recH), paste(hvst_tz), paste(plnt_tz)))

          }else {
            recR <- paste0(exp_rw,
                           ifelse(DP < 0, paste(dec_rw), paste(inc_rw)), root_rw, " ", abs(DP), " ", ton_rw,
                           notot_rw,
                           ifelse(!is.null(recH), paste(hvst_rw), paste(plnt_rw)))
          }
        }else {

          if (country == "NG" | country == "GH") {
            recR <- paste0(exp_ng,
                           ifelse(DP < 0, paste(dec_ng), paste(inc_ng)), root_ng, abs(DP), " ", ton_ng,
                           ifelse(DP < 0, paste(but_ng), paste(and_ng)),
                           valinc_ng, currency, " ", dGR, ".")
          }else if (country == "TZ") {
            recR <- paste0(exp_tz,
                           ifelse(DP < 0, paste(dec_tz), paste(inc_tz)), root_tz, " ", abs(DP), ton_tz,
                           ifelse(DP < 0, paste(but_tz), paste(and_tz)),
                           valinc_tz, currency, " ", dGR, ".")
          }else {
            recR <- paste0(exp_rw,
                           ifelse(DP < 0, paste(dec_rw), paste(inc_rw)), root_rw, " ", abs(DP), ton_rw,
                           ifelse(DP < 0, paste(but_rw), paste(and_rw)),
                           valinc_rw, currency, " ", dGR, ".")
          }


        }
      }

      if (ds[1,]$PD != ds[ds$CP == TRUE,]$PD & ds[1,]$HD != ds[ds$CP == TRUE,]$HD) {

        if (country == "NG" | country == "GH") {
          rec <- paste0(recrev_ng, " ( ", format(ds[1,]$PD, "%d %B %Y"), " ) ",
                        hvsdate_ng, " ( ", format(ds[1,]$HD, "%d %B %Y"), " ) ",
                        nochange_ng)
        }else if (country == "TZ") {
          rec <- paste0(recrev_tz, " ( ", format(ds[1,]$PD, "%d %B %Y"), " ) ",
                        hvsdate_tz, " ( ", format(ds[1,]$HD, "%d %B %Y"), " ) ",
                        nochange_tz)
        }else {
          rec <- paste0(recrev_rw, " ( ", format(ds[1,]$PD, "%d %B %Y"), " ) ",
                        hvsdate_rw, " ( ", format(ds[1,]$HD, "%d %B %Y"), " ) ",
                        nochange_rw)
        }
      }else {
        rec <- paste0(recP, recH, recR)
      }


      #TODO: This only provides the minimal information to return to the user. We may consider adding following information:
      #1. Risks of harvesting later (especially in CBSD-affected areas)
      #2. Importance of using the right varieties
      #3. Reasons underlying recommendations (driven by yield, price or both)
      #4. Implications on agronomic practices, requirements for ridging, fertilizer application,...
      #5. Possible issues with the input data - especially if user provides unrealistic prices.
    }
  }
  rec <- gsub("  ", " ", rec)

  return(rec)
}



## schedule planting and harvest dates is absed on searching highest return on investemnt within 1- or 2-months window around the user intended plant/harvest dates;
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
getSPrecommendations <- function(areaHa,
                                 country,
                                 lat,
                                 lon,
                                 PD,
                                 HD,
                                 PD_window,
                                 HD_window,
                                 saleSF,
                                 nameSF,
                                 FCY,
                                 rootUP,
                                 rootUP_m1,
                                 rootUP_m2,
                                 rootUP_p1,
                                 rootUP_p2) {

  #rounding lat and lon to centroid of 5x5km pixel
  latr <- as.factor(floor(lat * 10) / 10 + ifelse(lat - (floor(lat * 10) / 10) < 0.05, 0.025, 0.075))
  lonr <- as.factor(floor(lon * 10) / 10 + ifelse(lat - (floor(lon * 10) / 10) < 0.05, 0.025, 0.075))
  latr <- as.numeric(levels(latr))
  lonr <- as.numeric(levels(lonr))


  SoilData_fcy1 <- readRDS("./data/input/SoilData_4Country.RDS")
  SoilData <- SoilData_fcy1[SoilData_fcy1$long == lonr & SoilData_fcy1$lat == latr,]


  if (country == "NG") {
    WLY_15M <- readRDS("./data/input/Nigeria_WLY_LINTUL_2020_Server.RDS")
  } else if (country == "TZ") {
    WLY_15M <- readRDS("./data/input/Tanzania_WLY_LINTUL_2020_Server.RDS")
  } else if (country == "GH") {
    WLY_15M <- readRDS("./data/input/Ghana_WLY_LINTUL_SP.RDS")
  }


  latlon <- paste(latr, lonr, sep = "_")

  WLYDataLintul <- WLY_15M[WLY_15M$location == latlon,] ##WLY_15M[WLY_15M$long == lonr & WLY_15M$lat == latr, ]
  WLY_CY <- NULL
  if (nrow(WLYDataLintul) > 0) {
    WLYDataLintul <- merge(WLYDataLintul, data.frame(daysOnField = seq(235, 455, 7), haw = 34:65), by = "daysOnField")
    for (k in 1:nrow(WLYDataLintul)) {
      #print(k)
      wlyd <- WLYDataLintul[k,]
      if (!is.na(SoilData$soilN)) {
        wlyd$Current_Yield <- QUEFTS_WLY_CY(SoilData = SoilData, country = country, wlyd = wlyd) # in kg/ha dry
        WLY_CY <- rbind(WLY_CY, wlyd)
      }
    }
  }


  TRNS <- read.csv("./data/input/translations_TEST.csv", stringsAsFactors = FALSE)
  frnotrec_ng <- gsub(pattern = "\"", replacement = "", TRNS$frnotrec[1]);
  frnotrec_tz <- gsub(pattern = "\"", replacement = "", TRNS$frnotrec[2]);
  frnotrec_rw <- gsub(pattern = "\"", replacement = "", TRNS$frnotrec[3])

  if (is.null(WLY_CY)) {
    # if(!file.exists(flp)){
    #issuereturn
    #trans
    ds <- NULL
    #return("No recommendations available for this location because your location is not within the recommendation domain of AKILIMO.")

    if (country == "NG" | country == "GH") {
      return("We do not have fertilizer recommendation for your location because your location is out of the recommendation domain AKILIMO is currently serving.")
    }else if (country == "TZ") {
      return("Hatuna mapendekezo yoyote kwa eneo lako kwa sababu eneo lako liko nje la eneo ambalo AKILIMO linafanya kazi kwa sasa")
    }else {
      return("kinyarwanda here")
    }

  } else {


    yld <- unique(data.frame(plw = WLY_CY$PlweekNr, haw = WLY_CY$haw, CY = WLY_CY$Current_Yield, WY = WLY_CY$water_limited_yield)) ## is still dry weight
    yld$CY <- round(yld$CY / 1000, digits = 0)
    yld$WY <- round(yld$WY / 1000, digits = 0)


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
      ds$RFCY[k] <- getRFY(HD = ds$HD[k], RDY = ds$CY[k], country = "NG") ## TZ function is giving very strange values, need to be checked
      ds$RFWY[k] <- getRFY(HD = ds$HD[k], RDY = ds$WY[k], country = "NG")
    }


    #scaling predicted yield based on farmer-reported current yield relative to modelled CY and WY:
    ds$RY <- (ds$RFWY - ds$RFCY) / (13.5 - 1.5) / 2.5 * (FCY - 1.5 * 2.5) + ds$RFCY
    ds$RP <- ds$RY * areaHa

    #If selling to a starch factory: rootUP is determined by starch content of roots
    if (saleSF) {

      ds$SC <- 0.75 * ds$WY / ds$RFWY * 100
      SF <- read.csv("./data/input/starchPrices.csv")
      SF <- SF[SF$starchFactory == nameSF,]
      price <- NULL
      for (i in 1:nrow(ds)) {
        price <- c(price, max(SF[SF$minStarch < ds[i,]$SC,]$price))
      }
      ds$rootUP <- price
      ds <- subset(ds, select = -SC)
      #If not selling to a starch factory, user needs to specify rootUP across the harvest window
    }else {
      dp <- data.frame(rHWnr = c(-8, -4, 0, 4, 8),
                       rootUP = c(rootUP_m2, rootUP_m1, rootUP, rootUP_p1, rootUP_p2))
      dpm <- suppressWarnings(loess(rootUP ~ rHWnr, data = dp))
      dpp <- data.frame(rHWnr = seq((-4 * HD_window), (4 * HD_window), by = 2),
                        rootUP = predict(dpm, data.frame(rHWnr = seq((-4 * HD_window), (4 * HD_window), by = 2))))
      ds <- merge(ds, dpp)
    }

    ds$GR <- ds$RP * ds$rootUP
    ds$CP <- ifelse(ds$rPWnr == 0 & ds$rHWnr == 0, TRUE, FALSE)
    ds$dGR <- ds$GR - ds[ds$CP == TRUE,]$GR

    #sort by decreasing GR, increasing HWnr, decreasing PWnr
    #recommendation is highest GR, earliest harvesting, latest planting combination
    ds <- ds[order(-ds$dGR, ds$rHWnr, -ds$rPWnr),]
    write.csv(ds, "./temp/SP_rec.csv", row.names = FALSE)
    return(ds)
  }

}


process_SP <- function(
  SPP, SPH, country, PD_window, HD_window, areaHa, lat, lon, PD, HD, saleSF, nameSF, FCY,
  rootUP, rootUP_m1, rootUP_m2, rootUP_p1, rootUP_p2, user, userField,
  area, areaUnits, maxInv, ploughing, ridging, method_ploughing, method_ridging, CMP, riskAtt,
  cassPD, cassUW, cassUP, cassUP_m1, cassUP_m2, cassUP_p1, cassUP_p2, res, recText
) {
  SPrecom <- NULL

  if (PD_window == 0 && HD_window == 0) {
    SPrecom <- FALSE
    recText[["SP"]] <- if (country %in% c("NG", "GH")) {
      "AKILIMO provides advice for schedule planting if only at least your planting or harvest time or both are flexible. Please provide this information and you will be advised when the best time is for your location."
    } else {
      "AKILIMO hutoa ushauri wa upandaji wa ratiba ikiwa angalau wakati wako wa upandaji au wakati wa kuvuna au zote mbili zinabadilika. Tafadhali toa habari hii na utashauriwa wakati mzuri wa kupanda na kuvuna kwa eneo lako"
    }
  } else {
    message("Processing SP")

    res[["SP"]] <- getSPrecommendations(
		areaHa = areaHa, country = country, lat = lat, lon = lon, PD = PD, HD = HD, 
		PD_window = PD_window, HD_window = HD_window, saleSF = saleSF, nameSF = nameSF,
		FCY = FCY, rootUP = rootUP, rootUP_m1 = rootUP_m1, rootUP_m2 = rootUP_m2, rootUP_p1 = rootUP_p1,
		rootUP_p2 = rootUP_p2)

    if (!is.data.frame(res[["SP"]])) {
      SPrecom <- FALSE
      recText[["SP"]] <- res$SP
    } else {
      SPrecom <- TRUE
      recText[["SP"]] <- getSPrecText(
        ds = res$SP,
        country = country,
        PD = PD,
        HD = HD
      )

      write.csv(recText$SP, './temp/SP_recText.csv', row.names = FALSE)

      SP_MarkdownText(
        user = user, country = country,
        userField = userField, area = area, areaUnits = areaUnits,
        PD = PD, HD = HD, lat = lat, lon = lon,
        saleSF = saleSF, nameSF = nameSF,
        maxInv = maxInv, ploughing = ploughing,
        ridging = ridging, method_ploughing = method_ploughing,
        method_ridging = method_ridging,
        CMP = CMP, riskAtt = riskAtt,
        PD_window = PD_window, HD_window = HD_window,
        cassPD = cassPD, cassUW = cassUW, cassUP = cassUP, cassUP_m1 = cassUP_m1,
        cassUP_m2 = cassUP_m2, cassUP_p1 = cassUP_p1, cassUP_p2 = cassUP_p2
      )
    }
  }

#  return(list(SPrecom = SPrecom, plumberRes = res, recText = recText))
  return(list(SPrecom = SPrecom, plumberRes = list(rec=res, dummy="dummy to prevent bug in test 10"), recText = recText))
}
