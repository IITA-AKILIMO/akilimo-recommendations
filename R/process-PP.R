

getPPrecommendations <- function(areaHa, costLMO,
		#select one
        ploughing, ridging, method_ploughing, method_ridging,
        FCY, rootUP, riskAtt) {
  #creating ploughing and ridging scenarios

  ds <- expand.grid(method_ploughing = c("N/A", "manual", "tractor"), method_ridging = c("N/A", "manual", "tractor"), stringsAsFactors = FALSE)
  ds$ploughing <- ds$method_ploughing != "N/A"
  ds$ridging <- ds$method_ridging != "N/A"
  ds$cost_ploughing <- ifelse(ds$method_ploughing == "N/A", 0,
                       ifelse(ds$method_ploughing == "manual",
                        costLMO[costLMO$operation == "ploughing" & costLMO$method == "manual", "costHa"],
                        costLMO[costLMO$operation == "ploughing" & costLMO$method == "tractor", "costHa"]))
  ds$cost_ridging <- ifelse(ds$method_ridging == "N/A", 0,
                     ifelse(ds$method_ridging == "manual",
                      costLMO[costLMO$operation == "ridging" & costLMO$method == "manual", "costHa"],
                      costLMO[costLMO$operation == "ridging" & costLMO$method == "tractor", "costHa"]))
  ds <- na.omit(ds)
  #adding cost saving for weeding
# this seems wrong (and can lead to negative costs)
#  ds$cost_weeding <- ifelse(ds$ridging, -costLMO[costLMO$operation == "weeding1",]$costHa, 0)
  w1 <- costLMO$operation == "weeding1"
  w2 <- costLMO$operation == "weeding2"
  ds$cost_weeding <- ifelse(ds$ridging, costLMO[w2, "costHa"], costLMO[w1, "costHa"] + costLMO[w2, "costHa"])

  #adding expected yields
  yd <- expand.grid(ploughing = c(FALSE, TRUE), ridging = c(TRUE, FALSE), YL = c("low", "high"))
  yd$RY <- c(rep(10, 4), 20, 25, 15, 22)
  yd <- yd[yd$YL == ifelse(FCY < 15, "low", "high"),]
  ds <- merge(ds, yd)
  ds$RP <- ds$RY * areaHa

  #calculating total cost, gross and net revenue
  ds$TC <- (ds$cost_ploughing + ds$cost_ridging + ds$cost_weeding) * areaHa
  ds$GR <- ds$RP * rootUP
  ds$NR <- ds$GR - ds$TC


  # Order by decreasing net revenue, increasing ridging, increasing ploughing
  # so the recommended row is always first.
  ds <- ds[order(-ds$NR, ds$ridging, ds$ploughing),]

  # Mark the row that matches the farmer's current practice.
  # ds$ploughing / ds$ridging are column vectors; ploughing / ridging are scalar flags from the request.
  ds$CP <- (ds$ploughing == ploughing) &
           (ds$ridging   == ridging)   &
           (!ploughing | ds$method_ploughing == method_ploughing) &
           (!ridging   | ds$method_ridging   == method_ridging)

  cp_idx <- which(ds$CP)
  if (length(cp_idx) == 0) {
      warning("No current-practice row matched in PP recommendations; using lowest-NR row as baseline")
      cp_idx <- which.min(ds$NR)
  }
  cp_idx <- cp_idx[1]  # guard against duplicate matches
  ds$dTC <- ds$TC - ds$TC[cp_idx]
  ds$dRP <- ds$RP - ds$RP[cp_idx]
  ds$dGR <- ds$GR - ds$GR[cp_idx]
  ds$dNR <- ds$NR - ds$NR[cp_idx]

  #minimal required net revenue increase from fertilizer needed (taking into account risk attitude of user)
  ds$dNRmin <- ds$TC * min_nr_multiplier(riskAtt)
  # needs refinement. Use the cheapest solution of all are above threshold
  ds <- ds[ds$CP | (ds$NR > ds$dNRmin),]
  ds
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
getPPrecText <- function(ds, country, lang) {

  method_tractor <- tr("method_tractor", lang)
  method_manual  <- tr("method_manual",  lang)

  # Translate a raw method value ("tractor"/"manual"/"N/A") to a localised label.
  translate_method <- function(m) {
      switch(as.character(m),
          tractor = method_tractor,
          manual  = method_manual,
          tr("word_no", lang)   # N/A and any unexpected value
      )
  }

  plo_label  <- translate_method(ds[1,]$method_ploughing)
  ridg_label <- translate_method(ds[1,]$method_ridging)

  if (ds[1,]$CP) {
      tr("pp_practice_optimal", lang, plo_method = plo_label, ridg_method = ridg_label)
  } else {
    recT <- if (ds[1,]$ploughing && ds[1,]$ridging) {
        tr("pp_rec_plough_ridge", lang, plo_method = plo_label, ridg_method = ridg_label)
    } else if (!(ds[1,]$ploughing || ds[1,]$ridging)) {
        tr("pp_zero_tillage", lang)
    } else if (ds[1,]$ploughing) {
        tr("pp_rec_plough_only", lang, plo_method = plo_label)
    } else {
        tr("pp_rec_ridge_only", lang, ridg_method = ridg_label)
    }

    rcost <- if (ds[1,]$ploughing | ds[1,]$ridging) {
        if (ds[1,]$dTC == 0) {
            tr("pp_cost_no_change", lang)
        } else {
            dTC_fmt <- formatC(abs(ds[1,]$dTC), format = "f", big.mark = ",", digits = 0)
            tr("pp_cost_change", lang,
               direction = tr(ifelse(ds[1,]$dTC < 0, "decr", "incr"), lang),
               amount    = dTC_fmt)
        }
    } else {""}

    thank <- trimws(sub("^\\.", "", tr("closing_thanks", lang)))
    parts <- Filter(nzchar, trimws(c(recT, rcost)))
    paste(c(parts, thank), collapse = " ")
  }

  #TODO: This only provides the minimal information to return to the user. We may consider adding following information:
  #1. Beware that planting on flat may not be advisable in your specific conditions. You should ridge if the land is sometimes very wet (water-logging problems), if controlling weed is very challenging, if the soil is very clayey, or if you plan to harvest during the dry season.
  #2. We currently do not consider costs and benefits of harrowing - we have not investigated this.
  #3. Explicit reasons underlying recommendations (driven by cost-saving or revenue increase).
  #4. Our selection of the best option may differ from the one by the farmer. A farmer may be willing to choose an option that has a lower net revenue change than the recommended, but also a lower cost.
  #5. Possible issues with the input data - especially if user provides unrealistic prices.
  
}


process_PP <- function(PP, country, lang, areaHa, costLMO, ploughing, ridging,
		method_ploughing, method_ridging, FCY, rootUP, riskAtt, user,
		userField, area, areaUnits, PD, HD, lat, lon, cassPD, cassUW, maxInv) {

  # Generate PP recommendations
  res <- getPPrecommendations(areaHa = areaHa, costLMO = costLMO, ploughing = ploughing,
				ridging = ridging, method_ploughing = method_ploughing,
				method_ridging = method_ridging, FCY = FCY, rootUP = rootUP, riskAtt = riskAtt )

  recText <- getPPrecText(ds = res, country = country, lang = lang)

  list(rec_type = "PP", recommendation = recText, data = res,
       costLMO = costLMO, ploughing = ploughing, ridging = ridging,
       method_ploughing = method_ploughing, method_ridging = method_ridging)

}
