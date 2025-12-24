

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
  yd <- yd[yd$YL == ifelse(FCY < 12.5, "low", "high"),]
  ds <- merge(ds, yd)
  ds$RP <- ds$RY * areaHa

  #calculating total cost, gross and net revenue
  ds$TC <- (ds$cost_ploughing + ds$cost_ridging + ds$cost_weeding) * areaHa
  ds$GR <- ds$RP * rootUP
  ds$NR <- ds$GR - ds$TC


#  ds <- subset(ds, select = -c(cost_ploughing, cost_ridging, cost_weeding, YL, RY))
  #order by decreasing net revenue, increasing ridging and increasing ploughing so that recommendation is first row
  ds <- ds[order(-ds$NR, ds$ridging, ds$ploughing),]

  #comparing to current practice
  # Create a logical column 'CP' based on conditions
  # bad!
  #ds$CP <- with(ds, ploughing == ploughing &
  #  method_ploughing == method_ploughing &
  #  ridging == ridging &
  #  method_ridging == method_ridging)

  ds$CP <-  ifelse(ds$ploughing, ploughing & ds$method_ploughing == method_ploughing, !ploughing) &
			ifelse(ds$ridging, ridging & ds$method_ridging == method_ridging, !ridging)     

# Calculate the differences only for rows where 'CP' is TRUE
# bad: there must be one that is true...
#  if (any(ds$CP)) {
#    cp_values <- ds[ds$CP,]
#    ds$dTC <- ds$TC - cp_values$TC
#    ds$dRP <- ds$RP - cp_values$RP
#    ds$dGR <- ds$GR - cp_values$GR
#    ds$dNR <- ds$NR - cp_values$NR
#  } else {
#    # Handle case where no rows match the 'CP' condition
#    ds$dTC <- ds$TC
#    ds$dRP <- ds$RP
#    ds$dGR <- ds$GR
#    ds$dNR <- ds$NR
#  }

  ds$dTC <- ds$TC - ds$TC[ds$CP]
  ds$dRP <- ds$RP - ds$RP[ds$CP]
  ds$dGR <- ds$GR - ds$GR[ds$CP]
  ds$dNR <- ds$NR - ds$NR[ds$CP]

  #minimal required net revenue increase from fertilizer needed (taking into account risk attitude of user)
  # ds$dNRmin <- ds$TC * ifelse(riskAtt == 0, 2.8, ifelse(riskAtt == 1, 2, 1.2))
  ds$dNRmin <- ds$TC * ifelse(riskAtt == 0, 1.8, ifelse(riskAtt == 1, 1, 0.2))
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
getPPrecText <- function(ds, country = c("NG", "TZ", "RW")) {

	tr <- get_data("TRNS")
  
  ds$method_ploughing <- as.character(ds$method_ploughing)
  ds$method_ridging <- as.character(ds$method_ridging)

  ds$method_ploughing <- ifelse(country == "TZ" & ds$method_ploughing == "tractor", "kwa trekta", ds$method_ploughing)
  ds$method_ploughing <- ifelse(country == "TZ" & ds$method_ploughing == "manual", "kwa jembe la mkono", ds$method_ploughing)
  ds$method_ridging <- ifelse(country == "TZ" & ds$method_ridging == "tractor", "kwa trekta", ds$method_ridging)
  ds$method_ridging <- ifelse(country == "TZ" & ds$method_ridging == "manual", "kwa jembe la mkono", ds$method_ridging)
	
	cni <- ifelse(country=="NG", 1, ifelse(country=="TZ", 2, 3))

  if (ds[1,]$CP) {
      paste0(tr$optim[cni],
             ifelse(ds[1,]$method_ploughing == "N/A", tr$no[cni], ds[1,]$method_ploughing), paste(tr$plo[cni]),
             ifelse(ds[1,]$method_ridging == "N/A", tr$no[cni], ds[cni,]$method_ridging), paste(tr$ridg[cni]), "\n", " ", tr$decnet[cni])
  } else {
    recT <- if (ds[1,]$ploughing && ds[1,]$ridging) {
      if (country == "TZ") {
        paste0(tr$werec_[2], tr$plofol[2], ds[1,]$method_ploughing, tr$plofol2[2], ds[1,]$method_ridging, tr$ridg[2], "\n")
      } else {
        paste0(tr$werec_[cni], ds[1,]$method_ploughing, tr$plofol[cni], ds[1,]$method_ridging, tr$ridg[cni], "\n")		
      }
    } else if (!(ds[1,]$ploughing || ds[1,]$ridging)) {
        paste0(tr$zerot[cni], "\n")
    } else if (ds[1,]$ploughing) {
        paste0(tr$werec_[cni], ds[1,]$method_ploughing, tr$noridg[cni], "\n")
    } else if (ds[1,]$ridging) {
        paste0(tr$noplo[cni], ds[1,]$method_ridging, "\n")
	} else {""}
	
    rcost <- if (ds[1,]$ploughing | ds[1,]$ridging) {
        paste0(tr$changcost[cni], tr$this[cni], tr$decr[cni], tr$incr[cni], ds[1,]$cost, tr$costb[cni], tr$rtprod[cni], "\n")
    } else {""}
	
	
    paste(recT, rcost, tr$thank[1])
  }

  #TODO: This only provides the minimal information to return to the user. We may consider adding following information:
  #1. Beware that planting on flat may not be advisable in your specific conditions. You should ridge if the land is sometimes very wet (water-logging problems), if controlling weed is very challenging, if the soil is very clayey, or if you plan to harvest during the dry season.
  #2. We currently do not consider costs and benefits of harrowing - we have not investigated this.
  #3. Explicit reasons underlying recommendations (driven by cost-saving or revenue increase).
  #4. Our selection of the best option may differ from the one by the farmer. A farmer may be willing to choose an option that has a lower net revenue change than the recommended, but also a lower cost.
  #5. Possible issues with the input data - especially if user provides unrealistic prices.
  
}


process_PP <- function(PP, country, areaHa, costLMO, ploughing, ridging,
		method_ploughing, method_ridging, FCY, rootUP, riskAtt, user,
		userField, area, areaUnits, PD, HD, lat, lon, cassPD, cassUW, maxInv) {

  # Generate PP recommendations
  res <- getPPrecommendations(areaHa = areaHa, costLMO = costLMO, ploughing = ploughing,
				ridging = ridging, method_ploughing = method_ploughing,
				method_ridging = method_ridging, FCY = FCY, rootUP = rootUP, riskAtt = riskAtt )

  # Generate recommendation text
  recText <- getPPrecText(ds = res, country = country)

  # Write output files
  write.csv(res, './temp/PP_rec.csv', row.names = FALSE)
  write.csv(recText, './temp/PP_recText.csv', row.names = FALSE)

  # Generate markdown output
  PP_MarkdownText(user = user, country = country, userField = userField, area = area, areaUnits = areaUnits,
    PD = PD, HD = HD, lat = lat, lon = lon, rootUP = rootUP, cassPD = cassPD, cassUW = cassUW, maxInv = maxInv,
    ploughing = ploughing, ridging = ridging, method_ploughing = method_ploughing, method_ridging = method_ridging)

  #list(PPrecom = TRUE, plumberRes = res, recText = recText)
	list(rec_type="PP", message=recText, data=res)

}
