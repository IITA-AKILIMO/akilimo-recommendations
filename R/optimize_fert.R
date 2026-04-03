
#' get optimized fertilizer rate, and associated yield for the recommended rate and net revenue given cost and investment
#'
#' @param rootUP root price
#' @param QID soil data
#' @param fertilizer fertilizer types and prices
#' @param invest investment capacity
#' @param plDate planting date
#' @param WLYData WLY data , this is not needed check and remove
#' @param lat
#' @param lon
#' @param areaHa farm size
#' @param HD harvest date
#' @param WLY water limitied yeild
#' @param DCY dry current wt, the output of QUEFTS
#' @param country
run_Optim_NG2 <- function(rootUP, QID, fertilizer, invest, WLYData, lat, lon, areaHa, HD, country) {

	DCY <- WLYData$Current_Yield
	QID$WLY <- WLY <- WLYData$WLY
	plDate  <- WLYData$pl_Date

## input of CY and WLY are in dry wt in KG/ha
	initial <- rep(0, nrow(fertilizer))
	lowerST <- rep(0, nrow(fertilizer))

## both CY and TY should be changed to user land size in ton/ha and fresh wt

	# DEFERRED (LOG-18 / technical debt): country is hardcoded to "NG" for the
	# dry→fresh yield conversion in three places below (lines ~29, ~30, ~35, ~63).
	# getRFY() is now country-aware (LOG-1 fix), but the TZ dry-matter data
	# produced unreliably high values when tested, so the NG conversion factor is
	# intentionally used for all countries until country-specific validation is done.
	# Affected: FR recommendations for TZ, RW, GH, BI use a slightly wrong net-revenue
	# estimate inside the optimiser objective, which may produce a suboptimal NPK rate.
	# To fix: validate country-specific dry-matter data and pass `country` to getRFY()
	# and to the DC lookup below.
	CY_user <- ((getRFY(HD = HD, RDY = DCY, country = "NG")) / 1000) * areaHa
	WLY_user <- ((getRFY(HD = HD, RDY = WLY, country = "NG")) / 1000) * areaHa

### avoid calling getRFY in each step of the optimization
	fd <- get_data("dry_matter")
	d <- as.integer(format(as.Date(HD), "%j"))
	DC <- fd[(fd$dayNr == d) & (fd$country == "NG"), "DMCont"] * 10
### 
	
## this is where the optimization is done, and thereuslt is the NPK rate that gives max profit
	invest[is.na(invest)] <- Inf
	FR <- optim(par = initial, fn = optim_NR, lower = lowerST, method = "L-BFGS-B", 
		control = list(fnscale = -1, ndeps=rep(1, length(initial))),
		rootUP = rootUP, QID = QID, CY = DCY, fertilizer = fertilizer, 
		invest = invest, HD = HD, country = country, DC=DC)$par

	if (all(FR == 0)) {
		return(data.frame(lat = lat, lon = lon, plDate, N = 0, P = 0, K = 0, WLY = WLY_user, CurrentY = CY_user, TargetY = 	CY_user, TC = 0, NR = 0))
	} else {
		fertilizer$FR <- FR
		## NPK rate for ha of land
		N <- as.vector(FR %*% fertilizer$N_cont)
		P <- as.vector(FR %*% fertilizer$P_cont)
		K <- as.vector(FR %*% fertilizer$K_cont)
		rec <- c(N, P, K)

		## NPK rate for user land size
		NPK_user <- rec * areaHa

		# Yield possible at recommended NPK in kg/ha dry wt.
		# TY for ha of land
		TY <- QUEFTS(QID, rec)    

		## both CY and TY should be changed to user land size in ton/ha and fresh wt
		TY_user <- ((getRFY(HD = HD, RDY = TY, country = "NG")) / 1000) * areaHa  # DEFERRED LOG-18: see comment above

		## reporting the recommended fertilizers
		Recomfr <- fertilizer[fertilizer$FR > 0,]
		Recomfr$FR <- round(Recomfr$FR * areaHa)

		## total cost per ha
		TC <- as.numeric(Recomfr$FR %*% Recomfr$price)

		## net revenue on the users land size
		# Gross revenue given root up is for fresh wt ton/ha
		GR <- (TY_user - CY_user) * rootUP  
		TC <- round(TC, -2)
		GR <- round(GR, -2)

		# Net Revenue
		NR <- round(GR - TC, digits = 0) 

	#    Recomfr_wide <- tidyr::spread(Recomfr[, c('type', 'FR')], type, FR)
		Recomfr_wide <- as.list(Recomfr$FR)
		names(Recomfr_wide) <- Recomfr$type
		Recomfr_wide <- as.data.frame(Recomfr_wide)

		d1 <- data.frame(lat = lat, lon = lon, plDate, N = NPK_user[1], P = NPK_user[2], K = NPK_user[3],
                     WLY = WLY_user, CurrentY = CY_user, TargetY = TY_user, TC = TC, NR = NR)
		d2 <- cbind(d1, Recomfr_wide)
		row.names(d2) <- NULL
		if ((d2$NR <= 0) || (d2$TargetY <= d2$CurrentY)) {
			d2 <- subset(d2, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
			d2$N <- d2$P <- d2$K <- d2$TC <- d2$NR <- 0
			d2$TargetY <- d2$CurrentY
		}
		return(d2)
	}

}


#' is a function called within run_Optim_NG2 as function for optim.
#'
#' @param fertRate is the different NPK rate created and passed to the optim in serch for a combination that gives max prifit
#' @param rootUP root prise
#' @param QID soil data
#' @param CY current yiueld
#' @param fertilizer fertilizer types and prices
#' @param invest investment capacity
#' @param HD harvest date
#' @param country


optim_NR <- function(fertRate, rootUP, QID, CY, fertilizer, invest, HD, country, DC) {

	TC <- sum(round(fertRate) * fertilizer$price)
	rec <- c(
		as.vector(fertRate %*% fertilizer$N_cont),
		as.vector(fertRate %*% fertilizer$P_cont),
		as.vector(fertRate %*% fertilizer$K_cont)
	)
	yield <- QUEFTS(QID, rec)

 ## chage in DM is converted to FW
	AdditionalYield <- (yield - CY) / DC
	NetRev <- AdditionalYield * rootUP - TC
	if (TC > invest) { 
	#penalize NR if costs exceed investment cap
		NetRev <- NetRev - (invest - TC)^2 
	#RH NetRev <- 0 #?	
	} 
	NetRev
}

