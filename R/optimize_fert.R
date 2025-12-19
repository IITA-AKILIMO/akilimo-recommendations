
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
run_Optim_NG2 <- function(rootUP, QID, fertilizer, invest, plDate, WLYData, lat, lon, areaHa, HD, WLY, DCY, country) {

  ## input of CY and WLY are in dry wt in KG/ha
  QID$WLY <- WLY
  initial <- rep(0, nrow(fertilizer))
  lowerST <- rep(0, nrow(fertilizer))

  ## both CY and TY should be changed to user land size in ton/ha and fresh wt
  CY_user <- ((getRFY(HD = HD, RDY = DCY, country = "NG")) / 1000) * areaHa ## TZ model is extrememly high
  WLY_user <- ((getRFY(HD = HD, RDY = WLY, country = "NG")) / 1000) * areaHa

### avoid calling getRFY in each step of the optimization
  fd <- get_data("dry_matter")
  d <- as.integer(format(as.Date(HD), "%j"))
  DC <- fd[(fd$dayNr == d) & (fd$country == "NG"), "DMCont"] * 10
### 
	
  ## this is where the optimization is done, and thereuslt is the NPK rate that gives max profit
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
    TY_user <- ((getRFY(HD = HD, RDY = TY, country = "NG")) / 1000) * areaHa

    # CY_user <- CY * areaHa * 0.003
    # TY_user <- TY * areaHa * 0.003
    # WLY_user <- QID$water_limited_yield * areaHa * 0.003

    ## reporting the recommended fertilizers
    Recomfr <- fertilizer[fertilizer$FR > 0,]
    Recomfr$FR <- round(Recomfr$FR * areaHa, digits = 0)


    ## total cost per ha
    TC <- as.numeric(Recomfr$FR %*% Recomfr$price)

    ## net revenue on the users land size
    GR <- (TY_user - CY_user) * rootUP                      # Gross revenue given root up is for fresh wt ton/ha
    TC <- round(TC, -2)
    GR <- round(GR, -2)

    NR <- round(GR - TC, digits = 0)                                                # Net Revenue

    Recomfr_wide <- tidyr::spread(Recomfr[, c('type', 'FR')], type, FR)

    d1 <- data.frame(lat = lat, lon = lon, plDate, N = NPK_user[1], P = NPK_user[2], K = NPK_user[3],
                     WLY = WLY_user, CurrentY = CY_user, TargetY = TY_user, TC = TC, NR = NR)
    d2 <- cbind(d1, Recomfr_wide)
    row.names(d2) <- NULL
    if (d2$NR <= 0 | d2$TargetY <= d2$CurrentY) {
      fertinfo <- subset(d2, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
      fertinfo$N <- fertinfo$P <- fertinfo$K <- fertinfo$TC <- fertinfo$NR <- 0
      fertinfo$TargetY <- fertinfo$CurrentY
      d2 <- fertinfo
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

  ## Kg of Urea, Kg of NPK151515, Kg of NPK201010, Kg of MOP
	
	rec <- c(
		as.vector(fertRate %*% fertilizer$N_cont),
		as.vector(fertRate %*% fertilizer$P_cont),
		as.vector(fertRate %*% fertilizer$K_cont)
	)

	TotalYield <- QUEFTS(QID, rec)

 ## DM is converted to FW and then from KG/ha to ton/ha
	AdditionalYield <- (TotalYield - CY) / DC
#	AdditionalYield <- (getRFY(HD = HD, RDY = (TotalYield - CY), country = country)) / 1000
  #AdditionalYield <- (TotalYield - CY)*0.003
	NetRev <- AdditionalYield * rootUP - TC
	if (!is.na(invest) & TC > invest) { 
	#penalize NR if costs exceed investment cap
		NetRev <- NetRev - (invest - TC)^2 
	} 
  NetRev
}





#' after setting fertilizer recommendation <25 kg/ha Urea, MOP or Nafaka, target yield with the remaining recommended fertilizer is  re-estimated  and
#'  total cost, gross and net revenue are re calcuated.
#' @param rootUP cassava root price
#' @param zone
#' @param wdd has dry wt
#' @param rdd has fresh wt
#' @param fertilizer
#' @author Meklit
#' @export
Rerun_25kgKa_try <- function(rootUP, rdd, fertilizer, QID, onlyFert, country, WLY = WLY, DCY = DCY, HD = HD, areaHa = areaHa) {


  QID$WLY <- WLY
  fertilizer <- merge(fertilizer, onlyFert, by = 'type')
  TC <- (sum(fertilizer$price %*% fertilizer$rate)) * areaHa
  TC <- round(TC, -2)
  N <- as.vector(fertilizer$rate %*% fertilizer$N_cont)
  P <- as.vector(fertilizer$rate %*% fertilizer$P_cont)
  K <- as.vector(fertilizer$rate %*% fertilizer$K_cont)
  rec <- c(N, P, K)

  ## NPK rate for user land size
  NPK_user <- rec * areaHa

  TY <- QUEFTS(QID, rec) #dry wt yield in kg/ha
  #TY_user  <- ((getRFY(HD = as.Date(HD), RDY = TY, country = country))/1000) * areaHa
  TY_user <- ((getRFY(HD = HD, RDY = TY, country = country)) / 1000) * areaHa
  CY_user <- ((getRFY(HD = HD, RDY = DCY, country = country)) / 1000) * areaHa


  rdd$CurrentY <- CY_user
  rdd$TargetY <- TY_user
  rdd$TC <- TC
  nr <- (rdd$TargetY - rdd$CurrentY) * rootUP
  nr <- round(nr, -2)
  rdd$NR <- nr - rdd$TC
  rdd$N <- NPK_user[1]
  rdd$P <- NPK_user[2]
  rdd$K <- NPK_user[3]

  if (rdd$TargetY <= rdd$CurrentY) {
    rdd$N <- rdd$P <- rdd$K <- rdd$TC <- rdd$NR <- 0
    rdd$TargetY <- CY_user
  }

  if (rdd$NR <= 0 | rdd$TargetY <= rdd$CurrentY) {
    fertinfo <- subset(rdd, select = c(lat, lon, plDate, N, P, K, WLY, CurrentY, TargetY, TC, NR))
    fertinfo$N <- fertinfo$P <- fertinfo$K <- fertinfo$TC <- fertinfo$NR <- 0
    fertinfo$TargetY <- fertinfo$CurrentY
    rdd <- fertinfo
  }

  return(rdd)
}

