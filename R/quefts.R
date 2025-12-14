
## part f QUEFTS fucntion
final_yield_tools <- function(Uptake_Yield) {
  with(Uptake_Yield,
  {
    YNP <- yield_nutrients_combined(U1 = UN, d1 = dN, a1 = aN, Y2A = YPA, Y2D = YPD, Y3D = YKD, r1 = rN)
    YNK <- yield_nutrients_combined(U1 = UN, d1 = dN, a1 = aN, Y2A = YKA, Y2D = YKD, Y3D = YPD, r1 = rN)
    YPN <- yield_nutrients_combined(U1 = UP, d1 = dP, a1 = aP, Y2A = YNA, Y2D = YND, Y3D = YKD, r1 = rP)
    YPK <- yield_nutrients_combined(U1 = UP, d1 = dP, a1 = aP, Y2A = YKA, Y2D = YKD, Y3D = YND, r1 = rP)
    YKN <- yield_nutrients_combined(U1 = UK, d1 = dK, a1 = aK, Y2A = YNA, Y2D = YND, Y3D = YPD, r1 = rK)
    YKP <- yield_nutrients_combined(U1 = UK, d1 = dK, a1 = aK, Y2A = YPA, Y2D = YPD, Y3D = YND, r1 = rK)

    # Make sure the nutrient limited yields do not exceed the maximum possible yield = WLY
    YNPc <- min(c(YNP, YND, YPD, YKD, WLY))
    YNKc <- min(c(YNK, YND, YPD, YKD, WLY))
    YPNc <- min(c(YPN, YND, YPD, YKD, WLY))
    YPKc <- min(c(YPK, YND, YPD, YKD, WLY))
    YKNc <- min(c(YKN, YND, YPD, YKD, WLY))
    YKPc <- min(c(YKP, YND, YPD, YKD, WLY))

    #Final estimate
    YEc <- mean(c(YNPc, YNKc, YPNc, YPKc, YKNc, YKPc))

    return(YEc)
  })
}


#' Yield calculated based on the combined uptake of 2 nutrients, while taking into account the availability of the third nutrient.
yield_nutrients_combined <- function(U1 = NA, d1 = NA, a1 = NA, Y2A = NA, Y2D = NA, Y3D = NA, r1 = NA) {
  # Determine which nutrient limited yield is lowest.
  YxD = min(Y2D, Y3D)
  # If the uptake of one of the nutrients, and therefore the yield associated with that
  # nutrient, is zero the overall yield is also zero.
  if (U1 == 0 || YxD == 0) {
    Y12 = 0
  }else {
    Y12 = Y2A + (2 * (YxD - Y2A) * (U1 - r1 - Y2A / d1)) / (YxD / a1 - Y2A / d1) -
      (YxD - Y2A) * (U1 - r1 - Y2A / d1)^2 / (YxD / a1 - Y2A / d1)^2
  }
  # Return the calculated yield based on the uptake of nutrients 1 and 2
  return(Y12)
}


NUE <- function(HI, CmaxNroots = 6.6, CminNroots = 2.5, CmaxNtops = 17.9, CminNtops = 7.9, CmaxProots = 1.5, CminProots = 0.8, CmaxPtops = 2.8, CminPtops = 0.9,
                CmaxKroots = 11, CminKroots = 2.8, CmaxKtops = 18.8, CminKtops = 3.4) {
  aN = round(1000 * HI / (HI * CmaxNroots + (1 - HI) * CmaxNtops), digits = 0)
  dN = round(1000 * HI / (HI * CminNroots + (1 - HI) * CminNtops), digits = 0)

  aP = round(1000 * HI / (HI * CmaxProots + (1 - HI) * CmaxPtops), digits = 0)
  dP = round(1000 * HI / (HI * CminProots + (1 - HI) * CminPtops), digits = 0)

  aK = round(1000 * HI / (HI * CmaxKroots + (1 - HI) * CmaxKtops), digits = 0)
  dK = round(1000 * HI / (HI * CminKroots + (1 - HI) * CminKtops), digits = 0)

  return(data.frame(aN = aN, dN = dN, aP = aP, dP = dP, aK = aK, dK = dK))

}


#' Used within QUEFTS1_Pedotransfer function
#' using the output of function "NPK_TargetYield_forinput" and a data frame per lon and lat for intended NPK input
#' this function calculates the yield that can be obtained for intended NPK rate.
#' @param NutrUse_soilNPK
#' @param NPKdata: needs to be provided
#' @return
#'
#' @author Meklit
#' @export
NPK_TargetYield_forOutput <- function(NutrUse_soilNPK, N_rate, P_rate, K_rate) {
  NutrUse_soilNPK$N_rate <- N_rate
  NutrUse_soilNPK$P_rate <- P_rate
  NutrUse_soilNPK$K_rate <- K_rate

  ## Supply of nutrients to the crop
  NutrUse_soilNPK$SN <- NutrUse_soilNPK$N_rate + NutrUse_soilNPK$soilN
  NutrUse_soilNPK$SP <- NutrUse_soilNPK$P_rate + NutrUse_soilNPK$soilP
  NutrUse_soilNPK$SK <- NutrUse_soilNPK$K_rate + NutrUse_soilNPK$soilK

  ## Actual Uptake of nutrients: crop param + nutrient supply
#  tmp <- plyr::ddply(NutrUse_soilNPK, plyr::.(lat, long), actual_uptake_tool)
  tmp <- dd_ply(NutrUse_soilNPK, c("lat", "long"), actual_uptake_tool)
  NutrUse_soilNPK <- merge(NutrUse_soilNPK, tmp, by = c("lat", "long"))

  ## max and min yield: actual uptake and crop param. min of N uptake constrianed by availability of P, K and water
#  maxminY <- plyr::ddply(NutrUse_soilNPK, plyr::.(lat, long), max_min_yields_tools)
  maxminY <- dd_ply(NutrUse_soilNPK, c("lat", "long"), max_min_yields_tools)
  NutrUse_soilNPK <- merge(NutrUse_soilNPK, maxminY, by = c("lat", "long"))

  ## final yield: min yield for combined uptake of 2 nutrients assuming the 3rd is not limiting, should be < WLY, and take meanof the six combinations
#  Target_Yield <- plyr::ddply(NutrUse_soilNPK, plyr::.(lat, long), quefts_tools)
  Target_Yield <- dd_ply(NutrUse_soilNPK, c("lat", "long"), quefts_tools)
  TY <- data.frame(lat = Target_Yield$lat, lon = Target_Yield$long, TargetYield = Target_Yield$FinalYield)

  return(TY)
}


#' computes target yield in tonnes/ha from a given NPK rate, it is used within profit optimization and other fucntions need to predict yield from NPK
#' @param QID a data frame containing soil NPK, WLY (kg/ha dry wt.),
#' @param rec recomended NPK rate
#' @returnType
#' @return target yield in ton/ha dry matter
#'
#' @author Meklit
#' @export
QUEFTS1_Pedotransfer <- function(QID, rec) {
  QID$WLY <- QID$water_limited_yield

  crop_param <- cbind(NUE(HI = 0.52), data.frame(rN = 0, rP = 0, rK = 0, max_yield = QID$WLY, tolerance = 0.01))    ## nutrient use efficiency of the crop


  Queft_Input_Data_Var1 <- cbind(QID, crop_param)
 
 indata <- Queft_Input_Data_Var1[, c("lat", "long", "WLY", "aN", "dN", "aP", "dP", "aK", "dK", "rN", "rP", "rK", "soilN", "soilP", "soilK", "max_yield", "tolerance")]

#  N_rate <- rec[1]; P_rate <- rec[2]; K_rate <- rec[3]

  TargetYield_from_NPK <- NPK_TargetYield_forOutput(NutrUse_soilNPK = indata, rec[1], rec[2], rec[3])

  return(TargetYield_from_NPK$TargetYield)
}



## part of QUEFTS model
actual_uptake_tool <- function(ds_supply) {
  with(ds_supply,
  {
    UNP <- nutrient_uptake(S1 = SN, S2 = SP, d1 = dN, a1 = aN, d2 = dP, a2 = aP, r1 = rN, r2 = rP)
    UNK <- nutrient_uptake(S1 = SN, S2 = SK, d1 = dN, a1 = aN, d2 = dK, a2 = aK, r1 = rN, r2 = rK)
    UNW <- water_dependent_nutrient_uptake(S1 = SN, WLY = WLY, d1 = dN, a1 = aN, r1 = rN)
    UN <- min(UNP, UNK, UNW)


    UPN <- nutrient_uptake(S1 = SP, S2 = SN, d1 = dP, a1 = aP, d2 = dN, a2 = aN, r1 = rP, r2 = rN)
    UPK <- nutrient_uptake(S1 = SP, S2 = SK, d1 = dP, a1 = aP, d2 = dK, a2 = aK, r1 = rP, r2 = rK)
    UPW <- water_dependent_nutrient_uptake(S1 = SP, WLY = WLY, d1 = dP, a1 = aP, r1 = rP)
    UP <- min(UPN, UPK, UPW)


    UKN <- nutrient_uptake(S1 = SK, S2 = SN, d1 = dK, a1 = aK, d2 = dN, a2 = aN, r1 = rK, r2 = rN)
    UKP <- nutrient_uptake(S1 = SK, S2 = SP, d1 = dK, a1 = aK, d2 = dP, a2 = aP, r1 = rK, r2 = rP)
    UKW <- water_dependent_nutrient_uptake(S1 = SK, WLY = WLY, d1 = dK, a1 = aK, r1 = rK)
    UK <- min(UKN, UKP, UKW)


    return(data.frame(UN = UN, UP = UP, UK = UK))
  })
}


#' Nutrient uptake depends on the soil supply of the nutrient and the supply of other nutrients
nutrient_uptake <- function(S1 = NA, S2 = NA, d1 = NA, a1 = NA, d2 = NA, a2 = NA, r1 = NA, r2 = NA) {
  # N, P and K uptakes based on QUEFTS
  if (S1 < r1 + ((S2 - r2) * a2 / d1)) {
    uptakeX_givenY = S1
  } else if (S1 > r1 + ((S2 - r2) * (2 * d2 / a1 - a2 / d1))) {
    uptakeX_givenY = r1 + (S2 - r2) * (d2 / a1)
  } else {
    uptakeX_givenY = S1 - 0.25 * (S1 - r1 - (S2 - r2) * (a2 / d1))^2 / ((S2 - r2) * (d2 / a1 - a2 / d1))
  }
  # Nutrient uptake given availability of other nutrient
  return(uptakeX_givenY)
}


## part of QUEFTS model
water_dependent_nutrient_uptake <- function(S1 = NA, WLY = NA, d1 = NA, a1 = NA, r1 = NA) {
  if (S1 < r1 + WLY / d1) {
    uptakeX_givenWater = S1
  } else if (S1 > r1 + 2 * WLY / a1 - WLY / d1) {
    uptakeX_givenWater = WLY / a1
  } else {
    uptakeX_givenWater = S1 - 0.25 * (S1 - r1 - WLY / d1)^2 / (WLY / a1 - WLY / d1)
  }
  return(uptakeX_givenWater)
}


## part of QUEFTS
max_min_yields_tools <- function(dss) {

  YNA <- max((dss$UN - dss$rN), 0) * dss$aN
  YND <- max((dss$UN - dss$rN), 0) * dss$dN
  YPA <- max((dss$UP - dss$rP), 0) * dss$aP
  YPD <- max((dss$UP - dss$rP), 0) * dss$dP
  YKA <- max((dss$UK - dss$rK), 0) * dss$aK
  YKD <- max((dss$UK - dss$rK), 0) * dss$dK


  return(data.frame(YNA = YNA, YND = YND, YPA = YPA, YPD = YPD, YKA = YKA, YKD = YKD))

}


## part of QUEFTS
quefts_tools <- function(supply_wly) {
  # Actual uptake of nutrients.
  tmp <- actual_uptake_tool(supply_wly)
  supply_wly$UN <- tmp[[1]]
  supply_wly$UP <- tmp[[2]]
  supply_wly$UK <- tmp[[3]]

  # Maximum and minimum yields, depending on maximum accumulation and dilution.
  yields <- max_min_yields_tools(supply_wly)
  supply_wly$YNA <- yields$YNA
  supply_wly$YND <- yields$YND
  supply_wly$YPA <- yields$YPA
  supply_wly$YPD <- yields$YPD
  supply_wly$YKA <- yields$YKA
  supply_wly$YKD <- yields$YKD

  # Final yield based on the combinations of nutrient uptake and minimum + maximum yields.
  supply_wly$FinalYield <- final_yield_tools(supply_wly)

  return(supply_wly)
}


#' The soil NPK as obtained from random forest model
#' @param SoilData soil data with INS
#' @param wlyd:  water limited yield
#' @return modelled current yield for a location
#'
#' @author Meklit
#' @export


QUEFTS_WLY_CY <- function(SoilData, country, wlyd) {

	getsupply <- function(dss) {
	  data.frame(lat = dss$lat, long = dss$long, 
				rel_N = dss$rel_N, rel_P = dss$rel_P, rel_K = dss$rel_K, 
				SN = dss$soilN, SP = dss$soilP, SK = dss$soilK, 
				water_limited_yield = dss$water_limited_yield,
				aN = dss$aN, dN = dss$dN, aP = dss$aP, dP = dss$dP, aK = dss$aK, dK = dss$dK, 
				rN = dss$rN, rP = dss$rP, rK = dss$rK, max_yield = dss$max_yield, tolerance = dss$tolerance,
				WLY = dss$water_limited_yield)
	}


  #wly_plDate <- wly_data[wly_data$plantingDate == pl_Date, c("lat", "long", "wly_KgHa")]
	wlyd$long <- wlyd$lon
	wly_plDate <- wlyd[, c("lat", "long", "water_limited_yield")]

	# colnames(wly_plDate) <- c("lat", "long", "water_limited_yield")
	Quefts_Input_Data_wly <- merge(SoilData, wly_plDate, by = c("lat", "long"))

  ## HI: Median for Nigeria=0.55 and Tanzania=0.52. Q3, Nigeria=0.63 and Tanzania=0.61
	if (country == "NG" | country == "GH") {
		crop_param <- cbind(NUE(HI = 0.55), data.frame(rN = 0, rP = 0, rK = 0, max_yield = Quefts_Input_Data_wly$water_limited_yield, tolerance = 0.01))
	} else {
		crop_param <- cbind(NUE(HI = 0.55), data.frame(rN = 0, rP = 0, rK = 0, max_yield = Quefts_Input_Data_wly$water_limited_yield, tolerance = 0.01))
	}

	  ## 1. get soil nutrient supply
	Queft_Input_Data_Var <- cbind(Quefts_Input_Data_wly, crop_param)
	supply <- getsupply(Queft_Input_Data_Var) ## to get yield at zero input level


	  ## 2. Current yield:
	#  actualUptake <- merge(supply, plyr::ddply(supply, plyr::.(lat, long), actual_uptake_tool), by = c("lat", "long"))
	#  minmax_Yield <- merge(actualUptake, plyr::ddply(actualUptake, plyr::.(lat, long), max_min_yields_tools), by = c("lat", "long"))
	#  Current_Yield <- plyr::ddply(minmax_Yield, plyr::.(lat, long), final_yield_tools) ## yield at zero input

	## I suspect that merge can be replaced with cbind, as lat/longs are unique 
	## or even better, it seems that supply always has one row
	actualUptake <- merge(supply, dd_ply(supply, c("lat", "long"), actual_uptake_tool), by = c("lat", "long"))
	minmax_Yield <- merge(actualUptake, dd_ply(actualUptake, c("lat", "long"), max_min_yields_tools), by = c("lat", "long"))
	Current_Yield <- dd_ply(minmax_Yield, c("lat", "long"), final_yield_tools) ## yield at zero input

	colnames(Current_Yield)[3] <- "CurrentYield"
	Yield_Fertilizer <- merge(wly_plDate, Current_Yield, by = c("lat", "long"))

#	CurrentYield <- ifelse(Yield_Fertilizer$CurrentYield > Yield_Fertilizer$water_limited_yield,
#							as.character(as.numeric(Yield_Fertilizer$water_limited_yield)), 
#							as.numeric(Yield_Fertilizer$CurrentYield))

	CurrentYield <- min(Yield_Fertilizer$CurrentYield, Yield_Fertilizer$water_limited_yield)
	CurrentYield
}




