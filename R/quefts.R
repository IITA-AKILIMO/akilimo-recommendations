
## part of QUEFTS
max_min_yields_tools <- function(dss) {

  YNA <- max((dss$UN - dss$rN), 0) * dss$aN
  YND <- max((dss$UN - dss$rN), 0) * dss$dN
  YPA <- max((dss$UP - dss$rP), 0) * dss$aP
  YPD <- max((dss$UP - dss$rP), 0) * dss$dP
  YKA <- max((dss$UK - dss$rK), 0) * dss$aK
  YKD <- max((dss$UK - dss$rK), 0) * dss$dK

  data.frame(YNA = YNA, YND = YND, YPA = YPA, YPD = YPD, YKA = YKA, YKD = YKD)

}


NUE <- function(HI, CmaxNroots = 6.6, CminNroots = 2.5, CmaxNtops = 17.9, CminNtops = 7.9, 
				CmaxProots = 1.5, CminProots = 0.8, CmaxPtops = 2.8, CminPtops = 0.9,
                CmaxKroots = 11, CminKroots = 2.8, CmaxKtops = 18.8, CminKtops = 3.4) {
  
  data.frame(
	aN = round(1000 * HI / (HI * CmaxNroots + (1 - HI) * CmaxNtops)),
	dN = round(1000 * HI / (HI * CminNroots + (1 - HI) * CminNtops)),
	aP = round(1000 * HI / (HI * CmaxProots + (1 - HI) * CmaxPtops)),
	dP = round(1000 * HI / (HI * CminProots + (1 - HI) * CminPtops)),
	aK = round(1000 * HI / (HI * CmaxKroots + (1 - HI) * CmaxKtops)),
	dK = round(1000 * HI / (HI * CminKroots + (1 - HI) * CminKtops))
  )

}

## part f QUEFTS fucntion
final_yield_tools <- function(Uptake_Yield) {
	#' Yield calculated based on the combined uptake of 2 nutrients, while taking into account the availability of the third nutrient.
	yield_nutrients_combined <- function(U1, d1, a1, Y2A, Y2D, Y3D, r1) {
	  # Determine which nutrient limited yield is lowest.
	  YxD = min(Y2D, Y3D)
	  # If the uptake of one of the nutrients, and therefore the yield associated with that
	  # nutrient, is zero the overall yield is also zero.
	  if (U1 == 0 || YxD == 0) {
		0
	  } else {
		Y2A + (2 * (YxD - Y2A) * (U1 - r1 - Y2A / d1)) / (YxD / a1 - Y2A / d1) -
		  (YxD - Y2A) * (U1 - r1 - Y2A / d1)^2 / (YxD / a1 - Y2A / d1)^2
	  }
	  # Return the calculated yield based on the uptake of nutrients 1 and 2
	}


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



## part of QUEFTS model
actual_uptake_tool <- function(ds_supply) {

	#' Nutrient uptake depends on the soil supply of the nutrient and the supply of other nutrients
	nutrient_uptake <- function(S1, S2, d1, a1, d2, a2, r1, r2) {
	  # N, P and K uptakes based on QUEFTS
		if (S1 < (r1 + ((S2 - r2) * a2 / d1))) {
			S1
		} else if (S1 > (r1 + ((S2 - r2) * (2 * d2 / a1 - a2 / d1)))) {
			r1 + (S2 - r2) * (d2 / a1)
		} else {
			S1 - 0.25 * (S1 - r1 - (S2 - r2) * (a2 / d1))^2 / ((S2 - r2) * (d2 / a1 - a2 / d1))
		}
	}


	## part of QUEFTS model
	water_dependent_nutrient_uptake <- function(S1, WLY, d1, a1, r1) {
		if (S1 < r1 + WLY / d1) {
			S1
		} else if (S1 > r1 + 2 * WLY / a1 - WLY / d1) {
			WLY / a1
		} else {
			S1 - 0.25 * (S1 - r1 - WLY / d1)^2 / (WLY / a1 - WLY / d1)
		}
	}


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



do_quefts <- function(supply) {
	actualUptake <- cbind(supply, actual_uptake_tool(supply))
	minmax_Yield <- cbind(actualUptake, max_min_yields_tools(actualUptake))
 ## yield at zero input
	final_yield_tools(minmax_Yield)
}


#' computes target yield in tonnes/ha from a given NPK rate, it is used within profit optimization and other fucntions need to predict yield from NPK
#' @param QID a data frame containing soil NPK, WLY (kg/ha dry wt.),
#' @param rec recomended NPK rate
#' @returnType
#' @return target yield in ton/ha dry matter
#'
#' @author Meklit

QUEFTS_fertilizer <- function(QID, rec, HI=0.52) {

	QID$WLY <- QID$water_limited_yield
	## nutrient use efficiency of the crop
	## HI: Median for Nigeria=0.55 and Tanzania=0.52. Q3, Nigeria=0.63 and Tanzania=0.61
	crop_param <- cbind(NUE(HI=HI), data.frame(rN=0, rP=0, rK=0)) 

	QIn <- cbind(QID, crop_param)[, c("WLY", "aN", "dN", "aP", "dP", "aK", "dK", "rN", "rP", "rK", "soilN", "soilP", "soilK")]
	
 ## Supply of nutrients to the crop
	QIn$SN <- rec[1] + QIn$soilN
	QIn$SP <- rec[2] + QIn$soilP
	QIn$SK <- rec[3] + QIn$soilK

	do_quefts(QIn)
}


# RH this is yield at zero fertilizer 
# apparantly this is considered "current" yield. Because (for NG and TZ) the NPK supply 
# was estimated from current yield? But there is no evidence that no fertilizer was used? 

QUEFTS_no_fertilizer <- function(soil, country, wlyd) {
	Qinw <- data.frame(soil, WLY=wlyd, water_limited_yield=wlyd)
	CurrentYield <- QUEFTS_fertilizer(Qinw, c(0,0,0), HI=.55)
# this should not be needed:
	min(CurrentYield, wlyd)
}

