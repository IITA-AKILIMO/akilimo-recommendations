#SHORT DEF:   Wrapper function to calculate recommendations across all use cases.
#RETURNS:     Vector of with recommendation texts to display.
#DESCRIPTION: Function with all inputs required by the DST app, and calling use case-specific wrapper functions to calculated recommendations on IC, FR, PP and SP.
#             This function also calls on functions to send recommendation reports by SMS and/or email.
#INPUT:
#country    : Character, c("NG", "TZ", "GH", "RW", "BU")
#lat        : Numeric, Latitude in decimal degrees
#lon        : Numeric, Longitude in decimal degrees
#area  : Numeric,  value for area of the field, if the user does not give this value, it will be set at 1
#areaUnits  : Character, c("acre", "ha" or "m2")
#IC         : Logical, c(TRUE, FALSE), indicating if an intercrop (TRUE) or a monocrop (FALSE) is grown
#intercrop  : Character, c(NA, "maize", "sweetpotato"), Intercrop species grown, either maize or sweetpotato, or NA if monocrop, shold be in
#FR         : Logical, c(NA, TRUE, FALSE), indicating if fertilizer recommendations are requested, NA if IC == TRUE and country == "TZ"
#PP         : Logical, c(NA, TRUE, FALSE), indicating if planting practice recommendations are requested, NA if IC == TRUE or country == "TZ"
#SPP        : Logical, c(NA, TRUE, FALSE), indicating if scheduled planting - advice on planting date is requested, NA if IC == TRUE
#SPH        : Logical, c(NA, TRUE, FALSE), indicating if scheduled planting - advice on harvest date is requested, NA if IC == TRUE
#PD         : Character, Planting date (date format)
#HD         : Character, Harvest data (date format)
#PD_window  : planting day window, it should be c(0,1,2), zero if the user has one fixed PD, 1 if PD can be +- 1 month and 2 if PD can be +-2 months
#HD_window  : Harvest day window, it should be c(0,1,2), zero if the user has one fixed HD, 1 if HD can be +- 1 month and 2 if HD can be +-2 months
#fallowType  : Categorical: c("bush", "broad_leaves", "grass", "none"), type of fallow prior to land clearing
#fallowHeight: Categorical: c(NA, 100, 150, 200), height of the fallow prior to clearing
#fallowGreen : Logical: c(TRUE, FALSE), indicating if the fallow is lush, fresh and green (TRUE) or withered, dry or dead (FALSE)
#problemWeeds: Logical:  c(TRUE, FALSE), indicating the presence of any problem weeds that need to be controlled by herbicide (Tithonia, Imperata,...)
#tractor_plough: Logical, indicating if the user has access to a tractor with plough, NA if PP != TRUE
#tractor_harrow: Logical, indicating if the user has access to a tractor with harrow, NA if PP != TRUE
#tractor_ridger: Logical, indicating if the user has access to a tractor with ridger, NA if PP != TRUE
#cost_LMO_areaBasis: Categorical: c("areaUnit", "areaField"), indicating if the area basis for the cost of land operations is for 1 areaUnits, or for the entire field
#cost_tractor_ploughing: Cost for a ploughing operation by tractor c() for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField), NA if tractor_plough == FALSE
#cost_tractor_harrowing: Cost for a rarrowing operation by tractor for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField), NA if tractor_harrow == FALSE
#cost_tractor_ridging: Cost for a ridging operation by tractor for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField), NA if tractor_ridger == FALSE
#cost_manual_ploughing: Cost for a manual ploughing operation for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField)
#cost_manual_harrowing: Cost for a manual harrowing operation for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField)
#cost_manual_ridging: Cost for a manual ridging operation for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField)
#cost_weeding1: Cost for the first weeding operation for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField)
#cost_weeding2: Cost for the second and subsequent weeding operations for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField)
#ploughing   : Logical, c(NA, TRUE, FALSE), indicating if the user conducts a ploughing operation in current practice, NA if PP != TRUE
#harrowing   : Logical, c(NA, TRUE, FALSE), indicating if the user conducts a harrowing operation in current practice, NA if PP != TRUE
#ridging     : Logical, c(NA, TRUE, FALSE), indicating if the user ridges his/her field in current practice (NA, TRUE, FALSE), NA if PP != TRUE
#method_ploughing: Categorical: c("manual", "tractor", "N/A"), method of ploughing currently applied by the farmer. Note: "N/A" = not applicable becasue nPlough=0
#method_harrowing: Categorical: c("manual", "tractor", "N/A"), method of harrowing currently applied by the farmer. Note: "N/A" = not applicable becasue nHarrow=0
#method_ridging: Categorical: c("manual", "tractor", "N/A"), method of ridging currently applied by the farmer. Note: "N/A" = not applicable becasue ridges=FALSE
#FCY        : Farmer-reported current yield, in tonnes FM per ha (optional, default value = 11)
#CMP        : c(1,2,3,4,5),  Current maize performance, score on a scale of 1 (very yellow and stunted) .. 5 (tall and dark green), NA if IC != TRUE and FR != TRUE, or NA if the user does not know (NA = default)
#saleSF     : Logical, c(NA, TRUE, FALSE), indicating if the user is selling roots to a registered starch factory at factory-fixed prices
#nameSF     : c(NA, "AlliedAtlanticDistilleries", "MatnaStarch", "PsaltryMarketers", "PsaltryOutgrowers",	"Greentech", "ThaiFarm", "FJS"), Name of starch factory where roots will be sold, NA if saleSF = FALSE
#cassPD     : c("roots", "chips", "flour", "gari"), Type of cassava produce sold
#cassUW 	: c(1, 50, 100, 1000), Unit weight at which cassava produce is sold, in kg, common measures are 1 (per kg), 50 (per 50kg bag), 100 (per 100kg bag) and 1000 (per tonne).
#cassUP 	: Price of 1 cassava produce unit (fresh wt) in local currency, can be NA if user does not know and then 12000 NGN and ... TZS is used as default
#cassUP_m1: Price of 1 cassava produce unit (fresh wt) 1 month in the future, can be NA if users does not know;
#cassUP_m2: Price of 1 cassava produce unit (fresh wt) 2 month in the future, can be NA if users does not know;
#cassUP_p1: Price of 1 cassava produce unit (fresh wt) 1 month before, can be NA if users does not know
#cassUP_p2: Price of 1 cassava produce unit (fresh wt) 2 month before, can be NA if users does not know
#sweetPotatoPD: Type of sweet potato produce sold (tubers, flour), NA if IC != TRUE and country == "TZ"
#sweetPotatoUW: Unit weight at which sweet potato produce is sold, in kg; common measures are 1 (per kg), 50 (per 50kg bag), 100 (per 100kg bag), NA if IC != TRUE and country == "TZ"; can be NA if user does not know.
#sweetPotatoUP: Price of 1 sweet potato produce unit in local currency, NA if IC != TRUE and country == "TZ"; can be NA if user does not know.
#maizePD    : Type of maize produce sold (fresh cobs, dry cobs, grain), NA if IC != TRUE and country == "NG"
#maizeUW    : c(NA, 1, 50, 100), Unit weight at which maize produce is sold, in kg, common measures are 1 (per kg), 50 (per 50kg bag), 100 (per 100kg bag), NA if IC != TRUE and country == "NG" or NA if maizePD == "grain", can be NA if user does not know.
#maizeUP    : Price of 1 maize produce unit (or cob, if maizePC == TRUE) in local currency, NA if IC != TRUE and country == "NG", can be NA if user does not know.
#maxInv     : Maximal investment in fertilizer, for the area of the field in local currency, NA if FR != TRUE, default = NA (if user does not wish to set an investment ceiling)
#SMS        : Logical, c(TRUE, FALSE), indicating if recommendations must be sent by SMS to the user
#email      : Logical, c(TRUE, FALSE), indicating if recommendations must be sent by email to the user
#userPhoneCC: Country code of the phone number of the user requesting the recommendations (to send recommendations by SMS), default = NA (if user does not wish to receive recommendations by SMS), example 234 for Nigeria
#userPhoneNr: Phone number of the user requesting the recommendations, without the initial zero (to send recommendations by SMS), default = NA (if user does not wish to receive recommendations by SMS), excludes the initial zero, stored as numerical (e.g., 789123456)
#userName   : Name of the user requesting the recommendations (to be included in the email report), default = NA (if user does not wish to receive recommendations by email)
#userEmail  : Email address of the user requesting the recommendations (to be included in the email report), default = NA (if user does not wish to receive recommendations by email)
#userField  : Name or desciption of the field (to be included in the email report, and aid the user to recall for which field recommendations were requested), default = NA (if user does not wish to receive recommendations by email)
#riskAtt = c(0, 1, 2): Risk attitude of the farmer, with 0 being very risk-averse (low income farmers who cannot afford to loose on investment), 1 = risk-neutral and 2 = risk-loving (higher income farmers willing to take their chances for higher net returns)
#ureaavailable = TRUE 	: is urea available in the market
#ureaCostperBag = NA	: The cost of  abg of urea
#ureaBagWt = 50			: The weight of the bag of urea
#MOPavailable = TRUE
#MOPCostperBag = NA
#MOPBagWt = 50
#DAPavailable = TRUE
#DAPCostperBag = NA
#DAPBagWt = 50
#NPK201010available = TRUE
#NPK201010CostperBag = NA
#NPK201010BagWt =50
#NPK151515available = TRUE
#NPK151515CostperBag = NA
#NPK151515BagWt =50
#TSPavailable = TRUE
#TSPCostperBag = NA
#TSPBagWt = 50
#NPK171717available = TRUE
#NPK171717CostperBag = NA
#NPK171717BagWt = 50
#Nafakaavailable = FALSE
#NafakaCostperBag = NA
#NafakaBagWt = 50
#CANavailable = FALSE
#CANCostperBag = NA
#CANBagWt = 50
#SSPavailable = FALSE
#SSPCostperBag = NA
#SSPBagWt = 50
#newFert1name = NA		:this is for new fertilizer and if defined the N_cont, P2O5, K2O, (all in percent like 20, 15, ..)and CostperBag & BagWt have to be provided. if any of these input is missing, OtherFertilizers should be NULL
#newFert1N_cont=NA		: N content of the new fertilizer
#newFert1P2O5=NA	    : P2O5 of the new fertilizer
#newFertK2O = NA		: K2O of the new fertilizer
#newFertCostperBag=NA		: cost per bag,
#newFert1BagWt=NA			: The weight of the bag with new fertilizer
#newFert2name = NA
#newFert2N_cont=NA
#newFert2P2O5=NA
#newFert2K2O = NA
#newFert2CostperBag=NA
#newFert2BagWt=NA
#newFert3name = NA
#newFert3N_cont=NA
#newFert3P2O5=NA
#newFert3K2O = NA
#newFert3CostperBag=NA
#newFert3BagWt=NA
#newFert4name = NA
#newFert4N_cont=NA
#newFert4P2O5=NA
#newFert4K2O = NA
#newFert4CostperBag	=NA
#newFert4BagWt=NA
#newFert5name = NA
#newFert5N_cont=NA
#newFert5P2O5=NA
#newFert5K2O = NA
#newFert5CostperBag=NA
#newFert5BagWt=NA

getRecommendation <- function(country = c("NG", "TZ", "RW", "GH", "BU"),
                               lat,
                               lon,
                               area,
                               areaUnits = c("acre", "ha", "m2", "Are"),
                               IC = c(TRUE, FALSE),
                               intercrop = c("maize", "sweetpotato"),
                               FR = c(TRUE, FALSE),
                               PP = c(TRUE, FALSE),
                               SPP = c(TRUE, FALSE),
                               SPH = c(TRUE, FALSE),
                               PD,
                               HD,
                               PD_window = c(0, 1, 2),
                               HD_window = c(0, 1, 2),
              							   fallowType = c("bush", "broad_leaves", "grass", "none"),
              							   fallowHeight = c(NA, 100, 150, 200),
              							   fallowGreen = c(TRUE, FALSE),
              							   problemWeeds = c(TRUE, FALSE),
              							   tractor_plough = c(TRUE, FALSE),
              							   tractor_harrow = c(TRUE, FALSE),
              							   tractor_ridger = c(TRUE, FALSE),
              							   cost_LMO_areaBasis = c("areaUnit", "areaField"),
              							   cost_tractor_ploughing = NA,
              							   cost_tractor_harrowing = NA,
              							   cost_tractor_ridging = NA,
              							   cost_manual_ploughing = NA,
              							   cost_manual_harrowing = NA,
              							   cost_manual_ridging = NA,
              							   cost_weeding1 = NA,
              							   cost_weeding2 = NA,
              							   ploughing = c(TRUE, FALSE),
              							   harrowing = c(TRUE, FALSE),
              							   ridging = c(TRUE, FALSE),
              							   method_ploughing = c("manual", "tractor", "N/A"),
              							   method_harrowing = c("manual", "tractor", "N/A"),
              							   method_ridging = c("manual", "tractor", "N/A"),
                               FCY,
                               CMP,
                               saleSF = c(TRUE, FALSE),
                               nameSF = c(NA, "AlliedAtlanticDistilleries", "MatnaStarch", "PsaltryMarketers", "PsaltryOutgrowers",	"Greentech", "ThaiFarm", "FJS"),
                               cassPD = c("roots", "chips", "flour", "gari"),
                               cassUW = c(1, 50, 100, 1000),
                               cassUP,
              							   cassUP_m1,
              							   cassUP_m2,
              							   cassUP_p1,
              							   cassUP_p2,
                               maizePD =  c("fresh_cob", "grain"),
                               maizeUW = c(1, 50, 100),
                               maizeUP,
              							   sweetPotatoPD = c("tubers", "flour"),
              							   sweetPotatoUW = NA,
              							   sweetPotatoUP = NA,
                               maxInv = NA,
                               SMS = c(TRUE, FALSE),
                               email = c(TRUE, FALSE),
                               userPhoneCC,
                               userPhoneNr,
                               userName,
                               userEmail,
                               userField,
              							   riskAtt = c(0, 1, 2),
              							   NPK201216available = c(TRUE, FALSE), NPK201216CostperBag = NA, NPK201216BagWt = 50,
              							   ureaavailable = c(TRUE, FALSE), ureaCostperBag = NA, ureaBagWt =50,
              							   MOPavailable = c(TRUE, FALSE), MOPCostperBag = NA, MOPBagWt =50,
              							   DAPavailable = c(TRUE, FALSE), DAPCostperBag = NA, DAPBagWt =50,
              							   NPK201010available = c(TRUE, FALSE), NPK201010CostperBag = NA, NPK201010BagWt = 50,
              							   NPK151515available = c(TRUE, FALSE), NPK151515CostperBag = NA, NPK151515BagWt =50,
              							   TSPavailable = c(TRUE, FALSE), TSPCostperBag = NA, TSPBagWt =50,
              							   NPK171717available = c(TRUE, FALSE), NPK171717CostperBag = NA, NPK171717BagWt = 50,
              							   CANavailable = c(TRUE, FALSE), CANCostperBag = NA, CANBagWt =50,
              							   SSPavailable = c(TRUE, FALSE), SSPCostperBag = NA, SSPBagWt = 50,
              							   NPK112221available = c(TRUE, FALSE), NPK112221CostperBag = NA, NPK112221BagWt = 50, 
              							   NPK251010available = c(TRUE, FALSE), NPK251010CostperBag = NA, NPK251010BagWt = 50, 
              							   NPK152020available = c(TRUE, FALSE), NPK152020CostperBag = NA, NPK152020BagWt = 50, 
              							   NPK23105available = c(TRUE, FALSE), NPK23105CostperBag = NA, NPK23105BagWt = 50, 
              							   NPK123017available = c(TRUE, FALSE), NPK123017CostperBag = NA, NPK123017BagWt = 50,
              							   FOMIIMBURAavailable = c(TRUE, FALSE), FOMIIMBURACostperBag = NA, FOMIIMBURABagWt = 50, 
              							   FOMIBAGARAavailable = c(TRUE, FALSE), FOMIBAGARACostperBag = NA, FOMIBAGARABagWt = 50, 
              							   FOMITOTAHAZAavailable = c(TRUE, FALSE), FOMITOTAHAZACostperBag = NA, FOMITOTAHAZABagWt = 50,
              							   newFert1name = c(TRUE, FALSE) ,newFert1N_cont = NA,newFert1P2O5 = NA, newFert1K2O = NA,newFert1CostperBag = NA,newFert1BagWt = NA,
              							   newFert2name = c(TRUE, FALSE),newFert2N_cont = NA,newFert2P2O5 = NA, newFert2K2O = NA,newFert2CostperBag = NA,newFert2BagWt = NA,
              							   newFert3name = c(TRUE, FALSE), newFert3N_cont = NA, newFert3P2O5 = NA, newFert3K2O = NA, newFert3CostperBag = NA, newFert3BagWt = NA,
              							   newFert4name = c(TRUE, FALSE), newFert4N_cont = NA, newFert4P2O5 = NA, newFert4K2O = NA, newFert4CostperBag = NA, newFert4BagWt = NA,
              							   newFert5name = c(TRUE, FALSE), newFert5N_cont = NA, newFert5P2O5 = NA, newFert5K2O = NA, newFert5CostperBag = NA, newFert5BagWt = NA)
              							   {

  setwd("/home/akilimo/projects/akilimo_recommendation")
  source("AkilimoFunctions_5C.R")

  library(raster)
  library(dismo)
  library(randomForest)
  library(caret)
  library(rgdal)
  library(parallel)
  library(foreach)
  library(limSolve)
  library(httr)
  library(tidyr)
  library(plyr)
  library(leaflet)
  library(mapview)
  library(flexdashboard)
  library(lubridate)
  library(kableExtra)
  library(scales)
  library(mailR)

  
  Default_prices <- read.csv("/home/akilimo/projects/akilimo_recommendation/Default_prices.csv")


	fertilizers <-  fertilizerFunc(ureaavailable=ureaavailable, ureaCostperBag=ureaCostperBag,ureaBagWt=ureaBagWt,
			MOPavailable=MOPavailable, MOPCostperBag=MOPCostperBag, MOPBagWt=MOPBagWt,
			DAPavailable=DAPavailable, DAPCostperBag=DAPCostperBag, DAPBagWt=DAPBagWt,
			NPK201010available=NPK201010available, NPK201010CostperBag=NPK201010CostperBag, NPK201010BagWt=NPK201010BagWt,
			NPK151515available=NPK151515available, NPK151515CostperBag=NPK151515CostperBag, NPK151515BagWt=NPK151515BagWt,
			TSPavailable=TSPavailable, TSPCostperBag=TSPCostperBag, TSPBagWt=TSPBagWt,
			NPK171717available=NPK171717available, NPK171717CostperBag=NPK171717CostperBag, NPK171717BagWt=NPK171717BagWt,
			NPK201216available= NPK201216available, NPK201216CostperBag =NPK201216CostperBag, NPK201216BagWt=NPK201216BagWt,
			CANavailable=CANavailable, CANCostperBag=CANCostperBag, CANBagWt=CANBagWt,
			SSPavailable=SSPavailable, SSPCostperBag=SSPCostperBag, SSPBagWt=SSPBagWt,
			NPK112221available = NPK112221available, NPK112221CostperBag = NPK112221CostperBag, NPK112221BagWt = NPK112221BagWt, 
			NPK251010available = NPK251010available, NPK251010CostperBag = NPK251010CostperBag, NPK251010BagWt = NPK251010BagWt, 
			NPK152020available = NPK152020available, NPK152020CostperBag = NPK152020CostperBag, NPK152020BagWt = NPK152020BagWt, 
			NPK23105available = NPK23105available, NPK23105CostperBag = NPK23105CostperBag, NPK23105BagWt = NPK23105BagWt, 
			NPK123017available = NPK123017available, NPK123017CostperBag = NPK123017CostperBag, NPK123017BagWt = NPK123017BagWt,
			
			FOMIIMBURAavailable = FOMIIMBURAavailable, FOMIIMBURACostperBag = FOMIIMBURACostperBag, FOMIIMBURABagWt = FOMIIMBURABagWt, 
			FOMIBAGARAavailable = FOMIBAGARAavailable, FOMIBAGARACostperBag = FOMIBAGARACostperBag, FOMIBAGARABagWt = FOMIBAGARABagWt, 
			FOMITOTAHAZAavailable = FOMITOTAHAZAavailable, FOMITOTAHAZACostperBag = FOMITOTAHAZACostperBag, FOMITOTAHAZABagWt = FOMITOTAHAZABagWt,
			
			newFert1name=newFert1name, newFert1N_cont=newFert1N_cont, newFert1P2O5=newFert1P2O5,
			newFert1K2O=newFert1K2O, newFert1CostperBag=newFert1CostperBag, newFert1BagWt=newFert1BagWt,
			newFert2name=newFert2name, newFert2N_cont=newFert2N_cont, newFert2P2O5=newFert2P2O5,
			newFert2K2O=newFert2K2O, newFert2CostperBag=newFert2CostperBag, newFert2BagWt=newFert2BagWt,
			newFert3name=newFert3name, newFert3N_cont=newFert3N_cont, newFert3P2O5=newFert3P2O5,
			newFert3K2O=newFert3K2O, newFert3CostperBag=newFert3CostperBag, newFert3BagWt=newFert3BagWt,
			newFert4name=newFert4name, newFert4N_cont=newFert4N_cont, newFert4P2O5=newFert4P2O5,
			newFert4K2O=newFert4K2O, newFert4CostperBag=newFert4CostperBag, newFert4BagWt=newFert4BagWt,
			newFert5name=newFert5name, newFert5N_cont=newFert5N_cont, newFert5P2O5=newFert5P2O5,
			newFert5K2O=newFert5K2O, newFert5CostperBag=newFert5CostperBag, newFert5BagWt=newFert5BagWt, country=country)



	if(nameSF == "NA") nameSF <- NA
	if(userName == "NA") userName <- "AKILIMO farmer"
	if(userField == "NA") userField <- paste(round(lat, digits = 3), round(lon, digits = 3), sep="_")

	if(method_ploughing == "NA") method_ploughing <- "N/A"
	if(method_ridging == "NA") method_ridging <- "N/A"


	# if(cassUP_m1 == 0) cassUP_m1 <- NA
	# if(cassUP_m2 == 0) cassUP_m2 <- NA
	# if(cassUP_p1 == 0) cassUP_p1 <- NA
	# if(cassUP_p2 == 0) cassUP_p2 <- NA

	if(sweetPotatoUW == 0) sweetPotatoUW <- 1000 ## if it is not given default is a ton
	#if(sweetPotatoUP == 0) sweetPotatoUP <- NA

	#if(cassUP == 0) cassUP <- NA
	#if(maizeUP == 0) maizeUP <- NA
	if(cost_manual_ploughing == 0) cost_manual_ploughing <- NA
	if(cost_manual_harrowing == 0) cost_manual_harrowing <- NA

	if(cost_manual_ridging == 0) cost_manual_ridging <- NA
	if(cost_tractor_ploughing == 0) cost_tractor_ploughing <- NA
	if(cost_tractor_harrowing == 0) cost_tractor_harrowing <- NA
	if(cost_tractor_ridging == 0) cost_tractor_ridging <- NA

	if(cost_weeding1 == 0) cost_weeding1 <- NA
	if(cost_weeding2 == 0) cost_weeding2 <- NA
	if(maizeUW == 0) maizeUW <- NA
	if(maxInv == 0) maxInv <- NA
	if(fallowHeight == 0) fallowHeight <- NA


	PD <- as.Date(PD, format = "%Y-%m-%d")
	HD <- as.Date(HD, format = "%Y-%m-%d")

	## if cassava is to be sold to a processing factory, there should be a default price by factry and product
	# calculating rootUP based on cassUP, cassUW and conversion factor for cassava product sold
	rootConv <- data.frame(cassPD = c("roots", "chips", "flour", "gari"), conversion = c(1, 3, 3.2, 3.5))


	if(saleSF){
	  SF <- read.csv("/home/akilimo/projects/akilimo_recommendation/starchPrices.csv")
	  SF <- SF[SF$starchFactory == nameSF,]
	  cassUP <- max(SF$price)
	  cassUW <- 1000
	}else{
	  if(cassUP == 0 & cassPD=="roots" & country=="NG"){cassUP = Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "cassUP",]$Price; cassUW = 1000}
	  if(cassUP == 0 & cassPD=="chips" & country=="NG"){cassUP = 36000; cassUW = 1000}
	  if(cassUP == 0 & cassPD=="flour" & country=="NG"){cassUP = 38400; cassUW = 1000}
	  if(cassUP == 0 & cassPD=="gari"  & country=="NG"){cassUP = 42000; cassUW = 1000}

	  if(cassUP == 0 & cassPD=="roots" & country=="TZ"){cassUP = Default_prices[Default_prices$Country == "TZ" & Default_prices$Item == "cassUP",]$Price; cassUW = 1000}
	  if(cassUP == 0 & cassPD=="chips" & country=="TZ"){cassUP = 540000; cassUW = 1000}
	  if(cassUP == 0 & cassPD=="flour" & country=="TZ"){cassUP = 576000; cassUW = 1000}
	  if(cassUP == 0 & cassPD=="gari"  & country=="TZ"){cassUP = 630000; cassUW = 1000}
	  
	  if(cassUP == 0 & cassPD=="roots" & country=="GH"){cassUP = Default_prices[Default_prices$Country == "GH" & Default_prices$Item == "cassUP",]$Price; cassUW = 1000}
	  if(cassUP == 0 & cassPD=="chips" & country=="GH"){cassUP = 450; cassUW = 1000}
	  if(cassUP == 0 & cassPD=="flour" & country=="GH"){cassUP = 450; cassUW = 1000}
	  if(cassUP == 0 & cassPD=="gari"  & country=="GH"){cassUP = 450; cassUW = 1000}
	  
	  if(cassUP == 0 & cassPD=="roots" & country=="RW"){cassUP = Default_prices[Default_prices$Country == "RW" & Default_prices$Item == "cassUP",]$Price; cassUW = 1000}
	  if(cassUP == 0 & cassPD=="chips" & country=="RW"){cassUP = 75000; cassUW = 1000}
	  if(cassUP == 0 & cassPD=="flour" & country=="RW"){cassUP = 75000; cassUW = 1000}
	  if(cassUP == 0 & cassPD=="gari"  & country=="RW"){cassUP = 75000; cassUW = 1000}
	  
	  if(cassUP == 0 & cassPD=="roots" & country=="BU"){cassUP = Default_prices[Default_prices$Country == "BU" & Default_prices$Item == "cassUP",]$Price; cassUW = 1000}
	  if(cassUP == 0 & cassPD=="chips" & country=="BU"){cassUP = 700000; cassUW = 1000}
	  if(cassUP == 0 & cassPD=="flour" & country=="BU"){cassUP = 700000; cassUW = 1000}
	  if(cassUP == 0 & cassPD=="gari"  & country=="BU"){cassUP = 700000; cassUW = 1000}

	}


	rootUP <- cassUP / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000

	rootUP_m1 <- cassUP_m1 / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000
	rootUP_m2 <- cassUP_m2 / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000
	rootUP_p1 <- cassUP_p1 / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000
	rootUP_p2 <- cassUP_p2 / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000

	# calculating cobUP based on maizeUP, maizeUW and conversion from grain to cobs if maizePD == "grain"
	if(maizeUP == 0 & maizePD == "fresh_cob"){
	  maizeUP <- Default_prices[Default_prices$Item == "maize_freshCob",]$Price #default price for 1 large fresh cob
	  maizeUW <- 1
	}

	if(maizeUP == 0  & maizePD == "grain") {
	  maizeUP <- Default_prices[Default_prices$Item == "maize_grain",]$Price  #default price for 1 kg of maize grain
	  maizeUW <- 1
	}

	maizeUW <- ifelse(maizePD == "fresh_cob", maizeUW, as.numeric(as.character(maizeUW)))
	cobUP <- ifelse(maizePD == "fresh_cob", maizeUP, maizeUP / maizeUW / 7.64) #1 kg of grain ~ 7.64 cobs

	# calculating tuberUP based on sweetPotatoUP, sweetPotatoUW and conversion factor for sweetPotato product sold
	tuberConv <- data.frame(sweetPotatoPD = c("tubers", "flour"), conversion = c(1, 3.2))
	if(sweetPotatoUP == 0 & sweetPotatoPD=="tubers" & country=="TZ"){sweetPotatoUP = Default_prices[Default_prices$Item == "sweetPotato_tuber",]$Price; sweetPotatoUW = 1000}
	if(sweetPotatoUP == 0 & sweetPotatoPD=="flour"  & country=="TZ"){sweetPotatoUP = Default_prices[Default_prices$Item == "sweetPotato_flour",]$Price; sweetPotatoUW = 1000}

	tuberUP <- sweetPotatoUP / sweetPotatoUW / tuberConv[tuberConv$sweetPotatoPD==sweetPotatoPD,]$conversion * 1000

	# calculating the field area
	# areaHa <- area / ifelse(areaUnits=="ha", 1, ifelse(areaUnits=="acre", 2.47105, 10000))
	
	areaHa <- area / ifelse(areaUnits=="ha", 1, ifelse(areaUnits=="acre", 2.47105, ifelse(areaUnits=="are", 100, 10000)))


	# create dataframe with cost of land management operations in unit of ha
	costLMO <- data.frame(operation = c(rep(c("ploughing", "harrowing", "ridging"),2), "weeding1", "weeding2"),
	                      method = c(rep("manual", 3), rep("tractor", 3), NA, NA),
	                      cost = c(cost_manual_ploughing, cost_manual_harrowing, cost_manual_ridging, cost_tractor_ploughing, cost_tractor_harrowing, cost_tractor_ridging, cost_weeding1, cost_weeding2),
	                      area = ifelse(cost_LMO_areaBasis=="areaField", areaHa, ifelse(areaUnits=="acre", 0.404686, ifelse(areaUnits=="ha", 1, 0.0001))))

	costLMO_MD <- costLMO
	costLMO$costHa <- costLMO$cost / costLMO$area
	costLMO <- subset(costLMO, select=-c(area, cost))


	# add default values for LMO operations if missing
	if(country == "NG"){
	  if(is.na(cost_manual_ploughing))                   costLMO[costLMO$operation=="ploughing" & costLMO$method=="manual" ,]$costHa <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "manual_ploughing", ]$Price
	  if(is.na(cost_manual_harrowing))                   costLMO[costLMO$operation=="harrowing" & costLMO$method=="manual" ,]$costHa <- 15000
	  if(is.na(cost_manual_ridging))                     costLMO[costLMO$operation=="ridging"   & costLMO$method=="manual" ,]$costHa <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "manual_ridging", ]$Price
	  if(is.na(cost_tractor_ploughing) & tractor_plough) costLMO[costLMO$operation=="ploughing" & costLMO$method=="tractor" ,]$costHa <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "tractor_ploughing", ]$Price
	  if(is.na(cost_tractor_harrowing) & tractor_harrow) costLMO[costLMO$operation=="harrowing" & costLMO$method=="tractor" ,]$costHa <- 6000
	  if(is.na(cost_tractor_ridging)   & tractor_ridger) costLMO[costLMO$operation=="ridging"   & costLMO$method=="tractor" ,]$costHa <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "tractor_ridging", ]$Price
	  if(is.na(cost_weeding1))                           costLMO[costLMO$operation=="weeding1", ]$costHa                             <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "weeding1", ]$Price
	  if(is.na(cost_weeding2))                           costLMO[costLMO$operation=="weeding2", ]$costHa                             <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "weeding2", ]$Price

	}else if (country == "TZ") {
	  if(is.na(cost_manual_ploughing))                   costLMO[costLMO$operation=="ploughing" & costLMO$method=="manual" ,]$costHa <-  Default_prices[Default_prices$Country == "TZ" & Default_prices$Item == "manual_ploughing", ]$Price
	  if(is.na(cost_manual_harrowing))                   costLMO[costLMO$operation=="harrowing" & costLMO$method=="manual" ,]$costHa <- 150000 #* 2.47105
	  if(is.na(cost_manual_ridging))                     costLMO[costLMO$operation=="ridging"   & costLMO$method=="manual" ,]$costHa <- Default_prices[Default_prices$Country == "TZ" & Default_prices$Item == "manual_ridging", ]$Price #* 2.47105
	  if(is.na(cost_tractor_ploughing) & tractor_plough) costLMO[costLMO$operation=="ploughing" & costLMO$method=="tractor" ,]$costHa <- Default_prices[Default_prices$Country == "TZ" & Default_prices$Item == "tractor_ploughing", ]$Price# * 2.47105
	  if(is.na(cost_tractor_harrowing) & tractor_harrow) costLMO[costLMO$operation=="harrowing" & costLMO$method=="tractor" ,]$costHa <- 100000# * 2.47105
	  if(is.na(cost_tractor_ridging)   & tractor_ridger) costLMO[costLMO$operation=="ridging"   & costLMO$method=="tractor" ,]$costHa <-Default_prices[Default_prices$Country == "TZ" & Default_prices$Item == "tractor_ridging", ]$Price# * 2.47105
	  if(is.na(cost_weeding1))                           costLMO[costLMO$operation=="weeding1", ]$costHa                             <- Default_prices[Default_prices$Country == "TZ" & Default_prices$Item == "weeding1", ]$Price #* 2.47105
	  if(is.na(cost_weeding2))                           costLMO[costLMO$operation=="weeding2", ]$costHa                             <- Default_prices[Default_prices$Country == "TZ" & Default_prices$Item == "weeding2", ]$Price #* 2.47105


	}


	if(!is.na(cost_manual_ploughing)|!is.na(cost_manual_harrowing)|!is.na(cost_manual_ridging)|!is.na(cost_tractor_ploughing)|
	   !is.na(cost_tractor_harrowing)|!is.na(cost_tractor_ridging)|!is.na(cost_weeding1)|!is.na(cost_weeding2)){
	  costLMO_MD$area <- paste(costLMO_MD$area, areaUnits, sep="")
	  write.csv(costLMO_MD, "costLMO.csv", row.names = FALSE)
	}else{
	  costLMO_MD <- costLMO
	  names(costLMO_MD) <- c("operation", "method", "cost")
	  costLMO_MD$area <- "1ha"
	  costLMO_MD$cost <- formatC(signif(costLMO_MD$cost, digits=3), format="f", big.mark=",", digits=0)
	  write.csv(costLMO_MD, "costLMO.csv", row.names = FALSE)

	}

	### dates and weeks
	#pd         : Character, Planting date, in format of the ith day of the year (as.numeric(strftime(PD, format = "%j")))
	#pw         : planting week of the year = as.numeric(format(PD, format = "%W"))
	#hd         : harvest day of the year = as.numeric(strftime(HD, format = "%j"))
	#hw         : harvest week of the year = as.numeric(format(HD, format = "%W"))
	#had        : age of the crop at harvest in days since planting = as.numeric(HD - PD), number of days the crop was on the field
	#haw        : age of the crop at harvest in weeks since planting = round(had / 7), number of weeks the crop was on the field

	pd <- as.numeric(strftime(PD, format = "%j"))
	pw <- as.numeric(strftime(PD, format = "%W"))
	hd <- as.numeric(strftime(HD, format = "%j"))
	hw <- as.numeric(strftime(HD, format = "%W"))
	had <- as.numeric(as.Date(HD) - as.Date(PD))
	haw <- round(had / 7)


	# generate list with requested recommendations
	res <- list()
	recText <- list()

	res[["PP"]] <- NULL
	res[["SP"]] <- NULL

	FRrecom <- NULL
	ICrecom <- NULL
	PPrecom <- FALSE
	SPrecom <- NULL


	TRNS <- read.csv("translations_TEST.csv",  stringsAsFactors = FALSE)
	recloc_ng <- gsub(pattern = "\"",replacement = "",TRNS$recloc[1]) ; 	recloc_tz <- gsub(pattern = "\"",replacement = "",TRNS$recloc[2]) ; recloc_rw <- gsub(pattern = "\"",replacement = "",TRNS$recloc[3]) ; 
	frnotrec_ng <- gsub(pattern = "\"",replacement = "",TRNS$frnotrec[1]) ;	frnotrec_tz <- gsub(pattern = "\"",replacement = "",TRNS$frnotrec[2]) ; frnotrec_rw <- gsub(pattern = "\"",replacement = "",TRNS$frnotrec[3]) ; 
	spinfo_ng <- gsub(pattern = "\"",replacement = "",TRNS$spinfo[1]) ; spinfo_rw <- gsub(pattern = "\"",replacement = "",TRNS$spinfo[3]) ; spinfo_tz <- gsub(pattern = "\"",replacement = "",TRNS$spinfo[2])


	if(FR == TRUE)   {res[["FR"]] <- getFRrecommendations(lat = lat,
	                                                      lon = lon,
	                                                      pd = pd,
	                                                      pw = pw,
	                                                      HD=HD,
	                                                      had=had,
	                                                      maxInv = maxInv,
	                                                      fertilizers=fertilizers,
	                                                      rootUP = rootUP,
	                                                      areaHa=areaHa,
	                                                      country=country,
	                                                      FCY=FCY,
	                                                      riskAtt = riskAtt)

                                          	if(all(res$FR == "We do not have fertilizer recommendation for your location because your location is out of the recommendation domain AKILIMO is currently serving.") & country == "NG"){
                                          	  FRrecom <- FALSE
                                          	  recText[["FR"]] <- res$FR
                                          	}else if (all(res$FR == "We do not have fertilizer recommendation for your location because your location is out of the recommendation domain AKILIMO is currently serving.") & country == "GH"){
                                          	  FRrecom <- FALSE
                                          	  recText[["FR"]] <- res$FR
                                          	}else if (all(res$FR == "Hatuna mapendekezo yoyote  kwa eneo lako kwa sababu eneo lako liko nje la eneo ambalo AKILIMO linafanya kazi kwa sasa") & country == "TZ"){
                                          	  FRrecom <- FALSE
                                          	  recText[["FR"]] <- res$FR
                                          	}else if (all(res$FR == "kinyarwanda here") & country == "RW"){
                                          	  FRrecom <- FALSE
                                          	  recText[["FR"]] <- res$FR
                                          	}else{
                                          	  if(res[["FR"]]$rec$NR > 0){
                                          	    FRrecom <- TRUE
                                          	    recText[["FR"]] <- getFRrecText(ds = res$FR , country = country, fertilizers=fertilizers, rootUP=rootUP)
                                          	    write.csv(recText$FR, 'FR_recText.csv', row.names = FALSE)
                                          	    FR_MarkdownText(rr=res$FR, fertilizers=fertilizers, userName=userName, country=country,
                                          	                    userPhoneNr=userPhoneNr, userField=userField, area=area, areaUnits=areaUnits,
                                          	                    PD =PD, HD=HD, email=email, lat=lat, lon=lon, userPhoneCC=userPhoneCC,
                                          	                    rootUP = rootUP, cassPD = cassPD, cassUW = cassUW, maxInv = maxInv)
                                          	    fertilizerAdviseTable(FR=TRUE, IC=FALSE, country=country, areaUnits=areaUnits)
                                          	  }else{
                                          	    FRrecom <- FALSE
                                          	    if(country == "NG" | country == "GH" | country == "BU") {
                                          	      recText[["FR"]] <-frnotrec_ng
                                          	    }else if (country == "RW") {
                                          	      recText[["FR"]] <-frnotrec_rw
                                          	    }else if(country == "TZ") {
                                          	      recText[["FR"]] <-frnotrec_tz
                                          	    }
                                          	  }
                                          	}
                                          	 
                                          	
                              	}

	### if urea or NPK 151515 is not available, no recommendation is possible.TODO
	if(IC == TRUE & country == "NG")                        {res[["IC"]] <- getICrecommendations(areaHa = areaHa,
	                                                                           CMP = CMP,
	                                                                           cobUP = cobUP,
	                                                                           fertilizers = fertilizers,
	                                                                           riskAtt = riskAtt)
          	                                                      if (nrow(res$IC[[2]]) > 0){
          	                                                        ICrecom <- TRUE
          	                                                        recText[["IC"]] <- getICrecText(ds = res$IC, maizePD)
          	                                                        write.csv(res$IC, 'IC_rec.csv', row.names = FALSE)
          	                                                        write.csv(recText$IC, 'IC_recText.csv', row.names = FALSE)
          	                                                        IC_MarkdownText(rr = res$IC, fertilizers=fertilizers, userName=userName, country=country,
          	                                                                        userPhoneNr=userPhoneNr, userField=userField, area=area, areaUnits=areaUnits,
          	                                                                        PD =PD, HD=HD, email=email, lat=lat, lon=lon, userPhoneCC=userPhoneCC,maizeUW=maizeUW,
          	                                                                        maizePD=maizePD,cassUW = cassUW, saleSF = saleSF, nameSF = nameSF, rootUP = rootUP, cassPD = cassPD, maxInv = maxInv, CMP=CMP, maizeUP=maizeUP)
          	                                                        fertilizerAdviseTable(FR=FALSE, IC=TRUE, country = "NG", areaUnits=areaUnits)
          	                                                      }else{
          	                                                        ICrecom <- FALSE
          	                                                        recText[["IC"]] <- res[["IC"]]$rec$reason_F
          	                                                      }
	                                                          }


	### if NPK 171717  is not available, no recommendation is possible.
	if(IC == TRUE & country == "TZ")                        {res[["IC"]] <- getCISrecommendations(areaHa = areaHa,
	                                                                                           FCY = FCY,
	                                                                                           tuberUP = tuberUP,
	                                                                                           rootUP=rootUP,
	                                                                                           fertilizers = fertilizers,
	                                                                                           riskAtt = riskAtt)
                                                            	if (nrow(res$IC[[2]]) > 0){
                                                            	  ICrecom <- TRUE
                                                            	  recText[["IC"]] <- getCISrecText(ds = res$IC)
                                                            	  
                                                            	  write.csv(recText$IC, 'CIS_recText.csv', row.names = FALSE)
                                                            	  CIS_MarkdownText(rr = res$IC, fertilizers=fertilizers, userName=userName, country=country,
                                                            	                   userPhoneNr=userPhoneNr, userField=userField, area=area, areaUnits=areaUnits,
                                                            	                   PD =PD, HD=HD, email=email, lat=lat, lon=lon, userPhoneCC=userPhoneCC,
                                                            	                   sweetPotatoUP =sweetPotatoUP, sweetPotatoPD=sweetPotatoPD, sweetPotatoUW=sweetPotatoUW,
                                                            	                   rootUP = rootUP, cassUW = cassUW, cassPD = cassPD, maxInv = maxInv, tuberUP=tuberUP)

                                                            	  fertilizerAdviseTable(FR=FALSE, IC=TRUE, country = "TZ", areaUnits=areaUnits)
                                                            	}else{
                                                            	  ICrecom <- FALSE
                                                            	  recText[["IC"]] <- res[["IC"]]$rec$reason_F
                                                            	}
                                                        	}


	if(PP == TRUE)                        {res[["PP"]] <- getPPrecommendations(areaHa = areaHa,
	                                                                           costLMO = costLMO,
	                                                                           ploughing = ploughing,
	                                                                           ridging = ridging,
	                                                                           method_ploughing = method_ploughing,
	                                                                           method_ridging = method_ridging,
	                                                                           FCY = FCY,
	                                                                           rootUP = rootUP,
	                                                                           riskAtt=riskAtt)

                                                  	recText[["PP"]] <- getPPrecText(ds = res$PP , country = country)
                                                  	write.csv(res$PP, 'PP_rec.csv', row.names = FALSE)
                                                  	write.csv(recText$PP, 'PP_recText.csv', row.names = FALSE)
                                                  	PP_MarkdownText(userName, country, userPhoneNr, userField, area, areaUnits, PD, HD, email,lat, lon,rootUP,cassPD,cassUW,
                                                  	                maxInv, ploughing, ridging, method_ploughing, method_ridging, userPhoneCC)

                                                  	}


	if(SPP == TRUE | SPH == TRUE)         {if(PD_window == 0 & HD_window ==0){
                                          	  SPrecom <- FALSE
                                          	  recText[["SP"]] <- if(country == "NG" | country == "GH"){
                                          	    "AKILIMO provides advice for schedule planting if only at least your planting or harvest time or both are flexible. Please provide this information and you will be advised when the best time is for your location."

                                          	    }else{
                                          	    "AKILIMO hutoa ushauri wa upandaji wa ratiba ikiwa angalau wakati wako wa upandaji au wakati wa kuvuna au zote mbili zinabadilika. Tafadhali toa habari hii na utashauriwa wakati mzuri wa kupanda na kuvuna kwa eneo lako"
                                          	  }
                                      	}else{
                                      	  res[["SP"]] <- getSPrecommendations(areaHa = areaHa,
                                      	                                      country = country,
                                      	                                      lat = lat,
                                      	                                      lon = lon,
                                      	                                      PD = PD,
                                      	                                      HD = HD,
                                      	                                      PD_window = PD_window,
                                      	                                      HD_window = HD_window,
                                      	                                      saleSF = saleSF,
                                      	                                      nameSF = nameSF,
                                      	                                      FCY = FCY,
                                      	                                      rootUP = rootUP,
                                      	                                      rootUP_m1 = rootUP_m1,
                                      	                                      rootUP_m2 = rootUP_m2,
                                      	                                      rootUP_p1 = rootUP_p1,
                                      	                                      rootUP_p2 = rootUP_p2)

                                      	  if(!is.data.frame(res[["SP"]])){
                                      	    SPrecom <- FALSE
                                      	    recText[["SP"]] <- res$SP
                                      	  }else{
                                      	    SPrecom <- TRUE
                                      	    recText[["SP"]] <- getSPrecText(ds = res$SP , country = country, PD, HD)
                                      	    write.csv(recText$SP, 'SP_recText.csv', row.names = FALSE)
                                      	    SP_MarkdownText(userName, country, userPhoneNr, userField, area, areaUnits, PD, HD, email,lat, lon,saleSF, nameSF,
                                      	                    maxInv, ploughing, ridging, method_ploughing, method_ridging, userPhoneCC, CMP,riskAtt,
                                      	                    PD_window, HD_window,cassPD,cassUW,cassUP,cassUP_m1, cassUP_m2, cassUP_p1,cassUP_p2)
                                      	  }
                                      	}
	                                   }



      if(is.null(FRrecom)){
        FRrecom <- FALSE
      }
      if(is.null(ICrecom)){
       ICrecom <- FALSE
      }
	    if(!is.null(res[["PP"]])){
	     PPrecom <- TRUE
	    }
      if(is.null(SPrecom)){
      SPrecom <- FALSE
      }

    	if(SPP == TRUE | SPH == TRUE){
    	  SP <- TRUE
    	}else{
    	  SP <- FALSE
    	}


  if(email == TRUE)                     {sendEmailReport(userEmail=userEmail, FR=FR, IC=IC, PP=PP, SP=SP, country=country, FRrecom=FRrecom, ICrecom=ICrecom, PPrecom=PPrecom, SPrecom=SPrecom)}


  return(recText)

}



