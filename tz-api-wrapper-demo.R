#* DESCRIPTION: Function with all inputs required by the DST app, and calling use case-specific wrapper functions to calculated recommendations on IC, FR, PP and SP.
#* @json
#* @post /compute
getRecommendation <- function()
{

  setwd("/home/akilimo/projects/akilimo_recommendation")
  source("AkilimoFunctions_dev.R")

    print("Setting demo tanzania variables")
    ####################################################################################################################################
## get the input from the app for Tanzania
####################################################################################################################################
NPK201226available = FALSE; NPK201226CostperBag =0; NPK201226BagWt = 50;

ureaavailable = TRUE; ureaCostperBag = 0; ureaBagWt = 50;

MOPavailable = TRUE; MOPCostperBag = 0; MOPBagWt = 50;

DAPavailable = FALSE; DAPCostperBag = 0; DAPBagWt = 50;

NPK201010available = FALSE; NPK201010CostperBag = 0; NPK201010BagWt =50;

NPK151515available = FALSE; NPK151515CostperBag = 0; NPK151515BagWt =50;

TSPavailable = FALSE; TSPCostperBag = 0; TSPBagWt = 50;

NPK171717available = FALSE; NPK171717CostperBag = 0; NPK171717BagWt = 50;

CANavailable = FALSE; CANCostperBag = 0; CANBagWt = 50;

SSPavailable = FALSE; SSPCostperBag = 0; SSPBagWt = 50; country="TZ";

newFert1name="NA"; newFert1N_cont="NA"; newFert1P2O5="NA"; newFert1K2O="NA"; newFert1CostperBag=0; newFert1BagWt=50;
newFert2name="NA"; newFert2N_cont="NA"; newFert2P2O5="NA"; newFert2K2O="NA"; newFert2CostperBag=0; newFert2BagWt=50;
newFert3name="NA"; newFert3N_cont="NA"; newFert3P2O5="NA"; newFert3K2O="NA"; newFert3CostperBag=0; newFert3BagWt=50;
newFert4name="NA"; newFert4N_cont="NA"; newFert4P2O5="NA"; newFert4K2O="NA"; newFert4CostperBag=0; newFert4BagWt=50;
newFert5name="NA"; newFert5N_cont="NA"; newFert5P2O5="NA"; newFert5K2O="NA"; newFert5CostperBag=0; newFert5BagWt=50


lat = -1.4805308525472043; lon = 31.36339044857442;
lat = -1.6022405743997012; lon = 31.215062576671613;
#lat=-1.1788347; lon=36.8907865

IC= SPH = SPP = PP = FALSE
FR <- TRUE


PD = "2020-10-10"; HD = "2021-10-10"
PD_window = 0
HD_window = 0


maizePD = "fresh_cob"
sweetPotatoPD <- "tubers"
cassPD = "roots"


maizeUW = 50 ## if fresh_cob it can only be 1
sweetPotatoUW = 50
cassUW = 1000

maizeUP = 0
sweetPotatoUP = 0
cassUP = 166000.0

saleSF = FALSE; nameSF = "NA" ;

country = "TZ";
maxInv = 72000; ## if rootUP  > 1.4 tuberUP Monocrop is advised. with the current default fertilizer prices, even if tuberUp  = 2*rootUP, fertilizer use is not advised
area = 1; areaUnits = "acre";

FCY = 11 ## fresh wt in t/ha

cassUP_m1 = cassUP_m2 = cassUP_p1 = cassUP_p2 = 0

cost_weeding1 = cost_weeding2 =  0

cost_tractor_harrowing = cost_tractor_ridging = 0
cost_manual_harrowing = 0
cost_manual_ploughing = 0
cost_manual_ridging = 0
cost_tractor_ploughing = 0

fallowType = "none"; fallowHeight = 100;
fallowGreen = FALSE;
problemWeeds = FALSE



ploughing = FALSE;
ridging = FALSE
harrowing = FALSE
method_ploughing = "NA"
method_ridging = "NA"
method_harrowing = "NA"

CMP = 3; riskAtt = 0;

tractor_plough = FALSE
tractor_ridger = FALSE
tractor_harrow = FALSE
cost_LMO_areaBasis = "areaUnit";
SMS = email = FALSE; 
userPhoneCC = 254; 
userPhoneNr = 702974480; 
userName = "NA"; 
userEmail = "user@cgiar.org"; 
userField = "NA"

    print("Finished setting demo tanzania parameters here")


  fertilizers <-  fertilizerFunc(ureaavailable=ureaavailable, ureaCostperBag=ureaCostperBag,ureaBagWt=ureaBagWt,
                                 MOPavailable=MOPavailable, MOPCostperBag=MOPCostperBag, MOPBagWt=MOPBagWt,
                                 DAPavailable=DAPavailable, DAPCostperBag=DAPCostperBag, DAPBagWt=DAPBagWt,
                                 NPK201010available=NPK201010available, NPK201010CostperBag=NPK201010CostperBag, NPK201010BagWt=NPK201010BagWt,
                                 NPK151515available=NPK151515available, NPK151515CostperBag=NPK151515CostperBag, NPK151515BagWt=NPK151515BagWt,
                                 TSPavailable=TSPavailable, TSPCostperBag=TSPCostperBag, TSPBagWt=TSPBagWt,
                                 NPK171717available=NPK171717available, NPK171717CostperBag=NPK171717CostperBag, NPK171717BagWt=NPK171717BagWt,
                                 CANavailable=CANavailable, CANCostperBag=CANCostperBag, CANBagWt=CANBagWt,
                                 SSPavailable=SSPavailable, SSPCostperBag=SSPCostperBag, SSPBagWt=SSPBagWt,
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
	  if(cassUP == 0 & cassPD=="roots" & country=="NG"){cassUP = 12000; cassUW = 1000}
	  if(cassUP == 0 & cassPD=="chips" & country=="NG"){cassUP = 36000; cassUW = 1000}
	  if(cassUP == 0 & cassPD=="flour" & country=="NG"){cassUP = 38400; cassUW = 1000}
	  if(cassUP == 0 & cassPD=="gari"  & country=="NG"){cassUP = 42000; cassUW = 1000}

	  if(cassUP == 0 & cassPD=="roots" & country=="TZ"){cassUP = 180000; cassUW = 1000}
	  if(cassUP == 0 & cassPD=="chips" & country=="TZ"){cassUP = 540000; cassUW = 1000}
	  if(cassUP == 0 & cassPD=="flour" & country=="TZ"){cassUP = 576000; cassUW = 1000}
	  if(cassUP == 0 & cassPD=="gari"  & country=="TZ"){cassUP = 630000; cassUW = 1000}

	}


	rootUP <- cassUP / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000

	rootUP_m1 <- cassUP_m1 / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000
	rootUP_m2 <- cassUP_m2 / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000
	rootUP_p1 <- cassUP_p1 / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000
	rootUP_p2 <- cassUP_p2 / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000

	# calculating cobUP based on maizeUP, maizeUW and conversion from grain to cobs if maizePD == "grain"
	if(maizeUP == 0 & maizePD == "fresh_cob"){
	  maizeUP <- 50 #default price for 1 large fresh cob
	  maizeUW <- 1
	}

	if(maizeUP == 0  & maizePD == "grain") {
	  maizeUP <- 230 #default price for 1 kg of maize grain
	  maizeUW <- 1
	}


	cobUP <- ifelse(maizePD == "fresh_cob", maizeUP, maizeUP / maizeUW / 7.64) #1 kg of grain ~ 7.64 cobs

	# calculating tuberUP based on sweetPotatoUP, sweetPotatoUW and conversion factor for sweetPotato product sold
	tuberConv <- data.frame(sweetPotatoPD = c("tubers", "flour"), conversion = c(1, 3.2))
	if(sweetPotatoUP == 0 & sweetPotatoPD=="tubers" & country=="TZ"){sweetPotatoUP = 120000; sweetPotatoUW = 1000}
	if(sweetPotatoUP == 0 & sweetPotatoPD=="flour"  & country=="TZ"){sweetPotatoUP = 384000; sweetPotatoUW = 1000}

	tuberUP <- sweetPotatoUP / sweetPotatoUW / tuberConv[tuberConv$sweetPotatoPD==sweetPotatoPD,]$conversion * 1000

	# calculating the field area
	areaHa <- area / ifelse(areaUnits=="ha", 1, ifelse(areaUnits=="acre", 2.47105, 10000))


	# create dataframe with cost of land management operations
	costLMO <- data.frame(operation = c(rep(c("ploughing", "harrowing", "ridging"),2), "weeding1", "weeding2"),
	                      method = c(rep("manual", 3), rep("tractor", 3), NA, NA),
	                      cost = c(cost_manual_ploughing, cost_manual_harrowing, cost_manual_ridging, cost_tractor_ploughing, cost_tractor_harrowing, cost_tractor_ridging, cost_weeding1, cost_weeding2),
	                      area = ifelse(cost_LMO_areaBasis=="areaField", areaHa, ifelse(areaUnits=="acre", 0.404686, ifelse(areaUnits=="ha", 1, 0.0001))))

	costLMO_MD <- costLMO
	costLMO$costHa <- costLMO$cost / costLMO$area
	costLMO <- subset(costLMO, select=-c(area, cost))


	# add default values for LMO operations if missing
	if(country == "NG"){
	  if(is.na(cost_manual_ploughing))                   costLMO[costLMO$operation=="ploughing" & costLMO$method=="manual" ,]$costHa <- 17000 * 2.47105
	  if(is.na(cost_manual_harrowing))                   costLMO[costLMO$operation=="harrowing" & costLMO$method=="manual" ,]$costHa <- 15000 * 2.47105
	  if(is.na(cost_manual_ridging))                     costLMO[costLMO$operation=="ridging"   & costLMO$method=="manual" ,]$costHa <- 12000 * 2.47105
	  if(is.na(cost_tractor_ploughing) & tractor_plough) costLMO[costLMO$operation=="ploughing" & costLMO$method=="tractor" ,]$costHa <- 6000 * 2.47105
	  if(is.na(cost_tractor_harrowing) & tractor_harrow) costLMO[costLMO$operation=="harrowing" & costLMO$method=="tractor" ,]$costHa <- 6000 * 2.47105
	  if(is.na(cost_tractor_ridging)   & tractor_ridger) costLMO[costLMO$operation=="ridging"   & costLMO$method=="tractor" ,]$costHa <- 6000 * 2.47105
	  if(is.na(cost_weeding1))                           costLMO[costLMO$operation=="weeding1", ]$costHa                             <- 30000 * 2.47105
	  if(is.na(cost_weeding2))                           costLMO[costLMO$operation=="weeding2", ]$costHa                             <- 30000 * 2.47105

	}else{
	  if(is.na(cost_manual_ploughing))                   costLMO[costLMO$operation=="ploughing" & costLMO$method=="manual" ,]$costHa <- 175000 * 2.47105
	  if(is.na(cost_manual_harrowing))                   costLMO[costLMO$operation=="harrowing" & costLMO$method=="manual" ,]$costHa <- 150000 * 2.47105
	  if(is.na(cost_manual_ridging))                     costLMO[costLMO$operation=="ridging"   & costLMO$method=="manual" ,]$costHa <- 225000 * 2.47105
	  if(is.na(cost_tractor_ploughing) & tractor_plough) costLMO[costLMO$operation=="ploughing" & costLMO$method=="tractor" ,]$costHa <- 150000 * 2.47105
	  if(is.na(cost_tractor_harrowing) & tractor_harrow) costLMO[costLMO$operation=="harrowing" & costLMO$method=="tractor" ,]$costHa <- 100000 * 2.47105
	  if(is.na(cost_tractor_ridging)   & tractor_ridger) costLMO[costLMO$operation=="ridging"   & costLMO$method=="tractor" ,]$costHa <- 115000 * 2.47105
	  if(is.na(cost_weeding1))                           costLMO[costLMO$operation=="weeding1", ]$costHa                             <- 60000 * 2.47105
	  if(is.na(cost_weeding2))                           costLMO[costLMO$operation=="weeding2", ]$costHa                             <- 45000 * 2.47105


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
	recloc_ng <- gsub(pattern = "\"",replacement = "",TRNS$recloc[1]) ; 	recloc_tz <- gsub(pattern = "\"",replacement = "",TRNS$recloc[2]) ; frnotrec_ng <- gsub(pattern = "\"",replacement = "",TRNS$frnotrec[1]) ;
 	frnotrec_tz <- gsub(pattern = "\"",replacement = "",TRNS$frnotrec[2]) ; spinfo_ng <- gsub(pattern = "\"",replacement = "",TRNS$spinfo[1]) ; spinfo_tz <- gsub(pattern = "\"",replacement = "",TRNS$spinfo[2])


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
	                                                      FCY=FCY)

                                          	if(all(res$FR == "We do not have fertilizer recommendation for your location because your location is out of the recommendation domain AKILIMO is currently serving.") & country == "NG"){
                                          	  FRrecom <- FALSE
                                          	  recText[["FR"]] <- res$FR
                                          	}else if (all(res$FR == "Hatuna mapendekezo yoyote  kwa eneo lako kwa sababu eneo lako liko nje la eneo ambalo AKILIMO linafanya kazi kwa sasa") & country == "TZ"){
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
                                          	    if(country == "NG") {
                                          	      recText[["FR"]] <-frnotrec_ng
                                          	    }else{
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
          	                                                        write.csv(recText$IC, 'IC_recText.csv', row.names = FALSE)
          	                                                        IC_MarkdownText(rr = res$IC, fertilizers=fertilizers, userName=userName, country=country,
          	                                                                        userPhoneNr=userPhoneNr, userField=userField, area=area, areaUnits=areaUnits,
          	                                                                        PD =PD, HD=HD, email=email, lat=lat, lon=lon, userPhoneCC=userPhoneCC,maizeUW=maizeUW,
          	                                                                        maizePD=maizePD,cassUW = cassUW, rootUP = rootUP, cassPD = cassPD, maxInv = maxInv, CMP=CMP, maizeUP=maizeUP)
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
	                                                                           rootUP = rootUP)

                                                  	recText[["PP"]] <- getPPrecText(ds = res$PP , country = country)
                                                  	write.csv(res$PP, 'PP_rec.csv', row.names = FALSE)
                                                  	write.csv(recText$PP, 'PP_recText.csv', row.names = FALSE)
                                                  	PP_MarkdownText(userName, country, userPhoneNr, userField, area, areaUnits, PD, HD, email,lat, lon,rootUP,cassPD,cassUW,
                                                  	                maxInv, ploughing, ridging, method_ploughing, method_ridging, userPhoneCC)

                                                  	}


	if(SPP == TRUE | SPH == TRUE)         {if(PD_window == 0 & HD_window ==0){
                                          	  SPrecom <- FALSE
                                          	  recText[["SP"]] <- if(country == "NG"){
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
                                      	    recText[["SP"]] <- getSPrecText(ds = res$SP , country = country)
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

  print(paste("Returning recommendations back akilimo API....."))
  return(list(res, recText))
}



