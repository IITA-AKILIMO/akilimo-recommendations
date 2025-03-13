
setwd("/home/akilimo/projects/akilimo_recommendation")



####################################################################################################################################
## get the input from the app for Nigeria
####################################################################################################################################
##NPK20:12:16 having 0.20N; 0.052P, 0.216K
NPK201216available = FALSE; NPK201216CostperBag =0; NPK201216BagWt = 50;
ureaavailable = TRUE; ureaCostperBag =0; ureaBagWt = 50;# ureaavailable = TRUE; ureaCostperBag = "NA"; ureaBagWt = 50;
MOPavailable = FALSE; MOPCostperBag = 0; MOPBagWt = 50;
DAPavailable = TRUE; DAPCostperBag = 83000; DAPBagWt = 50;
NPK201010available = FALSE; NPK201010CostperBag = 0; NPK201010BagWt =50;
NPK151515available = TRUE; NPK151515CostperBag = 0; NPK151515BagWt =50;
TSPavailable = FALSE; TSPCostperBag = 0; TSPBagWt = 50;
NPK171717available = FALSE; NPK171717CostperBag = 48000; NPK171717BagWt = 50;
CANavailable = FALSE; CANCostperBag = 45000; CANBagWt = 50;
SSPavailable = FALSE; SSPCostperBag = 0; SSPBagWt = 50; country="NG";
FOMIIMBURAavailable = FALSE; FOMIIMBURACostperBag = 0; FOMIIMBURABagWt = 50
FOMIBAGARAavailable = FALSE; FOMIBAGARACostperBag = 0; FOMIBAGARABagWt = 50 
FOMITOTAHAZAavailable = FALSE; FOMITOTAHAZACostperBag = 0; FOMITOTAHAZABagWt = 50
NPK112221available = FALSE; NPK112221CostperBag = 0; NPK112221BagWt = 50
NPK251010available = FALSE; NPK251010CostperBag = 0; NPK251010BagWt = 50
NPK152020available = FALSE; NPK152020CostperBag = 0; NPK152020BagWt = 50 
NPK23105available = FALSE; NPK23105CostperBag = 0; NPK23105BagWt = 50
NPK123017available = FALSE; NPK123017CostperBag = 0; NPK123017BagWt = 50
newFert1name="NA"; newFert1N_cont="NA"; newFert1P2O5="NA"; newFert1K2O="NA"; newFert1CostperBag=50; newFert1BagWt=50;
newFert2name="NA"; newFert2N_cont="NA"; newFert2P2O5="NA"; newFert2K2O="NA"; newFert2CostperBag=50; newFert2BagWt=50;
newFert3name="NA"; newFert3N_cont="NA"; newFert3P2O5="NA"; newFert3K2O="NA"; newFert3CostperBag=50; newFert3BagWt=50;
newFert4name="NA"; newFert4N_cont="NA"; newFert4P2O5="NA"; newFert4K2O="NA"; newFert4CostperBag=50; newFert4BagWt=50;
newFert5name="NA"; newFert5N_cont="NA"; newFert5P2O5="NA"; newFert5K2O="NA"; newFert5CostperBag=50; newFert5BagWt=50

cassUP_m1 <- 42000
cassUP_m2 <- 48000
cassUP_p1 <- 56000
cassUP_p2 <- 51000
#lat = -8.147348532597064; lon = 39.16021204907534;
#lat = 6.71025557;  lon = 5.6483941;

lat=7.4201; lon=5.1063

PD = "2021-09-17"
HD = "2022-10-30"

SP = IC = SPP = PP = TRUE
FR = TRUE


maizePD = "fresh_cob" #grain
sweetPotatoPD <- "tubers"
cassPD = "roots"


maizeUW = 0
sweetPotatoUW = 1#,50,100,1000
cassUW = 1000# 50, 100, 1000

maizeUP = 150 # 250 NGN/kg  * 50 kg grain WITH (2*7500) + (6*8500)
sweetPotatoUP = 112500
cassUP = 50000

cost_weeding1 = cost_weeding2 = 0
cost_tractor_ploughing = cost_tractor_harrowing = cost_tractor_ridging = cost_manual_ploughing = cost_manual_harrowing = cost_manual_ridging = 0

#country = "RW";
country = "NG";  FCY = 11## HD = number of days on the field
area = 5; areaUnits = "acre"; maxInv = 150000;

#ploughing = TRUE; ridging = TRUE
ploughing = FALSE; method_ploughing = "NA"
ridging = FALSE; method_ridging = "tractor"

CMP = 2; riskAtt = 1; PD_window = 2; HD_window = 2;
saleSF = FALSE; nameSF = "PsaltryMarketers"
fallowType = "bush"; fallowHeight = 100; fallowGreen = TRUE; problemWeeds = FALSE
tractor_plough = tractor_harrow = tractor_ridger = TRUE
cost_LMO_areaBasis = "areaUnit";


SMS = email = TRUE; userPhoneCC = 254; userPhoneNr = 70125634; userName = "Mkulima Hodari";
userEmail = "akilimo@gmail.com"; userField = "NA"


CMP = 1;  maizePD = "grain";   maizeUP =  29200.0;   maizeUW=  50;   riskAtt= 0

####################################################################################################################################
## get the input from the app for Tanzania NPK201216available
####################################################################################################################################
NPK201216available = FALSE; NPK201216CostperBag =0; NPK201216BagWt = 50;
ureaavailable = FALSE; ureaCostperBag = 0; ureaBagWt = 50;
MOPavailable = FALSE; MOPCostperBag = 0; MOPBagWt = 50;
DAPavailable = TRUE; DAPCostperBag = 83000; DAPBagWt = 50;
NPK201010available = FALSE; NPK201010CostperBag = 0; NPK201010BagWt =50;
NPK151515available = FALSE; NPK151515CostperBag = 0; NPK151515BagWt =50;
TSPavailable = FALSE; TSPCostperBag = 0; TSPBagWt = 50;
NPK171717available = TRUE; NPK171717CostperBag = 48000; NPK171717BagWt = 50;
CANavailable = TRUE; CANCostperBag = 45000; CANBagWt = 50;
SSPavailable = FALSE; SSPCostperBag = 0; SSPBagWt = 50; country="TZ";
FOMIIMBURAavailable = FALSE; FOMIIMBURACostperBag = 0; FOMIIMBURABagWt = 50
FOMIBAGARAavailable = FALSE; FOMIBAGARACostperBag = 0; FOMIBAGARABagWt = 50 
FOMITOTAHAZAavailable = FALSE; FOMITOTAHAZACostperBag = 0; FOMITOTAHAZABagWt = 50
NPK112221available = FALSE; NPK112221CostperBag = 0; NPK112221BagWt = 50
NPK251010available = FALSE; NPK251010CostperBag = 0; NPK251010BagWt = 50
NPK152020available = FALSE; NPK152020CostperBag = 0; NPK152020BagWt = 50 
NPK23105available = FALSE; NPK23105CostperBag = 0; NPK23105BagWt = 50
NPK123017available = FALSE; NPK123017CostperBag = 0; NPK123017BagWt = 50
newFert1name="NA"; newFert1N_cont="NA"; newFert1P2O5="NA"; newFert1K2O="NA"; newFert1CostperBag=0; newFert1BagWt=50;
newFert2name="NA"; newFert2N_cont="NA"; newFert2P2O5="NA"; newFert2K2O="NA"; newFert2CostperBag=0; newFert2BagWt=50;
newFert3name="NA"; newFert3N_cont="NA"; newFert3P2O5="NA"; newFert3K2O="NA"; newFert3CostperBag=0; newFert3BagWt=50;
newFert4name="NA"; newFert4N_cont="NA"; newFert4P2O5="NA"; newFert4K2O="NA"; newFert4CostperBag=0; newFert4BagWt=50;
newFert5name="NA"; newFert5N_cont="NA"; newFert5P2O5="NA"; newFert5K2O="NA"; newFert5CostperBag=0; newFert5BagWt=50


lat = -1.865464231931952; lon = 34.38165065822872;
#lat=-1.1788347; lon=36.8907865

IC= SPH = SPP = PP = FR = TRUE

PD = "2021-05-11"; HD = "2022-01-11"
PD_window = 0
HD_window = 0


maizePD = "fresh_cob"
sweetPotatoPD <- "tubers"
cassPD = "roots"


maizeUW = 50 ## if fresh_cob it can only be 1
sweetPotatoUW = 50
cassUW = 50

maizeUP = 0
sweetPotatoUP = 0
cassUP = 5000

saleSF = FALSE; nameSF = "NA" ;

country = "TZ";
maxInv =  225000; ## if rootUP  > 1.4 tuberUP Monocrop is advised. with the current default fertilizer prices, even if tuberUp  = 2*rootUP, fertilizer use is not advised
area = 1; areaUnits = "acre";

FCY = 11 ## this is what it should be but for data sourcing we need FCY1 or FCY2... TODDO

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



ploughing = TRUE;
ridging = FALSE
harrowing = FALSE
method_ploughing = "manual"
method_ridging = "NA"
method_harrowing = "NA"

CMP = 3; riskAtt = 1;

tractor_plough = FALSE
tractor_ridger = FALSE
tractor_harrow = FALSE
cost_LMO_areaBasis = "areaUnit";
SMS = email = TRUE; userPhoneCC = 254; userPhoneNr = 12356 ; userName = "NA"; userEmail = "m.chernet@cgiar.org"; userField = "NA"




setwd("/home/akilimo/projects/akilimo_recommendation")
####################################################################################################################################
## get the input from the app for Rwanda
####################################################################################################################################
NPK201216available = FALSE; NPK201216CostperBag =0; NPK201216BagWt = 50;
ureaavailable = TRUE; ureaCostperBag =0; ureaBagWt = 50; ##28200
MOPavailable = TRUE; MOPCostperBag = 0; MOPBagWt = 50; ##27900
DAPavailable = TRUE; DAPCostperBag = 0; DAPBagWt = 50;#31650
NPK201010available = FALSE; NPK201010CostperBag = 0; NPK201010BagWt =50;
NPK151515available = FALSE; NPK151515CostperBag = 0; NPK151515BagWt =50;
TSPavailable = FALSE; TSPCostperBag = 0; TSPBagWt = 50;
NPK171717available = TRUE; NPK171717CostperBag = 0; NPK171717BagWt = 50;##35650
CANavailable = FALSE; CANCostperBag = 0; CANBagWt = 50;
SSPavailable = FALSE; SSPCostperBag = 0; SSPBagWt = 50; country="NG";
newFert1name="NA"; newFert1N_cont="NA"; newFert1P2O5="NA"; newFert1K2O="NA"; newFert1CostperBag=50; newFert1BagWt=50;
newFert2name="NA"; newFert2N_cont="NA"; newFert2P2O5="NA"; newFert2K2O="NA"; newFert2CostperBag=50; newFert2BagWt=50;
newFert3name="NA"; newFert3N_cont="NA"; newFert3P2O5="NA"; newFert3K2O="NA"; newFert3CostperBag=50; newFert3BagWt=50;
newFert4name="NA"; newFert4N_cont="NA"; newFert4P2O5="NA"; newFert4K2O="NA"; newFert4CostperBag=50; newFert4BagWt=50;
newFert5name="NA"; newFert5N_cont="NA"; newFert5P2O5="NA"; newFert5K2O="NA"; newFert5CostperBag=50; newFert5BagWt=50


lat=-1.325; lon=30.425

PD = "2021-03-21"
HD = "2022-04-14"

SP = IC = SPP = PP = FALSE
FR = TRUE


cassPD = "roots"
cassUW = 1000
cassUP = 75000

country = "RW";  FCY = "FCY1"
area = 1; areaUnits = "ha"; maxInv = 200000;


saleSF = FALSE; nameSF = "NA"
cost_LMO_areaBasis = "areaUnit";


SMS = email = TRUE; userPhoneCC = 254; userPhoneNr = 702974480; userName = "Mkulima Hodari";
userEmail = "akilimo@gmail.com"; userField = "NA"


CMP = 2;  area = 0.6;  areaUnits= "ha";  riskAtt= 1



####################################################################################################################################
## get the input from the app for Burundi
####################################################################################################################################
NPK201216available = FALSE; NPK201216CostperBag =0; NPK201216BagWt = 50;
ureaavailable = FALSE; ureaCostperBag =0; ureaBagWt = 50;# ureaavailable = TRUE; ureaCostperBag = "NA"; ureaBagWt = 50;
MOPavailable = FALSE; MOPCostperBag = 0; MOPBagWt = 50;
DAPavailable = FALSE; DAPCostperBag = 83000; DAPBagWt = 50;
NPK201010available = FALSE; NPK201010CostperBag = 0; NPK201010BagWt =50;
NPK151515available = FALSE; NPK151515CostperBag = 0; NPK151515BagWt =50;
TSPavailable = FALSE; TSPCostperBag = 0; TSPBagWt = 50;
NPK171717available = FALSE; NPK171717CostperBag = 48000; NPK171717BagWt = 50;
CANavailable = FALSE; CANCostperBag = 45000; CANBagWt = 50;
SSPavailable = FALSE; SSPCostperBag = 0; SSPBagWt = 50;
NPK112221available = FALSE; NPK112221CostperBag = 0; NPK112221BagWt = 50
NPK251010available = FALSE; NPK251010CostperBag = 0; NPK251010BagWt = 50
NPK152020available = FALSE; NPK152020CostperBag = 0; NPK152020BagWt = 50 
NPK23105available = FALSE; NPK23105CostperBag = 0; NPK23105BagWt = 50
NPK123017available = FALSE; NPK123017CostperBag = 0; NPK123017BagWt = 50

FOMIIMBURAavailable = TRUE; FOMIIMBURACostperBag = 85390; FOMIIMBURABagWt = 50
FOMIBAGARAavailable = TRUE; FOMIBAGARACostperBag = 77600; FOMIBAGARABagWt = 50 
FOMITOTAHAZAavailable = TRUE; FOMITOTAHAZACostperBag = 77000; FOMITOTAHAZABagWt = 50

newFert1name="NA"; newFert1N_cont="NA"; newFert1P2O5="NA"; newFert1K2O="NA"; newFert1CostperBag=50; newFert1BagWt=50;
newFert2name="NA"; newFert2N_cont="NA"; newFert2P2O5="NA"; newFert2K2O="NA"; newFert2CostperBag=50; newFert2BagWt=50;
newFert3name="NA"; newFert3N_cont="NA"; newFert3P2O5="NA"; newFert3K2O="NA"; newFert3CostperBag=50; newFert3BagWt=50;
newFert4name="NA"; newFert4N_cont="NA"; newFert4P2O5="NA"; newFert4K2O="NA"; newFert4CostperBag=50; newFert4BagWt=50;
newFert5name="NA"; newFert5N_cont="NA"; newFert5P2O5="NA"; newFert5K2O="NA"; newFert5CostperBag=50; newFert5BagWt=50

country = "BU"

maizeUP = 150 # 250 NGN/kg  * 50 kg grain WITH (2*7500) + (6*8500)
sweetPotatoUP = 112500
cassUP = 700000

cassUP_m1 <- 460
cassUP_m2 <- 480
cassUP_p1 <- 450
cassUP_p2 <- 470

lat = -3.106548
lon = 29.42757



PD = "2021-09-10" ### yyyy-mm-dd
HD = "2022-12-10"

IC = PP = SPH= SPP = SP = FALSE
FR  = TRUE


maizePD = "fresh_cob" #grain
sweetPotatoPD <- "tubers"
cassPD = "roots"


maizeUW = 0
sweetPotatoUW = 0
cassUW = 1000

cost_weeding1 = cost_weeding2 = 0
cost_tractor_ploughing = cost_tractor_harrowing = cost_tractor_ridging = cost_manual_ploughing = cost_manual_harrowing = cost_manual_ridging = 0

FCY = 11
area = 1; areaUnits = "ha"; maxInv = 403600;

ploughing = FALSE; method_ploughing = "NA"
ridging = FALSE; method_ridging = "tractor"

CMP = 0; riskAtt = 0; PD_window = 1; HD_window = 1;
saleSF = FALSE; nameSF = "PsaltryMarketers"
fallowType = "bush"; fallowHeight = 100; fallowGreen = TRUE; problemWeeds = FALSE
tractor_plough = tractor_harrow = tractor_ridger = TRUE
cost_LMO_areaBasis = "areaUnit";


SMS = email = TRUE; userPhoneCC = 254; userPhoneNr = 702974480; userName = "Mkulima Hodari";
userEmail = "akilimo@gmail.com"; userField = "lala"





####################################################################################################################################
## get the input from the app for Ghana
####################################################################################################################################
NPK201216available = FALSE; NPK201216CostperBag =0; NPK201216BagWt = 50;
ureaavailable = TRUE; ureaCostperBag =0; ureaBagWt = 50;# ureaavailable = TRUE; ureaCostperBag = "NA"; ureaBagWt = 50;
MOPavailable = FALSE; MOPCostperBag = 0; MOPBagWt = 50;
DAPavailable = FALSE; DAPCostperBag = 83000; DAPBagWt = 50;
NPK201010available = FALSE; NPK201010CostperBag = 0; NPK201010BagWt =50;
NPK151515available = FALSE; NPK151515CostperBag = 0; NPK151515BagWt =50;
TSPavailable = FALSE; TSPCostperBag = 0; TSPBagWt = 50;
NPK171717available = FALSE; NPK171717CostperBag = 48000; NPK171717BagWt = 50;
CANavailable = FALSE; CANCostperBag = 45000; CANBagWt = 50;
SSPavailable = FALSE; SSPCostperBag = 0; SSPBagWt = 50;
NPK112221available = TRUE; NPK112221CostperBag = 0; NPK112221BagWt = 50
NPK251010available = TRUE; NPK251010CostperBag = 0; NPK251010BagWt = 50
NPK152020available = TRUE; NPK152020CostperBag = 0; NPK152020BagWt = 50 
NPK23105available = FALSE; NPK23105CostperBag = 0; NPK23105BagWt = 50
NPK123017available = TRUE; NPK123017CostperBag = 0; NPK123017BagWt = 50
FOMIIMBURAavailable = FALSE; FOMIIMBURACostperBag = 0; FOMIIMBURABagWt = 50
FOMIBAGARAavailable = FALSE; FOMIBAGARACostperBag = 0; FOMIBAGARABagWt = 50 
FOMITOTAHAZAavailable = FALSE; FOMITOTAHAZACostperBag = 0; FOMITOTAHAZABagWt = 50
newFert1name="NA"; newFert1N_cont="NA"; newFert1P2O5="NA"; newFert1K2O="NA"; newFert1CostperBag=50; newFert1BagWt=50;
newFert2name="NA"; newFert2N_cont="NA"; newFert2P2O5="NA"; newFert2K2O="NA"; newFert2CostperBag=50; newFert2BagWt=50;
newFert3name="NA"; newFert3N_cont="NA"; newFert3P2O5="NA"; newFert3K2O="NA"; newFert3CostperBag=50; newFert3BagWt=50;
newFert4name="NA"; newFert4N_cont="NA"; newFert4P2O5="NA"; newFert4K2O="NA"; newFert4CostperBag=50; newFert4BagWt=50;
newFert5name="NA"; newFert5N_cont="NA"; newFert5P2O5="NA"; newFert5K2O="NA"; newFert5CostperBag=50; newFert5BagWt=50

country="GH"

maizeUP = 150 # 250 NGN/kg  * 50 kg grain WITH (2*7500) + (6*8500)
sweetPotatoUP = 112500
cassUP = 450

cassUP_m1 <- 460
cassUP_m2 <- 480
cassUP_p1 <- 450
cassUP_p2 <- 470
#lat = -8.147348532597064; lon = 39.16021204907534;
#lat = 6.71025557;  lon = 5.6483941;

# lat=5.075; lon=-1.575

lat = 7.647082219263737
lon = -1.4483928539849558



PD = "2021-08-10" ### yyyy-mm-dd
HD = "2022-11-10"

IC = PP = SPH = FALSE
FR = SPP = SP = TRUE


maizePD = "fresh_cob" #grain
sweetPotatoPD <- "tubers"
cassPD = "roots"


maizeUW = 0
sweetPotatoUW = 0
cassUW = 1000

cost_weeding1 = cost_weeding2 = 0
cost_tractor_ploughing = cost_tractor_harrowing = cost_tractor_ridging = cost_manual_ploughing = cost_manual_harrowing = cost_manual_ridging = 0

country = "GH";  FCY = 11
area = 1; areaUnits = "ha"; maxInv = 1200;

ploughing = FALSE; method_ploughing = "NA"
ridging = FALSE; method_ridging = "tractor"

CMP = 0; riskAtt = 0; PD_window = 1; HD_window = 1;
saleSF = FALSE; nameSF = "PsaltryMarketers"
fallowType = "bush"; fallowHeight = 100; fallowGreen = TRUE; problemWeeds = FALSE
tractor_plough = tractor_harrow = tractor_ridger = TRUE
cost_LMO_areaBasis = "areaUnit";


SMS = email = TRUE; userPhoneCC = 254; userPhoneNr = 702974480; userName = "Mkulima Hodari";
userEmail = "akilimo@gmail.com"; userField = "NA"












####################################################################################################################################
## test values
############################################                                      ########################################################################################
setwd("D:/ACAI_Wrapper/cloud_compute")
source("AkilimoFunctions_2.R")
#source("AkilimoFunctions_dev_TO.R")
testValues <- function(){
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

  fertilizers <-  fertilizerFunc(ureaavailable=ureaavailable, ureaCostperBag=ureaCostperBag,ureaBagWt=ureaBagWt,
                                 MOPavailable=MOPavailable, MOPCostperBag=MOPCostperBag, MOPBagWt=MOPBagWt,
                                 DAPavailable=DAPavailable, DAPCostperBag=DAPCostperBag, DAPBagWt=DAPBagWt,
                                 NPK201010available=NPK201010available, NPK201010CostperBag=NPK201010CostperBag, NPK201010BagWt=NPK201010BagWt,
                                 NPK151515available=NPK151515available, NPK151515CostperBag=NPK151515CostperBag, NPK151515BagWt=NPK151515BagWt,
                                 TSPavailable=TSPavailable, TSPCostperBag=TSPCostperBag, TSPBagWt=TSPBagWt,
                                 NPK171717available=NPK171717available, NPK171717CostperBag=NPK171717CostperBag, NPK171717BagWt=NPK171717BagWt,
                                 #Nafakaavailable=Nafakaavailable, NafakaCostperBag=NafakaCostperBag, NafakaBagWt=NafakaBagWt,
                                 CANavailable=CANavailable, CANCostperBag=CANCostperBag, CANBagWt=CANBagWt,
                                 SSPavailable=SSPavailable, SSPCostperBag=SSPCostperBag, SSPBagWt=SSPBagWt,
                                 YaraMila_UNIKavailable=YaraMila_UNIKavailable, YaraMila_UNIKCostperBag=YaraMila_UNIKCostperBag, YaraMila_UNIKBagWt=YaraMila_UNIKBagWt,
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
  if(cassUP_m1 == 0) cassUP_m1 <- NA
  if(cassUP_m2 == 0) cassUP_m2 <- NA
  if(cassUP_p1 == 0) cassUP_p1 <- NA
  if(cassUP_p2 == 0) cassUP_p2 <- NA

  if(sweetPotatoUW == 0) sweetPotatoUW <- NA
  if(sweetPotatoUP == 0) sweetPotatoUP <- NA

  if(cassUP == 0) cassUP <- NA
  if(maizeUP == 0) maizeUP <- NA
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
  if(HD_window == 0) HD_window <- NA
  if(PD_window == 0) PD_window <- NA



  PD <- as.Date(PD, format = "%Y-%m-%d")
  HD <- as.Date(HD, format = "%Y-%m-%d")

  ## if cassava is to be sold to a processing factory, there should be a default price by factry and product
  # calculating rootUP based on cassUP, cassUW and conversion factor for cassava product sold
  rootConv <- data.frame(cassPD = c("roots", "chips", "flour", "gari"), conversion = c(1, 3, 3.2, 3.5))

  if(is.na(cassUP) & cassPD=="roots" & country=="NG"){cassUP = 12000; cassUW = 1000}
  if(is.na(cassUP) & cassPD=="chips" & country=="NG"){cassUP = 36000; cassUW = 1000}
  if(is.na(cassUP) & cassPD=="flour" & country=="NG"){cassUP = 38400; cassUW = 1000}
  if(is.na(cassUP) & cassPD=="gari"  & country=="NG"){cassUP = 42000; cassUW = 1000}

  if(is.na(cassUP) & cassPD=="roots" & country=="TZ"){cassUP = 180000; cassUW = 1000}
  if(is.na(cassUP) & cassPD=="chips" & country=="TZ"){cassUP = 540000; cassUW = 1000}
  if(is.na(cassUP) & cassPD=="flour" & country=="TZ"){cassUP = 576000; cassUW = 1000}
  if(is.na(cassUP) & cassPD=="gari"  & country=="TZ"){cassUP = 630000; cassUW = 1000}

  rootUP <- cassUP / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000

  rootUP_m1 <- cassUP_m1 / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000
  rootUP_m2 <- cassUP_m2 / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000
  rootUP_p1 <- cassUP_p1 / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000
  rootUP_p2 <- cassUP_p2 / cassUW / rootConv[rootConv$cassPD==cassPD,]$conversion * 1000

  # calculating cobUP based on maizeUP, maizeUW and conversion from grain to cobs if maizePD == "grain"
  if(is.na(maizeUP) & maizePD == "fresh_cob") maizeUP <- 50 #default price for 1 large fresh cob

  if(is.na(maizeUP)  & maizePD == "grain") {
    maizeUP <- 230 #default price for 1 kg of maize grain
    maizeUW <- 1
  }

  cobUP <- ifelse(maizePD == "fresh_cob", maizeUP, maizeUP / maizeUW / 7.64) #1 kg of grain ~ 7.64 cobs

  # calculating tuberUP based on sweetPotatoUP, sweetPotatoUW and conversion factor for sweetPotato product sold
  tuberConv <- data.frame(sweetPotatoPD = c("tubers", "flour"), conversion = c(1, 3.2))
  if(is.na(sweetPotatoUP) & sweetPotatoPD=="tubers" & country=="TZ"){sweetPotatoUP = 120000; sweetPotatoUW = 1000}
  if(is.na(sweetPotatoUP) & sweetPotatoPD=="flour"  & country=="TZ"){sweetPotatoUP = 384000; sweetPotatoUW = 1000}

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
    if(is.na(cost_weeding1))                           costLMO[costLMO$operation=="weeding1", ]$costHa                             <- 12500 * 2.47105
    if(is.na(cost_weeding2))                           costLMO[costLMO$operation=="weeding2", ]$costHa                             <- 12500 * 2.47105

  }else{
    if(is.na(cost_manual_ploughing))                   costLMO[costLMO$operation=="ploughing" & costLMO$method=="manual" ,]$costHa <- 175000 * 2.47105
    if(is.na(cost_manual_harrowing))                   costLMO[costLMO$operation=="harrowing" & costLMO$method=="manual" ,]$costHa <- 150000 * 2.47105
    if(is.na(cost_manual_ridging))                     costLMO[costLMO$operation=="ridging"   & costLMO$method=="manual" ,]$costHa <- 225000 * 2.47105
    if(is.na(cost_tractor_ploughing) & tractor_plough) costLMO[costLMO$operation=="ploughing" & costLMO$method=="tractor" ,]$costHa <- 150000 * 2.47105
    if(is.na(cost_tractor_harrowing) & tractor_harrow) costLMO[costLMO$operation=="harrowing" & costLMO$method=="tractor" ,]$costHa <- 100000 * 2.47105
    if(is.na(cost_tractor_ridging)   & tractor_ridger) costLMO[costLMO$operation=="ridging"   & costLMO$method=="tractor" ,]$costHa <- 115000 * 2.47105
    if(is.na(cost_weeding1))                           costLMO[costLMO$operation=="weeding1", ]$costHa                             <- 100000 * 2.47105
    if(is.na(cost_weeding2))                           costLMO[costLMO$operation=="weeding2", ]$costHa                             <- 850000 * 2.47105

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
  FRrecom <- NULL
  ICrecom <- NULL
  library(plyr)
  library(reshape2)
  library(tidyr)

 if(FR == TRUE)          {res[["FR"]] <- getFRrecommendations(lat = lat,
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
                                                                               FCY=FCY)}
  return(res)


}

FRRecom <- testValues()

getFRrecText(ds = FRRecom$FR , country = country)
####################################################################################################################################
## call function getRecommendation from R_wrapper and give the input from the app and it returns recommendations.
####################################################################################################################################


getRecommendation(country = country,
                  lat =lat,
                  lon =lon,
                  area = area,
                  areaUnits = areaUnits,
                  IC = IC,
                  intercrop = intercrop,
                  FR = FR,
                  PP = PP,
                  SPP = SPP,
                  SPH = SPH,
                  PD = PD,
                  HD = HD,
                  PD_window = PD_window,
                  HD_window = HD_window,
                  fallowType = fallowType,
                  fallowHeight = fallowHeight,
                  fallowGreen = fallowGreen,
                  problemWeeds = problemWeeds,
                  tractor_plough = tractor_plough,
                  tractor_harrow = tractor_harrow,
                  tractor_ridger = tractor_ridger,
                  cost_LMO_areaBasis = cost_LMO_areaBasis,
                  cost_tractor_ploughing = cost_tractor_ploughing,
                  cost_tractor_harrowing = cost_tractor_harrowing,
                  cost_tractor_ridging = cost_tractor_ridging,
                  cost_manual_ploughing = cost_manual_ploughing,
                  cost_manual_harrowing = cost_manual_harrowing,
                  cost_manual_ridging = cost_manual_ridging,
                  cost_weeding1 = cost_weeding1,
                  cost_weeding2 = cost_weeding2,
                  ploughing = ploughing,
                  harrowing = harrowing,
                  ridging = ridging,
                  method_ploughing = method_ploughing,
                  method_harrowing = method_harrowing,
                  method_ridging = method_ridging,
                  FCY,
                  CMP="3",
                  saleSF = saleSF,
                  nameSF = nameSF,
                  cassPD = cassPD,
                  cassUW = cassUW,
                  cassUP = cassUP,
                  cassUP_m1 = cassUP_m1,
                  cassUP_m2 = cassUP_m2,
                  cassUP_p1 = cassUP_p1,
                  cassUP_p2 = cassUP_p2,
                  maizePD = maizePD,
                  maizeUW = maizeUW,
                  maizeUP = maizeUP,
                  maxInv = maxInv,
                  SMS = SMS,
                  email = email,
                  userPhoneCC = userPhoneCC,
                  userPhoneNr = userPhoneNr,
                  userName = userName,
                  userEmail = userEmail,
                  userField = userField,
                  riskAtt = riskAtt,
                  ureaavailable = ureaavailable, ureaCostperBag = ureaCostperBag, ureaBagWt = ureaBagWt,
                  MOPavailable = MOPavailable, MOPCostperBag = MOPCostperBag, MOPBagWt = MOPBagWt,
                  DAPavailable = DAPavailable, DAPCostperBag = DAPCostperBag, DAPBagWt = DAPBagWt,
                  NPK201010available = NPK201010available, NPK201010CostperBag = NPK201010CostperBag, NPK201010BagWt = NPK201010BagWt,
                  NPK151515available = NPK151515available, NPK151515CostperBag = NPK151515CostperBag, NPK151515BagWt = NPK151515BagWt,
                  TSPavailable = TSPavailable, TSPCostperBag = TSPCostperBag, TSPBagWt = TSPBagWt,
                  NPK171717available = NPK171717available, NPK171717CostperBag = NPK171717CostperBag, NPK171717BagWt = NPK171717BagWt,
                  Nafakaavailable = Nafakaavailable, NafakaCostperBag = NafakaCostperBag, NafakaBagWt = NafakaBagWt,
                  CANavailable = CANavailable, CANCostperBag = CANCostperBag, CANBagWt = CANBagWt,
                  SSPavailable = SSPavailable, SSPCostperBag = SSPCostperBag, SSPBagWt = SSPBagWt,
                  newFert1name = newFert1name, newFert1N_cont = newFert1N_cont, newFert1P2O5 = newFert1P2O5, newFert1K2O = newFert1K2O,newFert1CostperBag = newFert1CostperBag,newFert1BagWt = NA,
                  newFert2name = newFert2name,newFert2N_cont = newFert2N_cont,newFert2P2O5 = newFert2P2O5, newFert2K2O = newFert2K2O,newFert2CostperBag = newFert2CostperBag,newFert2BagWt = newFert2BagWt,
                  newFert3name = newFert3name, newFert3N_cont = newFert3N_cont, newFert3P2O5 = newFert3P2O5, newFert3K2O = newFert3K2O, newFert3CostperBag = newFert3CostperBag, newFert3BagWt = newFert3BagWt,
                  newFert4name = newFert4name, newFert4N_cont = newFert4N_cont, newFert4P2O5 = newFert4P2O5, newFert4K2O = newFert4K2O, newFert4CostperBag = newFert4CostperBag, newFert4BagWt = newFert4BagWt,
                  newFert5name = newFert5name, newFert5N_cont = newFert5N_cont, newFert5P2O5 = newFert5P2O5, newFert5K2O = newFert5K2O, newFert5CostperBag = newFert5CostperBag, newFert5BagWt = newFert5BagWt)


##################################################################################
##test values from Jason
"CMP" = "3";
"FCY" = 0;
"FR" = TRUE;
"IC" = FALSE;
"PP" = FALSE;
"SPH" = FALSE;
"SPP" = FALSE;
"ureaBagWt" = 50;
"ureaCostperBag" = "NA";
"ureaavailable" = TRUE;
"MOPBagWt" = 50;
"MOPCostperBag" = "NA";
"MOPavailable" = TRUE;
"CANBagWt" = 50;
"CANCostperBag" = "NA";
"CANavailable" = FALSE;
"DAPBagWt" = 50;
"DAPCostperBag" = "NA";
"DAPavailable" = FALSE;
"NPK151515BagWt" = 50;
"NPK151515CostperBag" = "NA";
"NPK151515available" = FALSE;
"NPK171717BagWt" = 50;
"NPK171717CostperBag" = "NA";
"NPK171717available" = FALSE;
"NPK201010BagWt" = 50;
"NPK201010CostperBag" = "NA";
"NPK201010available" = FALSE;
"NafakaBagWt" = 50;
"NafakaCostperBag" = "NA";
"Nafakaavailable" = FALSE;
"SSPBagWt" = 50;
"SSPCostperBag" = "NA";
"SSPavailable" = FALSE;
"TSPBagWt" = 50;
"TSPCostperBag" = "NA";
"TSPavailable" = FALSE;
"PD" = "2019-11-07";
"PD_window" = 0;
"HD" = "2020-08-07";
"HD_window" = 0;
"area" = 1.0;
"areaUnits" = "acre";
"cassPD" = "roots";
"cassUP" = "0";
"cassUP_m1" = "NA";
"cassUP_m2" = "NA";
"cassUP_p1" = "NA";
"cassUP_p2" = "NA";
"cassUW" = 0;
"cost_LMO_areaBasis" = "areaUnit";
"cost_manual_harrowing" = "NA";
"cost_manual_ploughing" = "NA";
"cost_manual_ridging" = "NA";
"cost_tractor_harrowing" = "NA";
"cost_tractor_ploughing" = "NA";
"cost_tractor_ridging" = "NA";
"cost_weeding1" = "NA";
"cost_weeding2" = "NA";
"country" = "NG";
"email" = FALSE;
"SMS" = FALSE;
"fallowGreen" = FALSE;
"fallowHeight" = 100;
"fallowType" = "none";
"harrowing" = FALSE;
"intercrop" = FALSE;
"lat" = 7.253681500291449;
"lon" = 7.567810296448954;
"maizePD" = "fresh_cob";
"maizeUP" = "NA";
"maizeUW" = "NA";
"sweetPotatoPD" = "tubers";
"sweetPotatoUW" = "NA";
"sweetPotatoUP" = "NA";
"maxInv" = "36000";
"method_harrowing" = "manual";
"method_ploughing" = "manual";
"method_ridging" = "manual";
"nameSF" = "NA";
"ploughing" = FALSE;
"problemWeeds" = FALSE;
"ridging" = FALSE;
"riskAtt" = 0;
"saleSF" = FALSE;
"tractor_harrow" = FALSE;
"tractor_plough" = FALSE;
"tractor_ridger" = FALSE;
"userEmail" = "NA";
"userField" = "Field description";
"userName" = "Akilimo";
"userPhoneCC" = "NA";
"userPhoneNr" = "NA";
"fullPhoneNumber" = "NA";
"YaraMila_UNIKavailable" = FALSE;
"YaraMila_UNIKCostperBag" = "NA";
"YaraMila_UNIKBagWt" = 50




require(plyr)
require(dplyr)
require(ggplot2)

ds <- read.csv("SP_rec.csv", stringsAsFactors = FALSE)
ds$PD <- as.Date(	ds$PD, format = "%Y-%m-%d")

ds$HD <- as.Date(ds$HD, format = "%Y-%m-%d")
country <- read.csv("SP_MarkDownText.csv", stringsAsFactors = FALSE)$country
if(country == "NG"){
  nameCurrency <- "Change in revenue [NGN]"
  tt <- "Change in revenue (NGN):"
}else{
  nameCurrency <- "Change in revenue [TZS]"
  tt <- "Change in revenue (TZS):"
}

if(!require("scales")) install.packages("plyr"); library("scales")




personalized_info <- read.csv("FR_MarkDownText.csv")

personalized_info$current_yield <- round(personalized_info$current_yield, digits=0)

personalized_info$actualmn <- lubridate::month(ymd(personalized_info$plant_date))
personalized_info$month  <- month.abb[personalized_info$actualmn]
personalized_info$actuald <- lubridate::day(ymd(personalized_info$plant_date))
personalized_info$actualday <- lubridate::wday(ymd(personalized_info$plant_date))
personalized_info$actualYR <- lubridate::year(ymd(personalized_info$plant_date))
personalized_info$plant_date2 <- paste(personalized_info$actuald, personalized_info$month, personalized_info$actualYR, sep = "-")

personalized_info$actualmn <- lubridate::month(ymd(personalized_info$hvst_date))
personalized_info$month  <- month.abb[personalized_info$actualmn]
personalized_info$actuald <- lubridate::day(ymd(personalized_info$hvst_date))
personalized_info$actualday <- lubridate::wday(ymd(personalized_info$hvst_date))
personalized_info$actualYR <- lubridate::year(ymd(personalized_info$hvst_date))
personalized_info$hvst_date2 <- paste(personalized_info$actuald, personalized_info$month, personalized_info$actualYR, sep = "-")



totalRevenuemoney <-read.csv("totalRevenuemoney.csv")
totalSalemoney <-read.csv("totalSalemoney.csv")
totalCostmoney <-read.csv("totalCostmoney.csv")


require(plyr)
require(dplyr)
require(ggplot2)
library(png)

ds <- read.csv("SP_rec.csv", stringsAsFactors = FALSE)
ds$PD <- as.Date(	ds$PD, format = "%Y-%m-%d")

ds$HD <- as.Date(ds$HD, format = "%Y-%m-%d")
country <- read.csv("SP_MarkDownText.csv", stringsAsFactors = FALSE)$country
if(country == "TZ"){
  nameCurrency <- "mabadiliko katika mapato [TZS]"
  tt <- "mabadiliko katika mapato (TZS):"

}else{
  nameCurrency <- "Change in revenue [NGN]"
  tt <- "Change in revenue (NGN):"
}

if(!require("scales")) install.packages("plyr"); library("scales")


gg <- ggplot(data=ds, aes(x=as.factor(rPWnr), y=as.factor(rHWnr), fill=dGR))+
  geom_tile(colour="grey95", size=2)+
  scale_fill_gradient2(low="red", high="green", mid="linen", midpoint=0, labels=comma, name=nameCurrency)+
  scale_colour_manual(values=c("NA"="grey90", "R" = "purple", "C"="black"))+
  theme(legend.key = element_rect(colour = 'purple', fill = 'pink', size = 0.5, linetype='dashed'))+
  geom_tile(data=ds[ds$CP==TRUE,], colour="black", size=2)+
  geom_tile(data=ds[1,])+
  geom_tile(data=ds[1,], colour="purple", size=2, show.legend=FALSE)+
  geom_text(data=ds[ds$CP==TRUE,], aes(label="Ulichopendekeza"), size=6)+
  geom_text(data=ds[1,], colour="purple", aes(label="Kinachopendekezwa"), size=6)+
  scale_x_discrete(breaks=seq(min(ds$rPWnr), max(ds$rPWnr), by=2), labels=format(sort(unique(ds$PD)), "%d-%b"))+
  scale_y_discrete(breaks=seq(min(ds$rHWnr), max(ds$rHWnr), by=2), labels=format(sort(unique(ds$HD)), "%d-%b"))+
  xlab("\nTarehe ya kupanda")+
  ylab("Tarehe ya kuvuna\n")+
  guides(fill=guide_legend(title=tt)) +
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))+
  theme_bw()+
  theme(axis.title = element_text(size=20, colour="black", face="bold"),
        axis.text.x = element_text(size=18, colour="black", hjust=0, vjust=0.5),
        axis.text.y = element_text(size=18, colour="black", angle = 0, hjust = 0.5),
        legend.title = element_text(size=18, face="bold"),
        legend.text = element_text(size=18),legend.position = "top",
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank()
  )

ggsave("spgg.png", gg, width=12, height = 8)
img <- readPNG('spgg.png')
grid::grid.raster(img)


TRNS <- read.csv("translations_TEST.csv",  stringsAsFactors = FALSE)
norecom_ng <- gsub(pattern = "\"",replacement = "",TRNS$norecom[1]); norecom_tz <- gsub(pattern = "\"",replacement = "",TRNS$norecom[2]);notapply_ng <- gsub(pattern = "\"",replacement = "",TRNS$notapply[1]); notapply_tz <- gsub(pattern = "\"",replacement = "",TRNS$notapply[2])
recloc_ng <- gsub(pattern = "\"",replacement = "",TRNS$recloc[1]);  recloc_tz <- gsub(pattern = "\"",replacement = "",TRNS$recloc[2]);frnotrec_ng <- gsub(pattern = "\"",replacement = "",TRNS$frnotrec[1]) ;frnotrec_tz <- gsub(pattern = "\"",replacement = "",TRNS$frnotrec[2])
spinfo_ng <- gsub(pattern = "\"",replacement = "",TRNS$spinfo[1]) ;spinfo_tz <- gsub(pattern = "\"",replacement = "",TRNS$spinfo[2]);rechange_ng <- gsub(pattern = "\"",replacement = "",TRNS$rechange[1]); rechange_tz <- gsub(pattern = "\"",replacement = "",TRNS$rechange[2])
hvst_ng <- gsub(pattern = "\"",replacement = "",TRNS$hvst[1]);hvst_tz <- gsub(pattern = "\"",replacement = "",TRNS$hvst[2]);plnt_ng <- gsub(pattern = "\"",replacement = "",TRNS$plnt[1]); plnt_tz <- gsub(pattern = "\"",replacement = "",TRNS$plnt[2])
recRatt1_ng <- gsub(pattern = "\"",replacement = "",TRNS$recRatt1[1]) ; recRatt1_tz <- gsub(pattern = "\"",replacement = "",TRNS$recRatt1[2]);recRatt2_ng <- gsub(pattern = "\"",replacement = "",TRNS$recRatt2[1]) ; recRatt2_tz <- gsub(pattern = "\"",replacement = "",TRNS$recRatt2[2])
recPln_ng <- gsub(pattern = "\"",replacement = "",TRNS$recPln[1]) ; recPln_tz <- gsub(pattern = "\"",replacement = "",TRNS$recPln[2]);recHvs_ng <- gsub(pattern = "\"",replacement = "",TRNS$recHvs[1]); recHvs_tz <- gsub(pattern = "\"",replacement = "",TRNS$recHvs[2])
wks_ng <- gsub(pattern = "\"",replacement = "",TRNS$wks[1]) ; wks_tz <- gsub(pattern = "\"",replacement = "",TRNS$wks[2]) ;recPlnP_ng <- gsub(pattern = "\"",replacement = "",TRNS$recPlnP[1]) ; recPlnP_tz <- gsub(pattern = "\"",replacement = "",TRNS$recPlnP[2])
early_ng <- gsub(pattern = "\"",replacement = "",TRNS$early[1]); early_tz <- gsub(pattern = "\"",replacement = "",TRNS$early[2]);late_ng <- gsub(pattern = "\"",replacement = "",TRNS$late[1]) ; late_tz <- gsub(pattern = "\"",replacement = "",TRNS$late[2])
recPhv_ng <- gsub(pattern = "\"",replacement = "",TRNS$recPhv[1]) ; recPhv_tz <- gsub(pattern = "\"",replacement = "",TRNS$recPhv[2]) ;recrev_ng <- gsub(pattern = "\"",replacement = "",TRNS$recrev[1]) ; recrev_tz <- gsub(pattern = "\"",replacement = "",TRNS$recrev[2])
hvsdate_ng <- gsub(pattern = "\"",replacement = "",TRNS$hvsdate[1]) ; hvsdate_tz <- gsub(pattern = "\"",replacement = "",TRNS$hvsdate[2]);nochange_ng <- gsub(pattern = "\"",replacement = "",TRNS$nochange[1]) ; nochange_tz <- gsub(pattern = "\"",replacement = "",TRNS$nochange[2])
exp_ng <- gsub(pattern = "\"",replacement = "",TRNS$exp[1]) ; exp_tz <- gsub(pattern = "\"",replacement = "",TRNS$exp[2]);dec_ng <- gsub(pattern = "\"",replacement = "",TRNS$dec[1]) ; dec_tz <- gsub(pattern = "\"",replacement = "",TRNS$dec[2])
inc_ng <- gsub(pattern = "\"",replacement = "",TRNS$inc[1]) ; inc_tz <- gsub(pattern = "\"",replacement = "",TRNS$inc[2]);root_ng <- gsub(pattern = "\"",replacement = "",TRNS$root[1]) ; root_tz <- gsub(pattern = "\"",replacement = "",TRNS$root[2])
ton_ng <- gsub(pattern = "\"",replacement = "",TRNS$ton[1]) ; ton_tz <- gsub(pattern = "\"",replacement = "",TRNS$ton[2]) ;but_ng <- gsub(pattern = "\"",replacement = "",TRNS$but[1]) ; but_tz <- gsub(pattern = "\"",replacement = "",TRNS$but[2])
and_ng <- gsub(pattern = "\"",replacement = "",TRNS$and[1]) ; and_tz <- gsub(pattern = "\"",replacement = "",TRNS$and[2]) ;valinc_ng <- gsub(pattern = "\"",replacement = "",TRNS$valinc[1]) ; valinc_tz <- gsub(pattern = "\"",replacement = "",TRNS$valinc[2])
notot_ng <- gsub(pattern = "\"",replacement = "",TRNS$notot[1]) ; notot_tz <- gsub(pattern = "\"",replacement = "",TRNS$notot[2]) ;optim_ng <- gsub(pattern = "\"",replacement = "",TRNS$optim[1]) ; optim_tz <- gsub(pattern = "\"",replacement = "",TRNS$optim[2])
no_ng <- gsub(pattern = "\"",replacement = "",TRNS$no[1]) ; no_tz <- gsub(pattern = "\"",replacement = "",TRNS$no[2]) ;plo_ng <- gsub(pattern = "\"",replacement = "",TRNS$plo[1]) ; plo_tz <- gsub(pattern = "\"",replacement = "",TRNS$plo[2]) AS
ridg_ng <- gsub(pattern = "\"",replacement = "",TRNS$ridg[1]) ; ridg_tz <- gsub(pattern = "\"",replacement = "",TRNS$ridg[2]) ;decnet_ng <- gsub(pattern = "\"",replacement = "",TRNS$decnet[1]); decnet_tz <- gsub(pattern = "\"",replacement = "",TRNS$decnet[2])
werec_ng <- gsub(pattern = "\"",replacement = "",TRNS$werec[1]) ; werec_tz <- gsub(pattern = "\"",replacement = "",TRNS$werec[2]) ;plofol_ng <- gsub(pattern = "\"",replacement = "",TRNS$plofol[1]) ; plofol_tz <- gsub(pattern = "\"",replacement = "",TRNS$plofol[2])
noridg_ng <- gsub(pattern = "\"",replacement = "",TRNS$noridg[1]) ; noridg_tz <- gsub(pattern = "\"",replacement = "",TRNS$noridg[2])
noplo_ng <- gsub(pattern = "\"",replacement = "",TRNS$noplo[1]) ;noplo_tz <- gsub(pattern = "\"",replacement = "",TRNS$noplo[2]);zerot_ng <- gsub(pattern = "\"",replacement = "",TRNS$zerot[1]) ; zerot_tz <- gsub(pattern = "\"",replacement = "",TRNS$zerot[2])
changcost_ng <- gsub(pattern = "\"",replacement = "",TRNS$changcos[1]) ; changcost_tz <- gsub(pattern = "\"",replacement = "",TRNS$changcos[2]) ; this_ng <- gsub(pattern = "\"",replacement = "",TRNS$this[1]); this_tz <- gsub(pattern = "\"",replacement = "",TRNS$this[2])
decr_ng <- gsub(pattern = "\"",replacement = "",TRNS$decr[1]);decr_tz <- gsub(pattern = "\"",replacement = "",TRNS$decr[2]);incr_ng <- gsub(pattern = "\"",replacement = "",TRNS$incr[1]);incr_tz <- gsub(pattern = "\"",replacement = "",TRNS$incr[2])
costb_ng <- gsub(pattern = "\"",replacement = "",TRNS$costb[1]);costb_tz <- gsub(pattern = "\"",replacement = "",TRNS$costb[2]);rtprod_ng <- gsub(pattern = "\"",replacement = "",TRNS$rtprod[1]);rtprod_tz <- gsub(pattern = "\"",replacement = "",TRNS$rtprod[2])
rtcurr_ng <- gsub(pattern = "\"",replacement = "",TRNS$rtcurr[1]);rtcurr_tz <- gsub(pattern = "\"",replacement = "",TRNS$rtcurr[2]);netinc_ng <- gsub(pattern = "\"",replacement = "",TRNS$netinc[1]);netinc_tz <- gsub(pattern = "\"",replacement = "",TRNS$netinc[2])
rtwill_ng <- gsub(pattern = "\"",replacement = "",TRNS$rtwill[1]);rtwill_tz <- gsub(pattern = "\"",replacement = "",TRNS$rtwill[2]);by_ng <- gsub(pattern = "\"",replacement = "",TRNS$by[1]);by_tz <- gsub(pattern = "\"",replacement = "",TRNS$by[2])
tonb_ng <- gsub(pattern = "\"",replacement = "",TRNS$tonb[1]);tonb_tz <- gsub(pattern = "\"",replacement = "",TRNS$tonb[2]);netno_ng <- gsub(pattern = "\"",replacement = "",TRNS$netno[1]);netno_tz <- gsub(pattern = "\"",replacement = "",TRNS$netno[2])
incomp_ng <- gsub(pattern = "\"",replacement = "",TRNS$incomp[1]);incomp_tz <- gsub(pattern = "\"",replacement = "",TRNS$incomp[2]);recap_ng <- gsub(pattern = "\"",replacement = "",TRNS$recap[1]);recap_tz <- gsub(pattern = "\"",replacement = "",TRNS$recap[2])
kgof_ng <- gsub(pattern = "\"",replacement = "",TRNS$kgof[1]);kgof_tz <- gsub(pattern = "\"",replacement = "",TRNS$kgof[2]);area_ng <- gsub(pattern = "\"",replacement = "",TRNS$area[1]);area_tz <- gsub(pattern = "\"",replacement = "",TRNS$area[2])
willc_ng <- gsub(pattern = "\"",replacement = "",TRNS$willc[1]);willc_tz <- gsub(pattern = "\"",replacement = "",TRNS$willc[2]);extrap_ng <- gsub(pattern = "\"",replacement = "",TRNS$extrap[1]);extrap_tz <- gsub(pattern = "\"",replacement = "",TRNS$extrap[2])
tonof_ng <- gsub(pattern = "\"",replacement = "",TRNS$tonof[1]);tonof_tz <- gsub(pattern = "\"",replacement = "",TRNS$tonof[2]);netincr_ng <- gsub(pattern = "\"",replacement = "",TRNS$netincr[1]) ; netincr_tz <- gsub(pattern = "\"",replacement = "",TRNS$netincr[2])





