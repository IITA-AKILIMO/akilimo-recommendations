#* SHORT DEF:   Wrapper function to calculate recommendations across all use cases.
#* RETURNS:     Vector of with recommendation texts to display.
#* DESCRIPTION: Function with all inputs required by the DST app, and calling use case-specific wrapper functions to calculated recommendations on IC, FR, PP and SP.
#* This function also calls on functions to send recommendation reports by SMS and/or email.
#* Input list
#* @param country    : Character, c("NG", "TZ")
#* @param lat        : Numeric, Latitude in decimal degrees
#* @param lon        : Numeric, Longitude in decimal degrees
#* @param area  : Numeric,  value for area of the field, if the user does not give this value, it will be set at 1
#* @param areaUnits  : Character, c("acre", "ha" or "m2")
#* @param IC         : Logical, c(TRUE, FALSE), indicating if an intercrop (TRUE) or a monocrop (FALSE) is grown
#* @param intercrop  : Character, c(NA, "maize", "sweetpotato"), Intercrop species grown, either maize or sweetpotato, or NA if monocrop, shold be in
#* @param FR         : Logical, c(NA, TRUE, FALSE), indicating if fertilizer recommendations are requested, NA if IC == TRUE and country == "TZ"
#* @param PP         : Logical, c(NA, TRUE, FALSE), indicating if planting practice recommendations are requested, NA if IC == TRUE or country == "TZ"
#* @param SPP        : Logical, c(NA, TRUE, FALSE), indicating if scheduled planting - advice on planting date is requested, NA if IC == TRUE
#* @param SPH        : Logical, c(NA, TRUE, FALSE), indicating if scheduled planting - advice on harvest date is requested, NA if IC == TRUE
#* @param PD         : Character, Planting date, in format of the ith day of the year
#* @param HD         : Character, Harvest data, in format of number of days the crpo was on the field between planting and harvest
#* @param PD_window  : planting day window, it should be c(0,1,2), zero if the user has one fixed PD, 1 if PD can be +- 1 month and 2 if PD can be +-2 months
#* @param HD_window  : Harvest day window, it should be c(0,1,2), zero if the user has one fixed HD, 1 if HD can be +- 1 month and 2 if HD can be +-2 months
#* @param fallowType  : Categorical: c("bush", "broad_leaves", "grass", "none"), type of fallow prior to land clearing
#* @param fallowHeight: Categorical: c(100, 150, 200), height of the fallow prior to clearing
#* @param fallowGreen : Logical: c(TRUE, FALSE), indicating if the fallow is lush, fresh and green (TRUE) or withered, dry or dead (FALSE)
#* @param problemWeeds: Logical:  c(TRUE, FALSE), indicating the presence of any problem weeds that need to be controlled by herbicide (Tithonia, Imperata,...)
#* @param tractor_plough: Logical, indicating if the user has access to a tractor with plough, NA if PP != TRUE
#* @param tractor_harrow: Logical, indicating if the user has access to a tractor with harrow, NA if PP != TRUE
#* @param tractor_ridger: Logical, indicating if the user has access to a tractor with ridger, NA if PP != TRUE
#* @param cost_LMO_areaBasis: Categorical: c("areaUnit", "areaField"), indicating if the area basis for the cost of land operations is for 1 areaUnits, or for the entire field
#* @param cost_tractor_ploughing: Cost for a ploughing operation by tractor c() for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField), NA if tractor_plough == FALSE
#* @param cost_tractor_harrowing: Cost for a rarrowing operation by tractor for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField), NA if tractor_harrow == FALSE
#* @param cost_tractor_ridging: Cost for a ridging operation by tractor for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField), NA if tractor_ridger == FALSE
#* @param cost_manual_ploughing: Cost for a manual ploughing operation for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField)
#* @param cost_manual_harrowing: Cost for a manual harrowing operation for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField)
#* @param cost_manual_ridging: Cost for a manual ridging operation for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField)
#* @param cost_weeding1: Cost for the first weeding operation for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField)
#* @param cost_weeding2: Cost for the second and subsequent weeding operations for 1 acre/hectare (if areaBasis is areaUnit) or for the entire field (if areaBasis is areaField)
#* @param ploughing   : Logical, c(NA, TRUE, FALSE), indicating if the user conducts a ploughing operation in current practice, NA if PP != TRUE
#* @param harrowing   : Logical, c(NA, TRUE, FALSE), indicating if the user conducts a harrowing operation in current practice, NA if PP != TRUE
#* @param ridging     : Logical, c(NA, TRUE, FALSE), indicating if the user ridges his/her field in current practice (NA, TRUE, FALSE), NA if PP != TRUE
#* @param method_ploughing: Categorical: c("manual", "tractor", "N/A"), method of ploughing currently applied by the farmer. Note: "N/A" = not applicable becasue nPlough=0
#* @param method_harrowing: Categorical: c("manual", "tractor", "N/A"), method of harrowing currently applied by the farmer. Note: "N/A" = not applicable becasue nHarrow=0
#* @param method_ridging: Categorical: c("manual", "tractor", "N/A"), method of ridging currently applied by the farmer. Note: "N/A" = not applicable becasue ridges=FALSE
#* @param FCY        : Farmer-reported current yield, in tonnes FM per ha (optional, default value = 11)
#* @param CMP        : c(1,2,3,4,5),  Current maize performance, score on a scale of 1 (very yellow and stunted) .. 5 (tall and dark green), NA if IC != TRUE and FR != TRUE, or NA if the user does not know (NA = default)
#* @param saleSF     : Logical, c(NA, TRUE, FALSE), indicating if the user is selling roots to a registered starch factory at factory-fixed prices
#* @param nameSF     : c(NA, "AlliedAtlanticDistilleries", "MatnaStarch", "PsaltryMarketers", "PsaltryOutgrowers",	"Greentech", "ThaiFarm", "FJS"), Name of starch factory where roots will be sold, NA if saleSF = FALSE
#* @param cassPD     : c("roots", "chips", "flour", "gari"), Type of cassava produce sold
#* @param cassUW 	: c(1, 50, 100, 1000), Unit weight at which cassava produce is sold, in kg, common measures are 1 (per kg), 50 (per 50kg bag), 100 (per 100kg bag) and 1000 (per tonne).
#* @param cassUP 	: Price of 1 cassava produce unit (fresh wt) in local currency, can be NA if user does not know and then 12000 NGN and ... TZS is used as default
#* @param cassUP_m1: Price of 1 cassava produce unit (fresh wt) 1 month in the future, can be NA if users does not know;
#* @param cassUP_m2: Price of 1 cassava produce unit (fresh wt) 2 month in the future, can be NA if users does not know;
#* @param cassUP_p1: Price of 1 cassava produce unit (fresh wt) 1 month before, can be NA if users does not know
#* @param cassUP_p2: Price of 1 cassava produce unit (fresh wt) 2 month before, can be NA if users does not know
#* @param sweetPotatoPD: Type of sweet potato produce sold (tubers, flour), NA if IC != TRUE and country == "TZ"
#* @param sweetPotatoUW: Unit weight at which sweet potato produce is sold, in kg; common measures are 1 (per kg), 50 (per 50kg bag), 100 (per 100kg bag), NA if IC != TRUE and country == "TZ"; can be NA if user does not know.
#* @param sweetPotatoUP: Price of 1 sweet potato produce unit in local currency, NA if IC != TRUE and country == "TZ"; can be NA if user does not know.
#* @param maizePD    : Type of maize produce sold (fresh cobs, dry cobs, grain), NA if IC != TRUE and country == "NG"
#* @param maizeUW    : c(NA, 1, 50, 100), Unit weight at which maize produce is sold, in kg, common measures are 1 (per kg), 50 (per 50kg bag), 100 (per 100kg bag), NA if IC != TRUE and country == "NG" or NA if maizePD == "grain", can be NA if user does not know.
#* @param maizeUP    : Price of 1 maize produce unit (or cob, if maizePC == TRUE) in local currency, NA if IC != TRUE and country == "NG", can be NA if user does not know.
#* @param maxInv     : Maximal investment in fertilizer, for the area of the field in local currency, NA if FR != TRUE, default = NA (if user does not wish to set an investment ceiling)
#* @param SMS        : Logical, c(TRUE, FALSE), indicating if recommendations must be sent by SMS to the user
#* @param email      : Logical, c(TRUE, FALSE), indicating if recommendations must be sent by email to the user
#* @param userPhoneCC: Country code of the phone number of the user requesting the recommendations (to send recommendations by SMS), default = NA (if user does not wish to receive recommendations by SMS), example 234 for Nigeria
#* @param userPhoneNr: Phone number of the user requesting the recommendations, without the initial zero (to send recommendations by SMS), default = NA (if user does not wish to receive recommendations by SMS), excludes the initial zero, stored as numerical (e.g., 789123456)
#* @param userName   : Name of the user requesting the recommendations (to be included in the email report), default = NA (if user does not wish to receive recommendations by email)
#* @param userEmail  : Email address of the user requesting the recommendations (to be included in the email report), default = NA (if user does not wish to receive recommendations by email)
#* @param userField  : Name or desciption of the field (to be included in the email report, and aid the user to recall for which field recommendations were requested), default = NA (if user does not wish to receive recommendations by email)
#* @param riskAtt = c(0, 1, 2): Risk attitude of the farmer, with 0 being very risk-averse (low income farmers who cannot afford to loose on investment), 1 = risk-neutral and 2 = risk-loving (higher income farmers willing to take their chances for higher net returns)
#* @param ureaavailable
#* @param ureaCostperBag
#* @param ureaBagWt
#* @param MOPavailable = TRUE
#* @param MOPCostperBag = NA
#* @param MOPBagWt = 50
#* @param DAPavailable = TRUE
#* @param DAPCostperBag = NA
#* @param DAPBagWt = 50
#* @param NPK201010available = TRUE
#* @param NPK201010CostperBag = NA
#* @param NPK201010BagWt =50
#* @param NPK151515available = TRUE
#* @param NPK151515CostperBag = NA
#* @param NPK151515BagWt =50
#* @param TSPavailable = TRUE
#* @param TSPCostperBag = NA
#* @param TSPBagWt = 50
#* @param NPK171717available = TRUE
#* @param NPK171717CostperBag = NA
#* @param NPK171717BagWt = 50
#* @param Nafakaavailable = FALSE
#* @param NafakaCostperBag = NA
#* @param NafakaBagWt = 50
#* @param CANavailable = FALSE
#* @param CANCostperBag = NA
#* @param CANBagWt = 50
#* @param SSPavailable = FALSE
#* @param SSPCostperBag = NA
#* @param SSPBagWt = 50
#* @param newFert1name = NA		:this is for new fertilizer and if defined the N_cont, P2O5, K2O, (all in percent like 20, 15, ..)and CostperBag & BagWt have to be provided. if any of these input is missing, OtherFertilizers should be NULL
#* @param newFert1N_cont=NA		: N content of the new fertilizer
#* @param newFert1P2O5=NA	    : P2O5 of the new fertilizer
#* @param newFertK2O = NA		: K2O of the new fertilizer
#* @param newFertCostperBag=NA		: cost per bag,
#* @param newFert1BagWt=NA			: The weight of the bag with new fertilizer
#* @param newFert2name = NA
#* @param newFert2N_cont=NA
#* @param newFert2P2O5=NA
#* @param newFert2K2O = NA
#* @param newFert2CostperBag=NA
#* @param newFert2BagWt=NA
#* @param newFert3name = NA
#* @param newFert3N_cont=NA
#* @param newFert3P2O5=NA
#* @param newFert3K2O = NA
#* @param newFert3CostperBag=NA
#* @param newFert3BagWt=NA
#* @param newFert4name = NA
#* @param newFert4N_cont=NA
#* @param newFert4P2O5=NA
#* @param newFert4K2O = NA
#* @param newFert4CostperBag	=NA
#* @param newFert4BagWt=NA
#* @param newFert5name = NA
#* @param newFert5N_cont=NA
#* @param newFert5P2O5=NA
#* @param newFert5K2O = NA
#* @param newFert5CostperBag=NA
#* @param newFert5BagWt=NA
#* @json
#* @post /compute
getRecommendation <- function(country = c("NG", "TZ"),
lat,
lon,
area,
areaUnits = c("acre", "ha", "m2"),
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
nameSF = c(NA, "AlliedAtlanticDistilleries", "MatnaStarch", "PsaltryMarketers", "PsaltryOutgrowers", "Greentech", "ThaiFarm", "FJS"),
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
ureaavailable = c(TRUE, FALSE), ureaCostperBag = NA, ureaBagWt =50,
MOPavailable = c(TRUE, FALSE), MOPCostperBag = NA, MOPBagWt =50,
DAPavailable = c(TRUE, FALSE), DAPCostperBag = NA, DAPBagWt =50,
NPK201010available = c(TRUE, FALSE), NPK201010CostperBag = NA, NPK201010BagWt = 50,
NPK151515available = c(TRUE, FALSE), NPK151515CostperBag = NA, NPK151515BagWt =50,
TSPavailable = c(TRUE, FALSE), TSPCostperBag = NA, TSPBagWt =50,
NPK171717available = c(TRUE, FALSE), NPK171717CostperBag = NA, NPK171717BagWt = 50,
Nafakaavailable = c(FALSE, FALSE), NafakaCostperBag = NA, NafakaBagWt =50,
YaraMila_UNIKavailable=TRUE, YaraMila_UNIKCostperBag=NA, YaraMila_UNIKBagWt=50,
CANavailable = c(TRUE, FALSE), CANCostperBag = NA, CANBagWt =50,
SSPavailable = c(TRUE, FALSE), SSPCostperBag = NA, SSPBagWt = 50,
newFert1name = c(TRUE, FALSE) , newFert1N_cont = NA, newFert1P2O5 = NA, newFert1K2O = NA, newFert1CostperBag = NA, newFert1BagWt = NA,
newFert2name = c(TRUE, FALSE), newFert2N_cont = NA, newFert2P2O5 = NA, newFert2K2O = NA, newFert2CostperBag = NA, newFert2BagWt = NA,
newFert3name = c(TRUE, FALSE), newFert3N_cont = NA, newFert3P2O5 = NA, newFert3K2O = NA, newFert3CostperBag = NA, newFert3BagWt = NA,
newFert4name = c(TRUE, FALSE), newFert4N_cont = NA, newFert4P2O5 = NA, newFert4K2O = NA, newFert4CostperBag = NA, newFert4BagWt = NA,
newFert5name = c(TRUE, FALSE), newFert5N_cont = NA, newFert5P2O5 = NA, newFert5K2O = NA, newFert5CostperBag = NA, newFert5BagWt = NA)
{
    print("Setting demo nigerian parameter")


    ureaavailable = FALSE;
    ureaCostperBag = "NA";
    ureaBagWt = 50;

    MOPavailable = FALSE;
    MOPCostperBag = "NA";
    MOPBagWt = 50;

    DAPavailable = FALSE;
    DAPCostperBag = "NA";
    DAPBagWt = 50;

    NPK201010available = FALSE;
    NPK201010CostperBag = "NA";
    NPK201010BagWt =50;

    NPK151515available = TRUE;
    NPK151515CostperBag = "NA";
    NPK151515BagWt =50;

    TSPavailable = TRUE;
    TSPCostperBag = "NA";
    TSPBagWt = 50;

    NPK171717available = FALSE;
    NPK171717CostperBag = "NA";
    NPK171717BagWt = 50;

    Nafakaavailable = FALSE;
    NafakaCostperBag = "NA";
    NafakaBagWt = 50;

    CANavailable = FALSE;
    CANCostperBag = "NA";
    CANBagWt = 50;

    SSPavailable = FALSE;
    SSPCostperBag = "NA";
    SSPBagWt = 50;

    YaraMila_UNIKavailable=FALSE;
    YaraMila_UNIKCostperBag="NA";
    YaraMila_UNIKBagWt=50

    country="NG";
    newFert1name="NA"; newFert1N_cont="NA"; newFert1P2O5="NA"; newFert1K2O="NA"; newFert1CostperBag="NA"; newFert1BagWt="NA";
    newFert2name="NA"; newFert2N_cont="NA"; newFert2P2O5="NA"; newFert2K2O="NA"; newFert2CostperBag="NA"; newFert2BagWt="NA";
    newFert3name="NA"; newFert3N_cont="NA"; newFert3P2O5="NA"; newFert3K2O="NA"; newFert3CostperBag="NA"; newFert3BagWt="NA";
    newFert4name="NA"; newFert4N_cont="NA"; newFert4P2O5="NA"; newFert4K2O="NA"; newFert4CostperBag="NA"; newFert4BagWt="NA";
    newFert5name="NA"; newFert5N_cont="NA"; newFert5P2O5="NA"; newFert5K2O="NA"; newFert5CostperBag="NA"; newFert5BagWt="NA"


    #lat = -1.235486; lon = 36.2548;
    lat = 8.725; lon = 4.025;

    #PD=254; HD=350;
    #
    # PD = "2018-09-11"
    # HD = "2019-08-27"


    cassUP = "NA"; cassUP_m1 = "NA"; cassUP_m2 = "NA"; cassUP_p1 = "NA"; cassUP_p2 = "NA"
    cost_weeding1 = cost_weeding2 = "NA"
    maizeUP = "NA"
    cost_tractor_ploughing = cost_tractor_harrowing = cost_tractor_ridging = cost_manual_ploughing = cost_manual_harrowing = cost_manual_ridging = "NA"



    country = "NG";  FCY = 11.25 ## HD = number of days on the field
    area = 1; areaUnits = "ha"; maxInv = 72000; saleSF = TRUE; nameSF = "PsaltryMarketers"; cassPD = "roots"
    cassUW = 1000;
    ploughing = TRUE; ridging = TRUE
    method_ploughing = method_ridging = "manual"

    saleSF = TRUE; nameSF = "PsaltryMarketers"
    fallowType = "bush"; fallowHeight = 100; fallowGreen = TRUE; problemWeeds = FALSE
    tractor_plough = tractor_harrow = tractor_ridger = TRUE
    cost_LMO_areaBasis = "areaUnit";
    maizePD = "grain"; maizeUW = 50;

    print("Finished setting nigerian demo parameter")

    print("Setting of uneeded tanzania parameters")
    sweetPotatoUW <- NA;
    sweetPotatoUP <- NA;
    sweetPotatoPD <- NA;
    print("End setting of uneeded tanzania parameters")

    fertilizers <- fertilizerFunc(ureaavailable = ureaavailable, ureaCostperBag = ureaCostperBag, ureaBagWt = ureaBagWt,
    MOPavailable = MOPavailable, MOPCostperBag = MOPCostperBag, MOPBagWt = MOPBagWt,
    DAPavailable = DAPavailable, DAPCostperBag = DAPCostperBag, DAPBagWt = DAPBagWt,
    NPK201010available = NPK201010available, NPK201010CostperBag = NPK201010CostperBag, NPK201010BagWt = NPK201010BagWt,
    NPK151515available = NPK151515available, NPK151515CostperBag = NPK151515CostperBag, NPK151515BagWt = NPK151515BagWt,
    TSPavailable = TSPavailable, TSPCostperBag = TSPCostperBag, TSPBagWt = TSPBagWt,
    NPK171717available = NPK171717available, NPK171717CostperBag = NPK171717CostperBag, NPK171717BagWt = NPK171717BagWt,
    Nafakaavailable = Nafakaavailable, NafakaCostperBag = NafakaCostperBag, NafakaBagWt = NafakaBagWt,
    CANavailable = CANavailable, CANCostperBag = CANCostperBag, CANBagWt = CANBagWt,
    SSPavailable = SSPavailable, SSPCostperBag = SSPCostperBag, SSPBagWt = SSPBagWt,
    YaraMila_UNIKavailable = YaraMila_UNIKavailable, YaraMila_UNIKCostperBag = YaraMila_UNIKCostperBag, YaraMila_UNIKBagWt = YaraMila_UNIKBagWt,
    newFert1name = newFert1name, newFert1N_cont = newFert1N_cont, newFert1P2O5 = newFert1P2O5,
    newFert1K2O = newFert1K2O, newFert1CostperBag = newFert1CostperBag, newFert1BagWt = newFert1BagWt,
    newFert2name = newFert2name, newFert2N_cont = newFert2N_cont, newFert2P2O5 = newFert2P2O5,
    newFert2K2O = newFert2K2O, newFert2CostperBag = newFert2CostperBag, newFert2BagWt = newFert2BagWt,
    newFert3name = newFert3name, newFert3N_cont = newFert3N_cont, newFert3P2O5 = newFert3P2O5,
    newFert3K2O = newFert3K2O, newFert3CostperBag = newFert3CostperBag, newFert3BagWt = newFert3BagWt,
    newFert4name = newFert4name, newFert4N_cont = newFert4N_cont, newFert4P2O5 = newFert4P2O5,
    newFert4K2O = newFert4K2O, newFert4CostperBag = newFert4CostperBag, newFert4BagWt = newFert4BagWt,
    newFert5name = newFert5name, newFert5N_cont = newFert5N_cont, newFert5P2O5 = newFert5P2O5,
    newFert5K2O = newFert5K2O, newFert5CostperBag = newFert5CostperBag, newFert5BagWt = newFert5BagWt, country = country)


    ### set "NA" to NA
    if (! is.na(cassUP_m1) & ! is.numeric(cassUP_m1))cassUP_m1 <- NA
    if (! is.na(cassUP_m2) & ! is.numeric(cassUP_m2))cassUP_m2 <- NA
    if (! is.na(cassUP_p1) & ! is.numeric(cassUP_p1))cassUP_p1 <- NA
    if (! is.na(cassUP_p2) & ! is.numeric(cassUP_p2))cassUP_p2 <- NA

    if (! is.na(sweetPotatoUW) & ! is.numeric(sweetPotatoUW))sweetPotatoUW <- NA
    if (! is.na(sweetPotatoUP) & ! is.numeric(sweetPotatoUP))sweetPotatoUP <- NA

    if (! is.na(cassUP) & ! is.numeric(cassUP))cassUP <- NA
    if (! is.na(maizeUP) & ! is.numeric(maizeUP))maizeUP <- NA
    if (! is.na(cost_manual_ploughing) & ! is.numeric(cost_manual_ploughing))cost_manual_ploughing <- NA
    if (! is.na(cost_manual_harrowing) & ! is.numeric(cost_manual_harrowing))cost_manual_harrowing <- NA

    if (! is.na(cost_manual_ridging) & ! is.numeric(cost_manual_ridging))cost_manual_ridging <- NA
    if (! is.na(cost_tractor_ploughing) & ! is.numeric(cost_tractor_ploughing))cost_tractor_ploughing <- NA
    if (! is.na(cost_tractor_harrowing) & ! is.numeric(cost_tractor_harrowing))cost_tractor_harrowing <- NA
    if (! is.na(cost_tractor_ridging) & ! is.numeric(cost_tractor_ridging))cost_tractor_ridging <- NA

    if (! is.na(cost_weeding1) & ! is.numeric(cost_weeding1))cost_weeding1 <- NA
    if (! is.na(cost_weeding2) & ! is.numeric(cost_weeding2))cost_weeding2 <- NA
    if (! is.na(maizeUW) & ! is.numeric(maizeUW))maizeUW <- NA
    if (! is.na(maizeUP) & ! is.numeric(maizeUP))maizeUW <- NA
    if (! is.na(maxInv) & ! is.numeric(maxInv))maxInv <- NA
    if (! is.na(fallowHeight) & ! is.numeric(fallowHeight))fallowHeight <- NA

    if (nameSF == "NA")nameSF <- NA


    PD <- as.Date(PD, format = "%Y-%m-%d")
    HD <- as.Date(HD, format = "%Y-%m-%d")


    ## calculating rootUP based on cassUP, cassUW and conversion factor for cassava product sold
    rootConv <- data.frame(cassPD = c("roots", "chips", "flour", "gari"),
    conversion = c(1, 3, 3.2, 3.5))

    ## if cassava is to be sold to a processing factory, there should be a default price by factry and product
    # calculating rootUP based on cassUP, cassUW and conversion factor for cassava product sold
    rootConv <- data.frame(cassPD = c("roots", "chips", "flour", "gari"),
    conversion = c(1, 3, 3.2, 3.5))
    if (is.na(cassUP) &
        cassPD == "roots" &
        country == "NG") {cassUP = 12000; cassUW = 1000}
    if (is.na(cassUP) &
        cassPD == "chips" &
        country == "NG") {cassUP = 36000; cassUW = 1000}
    if (is.na(cassUP) &
        cassPD == "flour" &
        country == "NG") {cassUP = 38400; cassUW = 1000}
    if (is.na(cassUP) &
        cassPD == "gari" &
        country == "NG") {cassUP = 42000; cassUW = 1000}

    if (is.na(cassUP) &
        cassPD == "roots" &
        country == "TZ") {cassUP = 180000; cassUW = 1000}
    if (is.na(cassUP) &
        cassPD == "chips" &
        country == "TZ") {cassUP = 540000; cassUW = 1000}
    if (is.na(cassUP) &
        cassPD == "flour" &
        country == "TZ") {cassUP = 576000; cassUW = 1000}
    if (is.na(cassUP) &
        cassPD == "gari" &
        country == "TZ") {cassUP = 630000; cassUW = 1000}


    rootUP <- cassUP /
        cassUW /
        rootConv[rootConv$cassPD == cassPD,]$conversion * 1000
    rootUP_m1 <- cassUP_m1 /
        cassUW /
        rootConv[rootConv$cassPD == cassPD,]$conversion * 1000
    rootUP_m2 <- cassUP_m2 /
        cassUW /
        rootConv[rootConv$cassPD == cassPD,]$conversion * 1000
    rootUP_p1 <- cassUP_p1 /
        cassUW /
        rootConv[rootConv$cassPD == cassPD,]$conversion * 1000
    rootUP_p2 <- cassUP_p2 /
        cassUW /
        rootConv[rootConv$cassPD == cassPD,]$conversion * 1000

    # calculating cobUP based on maizeUP, maizeUW and conversion from grain to cobs if maizePD == "grain"
    if (is.na(maizeUP) & maizePD == "fresh_cob")maizeUP <- 50 #default price for 1 large fresh cob
    if (is.na(maizeUP) & maizePD == "grain") {
        maizeUP <- 230 #default price for 1 kg of maize grain
        maizeUW <- 1
    }

    cobUP <- ifelse(maizePD == "fresh_cob", maizeUP, maizeUP / maizeUW / 7.64) #1 kg of grain ~ 7.64 cobs

    # calculating tuberUP based on sweetPotatoUP, sweetPotatoUW and conversion factor for sweetPotato product sold
    tuberConv <- data.frame(sweetPotatoPD = c("tubers", "flour"),
    conversion = c(1, 3.2))
    if (is.na(sweetPotatoUP) &
        sweetPotatoPD == "tubers" &
        country == "TZ") {sweetPotatoUP = 120000; sweetPotatoUW = 1000}
    if (is.na(sweetPotatoUP) &
        sweetPotatoPD == "flour" &
        country == "TZ") {sweetPotatoUP = 384000; sweetPotatoUW = 1000}

    tuberUP <- sweetPotatoUP /
        sweetPotatoUW /
        tuberConv[tuberConv$sweetPotatoPD == sweetPotatoPD,]$conversion * 1000



    # calculating the field area
    areaHa <- area / ifelse(areaUnits == "ha", 1, ifelse(areaUnits == "acre", 2.47105, 10000))

    # create dataframe with cost of land management operations
    costLMO <- data.frame(operation = c(rep(c("ploughing", "harrowing", "ridging"), 2), "weeding1", "weeding2"),
    method = c(rep("manual", 3), rep("tractor", 3), NA, NA),
    cost = c(cost_manual_ploughing, cost_manual_harrowing, cost_manual_ridging, cost_tractor_ploughing, cost_tractor_harrowing, cost_tractor_ridging, cost_weeding1, cost_weeding2),
    area = ifelse(cost_LMO_areaBasis == "areaField", areaHa, ifelse(areaUnits == "acre", 0.404686, ifelse(areaUnits == "ha", 1, 0.0001))))

    costLMO_MD <- costLMO
    costLMO$costHa <- costLMO$cost / costLMO$area
    costLMO <- subset(costLMO, select = - c(area, cost))

    # add default values for LMO operations if missing
    if (is.na(cost_manual_ploughing))costLMO[costLMO$operation == "ploughing" & costLMO$method == "manual" ,]$costHa <- 17000 * 2.47105
    if (is.na(cost_manual_harrowing))costLMO[costLMO$operation == "harrowing" & costLMO$method == "manual" ,]$costHa <- 15000 * 2.47105
    if (is.na(cost_manual_ridging))costLMO[costLMO$operation == "ridging" & costLMO$method == "manual" ,]$costHa <- 12000 * 2.47105
    if (is.na(cost_tractor_ploughing) & tractor_plough)costLMO[costLMO$operation == "ploughing" & costLMO$method == "tractor",]$costHa <- 6000 * 2.47105
    if (is.na(cost_tractor_harrowing) & tractor_harrow)costLMO[costLMO$operation == "harrowing" & costLMO$method == "tractor",]$costHa <- 6000 * 2.47105
    if (is.na(cost_tractor_ridging) & tractor_ridger)costLMO[costLMO$operation == "ridging" & costLMO$method == "tractor",]$costHa <- 6000 * 2.47105
    if (is.na(cost_weeding1))costLMO[costLMO$operation == "weeding1",]$costHa <- 12500 * 2.47105
    if (is.na(cost_weeding2))costLMO[costLMO$operation == "weeding2",]$costHa <- 12500 * 2.47105

    if (! is.na(cost_manual_ploughing) |
        ! is.na(cost_manual_harrowing) |
        ! is.na(cost_manual_ridging) |
        ! is.na(cost_tractor_ploughing) |
        ! is.na(cost_tractor_harrowing) |
        ! is.na(cost_tractor_ridging) |
        ! is.na(cost_weeding1) |
        ! is.na(cost_weeding2)) {
        costLMO_MD$area <- paste(costLMO_MD$area, areaUnits, sep = "")
        write.csv(costLMO_MD, "costLMO.csv", row.names = FALSE)
    }else {
        costLMO_MD <- costLMO
        names(costLMO_MD) <- c("operation", "method", "cost")
        costLMO_MD$area <- "1ha"
        costLMO_MD$cost <- formatC(signif(costLMO_MD$cost, digits = 3), format = "f", big.mark = ",", digits = 0)
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

    if (FR == TRUE) {res[["FR"]] <- getFRrecommendations(lat = lat,
    lon = lon,
    pd = pd,
    pw = pw,
    HD = HD,
    had = had,
    maxInv = maxInv,
    fertilizers = fertilizers,
    rootUP = rootUP,
    areaHa = areaHa,
    country = country,
    FCY = FCY)
        if (all(res[["FR"]] != "We donot have fertilizer recommendation for your location.")) {
            recText[["FR"]] <- getFRrecText(ds = res$FR , country = country)
            write.csv(recText$FR, 'FR_recText.csv', row.names = FALSE)
            FR_MarkdownText(rr = res$FR, fertilizers = fertilizers, userName = userName, country = country,
            userPhoneNr = userPhoneNr, userField = userField, area = area, areaUnits = areaUnits,
            PD = PD, HD = HD, email = email, lat = lat, lon = lon, userPhoneCC = userPhoneCC,
            rootUP = rootUP, cassPD = cassPD, maxInv = maxInv)
            fertilizerAdviseTable(FR = TRUE, IC = FALSE)
        }else {
            FR <- FALSE ## so that the mark down will not try to run and get error
        }}


    if (IC == TRUE & country == "NG") {res[["IC"]] <- getICrecommendations(areaHa = areaHa,
    CMP = CMP,
    cobUP = cobUP,
    fertilizers = fertilizers,
    riskAtt = riskAtt)
        recText[["IC"]] <- getICrecText(ds = res$IC , country = country)
        write.csv(recText$IC, 'IC_recText.csv', row.names = FALSE)
        IC_MarkdownText(rr = res$IC, fertilizers = fertilizers, userName = userName, country = country,
        userPhoneNr = userPhoneNr, userField = userField, area = area, areaUnits = areaUnits,
        PD = PD, HD = HD, email = email, lat = lat, lon = lon, userPhoneCC = userPhoneCC, maizeUW = maizeUW, maizePD = maizePD,
        rootUP = rootUP, cassPD = cassPD, maxInv = maxInv, CMP = CMP, maizeUP = maizeUP)
        fertilizerAdviseTable(FR = FALSE, IC = TRUE)}



    if (IC == TRUE & country == "TZ") {res[["IC"]] <- getCISrecommendations(areaHa = areaHa,
    FCY = FCY,
    cassUP = cassUP,
    rootUP = rootUP,
    tuberUP = tuberUP,
    fertilizers = fertilizers,
    riskAtt = riskAtt)
        recText[["IC"]] <- getCISrecText(ds = res$IC , country = country)
        write.csv(recText$IC, 'IC_recText.csv', row.names = FALSE)}






    if (PP == TRUE & country == "NG") {res[["PP"]] <- getPPrecommendations(areaHa = areaHa,
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
        PP_MarkdownText(userName, country, userPhoneNr, userField, area, areaUnits, PD, HD, email, lat, lon, rootUP, cassPD,
        maxInv, ploughing, ridging, method_ploughing, method_ridging, userPhoneCC)}


    if (SPP == TRUE | SPH == TRUE) {res[["SP"]] <- getSPrecommendations(areaHa = areaHa,
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
        if (! is.data.frame(res[["SP"]])) {
            SPP <- FALSE
        }else {
            SPP <- TRUE
            recText[["SP"]] <- getSPrecText(ds = res$SP , country = country)
            write.csv(recText$SP, 'SP_recText.csv', row.names = FALSE)
            SP_MarkdownText(userName, country, userPhoneNr, userField, area, areaUnits, PD, HD, email, lat, lon, saleSF, nameSF,
            maxInv, ploughing, ridging, method_ploughing, method_ridging, userPhoneCC, CMP, riskAtt,
            PD_window, HD_window, cassPD, cassUW, cassUP, cassUP_m1, cassUP_m2, cassUP_p1, cassUP_p2)
        }}

    # extract the recommendation texts to a vector
    # recText <- NULL
    # for(i in 1:length(res)){
    # 	recText <- c(recText, res[[i]][["recText"]])
    # }



    # send recommendations to user
    #  if(email == TRUE & country ==  "NG")                       {sendEmailReport(userEmail=userEmail, FR=FR, IC=IC, PP=PP, SPP=SPP)}
    #  if(email == TRUE & country ==  "TZ")                       {sendEmailReport(userEmail=userEmail, FR=FR, SPP=SPP, IC=FALSE, PP=FALSE)}
    #  if(SMS == TRUE)                       {sendSMSReport(SMStext = recText, dst = paste(userPhoneCC, userPhoneNr, sep=""))}

    # return(recText)
    print("Returning nigeria recommendations")
    return(list(res, recText))
}