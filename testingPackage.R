
# library(AKILIMOPackages)
# LazyData: TRUE
#
#
# setwd("/home/akilimo/projects/akilimo_recommendation")
# fd2 <- read.csv("fd2.csv")
# NOT_GIS_CON_2020 <- read.csv("NOT_GIS_CON_2020.csv")
# starchPrices <- read.csv("starchPrices.csv")
# translations_TEST <- read.csv("translations_TEST.csv")
# ISRIC_SoilData_2020 <- readRDS("ISRIC_SoilData_2020.RDS")
# Nigeria_WLY_LINTUL_2020 <- readRDS("Nigeria_WLY_LINTUL_2020.RDS")
# Nigeria_WLY_LINTUL_2020_Server <- readRDS("Nigeria_WLY_LINTUL_2020_Server.RDS")
# SoilData_fcy1 <- readRDS("SoilData_fcy1.RDS")
# Tanzania_WLY_LINTUL_2020 <- readRDS("Tanzania_WLY_LINTUL_2020.RDS")
# Tanzania_WLY_LINTUL_2020_Server <- readRDS("Tanzania_WLY_LINTUL_2020_Server.RDS")
#
# setwd("/home/akilimo/projects/akilimo_recommendation/AKILIMOPackages/data")
# save(fd2,NOT_GIS_CON_2020, starchPrices,translations_TEST,ISRIC_SoilData_2020,
#      Nigeria_WLY_LINTUL_2020, file = "AKIMOData.RData", compress="xz")
#
#
# save(fd2, file = "AKIMOData.rda", compress="xz")
# save(NOT_GIS_CON_2020, file = "NOT_GIS_CON_2020.RData", compress="xz")
# save(starchPrices, file = "starchPrices.RData", compress="xz")
# save(translations_TEST, file = "translations_TEST.RData", compress="xz")
# save(ISRIC_SoilData_2020, file = "ISRIC_SoilData_2020.RData", compress="xz")
# save(Nigeria_WLY_LINTUL_2020, file = "Nigeria_WLY_LINTUL_2020.RData", compress="xz")
# save(Nigeria_WLY_LINTUL_2020_Server, file = "Nigeria_WLY_LINTUL_2020_Server.RData", compress="xz")
# save(SoilData_fcy1, file = "SoilData_fcy1.RData", compress="xz")
# save(Tanzania_WLY_LINTUL_2020, file = "Tanzania_WLY_LINTUL_2020.RData", compress="xz")
# save(Tanzania_WLY_LINTUL_2020_Server, file = "Tanzania_WLY_LINTUL_2020_Server.RData", compress="xz")



# setwd("/home/akilimo/projects/akilimo_recommendation/AKILIMOPackages")
# readr::write_csv(fd2, path="inst/extdata/fd2.csv")
# readr::write_csv(NOT_GIS_CON_2020, path="inst/extdata/NOT_GIS_CON_2020.csv")
# readr::write_csv(starchPrices, path="inst/extdata/starchPrices.csv")
# readr::write_csv(translations_TEST, path="inst/extdata/translations_TEST.csv")
# readr::write_csv(ISRIC_SoilData_2020, path="inst/extdata/ISRIC_SoilData_2020.csv")
# readr::write_csv(Nigeria_WLY_LINTUL_2020, path="inst/extdata/Nigeria_WLY_LINTUL_2020.csv")
# readr::write_csv(Nigeria_WLY_LINTUL_2020_Server, path="inst/extdata/Nigeria_WLY_LINTUL_2020_Server.csv")
# readr::write_csv(SoilData_fcy1, path="inst/extdata/SoilData_fcy1.csv")
# readr::write_csv(Tanzania_WLY_LINTUL_2020, path="inst/extdata/Tanzania_WLY_LINTUL_2020.csv")
# readr::write_csv(Tanzania_WLY_LINTUL_2020_Server, path="inst/extdata/Tanzania_WLY_LINTUL_2020_Server.csv")

system.file("extdata","fd2.csv", package = "AKILIMOPackages")
sample_read(system.file("extdata","NOT_GIS_CON_2020.csv", package = "AKILIMOPackages"))
sample_read(system.file("extdata","fstarchPrices.csv", package = "AKILIMOPackages"))


fertilizerFunc(NPK201216available = TRUE, NPK201216CostperBag =0, NPK201216BagWt = 50,
               ureaavailable = TRUE, ureaCostperBag =0, ureaBagWt = 50,# ureaavailable = TRUE, ureaCostperBag = "NA", ureaBagWt = 50,
               MOPavailable = FALSE, MOPCostperBag = 0, MOPBagWt = 50,
               DAPavailable = FALSE, DAPCostperBag = 0, DAPBagWt = 50,
               NPK201010available = FALSE, NPK201010CostperBag = 0, NPK201010BagWt =50,
               NPK151515available = TRUE, NPK151515CostperBag = 0, NPK151515BagWt =50,
               TSPavailable = FALSE, TSPCostperBag = 0, TSPBagWt = 50,
               NPK171717available = FALSE, NPK171717CostperBag = 0, NPK171717BagWt = 50,
               CANavailable = FALSE, CANCostperBag = 0, CANBagWt = 50,
               SSPavailable = FALSE, SSPCostperBag = 0, SSPBagWt = 50,
               newFert1name="NA", newFert1N_cont="NA", newFert1P2O5="NA", newFert1K2O="NA", newFert1CostperBag=50, newFert1BagWt=50,
               newFert2name="NA", newFert2N_cont="NA", newFert2P2O5="NA", newFert2K2O="NA", newFert2CostperBag=50, newFert2BagWt=50,
               newFert3name="NA", newFert3N_cont="NA", newFert3P2O5="NA", newFert3K2O="NA", newFert3CostperBag=50, newFert3BagWt=50,
               newFert4name="NA", newFert4N_cont="NA", newFert4P2O5="NA", newFert4K2O="NA", newFert4CostperBag=50, newFert4BagWt=50,
               newFert5name="NA", newFert5N_cont="NA", newFert5P2O5="NA", newFert5K2O="NA", newFert5CostperBag=50, newFert5BagWt=50,
               country="NG")

getRecommendation(
  lat=8.725,
  lon=4.025,
  area = 1,
  areaUnits = "ha",
  IC = FALSE,
  FR = TRUE,
  PP = FALSE,
  SPP = FALSE,
  SPH = FALSE,
  PD = "2020-03-01",
  HD = "2021-04-01",
  PD_window = 2,
  HD_window = 2,
  fallowHeight = 100,
  fallowGreen = TRUE,
  problemWeeds = FALSE,
  tractor_plough = TRUE,
  tractor_harrow = TRUE,
  tractor_ridger = TRUE,
  cost_LMO_areaBasis = "areaUnit",
  cost_tractor_ploughing = 0,
  cost_tractor_harrowing = 0,
  cost_tractor_ridging = 0,
  cost_manual_ploughing = 0,
  cost_manual_harrowing = 0,
  cost_manual_ridging = 0,
  cost_weeding1 = 0,
  cost_weeding2 = 0,
  ploughing = FALSE,
  harrowing = TRUE,
  ridging = TRUE,
  method_ploughing = "manual",
  method_ridging = "tractor",
  FCY = 5,
  CMP = 2,
  saleSF = FALSE,
  nameSF = "PsaltryMarketers",
  cassPD = "roots",
  cassUW = 1000,
  cassUP = 0,
  cassUP_m1 = 0,
  cassUP_m2 = 0,
  cassUP_p1 = 0,
  cassUP_p2 = 0,
  maizePD = "grain",
  maizeUW = 50,
  maizeUP = 29200,
  sweetPotatoPD = "tubers",
  sweetPotatoUW = 100,
  sweetPotatoUP = 112500,
  maxInv = 90000,
  email = TRUE,
  userPhoneCC = 254,
  userPhoneNr = 702974480,
  userName = "Meklit",
  userEmail = "m.chernet@cgiar.org",
  userField = "NBO",
  riskAtt = 1,
  NPK201216available = TRUE, NPK201216CostperBag =0, NPK201216BagWt = 50,
  ureaavailable = TRUE, ureaCostperBag =0, ureaBagWt = 50,
  MOPavailable = FALSE, MOPCostperBag = 0, MOPBagWt = 50,
  DAPavailable = FALSE, DAPCostperBag = 0, DAPBagWt = 50,
  NPK201010available = FALSE, NPK201010CostperBag = 0, NPK201010BagWt =50,
  NPK151515available = TRUE, NPK151515CostperBag = 0, NPK151515BagWt =50,
  TSPavailable = FALSE, TSPCostperBag = 0, TSPBagWt = 50,
  NPK171717available = FALSE, NPK171717CostperBag = 0, NPK171717BagWt = 50,
  CANavailable = FALSE, CANCostperBag = 0, CANBagWt = 50,
  SSPavailable = FALSE, SSPCostperBag = 0, SSPBagWt = 50,
  newFert1name="NA", newFert1N_cont="NA", newFert1P2O5="NA", newFert1K2O="NA", newFert1CostperBag=50, newFert1BagWt=50,
  newFert2name="NA", newFert2N_cont="NA", newFert2P2O5="NA", newFert2K2O="NA", newFert2CostperBag=50, newFert2BagWt=50,
  newFert3name="NA", newFert3N_cont="NA", newFert3P2O5="NA", newFert3K2O="NA", newFert3CostperBag=50, newFert3BagWt=50,
  newFert4name="NA", newFert4N_cont="NA", newFert4P2O5="NA", newFert4K2O="NA", newFert4CostperBag=50, newFert4BagWt=50,
  newFert5name="NA", newFert5N_cont="NA", newFert5P2O5="NA", newFert5K2O="NA", newFert5CostperBag=50, newFert5BagWt=50,
  country="NG"
)






