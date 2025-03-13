#!/usr/bin/Rscript
#!/usr/bin/env Rscript

#require(methods)
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
library(limSolve)
library(leaflet)
library(mapview)
library(flexdashboard)
library(lubridate)
library(plumber)
library(grid)
library(mailR)
library(rJava)
# library(odkr)

print("Setting pandoc environment")
Sys.setenv(RSTUDIO_PANDOC = '/usr/lib/rstudio-server/bin/pandoc')

pandoc <- Sys.getenv("RSTUDIO_PANDOC")

print(paste("pandoc is located in :", pandoc))

setwd(getwd())
# source("AkilimoFunctionsStable.R")
source("AkilimoFunctions_2.R")


root <- plumber$new()

##Production endpoints
mainWrapper <- plumber$new("main-wrapper.R")

# tanzania <- plumber$new("tz-api-wrapper.R")
# nigeria <- plumber$new("ng-api-wrapper.R")

root$mount("/api/v2/dst/recommendation/tz", mainWrapper)
root$mount("/api/v2/dst/recommendation/ng", mainWrapper)

## DEMO endpoints
# tanzaniaDemo <- plumber$new("tz-api-wrapper-demo.R")
# root$mount("/api/v2/dst/recommendation/demo/tz", tanzaniaDemo)
# nigeriaDemo <- plumber$new("ng-api-wrapper-demo.R")
# root$mount("/api/v2/dst/recommendation/demo/ng", nigeriaDemo)

root$routes

print(paste("Plumber Production Script started at: ", Sys.time()))
root$run(port = 8000, host = "0.0.0.0", swagger = FALSE)

