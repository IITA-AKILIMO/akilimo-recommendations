#!/usr/bin/Rscript --vanilla

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

os <- .Platform$OS.type

if(os=='windows'){
setwd(getwd())
}else{
  setwd('/home/akilimo/projects/akilimo_recommendation')
}

print(os)
print("Setting pandoc environment")
Sys.setenv(RSTUDIO_PANDOC = '/usr/lib/rstudio-server/bin/pandoc')

pandoc <- Sys.getenv("RSTUDIO_PANDOC")

# source("AkilimoFunctions_dev.R")
source("AkilimoFunctions_5D.R")

root <- Plumber$new()

#test endpoint
healthWrapper <- Plumber$new("plumber-health.R")

##Production endpoints
mainWrapper <- Plumber$new("prod-wrapper-v3.R")

root$mount("/api/health",healthWrapper)

root$mount("/api/v2/dst/recommendation", mainWrapper)

#root$mount("/api/v3/dst/recommendation/tz", mainWrapper)
#root$mount("/api/v3/dst/recommendation/ng", mainWrapper)
#root$mount("/api/v3/dst/recommendation/rw", mainWrapper)
#root$mount("/api/v3/dst/recommendation/gh", mainWrapper)
#root$mount("/api/v3/dst/recommendation/bi", mainWrapper)

root$routes

print(paste("pandoc is located in :", pandoc))
print(paste("Updated Plumber Script started at: ", Sys.time()))

root$run(port = 8090, host = "0.0.0.0", docs = FALSE)

