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

setwd(getwd())
# source("AkilimoFunctions_dev.R")
source("AkilimoFunctions_4C.R")

root <- Plumber$new()

##Production endpoints
mainWrapper <- Plumber$new("prod-wrapper-v2.R")
# mainWrapper <- Plumber$new("dev-wrapper.R")

root$mount("/api/v2/dst/recommendation/tz", mainWrapper)
root$mount("/api/v2/dst/recommendation/ng", mainWrapper)
root$mount("/api/v2/dst/recommendation/rw", mainWrapper)
root$mount("/api/v2/dst/recommendation/gh", mainWrapper)

root$routes

print(paste("pandoc is located in :", pandoc))
print(paste("Updated Plumber Development Script started at: ", Sys.time()))
root$run(port = 8090, host = "0.0.0.0", swagger = FALSE)

