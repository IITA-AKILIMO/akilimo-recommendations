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
#library(renv)

#renv::init()

os <- .Platform$OS.type

if(os != "windows"){
  # setwd("/home/akilimo/projects/akilimo_recommendation")
  Sys.setenv(RSTUDIO_PANDOC = "/usr/lib/rstudio-server/bin/pandoc")
}

pandoc <- Sys.getenv("RSTUDIO_PANDOC")

source("AkilimoFunctions_5D.R")

source("process-FR.R")
source("process-IC.R")
source("process-PP.R")
source("process-SP.R")

root <- plumber$new()

#health endpoint
#healthWrapper <- plumber$new("api-health.R")
#root$mount("/api/", healthWrapper)

##Production endpoints
mainWrapper <- plumber$new("api-wrapper.R")

root$mount("/api/v1/dst/recommendation", mainWrapper)
root$routes


# Log when API is starting up
cat("Starting plumbR API...4 \n")
cat("Working dir:", getwd(), "\n")
cat("Available CPU cores:", detectCores(), "\n")
cat("R version:", R.version.string, "\n")


root$run(host = "0.0.0.0", port = 8090, swagger = FALSE)

