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

#setwd("/home/akilimo/projects/akilimo_recommendation")
#source("AkilimoFunctionsStable.R")
#* nohup R CMD BATCH ./main.R > custom-out.log &
#* ps -ef |grep nohup

#setwd("/home/akilimo/projects/akilimo_recommendation")
setwd(getwd())
source("AkilimoFunctionsStable.R")
print(paste("Plumber production script started at",Sys.time()))


# r <- plumber::plumb("api.R")
# r$run(port = 8000, host="0.0.0.0", swagger = TRUE)

root <- plumber$new()

tanzania <- plumber$new("tz-api-wrapper.R")
root$mount("/api/v2/dst/recommendation/tz", tanzania)

tanzaniaDemo <- plumber$new("tz-api-wrapper-demo.R")
root$mount("/api/v2/dst/recommendation/demo/tz", tanzaniaDemo)

nigeria <- plumber$new("ng-api-wrapper.R")
root$mount("/api/v2/dst/recommendation/ng", nigeria)

nigeriaDemo <- plumber$new("ng-api-wrapper-demo.R")
root$mount("/api/v2/dst/recommendation/demo/ng", nigeriaDemo)

root$routes
root$run(port = 8000, host="0.0.0.0", swagger = FALSE)