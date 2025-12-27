#!/usr/bin/Rscript --vanilla

pks <- c("plumber", "limSolve", "ncdf4", "webshot", "httr", "mailR")

# perhaps: leaflet, mapview, flexdashboard, lubridate, grid, rJava

library(plumber)

os <- .Platform$OS.type

if(os != "windows"){
  # setwd("/home/akilimo/projects/akilimo_recommendation")
  Sys.setenv(RSTUDIO_PANDOC = "/usr/lib/rstudio-server/bin/pandoc")
}

pandoc <- Sys.getenv("RSTUDIO_PANDOC")

for (f in grep("api", list.files(srcdir, pattern="\\.R$"), invert=TRUE, value=TRUE)) source(f)

root <- Plumber$new()

#health endpoint
#healthWrapper <- plumber$new("api-health.R")
#root$mount("/api/", healthWrapper)

##Production endpoints
mainWrapper <- Plumber$new("api-wrapper.R")


# mainWrapper <- plumber$new("new-api.R")

root$mount("/api/v1/dst/recommendation", mainWrapper)
root$mount("/api/v2/dst/recommendation", mainWrapper)
root$routes


# Log when API is starting up
cat("Starting plumbR API v4 \n")
cat("Working dir:", getwd(), "\n")
cat("Available CPU cores:", detectCores(), "\n")

cat("R version:", R.version.string, "\n")

root$run(host = "0.0.0.0", port = 80, swagger = FALSE)

