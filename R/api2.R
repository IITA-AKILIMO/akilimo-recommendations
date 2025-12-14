#!/usr/bin/Rscript --vanilla

this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	akpath <- "C:/github/omilika/akilimo-recommendations"
} else {
	akpath <- "."
}
srcdir <- file.path(akpath, "R")

pks <- c("limSolve", "tidyr", "webshot", "httr", "mailR")


for (f in grep("api", list.files(srcdir, full=TRUE), invert=TRUE, value=TRUE)) source(f)

library(plumber)
pr <- pr(file.path(akpath, "api-wrapper.R"))
pr_set_debug(pr)
pr_run(pr, port = 8000)

