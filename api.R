#!/usr/bin/Rscript --vanilla

akpath <- Sys.getenv("AKILIMO_ROOT", unset = ".")
setwd(akpath)

pks <- c("plumber", "limSolve", "ncdf4", "httr", "webshot", "mailR", "knitr", "leaflet")

srcdir <- file.path(akpath, "R")
for (f in list.files(srcdir, pattern="\\.R$")) source(file.path(srcdir, f))

library(plumber)
#pr <- pr(file.path(akpath, "api-wrapper.R"))
#pr_set_debug(pr)
#pr_run(pr, port = 8000)

pr <- Plumber$new()
pr$handle(
	method = "POST",
	path = "/compute",
	handler = function(req, res) {
		tryCatch({ run_akilimo(req$postBody)}, error = 
		function(e) {
			res$status <- 500
			print(e)
			data <- list(
				request_token = jsonlite::unbox(request_token),
				message = jsonlite::unbox(e$message),
				trace = jsonlite::unbox(capture.output(e))
			)
			list(status = jsonlite::unbox("error"), data = data)
		})
	}
)
pr_run(pr, host = "0.0.0.0", port = 8000)

