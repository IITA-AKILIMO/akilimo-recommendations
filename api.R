#!/usr/bin/Rscript --vanilla

akpath <- Sys.getenv("AKILIMO_ROOT", unset = ".")
setwd(akpath)

pks <- c("plumber", "limSolve", "ncdf4", "httr", "webshot", "mailR", "knitr", "leaflet")

srcdir <- file.path(akpath, "R")
for (f in list.files(srcdir, pattern="\\.R$")) source(file.path(srcdir, f))

library(plumber)

pr <- Plumber$new()
pr$handle(
	method = "POST",
	path = "/compute",
	handler = function(req, res) {
		tryCatch({
			result <- run_akilimo(req$postBody)
			# Map status string prefixes to HTTP codes
			status_str <- result[["status"]]
			if (!is.null(status_str) && grepl("^400", status_str)) res$status <- 400L
			result
		}, error = function(e) {
			res$status <- 500L
			message("ERROR: ", e$message)
			token <- tryCatch(
				jsonlite::unbox(jsonlite::fromJSON(req$postBody)[["request_token"]]),
				error = function(e2) jsonlite::unbox(NA_character_)
			)
			list(
				status = jsonlite::unbox("error"),
				data   = list(
					request_token = token,
					message       = jsonlite::unbox(e$message),
					trace         = jsonlite::unbox(paste(capture.output(traceback()), collapse = "\n"))
				)
			)
		})
	}
)
pr_run(pr, host = "0.0.0.0", port = 8000)

