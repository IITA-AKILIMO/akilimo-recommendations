#!/usr/bin/Rscript --vanilla

akpath <- Sys.getenv("AKILIMO_ROOT", unset = ".")
setwd(akpath)

pks <- c("plumber", "limSolve", "ncdf4", "httr", "webshot", "mailR", "knitr", "leaflet")

srcdir <- file.path(akpath, "R")
# Source in explicit dependency order.
# Only files in R/ (top-level) are loaded.
# R/preprocess/ and the project-root old/ directory are intentionally excluded.
for (f in c("misc.R", "get_data.R", "fertilizers.R", "quefts.R",
            "optimize_fert.R", "markdown.R", "sms_email.R",
            "process-FR.R", "process-IC.R", "process-PP.R", "process-SP.R",
            "AkilimoMain.R")) {
    source(file.path(srcdir, f))
}

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
			message("TRACE: ", paste(capture.output(traceback()), collapse = "\n"))
			token <- tryCatch(
				jsonlite::unbox(jsonlite::fromJSON(req$postBody)[["request_token"]]),
				error = function(e2) jsonlite::unbox(NA_character_)
			)
			list(
				status = jsonlite::unbox("error"),
				data   = list(
					request_token = token,
					message       = jsonlite::unbox(e$message)
				)
			)
		})
	}
)
pr_run(pr, host = "0.0.0.0", port = 8000)

