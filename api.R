#!/usr/bin/Rscript --vanilla

akpath <- Sys.getenv("AKILIMO_ROOT", unset = ".")
setwd(akpath)

# Headless PNG rendering — no X11 display required.
# ragg is preferred (faster, better fonts); Cairo is the fallback.
# Both avoid the "unable to open connection to X11 display" error on servers.
if (.Platform$OS.type == "unix") {
  if (requireNamespace("ragg", quietly = TRUE)) {
    options(bitmapType = "cairo")          # keeps png() working
    options(device    = ragg::agg_png)     # ggplot2::ggsave() picks this up
  } else {
    options(bitmapType = "cairo")
  }
}

srcdir <- file.path(akpath, "R")
# Source in explicit dependency order.
# Only files in R/ (top-level) are loaded.
# R/preprocess/ and the project-root old/ directory are intentionally excluded.
for (f in c("misc.R", "logging.R", "get_data.R", "prices_db.R", "fertilizers.R",
            "quefts.R", "optimize_fert.R", "markdown.R", "html_helpers.R",
            "pdf_builders.R", "sms_email.R",
            "process-FR.R", "process-IC.R", "process-PP.R", "process-SP.R",
            "AkilimoMain.R")) {
  source(file.path(srcdir, f))
}

# Open (or create and seed) the SQLite price database.
open_prices_db()

library(plumber)

pr <- Plumber$new()

pr$handle(
  method = "GET",
  path = "/health",
  handler = function(res) {
    res$status <- 200L
    list(
      status  = jsonlite::unbox("ok"),
      version = jsonlite::unbox("20251228"),
      time    = jsonlite::unbox(format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    )
  }
)

pr$handle(
  method = "POST",
  path = "/compute",
  handler = function(req, res) {
    tryCatch({
      result <- run_akilimo(req$postBody)
      status_str <- result[["status"]]
      if (!is.null(status_str) && grepl("^400", status_str)) res$status <- 400L

      log_write("DEBUG", "RESULT:", result)

      result
    }, error = function(e) {
      res$status <- 500L
      log_write("ERROR", "Message:", e$message)
      log_write("ERROR", "STACK:", e)
      log_write("ERROR", "Trace:", paste(capture.output(traceback()), collapse = "\n"))

      token <- tryCatch(
        jsonlite::unbox(jsonlite::fromJSON(req$postBody)[["request_token"]]),
        error = function(e2) jsonlite::unbox(NA_character_)
      )
      log_write("DEBUG", "Request token:", token)

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

api_host <- Sys.getenv("API_HOST", unset = "0.0.0.0")
api_port <- as.integer(Sys.getenv("API_PORT", unset = "8000"))
pr_run(pr, host = api_host, port = api_port)

