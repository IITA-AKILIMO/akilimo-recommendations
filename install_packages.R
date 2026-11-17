## Install all R packages required by the Akilimo Recommendation Engine.
## Works on Linux, macOS, and Windows.
##
## Usage:
##   Rscript install_packages.R
##   # or from an R session:
##   source("install_packages.R")

pkgs <- c(
  # API framework
  "plumber",

  # Optimisation / modelling
  "limSolve",

  # Spatial / climate data
  "ncdf4",
  "sf",          # GET /coverage footprints; Linux needs libgdal-dev libgeos-dev
                 #   libproj-dev libudunits2-dev libabsl-dev cmake

  # HTTP
  "httr",

  # Report generation (WeasyPrint — HTML/CSS → PDF via Python CLI)
  "base64enc",
  "qrcode",      # the report's reference as a QR code (left out when the package is missing)

  # Data wrangling
  "jsonlite",
  "tidyr",
  "dplyr",
  "plyr",
  "lubridate",

  # Visualisation / tables
  "ggplot2",
  "ragg",        # headless PNG device for ggplot2 (replaces X11/Cairo on servers)
  "kableExtra",
  "scales",
  "png",

  # Price database
  "DBI",
  "RSQLite",

  # Config
  "dotenv",
  "here",

  # Machine learning
  "randomForest",

  # Testing
  "tinytest",

  # Linting and formatting (tests/check_lint.R; configuration in .lintr)
  "lintr",
  "styler"
)

# ---------------------------------------------------------------------------
# Package installation — only install what is missing
# ---------------------------------------------------------------------------

installed_pkgs <- rownames(installed.packages())
already <- pkgs[pkgs %in% installed_pkgs]
missing <- pkgs[!pkgs %in% installed_pkgs]

if (length(already) > 0) {
  message("Already installed (", length(already), "): ",
          paste(already, collapse = ", "))
}

if (length(missing) > 0) {
  message("Installing (", length(missing), "): ",
          paste(missing, collapse = ", "))
  # Use a repo already configured (e.g. CI's binary package mirror) when set
  repos <- getOption("repos")
  cran  <- if ("CRAN" %in% names(repos)) repos[["CRAN"]] else ""
  if (!nzchar(cran) || cran == "@CRAN@")
    repos <- "https://cloud.r-project.org"
  install.packages(missing, repos = repos)
} else {
  message("Nothing to install.")
}

# ---------------------------------------------------------------------------
# WeasyPrint: Python CLI tool used to render HTML reports to PDF.
# Install with: pip install weasyprint
# System libraries required on Linux/Debian:
#   apt-get install -y libpango-1.0-0 libpangoft2-1.0-0 libgdk-pixbuf2.0-0
# See docs/PDF-GENERATION-PLAN.md §2 for full installation instructions.
# ---------------------------------------------------------------------------
res <- tryCatch(
  system2("weasyprint", "--version", stdout = TRUE, stderr = TRUE),
  error = function(e) NULL
)

if (is.null(res)) {
  warning(
    "WeasyPrint not found or not callable. PDF generation will fail.\n",
    "  Install with: pip install weasyprint\n",
    "  Linux also requires: apt-get install -y libpango-1.0-0 libpangoft2-1.0-0"
  )
} else {
  message("weasyprint: ", res)
}

# ---------------------------------------------------------------------------
# Fail loudly if any required package is still missing, so a deploy or CI
# run stops here instead of restarting the API without it.
# install.packages() only warns when a package fails to build.
# ---------------------------------------------------------------------------
still_missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(still_missing) > 0) {
  msg <- paste0(
    "Failed to install (", length(still_missing), "): ",
    paste(still_missing, collapse = ", "),
    "\n  Check the output above for missing system libraries."
  )
  if (interactive()) stop(msg, call. = FALSE)
  message("ERROR: ", msg)
  quit(status = 1, save = "no")
}

message("\nDone.")
