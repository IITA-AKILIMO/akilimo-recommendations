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

  # HTTP / notifications
  "httr",
  "mailR",

  # Report generation (WeasyPrint — HTML/CSS → PDF via Python CLI)
  "base64enc",

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
  "tinytest"
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
  install.packages(missing, repos = "https://cloud.r-project.org")
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
# mailR / rJava: needs a working Java installation
# Only checked if rJava is available.
#   Linux:   sudo apt-get install openjdk-17-jre-headless
#            sudo R CMD javareconf
#   Windows: install JDK from https://adoptium.net
#            (ensure JAVA_HOME is set and java.exe is on PATH)
#   macOS:   brew install openjdk, then sudo R CMD javareconf
# ---------------------------------------------------------------------------
if (requireNamespace("rJava", quietly = TRUE)) {
  tryCatch({
    rJava::.jinit()
    message("rJava: OK")
  }, error = function(e) {
    message("rJava could not initialise. mailR (email sending) will not work.")
    if (.Platform$OS.type == "windows") {
      message("  -> Install a JDK from https://adoptium.net, set JAVA_HOME, re-run this script.")
    } else if (Sys.info()[["sysname"]] == "Darwin") {
      message("  -> Run: brew install openjdk && sudo R CMD javareconf")
    } else {
      message("  -> Run: sudo apt-get install openjdk-21-jre-headless")
      message("          sudo R CMD javareconf")
    }
  })
}

message("\nDone.")
