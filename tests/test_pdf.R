
# PDF generation smoke test
#
# For each Rmd template, this test:
#   1. Runs a matching fixture through run_akilimo() to populate temp/ with the
#      CSV files the template reads.
#   2. Calls webshot::rmdshot() to render the template to a PDF.
#   3. Checks the output file exists and starts with the %PDF magic bytes.
#
# Requires PhantomJS (webshot::install_phantomjs()) or Chrome on PATH.
# Run from the project root:
#   Rscript tests/test_pdf.R

this <- system("hostname", TRUE)
akpath <- if (this == "denovo") "C:/github/omilika/akilimo-recommendations" else "."
setwd(akpath)

srcdir  <- file.path(akpath, "R")
testdir <- file.path(akpath, "tests")

for (f in c("misc.R", "get_data.R", "fertilizers.R", "quefts.R",
            "optimize_fert.R", "markdown.R", "sms_email.R",
            "process-FR.R", "process-IC.R", "process-PP.R", "process-SP.R",
            "AkilimoMain.R")) {
  source(file.path(srcdir, f))
}

if (!requireNamespace("webshot", quietly = TRUE)) stop("Package 'webshot' is required.")
if (!webshot::is_phantomjs_installed()) {
  message("PhantomJS not found — installing...")
  webshot::install_phantomjs()
}

dir.create("temp", showWarnings = FALSE)

is_valid_pdf <- function(path) {
  if (!file.exists(path) || file.size(path) == 0) return(FALSE)
  raw <- readBin(path, "raw", n = 4)
  rawToChar(raw) == "%PDF"
}

run_fixture <- function(name) {
  json <- readLines(file.path(testdir, "input", paste0(name, ".json")))
  run_akilimo(json)
}

pass <- 0L
fail <- 0L
skip <- 0L

check_pdf <- function(label, rmd, out_file) {
  cat(sprintf("%-55s", paste0(label, " ... ")))
  tryCatch({
    webshot::rmdshot(rmd, file = out_file, delay = 3)
    if (is_valid_pdf(out_file)) {
      cat("PASS\n"); pass <<- pass + 1L
    } else {
      cat("FAIL (not a valid PDF or empty)\n"); fail <<- fail + 1L
    }
  }, error = function(e) {
    cat("FAIL (", conditionMessage(e), ")\n", sep = ""); fail <<- fail + 1L
  })
}

skip_pdf <- function(label, reason) {
  cat(sprintf("%-55s", paste0(label, " ... ")))
  cat("SKIP (", reason, ")\n", sep = "")
  skip <<- skip + 1L
}

# ---------------------------------------------------------------------------
# FR — Fertilizer Recommendation (English, NG)
# Uses NG fixture: writes datall*.csv + FR_MarkDownText.csv
run_fixture("in_2_NG_FR_default_prices_riskAtt2_v1")
check_pdf("FR English (NG)",  "./Rmd/FR_markdown_VFT.Rmd", "temp/test_FR_en.pdf")

# FR — Fertilizer Recommendation (Swahili, TZ)
# Uses same NG fixture as above: TZ fixtures produce 0-fertilizer recommendations
# which prevents datall*.csv from being written, so template data is unusable.
run_fixture("in_2_NG_FR_default_prices_riskAtt2_v1")
check_pdf("FR Swahili (TZ)",  "./Rmd/FR_markdown_swa.Rmd", "temp/test_FR_sw.pdf")

# IC — Intercropping (English, NG)
# Fixtures 14-16 and 22 produce no IC recommendation; 18 and 19 do.
run_fixture("in_18_NG_IC_fresh_cob_manual_sms_email_riskAtt1")
check_pdf("IC English (NG)",  "./Rmd/IC_markdown_VFT.Rmd", "temp/test_IC_en.pdf")

# CIS — Cassava-Intercrop Swahili (TZ)
# Skipped: net/Akilimo Dashboard CIS.png is missing from the data assets.
skip_pdf("CIS Swahili (TZ)", "net/Akilimo Dashboard CIS.png missing from data assets")

# PP — Post-Planting (English, NG)
# Skipped: no PP fixture exists in tests/input yet.
skip_pdf("PP English (NG)",  "no PP fixture in tests/input")

# PP — Post-Planting (Swahili, TZ)
# Skipped: no PP fixture exists in tests/input yet.
skip_pdf("PP Swahili (TZ)",  "no PP fixture in tests/input")

# SP — Soil Preparation (English, NG)
run_fixture("in_29_NG_SP_riskAtt0")
check_pdf("SP English (NG)",  "./Rmd/SP_markdownVFT.Rmd",  "temp/test_SP_en.pdf")

# SP — Soil Preparation (Swahili, TZ)
# Skipped: no TZ SP fixture exists in tests/input yet.
skip_pdf("SP Swahili (TZ)",  "no TZ SP fixture in tests/input")

# ---------------------------------------------------------------------------
cat(sprintf("\n%d passed, %d failed, %d skipped\n", pass, fail, skip))
if (fail > 0) quit(status = 1L)
