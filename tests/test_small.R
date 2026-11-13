# Regression tests: runs run_akilimo() on every fixture in tests/input/ (listed
# below) and compares the result with the saved baseline in tests/test_small.rds.
# Needs the runtime data in data/ (yield, soil, input CSVs; see CLAUDE.md "First-
# Time Setup"). Skips itself when the data is missing. CI runs it after pulling
# the data from OSF.
#
#   Rscript -e 'tinytest::run_test_file("tests/test_small.R")'
#
# After an intended change in results, rebuild the baseline and commit it:
#   UPDATE_BASELINE=1 Rscript -e 'tinytest::run_test_file("tests/test_small.R")'
#
# What is compared, per fixture: the status, the recommendation text (exactly),
# and the data (numbers within 1%, which absorbs optimiser noise between
# platforms). The API version is ignored. PDFs, email, SMS and price refreshes
# are switched off, and prices come from a fresh database seeded from the CSVs,
# so results do not depend on the machine or on refreshed prices.

akpath <- Sys.getenv("AKILIMO_ROOT", unset = ".")
if (!file.exists(file.path(akpath, "data", "yield", "Nigeria_WLY_LINTUL_2020.RDS")))
  exit_file("runtime data not found under AKILIMO_ROOT/data")

setwd(akpath)
assign("akpath", akpath, envir = globalenv())
for (f in grep("api", list.files("R", pattern = "\\.R$"), invert = TRUE, value = TRUE))
  source(file.path("R", f))

# Deterministic run: no API refresh, no network, no reports, seed-only prices
Sys.setenv(AKILIMO_API_URL = "", MAPBOX_TOKEN = "", MAP_API_URL = "",
           AKILIMO_DB_PATH = tempfile(fileext = ".sqlite"))
assign("generate_pdfs",   function(...) character(0), envir = globalenv())
assign("sendEmailReport", function(...) invisible(NULL), envir = globalenv())
invisible(suppressMessages(open_akilimo_db()))

test_files <- c(
    "in_1_TZ_FR_starch_factory_riskAtt0",
    "in_2_NG_FR_default_prices_riskAtt2_v1",
    "in_3_NG_FR_default_prices_riskAtt2_v2",
    "in_4_NG_FR_default_prices_riskAtt2_v3",
    "in_5_NG_FR_default_prices_riskAtt2_v4",
    "in_6_NG_FR_default_prices_riskAtt2_v5",
    "in_7_NG_FR_custom_cassUP_with_maxInv_riskAtt1",
    "in_8_NG_FR_starch_premium_cassava_with_maxInv_riskAtt1",
    "in_9_NG_FR_custom_unit_price_with_maxInv_riskAtt1",
    "in_10_GH_SP_riskAtt2",
    "in_11_NG_FR_default_prices_sms_email_riskAtt2",
    "in_12_NG_FR_alt_location_riskAtt2_v1",
    "in_13_NG_FR_alt_location_riskAtt2_v2",
    "in_14_NG_IC_maize_grain_CMP4_riskAtt0",
    "in_15_NG_IC_maize_grain_CMP3_riskAtt0_v1",
    "in_16_NG_IC_maize_grain_CMP3_riskAtt0_v2",
    "in_17_NG_FR_starch_matna_high_invest_riskAtt2_v1",
    "in_18_NG_IC_fresh_cob_manual_sms_email_riskAtt1",
    "in_19_NG_IC_fresh_cob_small_area_riskAtt0",
    "in_20_NG_FR_starch_premium_cassava_riskAtt2_v1",
    "in_21_NG_FR_starch_psaltry_riskAtt1",
    "in_22_NG_IC_high_fcy_custom_price_maxInv_riskAtt1",
    "in_23_NG_FR_custom_cassUP_maxInv_riskAtt1",
    "in_24_NG_FR_custom_price_high_maxInv_riskAtt2",
    "in_25_NG_FR_starch_premium_cassava_riskAtt2_v2",
    "in_26_NG_FR_starch_matna_riskAtt2",
    "in_27_NG_FR_starch_matna_high_invest_riskAtt2_v2",
    "in_28_NG_FR_out_of_scope_location_riskAtt1",
    "in_29_NG_SP_riskAtt0",
    "in_30_NG_PP_riskAtt0",
    "in_31_TZ_PP_riskAtt0",
    "in_32_TZ_SP_riskAtt0",
    "in_33_TZ_IC_CIS_riskAtt2",
    "in_34_TZ_FR_sw_riskAtt2",
    "in_35_NG_IC_grain_zero_prices_riskAtt1",
    "in_36_RW_FR_default_prices_riskAtt2",
    "in_37_BI_FR_default_prices_riskAtt2",
    "in_38_RW_SP_window2_riskAtt2",
    "in_39_RW_IC_not_available",
    "in_40_RW_FR_ekari_riskAtt2",
    "in_41_RW_FR_small_area_m2_riskAtt2",
    "in_42_BI_SP_window2_riskAtt2"
)

run_fixture <- function(name) {
  json <- paste(readLines(file.path("tests", "input", paste0(name, ".json")), warn = FALSE), collapse = "\n")
  r <- tryCatch(suppressWarnings(suppressMessages(run_akilimo(json))),
                error = function(e) list(status = "R error", data = list(message = conditionMessage(e))))
  # Plain values only (no unbox classes), without the API version
  r <- jsonlite::fromJSON(jsonlite::toJSON(r, auto_unbox = TRUE, digits = NA, null = "null", na = "null"))
  r$version <- NULL
  r
}

baseline_file <- file.path("tests", "test_small.rds")
out <- setNames(lapply(test_files, run_fixture), test_files)

if (nzchar(Sys.getenv("UPDATE_BASELINE"))) {
  saveRDS(out, baseline_file)
  exit_file(sprintf("baseline rebuilt: %d fixtures written to %s", length(out), baseline_file))
}
if (!file.exists(baseline_file))
  exit_file("no baseline yet: run with UPDATE_BASELINE=1 and commit tests/test_small.rds")

cmp <- readRDS(baseline_file)
for (name in test_files) {
  x <- out[[name]]; y <- cmp[[name]]
  if (is.null(y)) {
    expect_true(FALSE, info = paste(name, "has no baseline: rebuild with UPDATE_BASELINE=1"))
    next
  }
  # On a status change, show the message too (e.g. the R error), not just "R error"
  status_info <- if (identical(x$status, y$status)) paste(name, "status")
                 else paste(name, "status; message:", paste(x$data$message, collapse = " "))
  expect_equal(x$status, y$status, info = status_info)
  expect_equal(x$data$recommendation, y$data$recommendation, info = paste(name, "recommendation text"))
  expect_equal(x$data$message, y$data$message, info = paste(name, "message"))
  expect_equivalent(x$data, y$data, tolerance = 0.01, info = paste(name, "data"))
}
