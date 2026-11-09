# End-to-end regressions for Rwanda requests reported from beta: runs run_akilimo() on fixtures in tests/input/.
# Needs the runtime data in data/ (yield, soil, input CSVs) and the price database, so it is a local check,
# not a CI one. With AKILIMO_ROOT pointing at the project root:
#   Rscript -e 'tinytest::run_test_file("tests/test_rw_regressions.R")'

akpath <- Sys.getenv("AKILIMO_ROOT", unset = ".")
if (!file.exists(file.path(akpath, "data", "yield", "Rwanda_WLY_LINTUL.RDS")))
  exit_file("runtime data not found under AKILIMO_ROOT/data")

setwd(akpath)
assign("akpath", akpath, envir = globalenv())
for (f in grep("api", list.files("R", pattern = "\\.R$"), invert = TRUE, value = TRUE))
  source(file.path("R", f))
invisible(open_akilimo_db())

run_fixture <- function(name) {
  json <- paste(readLines(file.path("tests", "input", paste0(name, ".json"))), collapse = "\n")
  suppressWarnings(suppressMessages(run_akilimo(json)))
}

# SP works for RW now that the planting-window NetCDF exists: a normal recommendation
r <- run_fixture("in_38_RW_SP_window2_riskAtt2")
expect_equal(as.character(r$status), "success")
expect_equal(as.character(r$data$rec_type), "SP")
expect_true(grepl("We recommend you plant", r$data$recommendation))
expect_true(grepl("harvest", r$data$recommendation))

# /coverage?type=SP now has an RW footprint, while BI stays empty
expect_true(nrow(coverage_points("RW", "SP")) > 0)
expect_equal(nrow(coverage_points("BI", "SP")), 0)

# IC exists only for NG and TZ: a clear 400, not "success" with an empty recommendation
r <- run_fixture("in_39_RW_IC_not_available")
expect_equal(as.character(r$status), "400 - bad request")
expect_true(grepl("Intercropping .* not available for country: RW", r$data$message))

# ekari is a documented areaUnits value
r <- run_fixture("in_40_RW_FR_ekari_riskAtt2")
expect_equal(as.character(r$status), "success")
expect_true(grepl("We recommend applying", r$data$recommendation))

# A 10 m2 field drops fertilizers under 25 kg/ha and re-runs the yield estimate; that used to fail for RW
# because fd2.csv has no RW dry-matter curve
r <- run_fixture("in_41_RW_FR_small_area_m2_riskAtt2")
expect_equal(as.character(r$status), "success")
expect_true(nzchar(as.character(r$data$recommendation)))
