# Unit tests for planting date and cultivar advice (process_PDC in R/process-PDC.R) with a small
# made-up DSSAT table, so no data files are needed and CI runs it:
#   Rscript -e 'tinytest::run_test_file("tests/test_pdc_units.R")'

akpath <- Sys.getenv("AKILIMO_ROOT", unset = ".")
source(file.path(akpath, "R", "process-PDC.R"))

# Two grid points; the first has a loss-making row (HWAH 0) that must be dropped
dssat <- data.frame(
  XLAT     = c(9.0, 9.0, 9.0, 9.0, 12.0),
  LONG     = c(8.0, 8.0, 8.0, 8.0, 3.0),
  PDAT     = as.Date(c("2026-05-01", "2026-06-01", "2026-07-01", "2026-08-01", "2026-06-01")),
  Variety  = c("A", "A", "B", "B", "A"),
  Cultivar = c("Early", "Early", "Late", "Late", "Early"),
  HWAH     = c(4200, 5100, 6100, 0, 3000),
  CWAM     = c(9000, 9500, 10000, 800, 7000),
  MDAT     = as.Date(c("2026-09-01", "2026-10-01", "2026-11-01", "2026-12-01", "2026-10-01"))
)
# The functions under test look these up in the global environment, where tinytest does not assign
assign("get_data", function(...) dssat, envir = globalenv())
assign("tp", function(filename) file.path(tempdir(), filename), envir = globalenv())

# The nearest grid point is used and rows are ranked by yield, best first, without HWAH = 0
res <- getPDCrecommendations(lat = 9.046, lon = 8.101, country = "NG", crop = "maize")
expect_true(is.data.frame(res))
expect_equal(res$HWAH, c(6100, 5100, 4200))
expect_equal(res$Cultivar[1], "Late")
expect_equal(nrow(res), 3)

# A location more than 0.75 degrees from any grid point is outside the domain
expect_null(getPDCrecommendations(lat = 5, lon = 20, country = "NG", crop = "maize"))

# The advice text names the best cultivar, date and yield; the data frame comes back as data
out <- process_PDC(PDC = TRUE, country = "NG", crop = "maize", lat = 9.046, lon = 8.101,
                   PD = NA, user = NULL, userField = NULL, area = 1, areaUnits = "ha")
expect_equal(out$rec_type, "PDC")
expect_true(grepl("Late", out$recommendation))
expect_true(grepl("6.1 t/ha", out$recommendation, fixed = TRUE))
expect_true(is.data.frame(out$data))

# Outside the domain: a clear message and no data
out <- process_PDC(PDC = TRUE, country = "NG", crop = "maize", lat = 5, lon = 20,
                   PD = NA, user = NULL, userField = NULL, area = 1, areaUnits = "ha")
expect_true(grepl("out of the recommendation domain", out$recommendation))
expect_null(out$data)

# Only maize has DSSAT output
out <- process_PDC(PDC = TRUE, country = "NG", crop = "cassava", lat = 9.046, lon = 8.101,
                   PD = NA, user = NULL, userField = NULL, area = 1, areaUnits = "ha")
expect_true(grepl("does not yet have", out$recommendation))
expect_null(out$data)
