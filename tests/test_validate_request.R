# Unit tests for request validation (R/AkilimoMain.R) and the dry-matter conversion (getRFY in R/misc.R).
# No data files or database needed: coverage and the dry-matter table are stubbed, so CI runs it:
#   Rscript -e 'tinytest::run_test_file("tests/test_validate_request.R")'

akpath <- Sys.getenv("AKILIMO_ROOT", unset = ".")
for (f in c("misc.R", "AkilimoMain.R")) source(file.path(akpath, "R", f))

# Every point counts as covered, so these tests reach the checks after the coverage gate.
assign("is_point_covered", function(country, lon, lat) TRUE, envir = globalenv())

req <- function(...) {
  b <- list(country = "RW", lat = -2.325, lon = 30.075, area = 1, areaUnits = "ha",
            FR = FALSE, IC = FALSE, PP = FALSE, SPP = FALSE, SPH = FALSE)
  modifyList(b, list(...))
}

# areaUnits: everything parse_request() converts is accepted, anything else is not
for (u in c("ha", "acre", "are", "m2", "ekari", "hekta", "string", "EKARI"))
  expect_null(validate_request(req(FR = TRUE, areaUnits = u)), info = u)
expect_true(grepl("Invalid or missing areaUnits", validate_request(req(FR = TRUE, areaUnits = "furlong"))))
expect_true(grepl("Invalid or missing areaUnits", validate_request(req(FR = TRUE, areaUnits = NULL))))

# The optional fertilizer subsidy selector is boolean when supplied.
expect_null(validate_request(req(FR = TRUE, subsidisedFertilizer = TRUE)))
expect_true(grepl("subsidisedFertilizer must be a boolean",
                  validate_request(req(FR = TRUE, subsidisedFertilizer = "yes"))))

# Scheduled planting has no planting-window data for RW or BI
for (cc in c("RW", "BI")) {
  for (flag in c("SPP", "SPH")) {
    b <- req(country = cc, PD_window = 2, HD_window = 2)
    b[[flag]] <- TRUE
    expect_true(grepl(paste0("Scheduled planting .* not available for country: ", cc),
                      validate_request(b)), info = paste(cc, flag))
  }
  # also with windows of 0, which would otherwise ask the user for windows that then fail
  expect_true(grepl("Scheduled planting", validate_request(req(country = cc, SPP = TRUE))), info = cc)
}
for (cc in c("NG", "TZ", "GH"))
  expect_null(validate_request(req(country = cc, SPP = TRUE, PD_window = 2, HD_window = 2)), info = cc)

# Intercropping exists only for NG and TZ
for (cc in c("RW", "GH", "BI"))
  expect_true(grepl(paste0("Intercropping .* not available for country: ", cc),
                    validate_request(req(country = cc, IC = TRUE))), info = cc)
for (cc in c("NG", "TZ"))
  expect_null(validate_request(req(country = cc, IC = TRUE)), info = cc)

# Only the flag that is processed counts: FR wins over IC (FR -> IC -> PP -> SP)
expect_null(validate_request(req(FR = TRUE, IC = TRUE)))
expect_null(validate_request(req(PP = TRUE, SPP = TRUE)))
expect_equal(active_rec_type(req(IC = TRUE, SPH = TRUE)), "IC")
expect_equal(active_rec_type(list(SPH = TRUE)), "SP")         # missing flags count as FALSE
expect_true(is.na(active_rec_type(req())))

# FR and PP work everywhere
for (cc in c("NG", "TZ", "RW", "GH", "BI")) {
  expect_null(validate_request(req(country = cc, FR = TRUE)), info = cc)
  expect_null(validate_request(req(country = cc, PP = TRUE)), info = cc)
}

# The coverage gate applies only to FR and SP; IC and PP use no location data
assign("is_point_covered", function(country, lon, lat) FALSE, envir = globalenv())
expect_true(grepl("outside the supported agricultural area", validate_request(req(country = "NG", FR = TRUE))))
expect_true(grepl("outside the supported agricultural area", validate_request(req(country = "NG", SPP = TRUE))))
expect_null(validate_request(req(country = "NG", IC = TRUE)))
expect_null(validate_request(req(country = "NG", PP = TRUE)))
# availability is reported before coverage: RW SP outside the grid says SP is unavailable
expect_true(grepl("Scheduled planting", validate_request(req(SPP = TRUE))))
assign("is_point_covered", function(country, lon, lat) TRUE, envir = globalenv())

# getRFY: countries without a dry-matter curve (RW, GH, BI) use the NG curve instead of returning numeric(0)
assign("get_data", function(x, ...) {
  stopifnot(x == "dry_matter")
  data.frame(dayNr = rep(1:366, 2), DMCont = rep(c(40, 25), each = 366), country = rep(c("NG", "TZ"), each = 366))
}, envir = globalenv())
hd <- as.Date("2024-11-10")
expect_equal(getRFY(HD = hd, RDY = 4000, country = "NG"), 10000)
expect_equal(getRFY(HD = hd, RDY = 4000, country = "TZ"), 16000)
for (cc in c("RW", "GH", "BI"))
  expect_equal(getRFY(HD = hd, RDY = 4000, country = cc), 10000, info = cc)
