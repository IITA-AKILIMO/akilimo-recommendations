# Unit tests for request validation (R/AkilimoMain.R) and the dry-matter conversion (getRFY in R/misc.R).
# No data files or database needed: coverage and the dry-matter table are stubbed, so CI runs it:
#   Rscript -e 'tinytest::run_test_file("tests/test_validate_request.R")'

akpath <- Sys.getenv("AKILIMO_ROOT", unset = ".")
for (f in c("misc.R", "AkilimoMain.R")) source(file.path(akpath, "R", f))

# Every point counts as covered, so these tests reach the checks after the coverage gate.
assign("is_point_covered", function(country, lon, lat) TRUE, envir = globalenv())

req <- function(...) {
  b <- list(country = "RW", lat = -2.325, lon = 30.075, area = 1, areaUnits = "ha",
            FR = FALSE, IC = FALSE, PP = FALSE, SPP = FALSE, SPH = FALSE, PDC = FALSE,
            ureaavailable = TRUE)
  modifyList(b, list(...))
}

# FR and IC need at least one fertilizer; the Android app sent none when its database had
# no fertilizers, and the engine answered 500 instead of 400.
for (flag in c("FR", "IC")) {
  none <- req(country = "NG"); none[[flag]] <- TRUE; none$ureaavailable <- NULL
  expect_true(grepl("No fertilizer is available", validate_request(none)), info = flag)
  off <- modifyList(none, list(ureaavailable = FALSE, MOPavailable = FALSE))
  expect_true(grepl("No fertilizer is available", validate_request(off)), info = flag)
}
expect_null(validate_request(modifyList(req(FR = TRUE), list(ureaavailable = NULL, newFert1name = "newFert1x"))))
# PP and SP use no fertilizers
expect_null(validate_request(modifyList(req(PP = TRUE), list(ureaavailable = NULL))))
expect_null(validate_request(modifyList(req(SPP = TRUE), list(ureaavailable = NULL))))

# areaUnits: everything parse_request() converts is accepted, anything else is not
for (u in c("ha", "acre", "are", "m2", "ekari", "hekta", "string", "EKARI"))
  expect_null(validate_request(req(FR = TRUE, areaUnits = u)), info = u)
expect_true(grepl("Invalid or missing areaUnits", validate_request(req(FR = TRUE, areaUnits = "furlong"))))
expect_true(grepl("Invalid or missing areaUnits", validate_request(req(FR = TRUE, areaUnits = NULL))))

# The optional fertilizer subsidy selector is boolean when supplied.
expect_null(validate_request(req(FR = TRUE, subsidisedFertilizer = TRUE)))
expect_true(grepl("subsidisedFertilizer must be a boolean",
                  validate_request(req(FR = TRUE, subsidisedFertilizer = "yes"))))

# Scheduled planting is available in every supported country (BI uses the FR soil by yield class)
for (cc in c("NG", "TZ", "GH", "RW", "BI"))
  expect_null(validate_request(req(country = cc, SPP = TRUE, PD_window = 2, HD_window = 2)), info = cc)

# Intercropping exists only for NG and TZ
for (cc in c("RW", "GH", "BI"))
  expect_true(grepl(paste0("Intercropping .* not available for country: ", cc),
                    validate_request(req(country = cc, IC = TRUE))), info = cc)
for (cc in c("NG", "TZ"))
  expect_null(validate_request(req(country = cc, IC = TRUE)), info = cc)

# Planting date and cultivar is currently NG maize only
expect_null(validate_request(req(country = "NG", PDC = TRUE)))
for (cc in c("TZ", "RW", "GH", "BI"))
  expect_true(grepl(paste0("Planting date and cultivar .* not available for country: ", cc),
                    validate_request(req(country = cc, PDC = TRUE))), info = cc)

# Only the flag that is processed counts: FR wins over IC (FR -> IC -> PP -> SP -> PDC)
expect_null(validate_request(req(FR = TRUE, IC = TRUE)))
expect_null(validate_request(req(PP = TRUE, SPP = TRUE)))
expect_equal(active_rec_type(req(IC = TRUE, SPH = TRUE)), "IC")
expect_equal(active_rec_type(list(SPH = TRUE)), "SP")         # missing flags count as FALSE
expect_equal(active_rec_type(req(PDC = TRUE)), "PDC")
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
expect_true(grepl("outside the supported agricultural area", validate_request(req(country = "RW", SPP = TRUE))))
expect_null(validate_request(req(country = "NG", IC = TRUE)))
expect_null(validate_request(req(country = "NG", PP = TRUE)))
# BI has scheduled planting now, so outside the grid it is the coverage gate that refuses it;
# availability is still reported before coverage (IC is not available for BI)
expect_true(grepl("outside the supported agricultural area", validate_request(req(country = "BI", SPP = TRUE))))
expect_true(grepl("Intercropping", validate_request(req(country = "BI", IC = TRUE))))
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

# A rejection carries a stable code next to the English message (the API and app translate by code)
code_of <- function(body) as.character(bad_request(validate_request(body))$data$code)
expect_equal(code_of(req(country = "NG", PDC = FALSE)), "no_recommendation_selected")
expect_equal(code_of(req(country = "RW", IC = TRUE)), "rec_type_unavailable")
expect_equal(code_of(req(country = "TZ", PDC = TRUE)), "rec_type_unavailable")
expect_equal(code_of(req(FR = TRUE, ureaavailable = NULL)), "no_fertilizer")
expect_equal(code_of(req(PP = TRUE, areaUnits = "mile")), "invalid_input")
assign("is_point_covered", function(country, lon, lat) FALSE, envir = globalenv())
expect_equal(code_of(req(FR = TRUE)), "outside_coverage")
expect_equal(as.character(bad_request("Malformed JSON body")$data$code), "invalid_input")

# Land-operation methods: the AKILIMO API sends "MANUAL"; the PP scenarios use lower case, so an
# upper-case method matched no row and getPPrecText() failed with "missing value where TRUE/FALSE needed".
expect_equal(normalise_method("MANUAL"), "manual")
expect_equal(normalise_method(" Tractor "), "tractor")
expect_equal(normalise_method("NA"), "N/A")
expect_equal(normalise_method("N/A"), "N/A")
expect_equal(normalise_method(NA), "N/A")
expect_equal(normalise_method(character(0)), "N/A")

source(file.path(akpath, "R", "process-PP.R"))
assign("min_nr_multiplier", function(riskAtt) 0, envir = globalenv())
assign("tr", function(key, lang, ...) key, envir = globalenv())
costLMO <- data.frame(
  operation = c(rep(c("ploughing", "harrowing", "ridging"), 2), "weeding1", "weeding2"),
  method = c(rep("manual", 3), rep("tractor", 3), NA, NA),
  costHa = c(100, 80, 120, NA, NA, NA, 50, 40))
for (m in c("manual", "MANUAL")) {
  pp <- process_PP(PP = TRUE, country = "TZ", lang = "en", areaHa = 0.4, costLMO = costLMO,
                   ploughing = TRUE, ridging = TRUE,
                   method_ploughing = normalise_method(m), method_ridging = normalise_method(m),
                   FCY = 18.75, rootUP = 200, riskAtt = 0, user = NULL, userField = NULL, area = 1,
                   areaUnits = "acre", PD = NULL, HD = NULL, lat = 0, lon = 0, cassPD = 0, cassUW = 0, maxInv = 0)
  expect_true(is.character(pp$recommendation) && nzchar(pp$recommendation), info = m)
}
