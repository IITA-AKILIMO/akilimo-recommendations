# Unit tests for the cassava-sweet potato intercrop calculation (getCISrecommendations
# in R/process-IC.R). No data files needed, so CI runs it:
#   Rscript -e 'tinytest::run_test_file("tests/test_ic_units.R")'

akpath <- Sys.getenv("AKILIMO_ROOT", unset = ".")
for (f in c("misc.R", "logging.R", "process-IC.R")) source(file.path(akpath, "R", f))

# Enough products for the linear programme to hit its N/P/K target exactly
fertilizers <- data.frame(type   = c("Urea", "TSP", "MOP", "NPK17_17_17"),
                          N_cont = c(0.46, 0,     0,   0.17),
                          P_cont = c(0,    0.2,   0,   0.074),
                          K_cont = c(0,    0,     0.5, 0.141),
                          price  = c(1.2,  1.6,   1.1, 1.5),   # per kg
                          stringsAsFactors = FALSE)
cis <- function(areaHa, FCY)
  suppressMessages(getCISrecommendations(areaHa = areaHa, FCY = FCY, tuberUP = 300, rootUP = 400,
                                         fertilizers = fertilizers, riskAtt = 0))$data

# Everything is for the whole field: doubling the field doubles cost and gain
# and leaves both decisions unchanged. (Cassava revenue used to stay per hectare
# while sweet potato and fertilizer cost were per field.)
for (fcy in c(11, 25)) {
  one <- cis(1, fcy); two <- cis(2, fcy); half <- cis(0.5, fcy)
  expect_true(one$dTC > 0, info = paste("fertilizer is costed at all, FCY", fcy))
  expect_equal(two$rec_IC, one$rec_IC, info = paste("intercrop decision, FCY", fcy))
  expect_equal(two$rec_F,  one$rec_F,  info = paste("fertilizer decision, FCY", fcy))
  expect_equal(two$dTC,  2 * one$dTC,  tolerance = 1e-8, info = paste("cost doubles, FCY", fcy))
  expect_equal(two$dNR,  2 * one$dNR,  tolerance = 1e-8, info = paste("net gain doubles, FCY", fcy))
  expect_equal(half$dNR, 0.5 * one$dNR, tolerance = 1e-8, info = paste("net gain halves, FCY", fcy))
}
