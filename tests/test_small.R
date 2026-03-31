
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	akpath <- "C:/github/omilika/akilimo-recommendations"
} else {
	akpath <- "."
}
setwd(akpath)

srcdir <- file.path(akpath, "R")
testdir <- file.path(akpath, "tests")

cmp <- readRDS(file.path(testdir, "test_small.rds"))

test <- function(i, x) {
	y <- cmp[[i]]
	a <- tinytest::expect_equal(x$recommendation, y$recommendation)
	if (!a) print(a)
	b <- tinytest::expect_equivalent(x$data, y$data, tolerance=0.1)
	if (!b) print(b)
}

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
    "in_29_NG_SP_riskAtt0"
)

run <- function(i) {
	cat("+--- ", i, " ---+\n"); flush.console()
	json <- readLines(file.path(testdir, "input", paste0(test_files[i], ".json")))
	run_akilimo(json)
}

for (f in grep("api", list.files(srcdir, pattern="\\.R$"), invert=TRUE, value=TRUE)) source(file.path(srcdir, f))

out <- lapply(1:29, \(i) {r <- run(i); test(i, r); r})

#timeout <- sapply(1:29, \(i) system.time(run(i))["elapsed"])
#saveRDS(out, file.path(testdir, "test_out6.rds"))

