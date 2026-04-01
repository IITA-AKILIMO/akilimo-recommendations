
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	setwd("C:/github/omilika/akilimo-recommendations")
} else {
	
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
    "in_29_NG_SP_riskAtt0",
    "in_30_NG_PP_riskAtt0",
    "in_31_TZ_PP_riskAtt0",
    "in_32_TZ_SP_riskAtt0",
    "in_33_TZ_IC_CIS_riskAtt2"
)

cmd <- 'curl -X POST http://localhost:8000/compute --data "@./tests/input/FILE.json"'
#cmd <- 'curl --ssl-no-revoke -X POST https://plumbr.akilimo.org/compute --data "@./tests/input/FILE.json"'

jout <- lapply(seq_along(test_files), \(i) {
	print(system.time(out <- system(gsub("FILE", test_files[i], cmd), intern=TRUE)[4]))
	print(out); flush.console()
	out
})

##saveRDS(outtst, "../akilimo-recommendations/test_out.rds")


compare <- function(i) {
	z <- readLines(gsub("xxx", i, "tests/output/out_xxx.json")) |> jsonlite::fromJSON()
	if (length(z) == 0) {
		print("no comparison data")
	} else {
		print(i)
		n <- jsonlite::fromJSON(jout[[i]])
		print(n$data)
		cat("-----------\n")
		print(c(as.list(z[[1]][[1]]), as.list(z[[2]])))
		cat("+============+\n")
	}
}



#out <- lapply(1:27, \(i) system(gsub("xxx", i, cmd), intern=TRUE)[4])
#which(sapply(out, \(x) grepl("recommendation", x)))
#[1] 14 15 16 18 19 22

#outtime <- lapply(1:27, \(i) system.time(system(gsub("xxx", i, cmd), intern=TRUE)[4]))
#sapply(outtime, \(x) x["elapsed"])
# 8.70   26.70   26.67   26.91   28.78   27.95   11.00    8.31    8.33    2.92   27.58   31.14   30.05    0.54    0.55    0.54   11.00    0.56    0.56    8.32    8.69    0.44    8.29   10.63    8.23    8.25   10.87 
#sapply(outtime, \(x) x["elapsed"]) |> sum()
#[1] 342.51
 
#curl -X POST http://localhost:8000/compute -H "Content-Type: application/json" --data "@./tests/input/in_10_GH_SP_riskAtt2.json"
