# Unit tests for the PDF report template (R/html_helpers.R): labels, units and the rows that are shown or left out.
# No data files, WeasyPrint or database needed, so CI runs it:
#   Rscript -e 'tinytest::run_test_file("tests/test_pdf_template.R")'

akpath <- Sys.getenv("AKILIMO_ROOT", unset = ".")
assign("akpath", akpath, envir = globalenv()) # html_open() reads the print CSS from akpath/net
for (f in c("misc.R", "logging.R", "markdown.R", "html_helpers.R")) source(file.path(akpath, "R", f))

# tr() normally reads the translations table; a stand-in that fills the two placeholders the template uses.
# It replaces the global tr() that the sourced helpers call (tinytest runs this file in its own environment).
assign("tr", function(key, lang, ...) {
  a <- list(...)
  if (key == "fert_rate_line") return(paste(a$rate, "kg of", a$fertilizer))
  key
}, envir = globalenv())
has <- function(html, pattern, fixed = TRUE) grepl(pattern, html, fixed = fixed)

# Area units are named, never assumed to be hectares
expect_equal(area_unit_label("acre"), "acre")
expect_equal(area_unit_label("ha", "sw"), "hekta")
expect_equal(area_unit_label("m2"), "m²")
expect_equal(area_unit_label("are"), "are")

# Dates name the month in the report's language
expect_equal(format_long_date(as.Date("2026-11-05"), "sw"), "05 Novemba 2026")
expect_equal(format_long_date(as.Date("2026-11-05"), "en"), "05 November 2026")
expect_equal(format_long_date(as.Date("2026-11-05"), "en", month_first = TRUE), "November 05, 2026")

# Placeholders count as not given
for (x in list(NULL, NA, "", " ", "NA", "na", "akilimo-mcp")) expect_true(is_blank_value(x))
expect_false(is_blank_value("Riverside plot"))

# The reported yield class is shown as its band
expect_equal(reported_yield_band(11.25), "7.5 - 15")
expect_equal(reported_yield_band(3.75), "< 7.5")
expect_equal(reported_yield_band(33.75), "> 30")
expect_null(reported_yield_band(NA))
expect_null(reported_yield_band("NA"))

# Every fertilizer gets its own line (tr() alone only filled in the first), named as farmers read it
expect_equal(strsplit(fert_rate_lines(c("82", "297"), c("Urea", "NPK15_15_15"), "en"), "\n")[[1]],
             c("82 kg of Urea", "297 kg of NPK15:15:15"))

# Personal details leave out what was not given and show yields per hectare
html <- html_personal_info(user = list(Name = "akilimo-mcp", Email = "NA"), userField = "NA",
                           area = 1, areaUnits = "acre", PD = "2026-11-25", HD = "2027-09-25",
                           current_yield = 11.64, reported_yield = 11.25)
expect_false(has(html, "Your name") || has(html, "Email") || has(html, "Your field"))
expect_true(has(html, "1 acre"))
expect_true(has(html, "7.5 - 15 tonnes per ha"))
expect_true(has(html, "11.6 tonnes per ha"))

named <- html_personal_info(user = list(Name = "Ada", Email = "ada@example.org"), userField = "River plot",
                            area = 2, areaUnits = "ha", PD = "2026-11-25", HD = "2027-09-25")
expect_true(has(named, "Ada"))
expect_true(has(named, "a***a@***.org"))
expect_true(has(named, "River plot"))

# The cassava price is per tonne of roots whatever unit was quoted, and no budget reads "No limit"
html <- html_fertilizer_table(fr = data.frame(), area = 1, areaUnits = "acre", currency = "NGN",
                              rootUP = 15000, cassPD = "gari", cassUW = 50, maxInv = NA)
expect_true(has(html, "NGN 15,000</strong> per tonne of roots"))
expect_true(has(html, "No limit"))
expect_false(has(gsub("NGN", "", html), "NA"))
budget <- html_fertilizer_table(fr = data.frame(), area = 1, areaUnits = "acre", currency = "NGN",
                                rootUP = 15000, cassPD = "roots", cassUW = 1000, maxInv = 200000)
expect_true(has(budget, "NGN 200,000</strong> for <strong>1 acre"))

# The recommendation keeps one line per part
expect_true(has(html_recommendation("We recommend applying\n82 kg of Urea\n297 kg of NPK15:15:15"),
                "applying<br>\n82 kg of Urea<br>\n297 kg"))

# The summary names the bags, the cost and the gain
fr <- data.frame(type = c("NPK15_15_15", "Urea"), bags = c(6, 1.5), bagWeight = c(50, 50))
summary_html <- html_buy_summary(fr, area_fmt = "1", area_unit = "acre", total_cost = 161937, extra_t = 11.3,
                                 net_gain = 516400, currency = "NGN")
expect_true(has(summary_html, "6 bags of 50kg NPK15:15:15, 1.5 bags of 50kg Urea for your 1 acre"))
expect_true(has(summary_html, "NGN 161,937"))
expect_true(has(summary_html, "11.3 tonnes more"))
expect_equal(html_buy_summary(data.frame(), "1", "acre", 0, 0, 0, "NGN"), "")

# Every page carries the reference, the model and the language
set_report_context(list(request_id = "3f2b6c1e-8a4d-4b2e-9c3f-1a2b3c4d5e6f", lang = "sw", country = "TZ",
                        riskAtt = 1, saleSF = FALSE, nameSF = NA, generated = as.POSIXct("2026-11-05 10:00:00")))
head <- html_open("Mapendekezo ya Mbolea")
expect_true(has(head, '<html lang="sw">'))
expect_true(has(head, '<meta name="author" content="AKILIMO (IITA)">'))
expect_true(has(head, "Kumb. 3f2b6c1e-8a4d-4b2e-9c3f-1a2b3c4d5e6f"))
expect_true(has(head, paste("Modeli", AKI_VERSION)))
expect_true(has(head, 'counter(page) " / " counter(pages)'))
basis <- html_basis("en")
expect_true(has(basis, "Balanced"))
expect_true(has(basis, "Open market"))
expect_true(has(head, 'content: element(refqr)'))
if (requireNamespace("qrcode", quietly = TRUE)) {
  set_temp_dir(tempdir())
  qr <- html_open("Report")
  expect_true(has(qr, '<div class="reference-qr"><img src="data:image/png;base64,'))
}
set_report_context(list(request_id = NA_character_, lang = "en"))
no_ref <- html_open("Report")
expect_false(has(no_ref, "Ref."))
expect_false(has(no_ref, 'class="reference-qr"'))
