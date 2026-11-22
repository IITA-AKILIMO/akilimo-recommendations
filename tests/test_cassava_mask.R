# Cassava-area mask (cassava_cells_ok() in R/get_data.R): a point must lie in a MapSPAM 2020 cell
# with at least CASSAVA_MIN_HA ha of cassava. Needs only data/input/cassava_area_spam2020.csv,
# which is tracked in git, so CI runs it:
#   Rscript -e 'tinytest::run_test_file("tests/test_cassava_mask.R")'

akpath <- Sys.getenv("AKILIMO_ROOT", unset = ".")
setwd(akpath)
for (f in c("misc.R", "get_data.R")) source(file.path("R", f))

expect_true(file.exists(data_path("input/cassava_area_spam2020.csv")), info = "mask file ships with the code")

old <- Sys.getenv("CASSAVA_MIN_HA", unset = NA)
on.exit(if (is.na(old)) Sys.unsetenv("CASSAVA_MIN_HA") else Sys.setenv(CASSAVA_MIN_HA = old), add = TRUE)
Sys.unsetenv("CASSAVA_MIN_HA")

# Threshold: 20 ha by default, overridable, and bad values fall back to the default
expect_equal(cassava_min_ha(), 20)
Sys.setenv(CASSAVA_MIN_HA = "250"); expect_equal(cassava_min_ha(), 250)
Sys.setenv(CASSAVA_MIN_HA = "0");   expect_equal(cassava_min_ha(), 0)
Sys.setenv(CASSAVA_MIN_HA = "-5");  expect_equal(cassava_min_ha(), 20)
Sys.setenv(CASSAVA_MIN_HA = "abc"); expect_equal(cassava_min_ha(), 20)
Sys.unsetenv("CASSAVA_MIN_HA")

# A point and the MapSPAM cell it falls in share a key, anywhere in the cell
expect_equal(spam_cell_key(7.675, 7.375), spam_cell_key(7.70, 7.40))
expect_false(spam_cell_key(7.675, 7.375) == spam_cell_key(7.76, 7.375))

# Every country has a mask and clear-cut points: Ibadan area (NG) is a strong cassava cell,
# a Sahel cell (NG, ~7 ha) is below the default threshold
for (cc in c("NG", "TZ", "RW", "GH", "BI"))
  expect_true(length(cassava_area_cells(cc)) > 0, info = paste(cc, "has mask rows"))
expect_true(cassava_cells_ok("NG", 7.675, 7.375), info = "NG cell with ~2000 ha is kept")
expect_false(cassava_cells_ok("NG", 5.79, 13.54), info = "NG cell with ~7 ha is below 20 ha")
expect_false(cassava_cells_ok("RW", 29.375, -2.825), info = "a cell with no recorded cassava is dropped")

# The threshold moves the boundary: the ~7 ha cell passes at 5 ha and fails at 20 ha
Sys.setenv(CASSAVA_MIN_HA = "5");  expect_true(cassava_cells_ok("NG", 5.79, 13.54))
Sys.setenv(CASSAVA_MIN_HA = "20"); expect_false(cassava_cells_ok("NG", 5.79, 13.54))
Sys.unsetenv("CASSAVA_MIN_HA")

# Vectorised, and no mask for a country means every point passes (fails open)
expect_equal(cassava_cells_ok("NG", c(7.675, 5.79), c(7.375, 13.54)), c(TRUE, FALSE))
expect_true(all(cassava_cells_ok("XX", c(0, 10), c(0, 10))))
