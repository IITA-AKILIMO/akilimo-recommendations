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
old_b <- Sys.getenv("CASSAVA_BUFFER_CELLS", unset = NA)
on.exit(if (is.na(old_b)) Sys.unsetenv("CASSAVA_BUFFER_CELLS") else Sys.setenv(CASSAVA_BUFFER_CELLS = old_b), add = TRUE)
Sys.unsetenv("CASSAVA_BUFFER_CELLS")

# Buffer width: 1 cell by default, 0 = own cell only, bad values fall back to 1
expect_equal(cassava_buffer_cells(), 1L)
Sys.setenv(CASSAVA_BUFFER_CELLS = "0"); expect_equal(cassava_buffer_cells(), 0L)
Sys.setenv(CASSAVA_BUFFER_CELLS = "2"); expect_equal(cassava_buffer_cells(), 2L)
Sys.setenv(CASSAVA_BUFFER_CELLS = "-1"); expect_equal(cassava_buffer_cells(), 1L)
Sys.setenv(CASSAVA_BUFFER_CELLS = "1.5"); expect_equal(cassava_buffer_cells(), 1L)
Sys.setenv(CASSAVA_BUFFER_CELLS = "x"); expect_equal(cassava_buffer_cells(), 1L)

# Own-cell rules below are tested with no buffer
Sys.setenv(CASSAVA_BUFFER_CELLS = "0")

# Threshold: 0 ha by default, overridable, and bad values fall back to the default
expect_equal(cassava_min_ha(), 0)
Sys.setenv(CASSAVA_MIN_HA = "250"); expect_equal(cassava_min_ha(), 250)
Sys.setenv(CASSAVA_MIN_HA = "0");   expect_equal(cassava_min_ha(), 0)
Sys.setenv(CASSAVA_MIN_HA = "-5");  expect_equal(cassava_min_ha(), 0)
Sys.setenv(CASSAVA_MIN_HA = "abc"); expect_equal(cassava_min_ha(), 0)
Sys.unsetenv("CASSAVA_MIN_HA")

# A point and the MapSPAM cell it falls in share a key, anywhere in the cell
expect_equal(spam_cell_key(7.675, 7.375), spam_cell_key(7.70, 7.40))
expect_false(spam_cell_key(7.675, 7.375) == spam_cell_key(7.76, 7.375))

# Every country has a mask and clear-cut points: Ibadan area (NG) is a strong cassava cell,
# a Sahel cell (NG, ~7 ha) is kept by default and dropped at 20 ha
for (cc in c("NG", "TZ", "RW", "GH", "BI"))
  expect_true(length(cassava_area_cells(cc)) > 0, info = paste(cc, "has mask rows"))
expect_true(cassava_cells_ok("NG", 7.675, 7.375), info = "NG cell with ~2000 ha is kept")
expect_true(cassava_cells_ok("NG", 5.79, 13.54), info = "NG cell with ~7 ha is kept by default")
expect_false(cassava_cells_ok("RW", 29.375, -2.825), info = "a cell with no recorded cassava is dropped")

# The threshold moves the boundary: the ~7 ha cell passes at 5 ha and fails at 20 ha
Sys.setenv(CASSAVA_MIN_HA = "5");  expect_true(cassava_cells_ok("NG", 5.79, 13.54))
Sys.setenv(CASSAVA_MIN_HA = "20"); expect_false(cassava_cells_ok("NG", 5.79, 13.54))
Sys.unsetenv("CASSAVA_MIN_HA")

# Vectorised, and no mask for a country means every point passes (fails open)
Sys.setenv(CASSAVA_MIN_HA = "20")
expect_equal(cassava_cells_ok("NG", c(7.675, 5.79), c(7.375, 13.54)), c(TRUE, FALSE))
Sys.unsetenv("CASSAVA_MIN_HA")
expect_true(all(cassava_cells_ok("XX", c(0, 10), c(0, 10))))

# Buffer: a TZ point two cells from the nearest cassava cell (none recorded in its own cell or
# the 8 around it) is covered with buffer 2 but not 1 or 0; a neighbour-only point (own cell
# empty, next cell has cassava) is covered with buffer 1
Sys.setenv(CASSAVA_MIN_HA = "0")
Sys.setenv(CASSAVA_BUFFER_CELLS = "0"); expect_false(cassava_cells_ok("TZ", 30.147, -6.089))
Sys.setenv(CASSAVA_BUFFER_CELLS = "1"); expect_false(cassava_cells_ok("TZ", 30.147, -6.089))
Sys.setenv(CASSAVA_BUFFER_CELLS = "2"); expect_true(cassava_cells_ok("TZ", 30.147, -6.089))
Sys.setenv(CASSAVA_MIN_HA = "20")       # the cassava cell there holds 8.2 ha
expect_false(cassava_cells_ok("TZ", 30.147, -6.089))
Sys.setenv(CASSAVA_MIN_HA = "0")
Sys.setenv(CASSAVA_BUFFER_CELLS = "0"); expect_false(cassava_cells_ok("TZ", 30.21, -6.089))
Sys.setenv(CASSAVA_BUFFER_CELLS = "1"); expect_true(cassava_cells_ok("TZ", 30.21, -6.089))
Sys.unsetenv("CASSAVA_MIN_HA")
Sys.unsetenv("CASSAVA_BUFFER_CELLS")
