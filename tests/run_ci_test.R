#!/usr/bin/env Rscript
# tests/run_ci_test.R — run one tinytest file the way CI does.
#
#   Rscript tests/run_ci_test.R tests/test_akilimo_db.R
#
# Prints the summary and every failing check (call and difference), and exits
# non-zero if any check failed or the file ran no checks at all (a file that
# skips itself, e.g. for missing data, must not count as a pass).

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("usage: Rscript tests/run_ci_test.R <test file>", call. = FALSE)

r <- tinytest::run_test_file(args[[1]], verbose = 1)
print(summary(r))

ok <- as.logical(r)
if (!all(ok)) print(r[!ok])
quit(status = if (length(r) > 0 && all(ok)) 0 else 1)
