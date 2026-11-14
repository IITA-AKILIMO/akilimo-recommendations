#!/usr/bin/env Rscript
## Lint R/ with the repo's .lintr and exit 1 when anything is reported.
## Works from any directory:
##   Rscript tools/lint_r.R

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) == 0L) {
  stop("Run this with Rscript tools/lint_r.R")
}
script <- sub("^--file=", "", file_arg[1])
root <- normalizePath(file.path(dirname(script), ".."))
old <- setwd(root)
on.exit(setwd(old), add = TRUE)

source(file.path(root, "tests", "check_lint.R"), local = TRUE)
