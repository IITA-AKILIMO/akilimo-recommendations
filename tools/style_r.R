#!/usr/bin/env Rscript
## Format R/ with styler. Works from any directory:
##   Rscript tools/style_r.R          # rewrite files in place
##   Rscript tools/style_r.R --dry    # show what would change, write nothing
##
## Review the diff after running it in place: styler only changes layout.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) == 0L) {
  stop("Run this with Rscript tools/style_r.R")
}
script <- sub("^--file=", "", file_arg[1])
root <- normalizePath(file.path(dirname(script), ".."))
old <- setwd(root)
on.exit(setwd(old), add = TRUE)

if (!requireNamespace("styler", quietly = TRUE)) {
  stop("styler is not installed; run install_packages.R first")
}

dry <- "--dry" %in% commandArgs(trailingOnly = TRUE)
## R/preprocess is offline tooling and is off limits (CLAUDE.md); .lintr skips it too.
result <- styler::style_dir(
  "R",
  dry = if (dry) "on" else "off",
  exclude_dirs = c("packrat", "renv", "preprocess")
)
if (dry && nrow(result) > 0L) {
  cat("\n", nrow(result), " file(s) would change; run without --dry to write them.\n", sep = "")
}
