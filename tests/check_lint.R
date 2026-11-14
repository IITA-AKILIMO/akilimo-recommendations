## Lint R/ and fail when lintr reports anything.
## Configuration: .lintr at the repo root (style linters are off for now).
## Run locally with: Rscript tests/check_lint.R

if (!requireNamespace("lintr", quietly = TRUE)) {
  stop("lintr is not installed; run install_packages.R first")
}

lints <- lintr::lint_dir("R")
print(lints)
if (length(lints) > 0L) {
  quit(status = 1L)
}
