#!/usr/bin/env Rscript
# tests/check_parse.R — fail if any active R source file has a syntax error.
#
# Run with:
#   Rscript tests/check_parse.R
#
# Covers api.R, the root scripts, R/ (top level only — R/preprocess/ is
# offline tooling), tests/ and scripts/. old/ is intentionally excluded.

akpath <- Sys.getenv("AKILIMO_ROOT", unset = ".")
setwd(akpath)

files <- c(
    list.files(".", pattern = "\\.R$"),
    list.files("R", pattern = "\\.R$", full.names = TRUE),
    list.files("tests", pattern = "\\.R$", full.names = TRUE),
    list.files("scripts", pattern = "\\.R$", full.names = TRUE)
)

bad <- 0L
for (f in files) {
    err <- tryCatch({ parse(f, encoding = "UTF-8"); NULL },
                    error = function(e) conditionMessage(e))
    if (!is.null(err)) {
        bad <- bad + 1L
        cat(sprintf("FAIL %s\n  %s\n", f, err))
    }
}

cat(sprintf("Parsed %d file(s), %d with errors\n", length(files), bad))
quit(status = if (bad > 0L) 1L else 0L)
