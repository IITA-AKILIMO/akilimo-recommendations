# tests/test_weasyprint_smoke.R — Phase 1 WeasyPrint smoke test
#
# Verifies that:
#   1. WeasyPrint is on PATH and reachable via system2().
#   2. render_pdf() produces a non-empty PDF from a minimal HTML document.
#   3. img_base64() correctly encodes a small PNG to a data URI.
#
# Run from the project root:
#   Rscript tests/test_weasyprint_smoke.R

this   <- system("hostname", TRUE)
akpath <- if (this == "denovo") "C:/github/omilika/akilimo-recommendations" else "."
setwd(akpath)

source(file.path(akpath, "R", "markdown.R"))    # tp(), set_temp_dir(), temp_dir()
source(file.path(akpath, "R", "html_helpers.R"))
source(file.path(akpath, "R", "pdf_builders.R"))

# Inline setup_temp_dir() to avoid loading all of AkilimoMain.R
dir.create("temp", showWarnings = FALSE)
req_id  <- paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_",
                  paste0(as.hexmode(sample.int(.Machine$integer.max, 2)), collapse = ""))
req_dir <- file.path("temp", req_id)
dir.create(req_dir)
set_temp_dir(req_dir)

pass <- 0L
fail <- 0L

ok <- function(label) {
  cat(sprintf("PASS  %s\n", label))
  pass <<- pass + 1L
}

fail_test <- function(label, reason) {
  cat(sprintf("FAIL  %s — %s\n", label, reason))
  fail <<- fail + 1L
}

# ── 1. WeasyPrint on PATH ────────────────────────────────────────────────────

cat("Checking WeasyPrint availability...\n")
wp_path <- Sys.which("weasyprint")
if (nchar(wp_path) == 0) {
  fail_test("WeasyPrint on PATH", "not found — install with: pip install weasyprint")
} else {
  version_out <- system2("weasyprint", "--version", stdout = TRUE, stderr = TRUE)
  ok(paste("WeasyPrint on PATH:", paste(version_out, collapse = " ")))
}

# ── 2. render_pdf() produces a valid PDF ────────────────────────────────────

cat("\nRunning render_pdf() smoke test...\n")
minimal_html <- '<!DOCTYPE html>
<html><head><meta charset="UTF-8"><title>Smoke test</title>
<style>body { font-family: Arial; font-size: 12px; } </style>
</head><body>
<h1>Akilimo WeasyPrint Smoke Test</h1>
<p>If you can read this, WeasyPrint is working correctly.</p>
</body></html>'

out_pdf <- file.path(temp_dir(), "smoke_test.pdf")

tryCatch({
  render_pdf(minimal_html, out_pdf)
  if (file.exists(out_pdf) && file.size(out_pdf) > 0) {
    raw <- readBin(out_pdf, "raw", n = 4)
    if (rawToChar(raw) == "%PDF") {
      ok(sprintf("render_pdf() → valid PDF (%d bytes)", file.size(out_pdf)))
    } else {
      fail_test("render_pdf()", "output does not start with %PDF magic bytes")
    }
  } else {
    fail_test("render_pdf()", "output file missing or empty")
  }
}, error = function(e) {
  fail_test("render_pdf()", conditionMessage(e))
})

# ── 3. img_base64() encodes a PNG ────────────────────────────────────────────

cat("\nRunning img_base64() smoke test...\n")

# Find any banner PNG to use as a test image
banner <- file.path(akpath, "net", "Akilimo_Dashboard_FR.png")
if (file.exists(banner)) {
  tryCatch({
    tag <- img_base64(banner, alt = "test")
    if (grepl("^<img src=\"data:image/png;base64,", tag)) {
      ok("img_base64() produces valid data URI")
    } else {
      fail_test("img_base64()", "unexpected output format")
    }
  }, error = function(e) {
    fail_test("img_base64()", conditionMessage(e))
  })
} else {
  cat(sprintf("SKIP  img_base64() — banner not found at %s\n", banner))
}

# ── 4. render_pdf() with embedded image ─────────────────────────────────────

if (file.exists(banner)) {
  cat("\nRunning render_pdf() with embedded image...\n")
  img_tag <- tryCatch(img_base64(banner), error = function(e) NULL)
  if (!is.null(img_tag)) {
    html_with_img <- sprintf(
      '<!DOCTYPE html><html><head><meta charset="UTF-8"><title>Image test</title>
<style>body{font-family:Arial;font-size:12px;} img{width:100%%;}</style>
</head><body><h1>Image embed test</h1>%s<p>Image rendered above.</p></body></html>',
      img_tag
    )
    out_pdf2 <- file.path(temp_dir(), "smoke_test_img.pdf")
    tryCatch({
      render_pdf(html_with_img, out_pdf2)
      if (file.exists(out_pdf2) && file.size(out_pdf2) > 0) {
        ok(sprintf("render_pdf() with base64 image → valid PDF (%d bytes)",
                   file.size(out_pdf2)))
      } else {
        fail_test("render_pdf() with image", "output missing or empty")
      }
    }, error = function(e) {
      fail_test("render_pdf() with image", conditionMessage(e))
    })
  }
}

# ── Summary ──────────────────────────────────────────────────────────────────

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
cat(sprintf("Temp dir: %s\n", temp_dir()))
if (fail > 0) quit(status = 1L)
