# Unit tests for logging (R/logging.R): PDF content never reaches the log, and one huge value cannot blow up a log line.
# No data files needed, so CI runs it:
#   Rscript tests/run_ci_test.R tests/test_logging.R

akpath <- Sys.getenv("AKILIMO_ROOT", unset = ".")
log_dir <- tempfile("akilimo-log-")
Sys.setenv(LOG_LEVEL = "DEBUG", LOG_DIR = log_dir, LOG_FILE_PREFIX = "test")
suppressMessages(source(file.path(akpath, "R", "logging.R")))

log_text <- function() paste(readLines(get_log_file(), warn = FALSE), collapse = "\n")

# A response as run_akilimo() builds it with returnPdf = TRUE (jsonlite::unbox, as pdf_reports() does)
payload <- paste(rep("QUJDREVGR0g=", 100000), collapse = "")   # ~1.2 MB of base64
result <- list(
  status  = jsonlite::unbox("success"),
  data    = list(rec_type = jsonlite::unbox("FR"), recommendation = jsonlite::unbox("Apply 50 kg of urea")),
  reports = list(list(name = jsonlite::unbox("fertilizer_advice.pdf"),
                      content_type = jsonlite::unbox("application/pdf"),
                      content = jsonlite::unbox(payload)))
)

# redact_reports(): the content goes, everything else stays
red <- redact_reports(result)
expect_false(grepl("QUJDREVGR0g", jsonlite::toJSON(red, auto_unbox = TRUE), fixed = TRUE))
expect_true(grepl("omitted: 1200000 base64 chars", red$reports[[1]]$content, fixed = TRUE))
expect_equal(as.character(red$reports[[1]]$name), "fertilizer_advice.pdf")
expect_equal(as.character(red$reports[[1]]$content_type), "application/pdf")
expect_equal(as.character(red$data$recommendation), "Apply 50 kg of urea")
expect_equal(nchar(payload), 1200000)                         # the response itself is not touched
expect_equal(nchar(result$reports[[1]]$content), 1200000)

# nothing to redact: returned as is
plain <- list(status = jsonlite::unbox("success"), data = list(x = 1))
expect_identical(redact_reports(plain), plain)
expect_identical(redact_reports(list(reports = list())), list(reports = list()))
expect_null(redact_reports(NULL))
no_content <- list(reports = list(list(name = "a.pdf")))
expect_identical(redact_reports(no_content), no_content)

# the way api.R logs the result: the log gets the name and size, not the PDF
suppressMessages(log_write("DEBUG", "RESULT:", redact_reports(result)))
txt <- log_text()
expect_true(grepl("fertilizer_advice.pdf", txt, fixed = TRUE))
expect_true(grepl("omitted: 1200000 base64 chars", txt, fixed = TRUE))
expect_false(grepl("QUJDREVGR0g", txt, fixed = TRUE))
expect_true(nchar(txt) < 2000)

# safety net: any single log line is capped, whatever the value
Sys.setenv(LOG_MAX_CHARS = "500")
unlink(get_log_file())
suppressMessages(log_write("INFO", "BIG:", paste(rep("x", 50000), collapse = "")))
txt <- log_text()
expect_true(nchar(txt) < 800)
expect_true(grepl("truncated, 49", txt))                      # 50000 + "BIG: " minus the 500 kept

# short lines are untouched
unlink(get_log_file())
suppressMessages(log_write("INFO", "short line"))
expect_true(grepl("short line", log_text(), fixed = TRUE))
expect_false(grepl("truncated", log_text(), fixed = TRUE))

# LOG_MAX_CHARS = 0 turns the cap off; a bad value falls back to the default
Sys.setenv(LOG_MAX_CHARS = "0")
expect_equal(log_max_chars(), 0)
unlink(get_log_file())
suppressMessages(log_write("INFO", paste(rep("y", 30000), collapse = "")))
expect_false(grepl("truncated", log_text(), fixed = TRUE))
Sys.setenv(LOG_MAX_CHARS = "abc")
expect_equal(log_max_chars(), 20000)
Sys.unsetenv("LOG_MAX_CHARS")
expect_equal(log_max_chars(), 20000)

# LOG_STDERR: the file is always written; stderr only when enabled (default on)
stderr_lines <- function(...) {
  seen <- character(0)
  withCallingHandlers(
    log_write("INFO", ...),
    message = function(m) { seen <<- c(seen, conditionMessage(m)); invokeRestart("muffleMessage") }
  )
  seen
}
Sys.unsetenv("LOG_STDERR")
expect_true(log_to_stderr())
unlink(get_log_file())
expect_equal(length(stderr_lines("to the console")), 1)        # default: printed
expect_true(grepl("to the console", log_text(), fixed = TRUE))

for (off in c("false", "FALSE", "0", "no", "off", " Off ")) {
  Sys.setenv(LOG_STDERR = off)
  expect_false(log_to_stderr(), info = off)
  unlink(get_log_file())
  expect_equal(length(stderr_lines("file only")), 0, info = off)   # nothing on stderr
  expect_true(grepl("file only", log_text(), fixed = TRUE), info = off)  # still in the file
}
for (on in c("true", "1", "yes", "", "garbage")) {
  Sys.setenv(LOG_STDERR = on)
  expect_true(log_to_stderr(), info = on)                       # anything else keeps stderr on
}
Sys.setenv(LOG_STDERR = "true")
expect_equal(length(stderr_lines("back on")), 1)
Sys.unsetenv("LOG_STDERR")

# loading the file prints the level to stderr by default, and nothing when LOG_STDERR is off
startup_messages <- function() {
  seen <- character(0)
  withCallingHandlers(
    source(file.path(akpath, "R", "logging.R")),
    message = function(m) { seen <<- c(seen, conditionMessage(m)); invokeRestart("muffleMessage") }
  )
  seen
}
expect_true(any(grepl("Current log level", startup_messages(), fixed = TRUE)))
Sys.setenv(LOG_STDERR = "false")
expect_equal(length(startup_messages()), 0)
Sys.unsetenv("LOG_STDERR")

unlink(log_dir, recursive = TRUE)
