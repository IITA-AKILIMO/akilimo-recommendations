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

# LOG_STDERR: the log file gets every line the log level allows. stderr gets all of them by default, and only INFO
# and above (INFO, SUCCESS, WARN, ERROR: not TRACE or DEBUG) when LOG_STDERR is false.
stderr_lines <- function(level, text) {
  seen <- character(0)
  withCallingHandlers(
    log_write(level, text),
    message = function(m) { seen <<- c(seen, conditionMessage(m)); invokeRestart("muffleMessage") }
  )
  seen
}

# default: everything is printed
Sys.unsetenv("LOG_STDERR")
for (lv in c("TRACE", "DEBUG", "INFO", "SUCCESS", "WARN", "ERROR")) expect_true(log_to_stderr(lv), info = lv)
unlink(get_log_file())
expect_equal(length(stderr_lines("DEBUG", "debug to the console")), 1)
expect_equal(length(stderr_lines("INFO", "info to the console")), 1)
expect_true(grepl("debug to the console", log_text(), fixed = TRUE))

# quiet: TRACE and DEBUG stay off stderr, INFO and above are printed, and the file still gets the DEBUG line
for (off in c("false", "FALSE", "0", "no", "off", " Off ")) {
  Sys.setenv(LOG_STDERR = off)
  expect_false(log_to_stderr("TRACE"), info = off)
  expect_false(log_to_stderr("DEBUG"), info = off)
  for (lv in c("INFO", "SUCCESS", "WARN", "ERROR")) expect_true(log_to_stderr(lv), info = paste(off, lv))
  expect_true(log_to_stderr(), info = off)                      # no level given counts as INFO
  unlink(get_log_file())
  expect_equal(length(stderr_lines("DEBUG", "debug file only")), 0, info = off)
  expect_true(grepl("debug file only", log_text(), fixed = TRUE), info = off)
  for (lv in c("INFO", "WARN", "ERROR")) expect_equal(length(stderr_lines(lv, paste(lv, "shown"))), 1, info = paste(off, lv))
}

# anything else keeps everything on stderr
for (on in c("true", "1", "yes", "", "garbage")) {
  Sys.setenv(LOG_STDERR = on)
  expect_true(log_to_stderr("DEBUG"), info = on)
  expect_true(log_to_stderr("TRACE"), info = on)
}
Sys.setenv(LOG_STDERR = "true")
expect_equal(length(stderr_lines("DEBUG", "back on")), 1)
Sys.unsetenv("LOG_STDERR")

# loading the file prints the log level (an INFO line) in both modes
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
expect_true(any(grepl("Current log level", startup_messages(), fixed = TRUE)))
Sys.unsetenv("LOG_STDERR")

unlink(log_dir, recursive = TRUE)
