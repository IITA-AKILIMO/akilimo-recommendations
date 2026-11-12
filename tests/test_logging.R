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

# ---------------------------------------------------------------------------
# 1. Personal data never goes in the log
# ---------------------------------------------------------------------------
body <- jsonlite::toJSON(list(
  country = "NG", lat = 7.4, lon = 5.2, FR = TRUE, email = TRUE, FCY = 11,
  userName = "Jane Farmer", userEmail = "jane@example.org", userPhoneNr = "0712345678",
  userPhoneCC = "+254", fullPhoneNumber = "+254712345678", userField = "Jane's plot"
), auto_unbox = TRUE)
red <- redact_request(as.character(body))
for (f in c("userName", "userEmail", "userPhoneNr", "userPhoneCC", "fullPhoneNumber", "userField"))
  expect_equal(red[[f]], "<redacted>", info = f)
expect_true(isTRUE(red$email))                                  # the yes/no flag is not personal data
expect_equal(red$country, "NG")
expect_equal(red$FCY, 11)
unlink(get_log_file())
suppressMessages(log_write("DEBUG", "REQUEST:", red))        # TRACE in the server; DEBUG here, the test's level
txt <- log_text()
expect_false(any(vapply(c("Jane", "jane@example.org", "0712345678", "254712345678"), grepl, logical(1), x = txt, fixed = TRUE)))
expect_match(redact_request("{not json"), "not valid JSON")
expect_match(redact_request(NULL), "not valid JSON")

# ---------------------------------------------------------------------------
# 2. Request id on every line
# ---------------------------------------------------------------------------
uuid <- "123e4567-e89b-12d3-a456-426614174000"
m <- request_meta(as.character(jsonlite::toJSON(list(requestId = uuid, country = "rw"), auto_unbox = TRUE)))
expect_equal(m$request_id, uuid)                                 # the API's own id is kept
expect_equal(m$country, "RW")
m2 <- request_meta('{"country":"NG","requestId":"not-a-uuid"}')
expect_match(m2$request_id, "^[0-9a-f]{8}$")                     # otherwise a short one is made
m3 <- request_meta("{broken")
expect_match(m3$request_id, "^[0-9a-f]{8}$")
expect_equal(m3$country, "?")

unlink(get_log_file())
set_log_context("req-42")
suppressMessages(log_write("INFO", "inside a request"))
clear_log_context()
suppressMessages(log_write("INFO", "outside a request"))
lines <- readLines(get_log_file(), warn = FALSE)
expect_true(grepl("[req=req-42] inside a request", lines[1], fixed = TRUE))
expect_false(grepl("[req=", lines[2], fixed = TRUE))
expect_null(get_log_request_id())

# ---------------------------------------------------------------------------
# 3. JSON lines for a log collector
# ---------------------------------------------------------------------------
Sys.setenv(LOG_FORMAT = "json")
unlink(get_log_file())
set_log_context("req-json")
suppressMessages(log_write("WARN", "two\nlines", "of text"))
clear_log_context()
suppressMessages(log_write("INFO", "no request"))
lines <- readLines(get_log_file(), warn = FALSE)
expect_equal(length(lines), 2)                                   # a multi-line message is still one line
j1 <- jsonlite::fromJSON(lines[1]); j2 <- jsonlite::fromJSON(lines[2])
expect_equal(j1$level, "WARN"); expect_equal(j1$request_id, "req-json"); expect_equal(j1$message, "two\nlines of text")
expect_true(!is.null(j1$time))
expect_null(j2$request_id)
Sys.unsetenv("LOG_FORMAT")
expect_equal(log_format(), "text")

# ---------------------------------------------------------------------------
# 4. Logging never breaks a request
# ---------------------------------------------------------------------------
blocker <- tempfile("not-a-dir-"); writeLines("x", blocker)
Sys.setenv(LOG_DIR = file.path(blocker, "logs"))                 # cannot be created: its parent is a file
seen <- character(0)
res <- withCallingHandlers(
  tryCatch({ log_write("INFO", "log dir is unusable"); "no error" }, error = function(e) "ERROR"),
  message = function(m) { seen <<- c(seen, conditionMessage(m)); invokeRestart("muffleMessage") }
)
expect_equal(res, "no error")
expect_true(any(grepl("log dir is unusable", seen, fixed = TRUE)))   # the line went to stderr instead
Sys.setenv(LOG_DIR = log_dir)
res <- suppressMessages(tryCatch({ log_write("INFO", stop("argument fails")); "no error" }, error = function(e) "ERROR"))
expect_equal(res, "no error")                                    # an argument that errors is swallowed too

# ---------------------------------------------------------------------------
# 5. R warnings become log lines
# ---------------------------------------------------------------------------
unlink(get_log_file())
set_log_context("req-warn")
warned <- 0
val <- withCallingHandlers(
  suppressMessages(with_logged_warnings({ warning("FCY is outside the range"); "the value" })),
  warning = function(w) { warned <<- warned + 1 }
)
clear_log_context()
expect_equal(val, "the value")                                   # the code carries on
expect_equal(warned, 0)                                          # and the warning is not left for stderr
txt <- log_text()
expect_true(grepl("[WARN] [req=req-warn] FCY is outside the range", txt, fixed = TRUE))
expect_error(with_logged_warnings(stop("a real error")), "a real error")   # errors pass through

# code that logs a problem and then warns about the same thing is logged once, not twice
unlink(get_log_file())
set_log_context("req-dup")
suppressMessages(with_logged_warnings({
  # the way the email senders do it: the log line in two parts, the warning in two parts, the same text
  log_write("WARN", "SMTP email not sent: missing env vars:", paste(c("EMAIL_HOST", "EMAIL_USER"), collapse = ", "))
  warning("SMTP email not sent: missing env vars: ", paste(c("EMAIL_HOST", "EMAIL_USER"), collapse = ", "))
  warning("a different warning")
}))
clear_log_context()
txt <- log_text()
expect_equal(lengths(regmatches(txt, gregexpr("SMTP email not sent", txt, fixed = TRUE))), 1)
expect_true(grepl("a different warning", txt, fixed = TRUE))
# the next request starts afresh: the same text is logged again
unlink(get_log_file())
set_log_context("req-next")
suppressMessages(with_logged_warnings(warning("SMTP email not sent: missing env vars: EMAIL_HOST")))
clear_log_context()
expect_true(grepl("SMTP email not sent", log_text(), fixed = TRUE))
# the warning names the function it came from, not a truncated call
sender <- function() warning("from a function")
unlink(get_log_file())
suppressMessages(with_logged_warnings(sender()))
expect_true(grepl("from a function (in sender)", log_text(), fixed = TRUE))

# the recipient's address never goes in the log: every log line in email.R that names the recipient masks it
# (mask_email() is in html_helpers.R, which the PDF report uses too)
email_src <- paste(readLines(file.path(akpath, "R", "email.R"), warn = FALSE), collapse = "\n")
formats_naming_recipient <- length(gregexpr("to=%s|to %s", email_src)[[1]])
expect_true(formats_naming_recipient >= 7)
expect_equal(length(gregexpr("mask_email\\(to\\)", email_src)[[1]]), formats_naming_recipient)

# ---------------------------------------------------------------------------
# 6. Old log files are removed
# ---------------------------------------------------------------------------
keep_dir <- tempfile("keep-"); dir.create(keep_dir)
touch <- function(n) writeLines("x", file.path(keep_dir, n))
touch("compute-20200101.log"); touch("compute-20200102.log"); touch(paste0("compute-", format(Sys.Date(), "%Y%m%d"), ".log"))
touch("other-20200101.log"); touch("compute-notes.txt")
expect_equal(prune_old_logs(keep_dir, "compute", keep_days = 0), 0L)            # 0 keeps everything
expect_equal(length(list.files(keep_dir)), 5)
expect_equal(prune_old_logs(keep_dir, "compute", keep_days = 30), 2L)
expect_equal(sort(list.files(keep_dir)), sort(c(paste0("compute-", format(Sys.Date(), "%Y%m%d"), ".log"),
                                                 "other-20200101.log", "compute-notes.txt")))   # only its own files
expect_equal(prune_old_logs(file.path(keep_dir, "missing"), "compute", keep_days = 30), 0L)
# and by itself, once a day, when a log line is written
touch("test-20200101.log")                                       # this test file's prefix is "test"
Sys.setenv(LOG_DIR = keep_dir, LOG_KEEP_DAYS = "30")
.log_ctx$pruned_on <- NULL
suppressMessages(log_write("INFO", "writing triggers the cleanup"))
expect_false(file.exists(file.path(keep_dir, "test-20200101.log")))
expect_true(file.exists(file.path(keep_dir, "compute-20200102.log")) == FALSE)   # removed by the direct call above
Sys.setenv(LOG_DIR = log_dir); Sys.unsetenv("LOG_KEEP_DAYS")
expect_equal(log_keep_days(), 0)

# ---------------------------------------------------------------------------
# 7. A summary at DEBUG, one line when the request is done
# ---------------------------------------------------------------------------
result <- list(
  status = jsonlite::unbox("success"),
  data = list(rec_type = jsonlite::unbox("FR"), recommendation = jsonlite::unbox(strrep("Apply urea. ", 100)),
              data = data.frame(N = 1:3), fertilizer_rates = data.frame(type = c("Urea", "DAP"), rate = c(50, 20))),
  reports = list(list(name = jsonlite::unbox("advice.pdf"), content = jsonlite::unbox(strrep("QUJD", 5000))))
)
s <- summarize_result(result)
expect_equal(s$status, "success"); expect_equal(s$rec_type, "FR")
expect_equal(s$data_rows, 3); expect_equal(s$fertilizer_rates, 2)
expect_equal(nchar(s$recommendation), 500)                       # cut, not the whole text
expect_equal(s$reports[[1]]$name, "advice.pdf"); expect_equal(s$reports[[1]]$base64_chars, 20000)
expect_null(s$reports[[1]]$content)
expect_true(nchar(as.character(jsonlite::toJSON(s, auto_unbox = TRUE))) < 1500)
bad <- list(status = "400 - bad request", data = list(message = "Invalid area"))
expect_equal(summarize_result(bad)$message, "Invalid area")

unlink(get_log_file())
set_log_context("req-done")
suppressMessages(log_request_done("NG", result, Sys.time() - 0.25))
suppressMessages(log_request_done("RW", bad, Sys.time()))
suppressMessages(log_request_done("TZ", NULL, Sys.time(), status = "500", reason = "object 'x' not found"))
Sys.setenv(LOG_SLOW_MS = "100")
suppressMessages(log_request_done("GH", result, Sys.time() - 5))
Sys.unsetenv("LOG_SLOW_MS")
clear_log_context()
lines <- readLines(get_log_file(), warn = FALSE)
expect_equal(length(lines), 4)
expect_true(grepl("[INFO] [req=req-done] REQUEST DONE status=success type=FR country=NG duration_ms=2", lines[1], fixed = TRUE))
expect_true(grepl("response_kb=", lines[1], fixed = TRUE))
expect_true(grepl("status=400 type=NA country=RW", lines[2], fixed = TRUE))
expect_true(grepl("reason=Invalid area", lines[2], fixed = TRUE))
expect_true(grepl("status=500", lines[3], fixed = TRUE)); expect_true(grepl("reason=object 'x' not found", lines[3], fixed = TRUE))
expect_true(grepl("[WARN]", lines[4], fixed = TRUE)); expect_true(grepl("SLOW(>100ms)", lines[4], fixed = TRUE))
expect_false(grepl("SLOW", lines[1], fixed = TRUE))              # under the default 30 s
expect_equal(log_slow_ms(), 30000)

# ---------------------------------------------------------------------------
# 8. The start-up summary shows settings, never secrets
# ---------------------------------------------------------------------------
Sys.setenv(EMAIL_HOST = "smtp.internal.example", MAPBOX_TOKEN = "pk.SECRET-TOKEN-VALUE")
unlink(get_log_file())
suppressMessages(log_startup_summary(version = "20260101", port = 8000))
txt <- log_text()
expect_true(grepl("STARTUP version=20260101 port=8000", txt, fixed = TRUE))
expect_true(grepl("log_format=text", txt, fixed = TRUE))
expect_true(grepl("EMAIL_HOST=set", txt, fixed = TRUE)); expect_true(grepl("MAPBOX_TOKEN=set", txt, fixed = TRUE))
expect_false(grepl("smtp.internal.example", txt, fixed = TRUE)); expect_false(grepl("SECRET-TOKEN-VALUE", txt, fixed = TRUE))
Sys.unsetenv(c("EMAIL_HOST", "MAPBOX_TOKEN"))

unlink(log_dir, recursive = TRUE)
