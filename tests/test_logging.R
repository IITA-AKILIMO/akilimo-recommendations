# Unit tests for logging (R/logging.R): PDF content never reaches the log, and one huge value cannot blow up a log line.
# No data files needed, so CI runs it:
#   Rscript tests/run_ci_test.R tests/test_logging.R

akpath <- Sys.getenv("AKILIMO_ROOT", unset = ".")
log_dir <- tempfile("akilimo-log-")
# logging.R loads the project's .env when it is sourced, and a .env can set any LOG_ variable over what is set here.
# pin_log_settings() puts the test's own settings back (and the level logging.R cached), so the tests give the same
# result whatever .env is in the project.
log_vars <- c("LOG_CHANNEL", "LOG_STACK", "LOG_FORMAT", "LOG_FILE_LEVEL", "LOG_STDERR_LEVEL", "LOG_STDOUT_LEVEL",
              "LOG_FILE_FORMAT", "LOG_STDERR_FORMAT", "LOG_STDOUT_FORMAT", "LOG_STDERR", "LOG_MAX_CHARS",
              "LOG_KEEP_DAYS", "LOG_SLOW_MS")
pin_log_settings <- function() {
  Sys.unsetenv(log_vars)
  Sys.setenv(LOG_LEVEL = "DEBUG", LOG_DIR = log_dir, LOG_FILE_PREFIX = "test")
  assign("min_log_level_value", 1, envir = globalenv())   # DEBUG
}
pin_log_settings()
suppressMessages(source(file.path(akpath, "R", "logging.R")))
pin_log_settings()

# The logger''s state lives in .log_ctx, which every source() of logging.R makes anew: always reach the current one
# (assigning into .log_ctx here would bind a stale copy in this file).
ctx_reset <- function(...) { ctx <- get(".log_ctx", envir = globalenv()); for (n in c(...)) ctx[[n]] <- NULL }

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

# ---------------------------------------------------------------------------
# The channel stack (like Laravel's): where a line goes, and at which level and format
# ---------------------------------------------------------------------------
# What each channel received for one line: what went to stderr, to stdout and to the file
receive <- function(level, text) {
  unlink(get_log_file())
  err <- character(0)
  out <- capture.output(withCallingHandlers(
    log_write(level, text),
    message = function(m) { err <<- c(err, conditionMessage(m)); invokeRestart("muffleMessage") }
  ))
  list(stderr = err, stdout = out,
       file = if (file.exists(get_log_file())) readLines(get_log_file(), warn = FALSE) else character(0))
}
counts <- function(r) c(file = length(r$file), stderr = length(r$stderr), stdout = length(r$stdout))

# default: the file and stderr, at LOG_LEVEL
expect_equal(log_stack(), c("file", "stderr"))
expect_equal(counts(receive("DEBUG", "default stack")), c(file = 1, stderr = 1, stdout = 0))

# LOG_STACK chooses the channels; spaces and case do not matter
Sys.setenv(LOG_STACK = "file")
expect_equal(counts(receive("DEBUG", "file only")), c(file = 1, stderr = 0, stdout = 0))
Sys.setenv(LOG_STACK = "stderr")
expect_equal(counts(receive("DEBUG", "stderr only")), c(file = 0, stderr = 1, stdout = 0))   # no file is made
Sys.setenv(LOG_STACK = "stdout")
expect_equal(counts(receive("DEBUG", "stdout only")), c(file = 0, stderr = 0, stdout = 1))
Sys.setenv(LOG_STACK = " File , STDERR,stdout ")
expect_equal(log_stack(), c("file", "stderr", "stdout"))
expect_equal(counts(receive("DEBUG", "all three")), c(file = 1, stderr = 1, stdout = 1))
Sys.setenv(LOG_STACK = "stderr,stderr")
expect_equal(log_stack(), "stderr")                                    # a channel listed twice is used once

# LOG_CHANNEL: "stack" uses LOG_STACK, any other value is that one channel on its own
Sys.setenv(LOG_STACK = "file,stderr,stdout", LOG_CHANNEL = "stderr")
expect_equal(log_stack(), "stderr")
expect_equal(counts(receive("DEBUG", "single channel")), c(file = 0, stderr = 1, stdout = 0))
Sys.setenv(LOG_CHANNEL = "file")
expect_equal(log_stack(), "file")
Sys.setenv(LOG_CHANNEL = "stack")
expect_equal(log_stack(), c("file", "stderr", "stdout"))
Sys.unsetenv(c("LOG_CHANNEL", "LOG_STACK"))

# null switches logging off; unknown names are ignored, and a stack with none left is the default one
Sys.setenv(LOG_STACK = "null")
expect_equal(log_stack(), character(0))
expect_false(should_log("ERROR"))
expect_equal(counts(receive("ERROR", "nowhere")), c(file = 0, stderr = 0, stdout = 0))
Sys.setenv(LOG_STACK = "file,syslog")
ctx_reset("unknown_noticed")
notice <- character(0)
withCallingHandlers(log_stack(), message = function(m) { notice <<- c(notice, conditionMessage(m)); invokeRestart("muffleMessage") })
expect_true(any(grepl("unknown log channel(s) ignored: syslog", notice, fixed = TRUE)))
expect_equal(log_stack(), "file")
Sys.setenv(LOG_STACK = "bogus")
expect_equal(suppressMessages(log_stack()), c("file", "stderr"))
Sys.setenv(LOG_STACK = "")
expect_equal(log_stack(), c("file", "stderr"))
Sys.unsetenv("LOG_STACK")

# each channel has its own level: LOG_<CHANNEL>_LEVEL, else LOG_LEVEL
Sys.setenv(LOG_STDERR_LEVEL = "WARN")
expect_equal(channel_level("stderr"), log_levels[["WARN"]][[1]])
expect_equal(channel_level("file"), log_levels[["DEBUG"]][[1]])
expect_equal(counts(receive("DEBUG", "debug")), c(file = 1, stderr = 0, stdout = 0))   # only in the file
expect_equal(counts(receive("INFO", "info")), c(file = 1, stderr = 0, stdout = 0))
expect_equal(counts(receive("WARN", "warn")), c(file = 1, stderr = 1, stdout = 0))
expect_equal(counts(receive("ERROR", "error")), c(file = 1, stderr = 1, stdout = 0))
Sys.setenv(LOG_STDERR_LEVEL = "warn")                                  # case does not matter
expect_equal(channel_level("stderr"), log_levels[["WARN"]][[1]])
# a channel can be more verbose than the others: the line is built when any channel takes it
Sys.setenv(LOG_FILE_LEVEL = "ERROR", LOG_STDERR_LEVEL = "DEBUG")
expect_true(should_log("DEBUG"))
expect_equal(counts(receive("DEBUG", "debug")), c(file = 0, stderr = 1, stdout = 0))
expect_equal(counts(receive("ERROR", "error")), c(file = 1, stderr = 1, stdout = 0))
Sys.setenv(LOG_FILE_LEVEL = "ERROR", LOG_STDERR_LEVEL = "ERROR")
expect_false(should_log("WARN"))                                       # no channel takes it
Sys.setenv(LOG_FILE_LEVEL = "loud", LOG_STDERR_LEVEL = "")             # not a level: LOG_LEVEL is used
expect_equal(channel_level("file"), log_levels[["DEBUG"]][[1]])
expect_equal(channel_level("stderr"), log_levels[["DEBUG"]][[1]])
Sys.unsetenv(c("LOG_FILE_LEVEL", "LOG_STDERR_LEVEL"))

# each channel has its own format, else LOG_FORMAT
Sys.setenv(LOG_FILE_FORMAT = "json")
r <- receive("DEBUG", "json in the file")
expect_equal(jsonlite::fromJSON(r$file)$message, "json in the file")   # the file line is JSON
expect_true(grepl("^\\[.*\\] \\[DEBUG\\] json in the file", r$stderr))   # stderr is still text
Sys.setenv(LOG_FORMAT = "json", LOG_STDERR_FORMAT = "text")
r <- receive("DEBUG", "mixed")
expect_equal(jsonlite::fromJSON(r$file)$level, "DEBUG")
expect_true(grepl("[DEBUG] mixed", r$stderr, fixed = TRUE))
expect_equal(channel_format("stdout"), "json")                          # LOG_FORMAT applies where nothing is set
Sys.unsetenv(c("LOG_FILE_FORMAT", "LOG_FORMAT", "LOG_STDERR_FORMAT"))

# the older LOG_STDERR=false still means: INFO and above on stderr
for (off in c("false", "FALSE", "0", "no", "off", " Off ")) {
  Sys.setenv(LOG_STDERR = off)
  expect_equal(channel_level("stderr"), log_levels[["INFO"]][[1]], info = off)
  expect_equal(counts(receive("DEBUG", "debug")), c(file = 1, stderr = 0, stdout = 0), info = off)
  expect_equal(counts(receive("INFO", "info")), c(file = 1, stderr = 1, stdout = 0), info = off)
}
Sys.setenv(LOG_STDERR = "false", LOG_STDERR_LEVEL = "DEBUG")           # an explicit channel level wins
expect_equal(counts(receive("DEBUG", "debug")), c(file = 1, stderr = 1, stdout = 0))
Sys.unsetenv("LOG_STDERR_LEVEL")
assign("min_log_level_value", log_levels[["WARN"]][[1]], envir = globalenv())   # LOG_LEVEL=WARN: never below it
expect_equal(channel_level("stderr"), log_levels[["WARN"]][[1]])
pin_log_settings()
for (on in c("true", "1", "yes", "", "garbage")) {
  Sys.setenv(LOG_STDERR = on)
  expect_equal(channel_level("stderr"), log_levels[["DEBUG"]][[1]], info = on)
}
Sys.unsetenv("LOG_STDERR")

# a file that cannot be written: the line is printed to stderr instead, once, whatever the stack
blocked <- tempfile("blocked-"); writeLines("x", blocked)
Sys.setenv(LOG_DIR = file.path(blocked, "logs"), LOG_STACK = "file")
ctx_reset("file_warned")
err <- character(0)
withCallingHandlers(log_write("INFO", "file channel is broken"),
                    message = function(m) { err <<- c(err, conditionMessage(m)); invokeRestart("muffleMessage") })
expect_equal(sum(grepl("file channel is broken", err, fixed = TRUE)), 1)
Sys.setenv(LOG_STACK = "file,stderr")                                  # with stderr in the stack: one copy, not two
err <- character(0)
withCallingHandlers(log_write("INFO", "both channels"),
                    message = function(m) { err <<- c(err, conditionMessage(m)); invokeRestart("muffleMessage") })
expect_equal(sum(grepl("both channels", err, fixed = TRUE)), 1)
Sys.unsetenv("LOG_STACK"); Sys.setenv(LOG_DIR = log_dir)

# loading the file prints the log level (an INFO line) on stderr, unless stderr is not in the stack
startup_messages <- function(stack = NULL) {
  seen <- character(0)
  if (!is.null(stack)) Sys.setenv(LOG_STACK = stack)
  withCallingHandlers(
    source(file.path(akpath, "R", "logging.R")),
    message = function(m) { seen <<- c(seen, conditionMessage(m)); invokeRestart("muffleMessage") }
  )
  pin_log_settings()   # the source above loaded .env again
  seen
}
# (skipped when the project's .env sets LOG_STACK or LOG_CHANNEL: loading the file applies it over the test's own)
env_file <- file.path(akpath, ".env")
env_sets_stack <- file.exists(env_file) && any(grepl("^\\s*LOG_(STACK|CHANNEL)\\s*=", readLines(env_file, warn = FALSE)))
if (!env_sets_stack) {
  expect_true(any(grepl("Current log level", startup_messages(), fixed = TRUE)))
  expect_equal(length(startup_messages("file")), 0)
  expect_true(any(grepl("Current log level", startup_messages("file,stderr"), fixed = TRUE)))
}

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
ctx_reset("pruned_on")
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
expect_true(grepl("log_stack=file,stderr file_level=DEBUG file_format=text stderr_level=DEBUG stderr_format=text", txt, fixed = TRUE))
expect_true(grepl("EMAIL_HOST=set", txt, fixed = TRUE)); expect_true(grepl("MAPBOX_TOKEN=set", txt, fixed = TRUE))
expect_false(grepl("smtp.internal.example", txt, fixed = TRUE)); expect_false(grepl("SECRET-TOKEN-VALUE", txt, fixed = TRUE))
Sys.unsetenv(c("EMAIL_HOST", "MAPBOX_TOKEN"))

unlink(log_dir, recursive = TRUE)
