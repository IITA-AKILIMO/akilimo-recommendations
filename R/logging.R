library(jsonlite)

library(dotenv)
library(here)

env_file <- here(".env")   # resolves relative to project root
if (file.exists(env_file)) {
  load_dot_env(env_file)
}

log_levels <- c("TRACE" = 0, "DEBUG" = 1, "INFO" = 2, "SUCCESS" = 3, "WARN" = 4, "ERROR" = 5)

# Settings, all optional and read from .env (see .env.example):
#   LOG_LEVEL       minimum level written (read once, at start-up)
#   LOG_STDERR      true (default) prints every line to stderr; false, 0, no or off prints INFO and above only
#   LOG_FORMAT      text (default) or json (one JSON object per line, for a log collector)
#   LOG_MAX_CHARS   a single line longer than this is cut (default 20000, 0 = no limit)
#   LOG_KEEP_DAYS   delete daily log files older than this many days (default 0 = keep them all)
#   LOG_SLOW_MS     a request slower than this is logged as a WARN (default 30000, 0 = never)
#   LOG_DIR, LOG_FILE_PREFIX   where the files go and how they are named

# Per-process state: the request being served (its id goes on every line) and two once-only flags
.log_ctx <- new.env(parent = emptyenv())

.first <- function(x) if (is.null(x) || length(x) == 0) NA else x[[1]]

# Whether a log line of this level is also printed to stderr (the console, or the systemd journal). The log file
# always gets every line the log level allows. LOG_STDERR in .env: true (the default) prints them all; false, 0, no
# or off is the quiet setting that prints INFO and above (INFO, SUCCESS, WARN, ERROR) and leaves TRACE and DEBUG,
# which are the long ones, to the file. Read on every call.
log_to_stderr <- function(level = "INFO") {
  quiet <- tolower(trimws(Sys.getenv("LOG_STDERR", unset = "true"))) %in% c("false", "0", "no", "off")
  !quiet || isTRUE(log_levels[toupper(level)] >= log_levels[["INFO"]])
}

# Cache the minimum log level numeric value once.
# Falls back to INFO for empty or unrecognised LOG_LEVEL values.
min_log_level_value <- {
  lvl <- Sys.getenv("LOG_LEVEL", unset = "INFO")
  val <- log_levels[toupper(lvl)]
  if (is.na(val)) {
    message("Unrecognised LOG_LEVEL '", lvl, "'; defaulting to INFO")
    val <- log_levels["INFO"]
  } else {
    message("Current log level: ", lvl)
  }
  val
}

should_log <- function(level) {
  lvl_value <- log_levels[toupper(level)]
  !is.na(lvl_value) && lvl_value >= min_log_level_value
}

# One log line longer than this is cut (LOG_MAX_CHARS in .env; 0 turns the cap off). Keeps a huge value, such as
# a base64 PDF, from filling the log file or crashing message().
log_max_chars <- function() {
  n <- suppressWarnings(as.numeric(Sys.getenv("LOG_MAX_CHARS", unset = "20000")))
  if (is.na(n) || n < 0) 20000 else n
}

# "text" (default) or "json"
log_format <- function() {
  if (tolower(trimws(Sys.getenv("LOG_FORMAT", unset = "text"))) == "json") "json" else "text"
}

# Days to keep daily log files; 0 (the default) keeps them all
log_keep_days <- function() {
  n <- suppressWarnings(as.numeric(Sys.getenv("LOG_KEEP_DAYS", unset = "0")))
  if (is.na(n) || n < 0) 0 else n
}

# A request slower than this many milliseconds is logged as a WARN; 0 turns it off
log_slow_ms <- function() {
  n <- suppressWarnings(as.numeric(Sys.getenv("LOG_SLOW_MS", unset = "30000")))
  if (is.na(n) || n < 0) 30000 else n
}

# Delete the daily log files ({prefix}-YYYYMMDD.log) older than `keep_days`; returns how many were removed.
# Only files that match that name are touched.
prune_old_logs <- function(logs_dir = Sys.getenv("LOG_DIR", unset = file.path(getwd(), "logs")),
                           prefix = Sys.getenv("LOG_FILE_PREFIX", unset = "compute"),
                           keep_days = log_keep_days(), today = Sys.Date()) {
  if (keep_days <= 0 || !dir.exists(logs_dir)) return(0L)
  files <- list.files(logs_dir, full.names = TRUE)
  files <- files[startsWith(basename(files), paste0(prefix, "-")) & grepl("-[0-9]{8}\\.log$", files)]
  day <- as.Date(sub(".*-([0-9]{8})\\.log$", "\\1", files), format = "%Y%m%d")
  old <- files[!is.na(day) & as.numeric(today - day) > keep_days]
  sum(file.remove(old))
}

get_log_file <- function() {
  logs_dir <- Sys.getenv("LOG_DIR", unset = file.path(getwd(), "logs"))
  if (!dir.exists(logs_dir)) suppressWarnings(dir.create(logs_dir, recursive = TRUE))

  prefix <- Sys.getenv("LOG_FILE_PREFIX", unset = "compute")
  today <- format(Sys.Date(), "%Y%m%d")
  if (!identical(.log_ctx$pruned_on, today)) {   # once a day, and never a reason to fail
    .log_ctx$pruned_on <- today
    try(prune_old_logs(logs_dir, prefix), silent = TRUE)
  }
  file.path(logs_dir, paste0(prefix, "-", today, ".log"))
}

# ---------------------------------------------------------------------------
# Request context: one id on every line of a request, so its lines can be found among the others, and matched
# with the AKILIMO API's own log through the requestId it sends
# ---------------------------------------------------------------------------
set_log_context <- function(request_id = NULL) {
  .log_ctx$request_id <- request_id
  .log_ctx$last_warn <- NULL
}
clear_log_context <- function() {
  .log_ctx$request_id <- NULL
  .log_ctx$last_warn <- NULL
}
get_log_request_id <- function() .log_ctx$request_id

# The request's id and country from the raw JSON body: requestId when it is a UUID (what the AKILIMO API sends),
# otherwise a short id made here, so every request can be followed. Never fails.
request_meta <- function(post_body) {
  b <- tryCatch(jsonlite::fromJSON(post_body), error = function(e) NULL)
  rid <- if (is.list(b)) as.character(.first(b[["requestId"]])) else NA_character_
  if (is.na(rid) || !grepl("^[0-9a-fA-F-]{36}$", rid))
    rid <- sprintf("%08x", as.integer((as.numeric(Sys.time()) * 1000) %% 2147483647))
  country <- if (is.list(b)) toupper(as.character(.first(b[["country"]]))) else NA_character_
  list(request_id = rid, country = if (is.na(country)) "?" else country)
}

# ---------------------------------------------------------------------------
# What may go in the log
# ---------------------------------------------------------------------------

# The request body with the farmer's personal details replaced by "<redacted>", for logging: every field that
# starts with "user" (userName, userEmail, userField, userPhoneNr, ...) or has "phone" in its name. The `email`
# yes/no flag stays. A body that is not valid JSON is not logged at all.
redact_request <- function(body) {
  b <- if (is.character(body)) tryCatch(jsonlite::fromJSON(body), error = function(e) NULL) else body
  if (!is.list(b)) return("<request body is not valid JSON and is not logged>")
  personal <- grepl("^user|phone", names(b), ignore.case = TRUE) & names(b) != "email"
  for (f in names(b)[personal]) b[[f]] <- "<redacted>"
  b
}

# The response with the content of each attached report replaced by a short note (name and size stay), for logging.
# The PDFs returned with returnPdf are base64 and can be megabytes, which has crashed the server when logged.
redact_reports <- function(result) {
  if (is.list(result) && is.list(result$reports)) {
    result$reports <- lapply(result$reports, function(r) {
      if (!is.null(r$content)) r$content <- sprintf("<omitted: %d base64 chars>", nchar(as.character(r$content)))
      r
    })
  }
  result
}

# A few lines instead of the whole result (all its rows) for the DEBUG log; the whole of it goes to TRACE.
summarize_result <- function(result) {
  if (!is.list(result)) return(result)
  d <- result$data
  out <- list(status = as.character(.first(result$status)),
              rec_type = as.character(.first(d$rec_type)))
  if (!is.null(d$recommendation)) out$recommendation <- substr(paste(d$recommendation, collapse = " "), 1, 500)
  if (!is.null(d$message)) out$message <- as.character(.first(d$message))
  if (!is.null(d$data)) out$data_rows <- NROW(d$data)
  if (!is.null(d$fertilizer_rates)) out$fertilizer_rates <- NROW(d$fertilizer_rates)
  if (is.list(result$reports)) {
    out$reports <- lapply(result$reports, function(r)
      list(name = as.character(.first(r$name)), base64_chars = nchar(as.character(.first(r$content)))))
  }
  out
}

# ---------------------------------------------------------------------------
# Writing
# ---------------------------------------------------------------------------

format_log_line <- function(level, msg, time = Sys.time()) {
  rid <- get_log_request_id()
  if (log_format() == "json") {
    fields <- list(time = format(time, "%Y-%m-%dT%H:%M:%S%z"), level = toupper(level))
    if (!is.null(rid)) fields$request_id <- rid
    fields$message <- msg
    return(as.character(toJSON(fields, auto_unbox = TRUE)))
  }
  paste0("[", format(time, "%Y-%m-%d %H:%M:%S"), "] [", toupper(level), "] ",
         if (!is.null(rid)) paste0("[req=", rid, "] "), msg)
}

.log_write <- function(level, ..., sep = " ", append = TRUE) {
  if (!should_log(level)) return(invisible(NULL))

  args <- list(...)
  args <- args[!sapply(args, is.null)]
  if (length(args) == 0) return(invisible(NULL))

  stringify <- function(x) {
    if (is.atomic(x)) {
      paste(x, collapse = sep)
    } else if (is.data.frame(x)) {
      paste(capture.output(str(x)), collapse = "\n")
    } else if (is.list(x)) {
      toJSON(x, pretty = TRUE, auto_unbox = TRUE)
    } else {
      as.character(x)
    }
  }

  msg_parts <- vapply(args, stringify, FUN.VALUE = character(1))
  msg <- paste(msg_parts, collapse = sep)
  # Remembered so that with_logged_warnings() does not log the same problem again when the code also warns about it
  if (toupper(level) == "WARN") .log_ctx$last_warn <- msg

  max_chars <- log_max_chars()
  if (max_chars > 0 && nchar(msg) > max_chars)
    msg <- paste0(substr(msg, 1, max_chars), "... [truncated, ", nchar(msg) - max_chars, " more characters]")

  line <- format_log_line(level, msg)

  # An unwritable log directory must not fail a request: say so once, and print the line to stderr instead
  written <- tryCatch({
    suppressWarnings(cat(line, "\n", file = get_log_file(), append = append))
    TRUE
  }, error = function(e) {
    if (!isTRUE(.log_ctx$file_warned)) {
      .log_ctx$file_warned <- TRUE
      message("[logging] cannot write the log file, printing to stderr instead: ", conditionMessage(e))
    }
    FALSE
  })
  if (!written || log_to_stderr(level)) message(line)
  invisible(NULL)
}

# Logging never breaks what it is logging: any error inside it (an argument that fails to evaluate, an unwritable
# file) is reported on stderr and swallowed.
log_write <- function(level = "INFO", ..., sep = " ", append = TRUE) {
  tryCatch(
    .log_write(level, ..., sep = sep, append = append),
    error = function(e) {
      try(message("[logging] failed: ", conditionMessage(e)), silent = TRUE)
      invisible(NULL)
    }
  )
}

# Run `expr`, and log each R warning it raises as a WARN line (with the request id) instead of leaving it on
# stderr without a time. Errors pass through untouched.
with_logged_warnings <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    msg <- conditionMessage(w)
    # Code that logs a problem and then also warns about it (the email senders do) is logged once
    if (!identical(msg, .log_ctx$last_warn)) {
      fn <- tryCatch(paste(deparse(conditionCall(w)[[1]]), collapse = ""), error = function(e) "")
      log_write("WARN", msg, if (nzchar(fn)) paste0("(in ", substr(fn, 1, 60), ")"))
    }
    invokeRestart("muffleWarning")
  })
}

# One line when a request is finished: how it ended, what it was, and how long it took. A request slower than
# LOG_SLOW_MS is logged at WARN. `status` overrides the one in the result (the 500 path has no result).
log_request_done <- function(country, result = NULL, started = Sys.time(), status = NULL, reason = NULL) {
  ms <- as.integer(round(as.numeric(difftime(Sys.time(), started, units = "secs")) * 1000))
  status <- if (!is.null(status)) status else sub(" .*", "", as.character(.first(result$status)))
  type <- as.character(.first(result$data$rec_type))
  if (is.null(reason) && !identical(status, "success")) reason <- as.character(.first(result$data$message))

  parts <- c(sprintf("status=%s", status), sprintf("type=%s", type), sprintf("country=%s", country),
             sprintf("duration_ms=%d", ms))
  if (is.list(result) && should_log("INFO"))
    parts <- c(parts, sprintf("response_kb=%.1f", nchar(toJSON(result, auto_unbox = TRUE, null = "null", na = "null")) / 1024))
  if (!is.null(reason) && !is.na(reason)) parts <- c(parts, paste0("reason=", reason))

  slow_ms <- log_slow_ms()
  slow <- slow_ms > 0 && ms > slow_ms
  if (slow) parts <- c(parts, sprintf("SLOW(>%dms)", slow_ms))
  log_write(if (slow) "WARN" else "INFO", "REQUEST DONE", paste(parts, collapse = " "))
}

# What the server runs with, once at start-up: the settings only, never a key or a password
log_startup_summary <- function(version = NA, port = NA) {
  set <- function(name) if (nzchar(Sys.getenv(name))) "set" else "not set"
  log_write("INFO", "STARTUP", paste(
    paste0("version=", version),
    paste0("port=", port),
    paste0("log_level=", Sys.getenv("LOG_LEVEL", unset = "INFO")),
    paste0("log_stderr=", Sys.getenv("LOG_STDERR", unset = "true")),
    paste0("log_format=", log_format()),
    paste0("log_max_chars=", log_max_chars()),
    paste0("log_keep_days=", log_keep_days()),
    paste0("log_slow_ms=", log_slow_ms()),
    paste0("log_dir=", Sys.getenv("LOG_DIR", unset = file.path(getwd(), "logs"))),
    paste0("AKILIMO_API_URL=", set("AKILIMO_API_URL")),
    paste0("EMAIL_HOST=", set("EMAIL_HOST")),
    paste0("MAP_API_URL=", set("MAP_API_URL")),
    paste0("MAPBOX_TOKEN=", set("MAPBOX_TOKEN"))))
}
