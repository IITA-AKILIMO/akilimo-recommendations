library(jsonlite)

library(dotenv)
library(here)

env_file <- here(".env")   # resolves relative to project root
if (file.exists(env_file)) {
  load_dot_env(env_file)
}

log_levels <- c("TRACE" = 0, "DEBUG" = 1, "INFO" = 2, "SUCCESS" = 3, "WARN" = 4, "ERROR" = 5)

# Whether log lines are also printed to stderr (the console, or the systemd journal). LOG_STDERR in .env: true by
# default; false, 0, no or off keep the log in the file only. The log file is always written. Read on every call.
log_to_stderr <- function() {
  !(tolower(trimws(Sys.getenv("LOG_STDERR", unset = "true"))) %in% c("false", "0", "no", "off"))
}

# Cache the minimum log level numeric value once.
# Falls back to INFO for empty or unrecognised LOG_LEVEL values.
min_log_level_value <- {
  lvl <- Sys.getenv("LOG_LEVEL", unset = "INFO")
  val <- log_levels[toupper(lvl)]
  if (is.na(val)) {
    if (log_to_stderr()) message("Unrecognised LOG_LEVEL '", lvl, "'; defaulting to INFO")
    val <- log_levels["INFO"]
  } else {
    if (log_to_stderr()) message("Current log level: ", lvl)
  }
  val
}

should_log <- function(level) {
  lvl_value <- log_levels[toupper(level)]
  !is.na(lvl_value) && lvl_value >= min_log_level_value
}

get_log_file <- function() {
  logs_dir <- Sys.getenv("LOG_DIR", unset = file.path(getwd(), "logs"))
  if (!dir.exists(logs_dir)) dir.create(logs_dir, recursive = TRUE)

  prefix <- Sys.getenv("LOG_FILE_PREFIX", unset = "compute")
  file.path(logs_dir, paste0(prefix, "-", format(Sys.Date(), "%Y%m%d"), ".log"))
}

# One log line longer than this is cut (LOG_MAX_CHARS in .env; 0 turns the cap off). Keeps a huge value, such as
# a base64 PDF, from filling the log file or crashing message().
log_max_chars <- function() {
  n <- suppressWarnings(as.numeric(Sys.getenv("LOG_MAX_CHARS", unset = "20000")))
  if (is.na(n) || n < 0) 20000 else n
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

log_write <- function(level = "INFO", ..., sep = " ", append = TRUE) {
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

  max_chars <- log_max_chars()
  if (max_chars > 0 && nchar(msg) > max_chars)
    msg <- paste0(substr(msg, 1, max_chars), "... [truncated, ", nchar(msg) - max_chars, " more characters]")

  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- paste0("[", timestamp, "] [", toupper(level), "] ", msg)

  cat(line, "\n", file = get_log_file(), append = append)
  if (log_to_stderr()) message(line)
}