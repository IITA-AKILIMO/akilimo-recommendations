# logging.R
library(jsonlite)

# Load .env file if present
if (requireNamespace("dotenv", quietly = TRUE)) {
  try(dotenv::load_dot_env(file = ".env"), silent = TRUE)
}

log_levels <- c("DEBUG" = 1, "INFO" = 2, "SUCCESS" = 3, "WARN" = 4, "ERROR" = 5)

get_min_log_level <- function() {
  lvl <- Sys.getenv("LOG_LEVEL", unset = "INFO")
  toupper(lvl)
}

should_log <- function(level) {
  min_lvl <- get_min_log_level()
  log_levels[toupper(level)] >= log_levels[min_lvl]
}

get_log_file <- function() {
  logs_dir <- Sys.getenv("LOG_DIR", unset = file.path(getwd(), "logs"))
  if (!dir.exists(logs_dir)) dir.create(logs_dir, recursive = TRUE)

  prefix <- Sys.getenv("LOG_FILE_PREFIX", unset = "compute")
  file.path(logs_dir, paste0(prefix, "-", format(Sys.Date(), "%Y%m%d"), ".log"))
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

  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- paste0("[", timestamp, "] [", toupper(level), "] ", msg)

  cat(line, "\n", file = get_log_file(), append = append)
  message(line)
}