library(jsonlite)

library(dotenv)
library(here)

env_file <- here(".env")   # resolves relative to project root
if (file.exists(env_file)) {
  load_dot_env(env_file)
}

log_levels <- c("TRACE" = 0, "DEBUG" = 1, "INFO" = 2, "SUCCESS" = 3, "WARN" = 4, "ERROR" = 5)

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