
# Look up a translation string by key and country.
# Falls back to "NG" (English) when the key is absent or blank for the given country.
# Stops with an error if the key is missing entirely.
# ---------------------------------------------------------------------------
# Named constants — replace magic numbers throughout the codebase
# ---------------------------------------------------------------------------

# Maize cobs-to-grain conversion: approximately 7.64 fresh cobs per 1 kg of
# shelled grain.  Used in dispatch_recommendations() (AkilimoMain.R) to
# convert a per-cob maize price to a per-kg-grain price, and in
# pdf_builders.R to back-convert extra grain to cobs for display.
MAIZE_COBS_PER_KG_GRAIN <- 7.64

# SP harvest-age window (days on field) modelled by the WLY NetCDF rasters.
# seq(235, 455, 7) produces 32 values (235, 242, ..., 452).
# Corresponding week numbers: 34:65 (32 values; merged by position after daysOnField join).
SP_HAW_DAYS_MIN   <- 235L   # minimum harvest age in days
SP_HAW_DAYS_MAX   <- 455L   # maximum harvest age in days (seq upper bound, last step = 452)
SP_HAW_DAYS_STEP  <- 7L     # weekly step
SP_HAW_WEEKS_MIN  <- 34L    # minimum harvest age in weeks (= first row week number)
SP_HAW_WEEKS_MAX  <- 65L    # maximum harvest age in weeks (= last row week number)

# API version string — single source of truth used by run_akilimo() and the
# /health endpoint.  Update here only; see also MNT-3 (full semantic versioning).
AKI_VERSION <- "20251228"

# SP yield-scaling formula bounds (tonnes/ha fresh weight, per-ha basis).
# FCY values outside [SP_FCY_MIN, SP_FCY_MAX] are outside the interpolation
# range; the formula extrapolates silently — see NEW-SP-2 guard in process-SP.R.
SP_FCY_MIN <- 1.5    # lower FCY bound (t/ha FW)
SP_FCY_MAX <- 13.5   # upper FCY bound (t/ha FW)
SP_FCY_DIV <- 2.5    # divisor used in the linear scaling expression


# Look up a translation string by key and country.
# Falls back to "NG" (English) when the key is absent or blank for the given country.
# Stops with an error if the key is missing entirely.
tr <- function(key, lang, ...) {
    tbl <- get_data("TRNS")
    known_langs <- setdiff(names(tbl), "key")   # e.g. c("en", "sw")
    if (!lang %in% known_langs) {
        warning(sprintf("tr(): unknown lang '%s' — falling back to 'en'", lang))
        lang <- "en"
    }
    row <- tbl[tbl$key == key, ]
    if (nrow(row) == 0) stop(sprintf("Missing translation key '%s'", key))
    val <- if (!is.null(row[[lang]])) row[[lang]] else character(0)
    if (length(val) == 0 || is.na(val) || !nzchar(trimws(val))) {
        val <- row[["en"]]
    }
    if (length(val) == 0 || is.na(val)) stop(sprintf("Missing translation key '%s'", key))
    args <- list(...)
    for (nm in names(args)) {
        val <- gsub(paste0("\\{", nm, "\\}"), args[[nm]], val, fixed = FALSE)
    }
    val
}


# Convert x to numeric, stopping with a clear message if coercion fails.
# Use in parse_request() for all fields that must be numeric. When the field is
# absent from the JSON body, from_json() already returns the default_value, so
# safe_numeric() only fires when the client sends an explicit non-numeric string.
safe_numeric <- function(x, field_name) {
  val <- suppressWarnings(as.numeric(x))
  if (is.na(val))
    stop(sprintf("Field '%s' must be numeric, got: %s", field_name, x))
  val
}


# Returns the minimum net-revenue-to-cost multiplier for a given risk attitude.
# A fertilizer application is recommended only when NR >= TC * min_nr_multiplier(riskAtt).
#   riskAtt 0 — risk averse:    threshold = 1.8× cost (highest bar)
#   riskAtt 1 — moderate:       threshold = 1.0× cost
#   other    — risk tolerant:   threshold = 0.2× cost (lowest bar)
min_nr_multiplier <- function(riskAtt) {
    switch(as.character(riskAtt), "0" = 1.8, "1" = 1.0, 0.2)
}


get_currency <- function(country) {
	m <- matrix(c("NG", "NGN", "RW", "RWF", "GH", "GHS", "BI", "BIF", "TZ", "TZS"), ncol=2, byrow=TRUE)
	i <- match(country, m[,1])
	m[i,2]
}


#SHORT DEF:   Function to convert root DM yield into root fresh matter yield (RFY)
#RETURNS:     RFY: root fresh yield in the same units as root DM yield input
#DESCRIPTION: Function to predict root FM yield based on date of harvest and country, using data from gravimetric starch measurements conducted across ACAI trials.
#INPUT:       HD: harvest date (Date format)
#             RDY: root dry matter yield (user's units)
#             country = c("NG", "TZ")

getRFY <- function(HD, RDY, country) {

  d <- as.numeric(strftime(HD, format = "%j"))
  #data.frame with day of the year (dayNr = [1..366]) and %DM (DMCont = [0..100], by country)
  fd <- get_data("dry_matter")
  DC <- merge(data.frame(dayNr = d), fd[fd$country == country,], sort = FALSE)$DMCont
  RFY <- RDY / DC * 100
  return(RFY)

}


# DEFERRED (technical debt): getRDY has no callers in the active codebase.
# Kept as the inverse of getRFY for a future use case. Do not call until verified.
#
# LOG-16 fix applied: HD is converted to integer day-of-year via strftime(), matching
# the approach used in getRFY(). The original guard `if (HD > 366) HD <- HD - 366`
# was incorrect when HD is a Date object — the subtraction returned a shifted Date,
# not a valid day-of-year integer.
#
#SHORT DEF:   Function to convert root FM yield into root dry matter yield (RDY)
#RETURNS:     RDY: root dry yield in the same units as root FM yield input
#INPUT:       HD: harvest date (Date format)
#             RFY: root fresh matter yield (user's units)
#             country = c("NG", "TZ")

getRDY <- function(HD, RFY, country) {
  d <- as.numeric(strftime(HD, format = "%j"))
  fd <- get_data("dry_matter")
  DC <- merge(data.frame(dayNr = d), fd[fd$country == country,], sort = FALSE)$DMCont
  RDY <- (RFY * DC) / 100
  return(RDY)
}


# Scan all .R files in srcdir for translation key literals and verify every key
# exists in the TRNS table. Called once at server startup so that a misspelled
# or deleted key is caught immediately rather than on the first live request.
# Comment lines (starting with #) are excluded to avoid matching example strings
# in docstrings. Logs INFO when all keys are present; logs ERROR for any missing.
# Never throws — startup must succeed even if data files are not yet available.
check_translation_keys <- function(srcdir) {
    tbl <- tryCatch(get_data("TRNS"), error = function(e) NULL)
    if (is.null(tbl)) {
        log_write("WARN", "check_translation_keys: TRNS table unavailable — skipping key check")
        return(invisible(NULL))
    }
    known <- tbl$key

    files <- list.files(srcdir, pattern = "\\.R$", full.names = TRUE, recursive = FALSE)
    keys_used <- character(0)
    for (f in files) {
        txt <- readLines(f, warn = FALSE)
        # Exclude comment lines so example strings in docstrings are not scanned.
        txt_code <- txt[!grepl("^\\s*#", txt)]
        # \b word boundary prevents matching tr(" inside str(", attr(", etc.
        # perl = TRUE for reliable \b and \( interpretation.
        hits <- regmatches(txt_code, gregexpr('\\btr\\("([a-zA-Z0-9_]+)"', txt_code, perl = TRUE))
        for (block in hits) {
            if (length(block) == 0) next
            # Strip tr(" prefix first, THEN remove the trailing " and anything after.
            keys_used <- c(keys_used, sub('".*$', "", sub('^tr\\("', "", block)))
        }
    }
    keys_used <- unique(keys_used)

    missing <- setdiff(keys_used, known)
    if (length(missing) > 0) {
        log_write("ERROR", sprintf(
            "check_translation_keys: %d key(s) used in source but absent from translations table: %s",
            length(missing), paste(sort(missing), collapse = ", ")))
    } else {
        log_write("INFO", sprintf(
            "check_translation_keys: all %d translation key(s) verified OK", length(keys_used)))
    }
    invisible(NULL)
}


