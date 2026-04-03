
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


#' Scan all .R files in srcdir for tr("key") literal calls and verify every key
#' exists in the TRNS translation table. Called once at server startup so that a
#' misspelled or deleted key is caught immediately rather than on the first live
#' request that reaches the affected code path.
#'
#' Logs INFO when all keys are present; logs ERROR for each missing key.
#' Never throws — startup must succeed even if data files are not yet available.
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
        hits <- regmatches(txt, gregexpr('tr\\("([a-zA-Z0-9_]+)"', txt))
        for (block in hits) {
            if (length(block) == 0) next
            # Strip the tr(" prefix and everything from the closing quote onward
            keys_used <- c(keys_used, sub('^tr\\("', "", sub('".*$', "", block)))
        }
    }
    keys_used <- unique(keys_used)

    missing <- setdiff(keys_used, known)
    if (length(missing) > 0) {
        log_write("ERROR", sprintf(
            "check_translation_keys: %d key(s) used in source but absent from translations.csv: %s",
            length(missing), paste(sort(missing), collapse = ", ")))
    } else {
        log_write("INFO", sprintf(
            "check_translation_keys: all %d translation key(s) verified OK", length(keys_used)))
    }
    invisible(NULL)
}


# DEFERRED (technical debt): getWMrecommendations is not wired into any request
# path yet. Kept for a planned future weed-management feature. Do not remove or
# call until the feature is scoped and implemented.
#
#SHORT DEF:   Function to obtain recommendations on land clearing (step 2 of 6 steps).
#RETURNS:     dataframe with recommendations on whether to slash and/or to spray.
#DESCRIPTION: Function to obtain recommendations on land clearing (slashing and spraying) based on decision tree in the paper-based tool
#INPUT:       See Cassava Crop Manager function for details

getWMrecommendations <- function(fallowType = c(NA, "bush", "broad_leaves", "grass", "none"),
                                 fallowHeight = c(NA, 100, 150, 200),
                                 fallowGreen = c(NA, TRUE, FALSE),
                                 problemWeeds = c(NA, TRUE, FALSE)) {
  slash <- ifelse(fallowType == "bush" & fallowHeight > 100 |
                    fallowType == "broad_leaves" & fallowGreen == FALSE |
                    fallowType == "broad_leaves" &
                      fallowGreen == TRUE &
                      fallowHeight > 150 |
                    fallowType == "grass" & fallowHeight > 150,
                  TRUE, FALSE)

  spray <- ifelse(fallowType == "bush" & fallowHeight <= 100 |
                    fallowType == "broad_leaves" &
                      fallowGreen == TRUE &
                      fallowHeight <= 150 |
                    fallowType == "grass" |
                    fallowType == "none" & problemWeeds == TRUE,
                  TRUE, FALSE)

  ds <- data.frame(operation = c("slash", "spray"), rec = c(slash, spray))

  return(ds)
}


