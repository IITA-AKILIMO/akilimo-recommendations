
# ---------------------------------------------------------------------------
# Per-request temp directory
# Each request uses an isolated subdirectory of temp/ so that concurrent
# requests cannot overwrite each other's files.
# ---------------------------------------------------------------------------
.akilimo_req <- new.env(parent = emptyenv())
.akilimo_req$temp_dir <- "temp"

#' Return the current request's temp directory (relative to project root).
temp_dir <- function() .akilimo_req$temp_dir

#' Set the current request's temp directory. Called once per request.
set_temp_dir <- function(path) .akilimo_req$temp_dir <- path

#' Build an absolute-ish path inside the current request's temp directory.
tp <- function(filename) file.path(temp_dir(), filename)

# Sanitise a user-supplied value before embedding it in a file path.
# Keeps only digits, letters, hyphens and plus signs (covers phone numbers
# like +234789123456) and strips any path traversal characters.
safe_filename_part <- function(x) {
    gsub("[^A-Za-z0-9+\\-]", "", as.character(x))
}

# ---------------------------------------------------------------------------
# Fertilizer display: bag colour and label, keyed by internal type name
# ---------------------------------------------------------------------------
FERT_COLOUR <- c(
    Urea          = "green",
    NPK15_15_15   = "blue",
    NPK20_10_10   = "yellow",
    NPK17_17_17   = "purple",
    NPK20_12_16   = "royal",
    NPK152020     = "orange",
    FOMI_TOTAHAZA = "red",
    FOMI_IMBURA   = "redMG",
    FOMI_BAGARA   = "grey"
)

FERT_LABEL <- c(
    Urea          = "Urea",
    NPK15_15_15   = "NPK15:15:15",
    NPK20_10_10   = "NPK20:10:10",
    NPK17_17_17   = "NPK17:17:17",
    NPK20_12_16   = "NPK20:12:16+2Mg",
    NPK152020     = "NPK15:20:20",
    FOMI_TOTAHAZA = "FOMI-TOTAHAZA",
    FOMI_IMBURA   = "FOMI-IMBURA",
    FOMI_BAGARA   = "FOMI-BAGARA"
)

# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------

# Round raw bag count to nearest whole or half bag.
round_bags <- function(raw, half_lo = 0.25, half_hi = 0.75) {
    full <- trunc(raw)
    frac <- raw - floor(raw)
    half <- ifelse(frac >= half_lo & frac <= half_hi, 0.5,
                   ifelse(frac < half_lo, 0, 1))
    full + half
}

# Merge fertilizer list with recommendation rates, compute cost and bags.
calc_fertilizer_recom <- function(fertilizers, rr, half_lo = 0.25, half_hi = 0.75) {
    fr <- fertilizers[fertilizers$type %in% rr$fertilizer_rates$type, ]
    if (nrow(fr) == 0) return(fr)
    fr <- merge(fr, rr$fertilizer_rates, by = "type")
    fr$rate      <- round(fr$rate, digits = 0)
    fr$cost      <- fr$rate * fr$price
    fr$bags      <- round_bags(fr$rate / fr$bagWeight, half_lo, half_hi)
    fr
}

