# html_helpers.R — shared HTML fragment generators for WeasyPrint PDF reports
#
# All functions return character(1) — an HTML string fragment.
# Full documents are assembled by the build_*_pdf() functions in pdf_builders.R.
#
# Dependencies: base64enc (for img_base64), httr (for static map fetch).
# FERT_COLOUR and FERT_LABEL are defined in markdown.R (sourced first).

# ── PDF label lookup ──────────────────────────────────────────────────────────
# Section headings and UI labels are not in translations.csv (which only holds
# recommendation text). We maintain them here with en/sw pairs.

.PDF_LABELS <- list(
  # ── Personal info ────────────────────────────────────────────────────────
  what_you_told_us    = c(en = "What you told us",                  sw = "Ulichotuambia"),
  your_name           = c(en = "Your name",                         sw = "Jina lako"),
  phone_number        = c(en = "Phone",                             sw = "Namba ya simu"),
  email_address       = c(en = "Email",                             sw = "Barua pepe"),
  field_name          = c(en = "Your field",                        sw = "Shamba lako"),
  field_area          = c(en = "Field area",                        sw = "Ukubwa wa shamba"),
  planting_date       = c(en = "Planting date",                     sw = "Tarehe ya kupanda"),
  harvest_date        = c(en = "Harvest date",                      sw = "Tarehe ya kuvuna"),
  current_yield       = c(en = "Current yield",                     sw = "Mavuno yako ya sasa"),
  current_yield_unit  = c(en = "tonnes per ha",                     sw = "tani kwa hekta"),
  area_unit_ha        = c(en = "ha",                                sw = "hekta"),
  area_unit_acre      = c(en = "acre",                              sw = "ekari"),

  # ── Location / map ───────────────────────────────────────────────────────
  your_location       = c(en = "Your location",                     sw = "Eneo lilipo shamba lako"),

  # ── Expected gain ────────────────────────────────────────────────────────
  expected_gain       = c(en = "Expected gain in total production", sw = "Matarajio ya kupata katika mavuno ya jumla"),
  # sprintf args: (bags_total, bags_total100)
  expected_gain_fmt   = c(en = "%s tonnes of cassava roots: ~ <strong>%s bags of 100 kg roots</strong>",
                           sw = "Tani <strong>%s</strong> ya mihogo: ~ Mifuko <strong>%s</strong> ya kilo mia moja ya mihogo"),

  # ── Fertilizer prices ────────────────────────────────────────────────────
  fertilizer_prices   = c(en = "Fertilizer prices",                 sw = "Bei ya mbolea"),
  bag_unit            = c(en = "%skg bag",                          sw = "mfuko wa kilo %s"),
  per_bag             = c(en = "per",                               sw = "kwa"),
  cassava_price       = c(en = "Cassava sold",                      sw = "Bei ya Muhogo"),
  per_tonne           = c(en = "per tonne of",                      sw = "kwa kila tani ya"),
  per_weight          = c(en = "per %s kg of",                      sw = "kwa kilo %s ya"),
  max_investment      = c(en = "Maximal investment",                sw = "Kiwango cha juu cha kuwekeza"),
  for_area            = c(en = "for",                               sw = "kwa"),
  no_fert_rec         = c(en = "No fertilizer recommendation for this location.",
                           sw = "Hakuna mapendekezo ya mbolea kwa eneo hili."),

  # ── Fertilizer recommendation rows ───────────────────────────────────────
  # sprintf args: (label, kgs, area_fmt, bags, bw, currency, cost_f)
  fert_row_fmt        = c(en = "%s: %s kg for %sha (~%s bags of %skg) = %s %s",
                           sw = "%s: kilo %s kwa %sha (~mifuko %s ya kilo %s) = %s %s"),

  # ── Cost-benefit ─────────────────────────────────────────────────────────
  cost_benefit        = c(en = "Cost benefit analysis",             sw = "Mchanganuo wa gharama na faida"),
  total_cost          = c(en = "Total cost",                        sw = "Jumla ya gharama"),
  total_revenue       = c(en = "Total calculated revenue",          sw = "Jumla ya mapato yaliyokokotolewa"),
  net_revenue         = c(en = "Your expected net revenue",         sw = "Jumla ya mapato yaliyotarajiwa"),

  # ── Recommendation ───────────────────────────────────────────────────────
  rec_generated_on    = c(en = "Recommendation generated on",       sw = "Mapendekezo yametolewa tarehe"),

  # ── Page titles ──────────────────────────────────────────────────────────
  fr_title            = c(en = "Tailored Fertilizer Recommendation",
                           sw = "Mapendekezo ya Mbolea"),

  # ── IC — Intercropping ────────────────────────────────────────────────────
  ic_title            = c(en = "Cassava-Maize Intercropping Recommendation",
                           sw = "Mapendekezo ya Kupanda Muhogo na Mahindi"),
  cis_title           = c(en = "Cassava-Sweet Potato Intercropping Recommendation",
                           sw = "Mapendekezo ya Kupanda Muhogo na Viazi Vitamu"),
  extra_production    = c(en = "Expected extra production",
                           sw = "Uzalishaji wa ziada unaotarajiwa"),
  maize_price         = c(en = "Maize sold",
                           sw = "Bei ya Mahindi"),
  sp_price            = c(en = "Sweet potato sold",
                           sw = "Bei ya Viazi Vitamu"),
  high_density_rec    = c(en = "Plant at high density: 1 m between rows, 25 cm within rows (40,000 plants/ha).",
                           sw = "Panda kwa msongamano mkubwa: mita 1 kati ya safu, cm 25 ndani ya safu (mimea 40,000/hekta)."),
  low_density_rec     = c(en = "Plant at low density: 1 m between rows, 50 cm within rows (20,000 plants/ha).",
                           sw = "Panda kwa msongamano mdogo: mita 1 kati ya safu, cm 50 ndani ya safu (mimea 20,000/hekta)."),
  per_fresh_cob       = c(en = "per fresh cob",
                           sw = "kwa bibo moja"),
  per_kg_grain        = c(en = "per %s kg of grain",
                           sw = "kwa kilo %s za nafaka"),
  extra_maize_grain_fmt = c(en = "%s kg extra maize grain",
                             sw = "kilo %s za nafaka ya mahindi zaidi"),
  extra_maize_cobs_fmt  = c(en = "%s extra maize cobs",
                             sw = "mabibo %s ya mahindi zaidi"),
  cis_rec_ic          = c(en = "Intercropping with sweet potato is recommended.",
                           sw = "Kilimo mseto na viazi vitamu kinapendekezwa."),
  cis_rec_ic_no_fert  = c(en = "Intercropping is recommended but fertilizer is not profitable.",
                           sw = "Kilimo mseto kinapendekezwa lakini mbolea haileti faida ya kutosha."),
  cis_no_ic           = c(en = "Cassava monocrop is more profitable for this location.",
                           sw = "Kupanda muhogo peke yake ni yenye faida zaidi kwa eneo hili."),

  # ── PP — Post-Planting / Tillage ────────────────────────────────────────
  pp_title            = c(en = "Cassava Tillage Advice",
                           sw = "Ushauri wa Kulima Muhogo"),
  current_practice    = c(en = "Your current practice",
                           sw = "Mazoea yako ya sasa"),
  lmo_table_heading   = c(en = "Cost of land management operations",
                           sw = "Gharama za usimamizi wa ardhi"),
  ploughing_label     = c(en = "Ploughing",
                           sw = "Kulima"),
  ridging_label       = c(en = "Ridging",
                           sw = "Kutengeneza matuta"),

  # ── PP — chart labels and LMO table ────────────────────────────────────
  chart_net_value     = c(en = "Net value: ",              sw = "Thamani halisi: "),
  chart_cost          = c(en = "\n Cost: ",                sw = "\n Gharama: "),
  chart_recommended   = c(en = "Recommended practice",     sw = "Mbinu inayopendekezwa"),
  lmo_col_operation   = c(en = "Operation",                sw = "Shughuli"),
  lmo_col_method      = c(en = "Method",                   sw = "Njia"),
  lmo_col_cost        = c(en = "Cost/ha",                  sw = "Gharama/hekta"),

  # ── SP — chart labels ────────────────────────────────────────────────────
  chart_current       = c(en = "Current",                  sw = "Sasa"),

  # ── Location card ────────────────────────────────────────────────────────
  coord_lat           = c(en = "Lat",                      sw = "Lat"),
  coord_lon           = c(en = "Lon",                      sw = "Lon"),

  # ── Email ─────────────────────────────────────────────────────────────────
  email_subject       = c(en = "AKILIMO recommendation",
                           sw = "Mapendekezo ya AKILIMO"),
  email_body          = c(en = "Please find attached your AKILIMO recommendation.\n\nBest regards,\nThe AKILIMO team",
                           sw = "Tafadhali tazama mapendekezo yako ya AKILIMO yaliyoambatanishwa.\n\nKwa heshima,\nTimu ya AKILIMO"),

  # ── SP — Scheduled Planting ─────────────────────────────────────────────
  sp_title            = c(en = "Scheduled Planting Advice",
                           sw = "Ushauri wa Kupanga Tarehe ya Kupanda"),
  planting_window     = c(en = "Planting window",
                           sw = "Muda wa kupanda"),
  harvest_window      = c(en = "Harvest window",
                           sw = "Muda wa kuvuna"),
  sold_to_sf          = c(en = "Sold to starch factory",
                           sw = "Kuuzwa kwa kiwanda cha wanga"),
  months_label        = c(en = "months",
                           sw = "miezi"),
  no_sp_data          = c(en = "No planting schedule data available for this location.",
                           sw = "Hakuna data ya ratiba ya kupanda kwa eneo hili.")
)

#' Look up a PDF UI label in English or Swahili.
html_label <- function(key, lang = "en") {
  entry <- .PDF_LABELS[[key]]
  if (is.null(entry)) return(key)
  l <- if (lang %in% names(entry)) entry[[lang]] else entry[["en"]]
  l
}

# ── Image helpers ─────────────────────────────────────────────────────────────

#' Embed a PNG file as a base64 data URI <img> tag.
#' @param path  Absolute path to PNG file.
#' @param alt   Alt text.
#' @param class Optional CSS class for the <img>.
#' @return character(1)
img_base64 <- function(path, alt = "", class = "") {
  raw <- readBin(path, "raw", n = file.info(path)$size)
  b64 <- base64enc::base64encode(raw)
  cls <- if (nchar(class) > 0) sprintf(' class="%s"', class) else ""
  sprintf('<img src="data:image/png;base64,%s" alt="%s"%s>', b64, htmlEscape(alt), cls)
}

#' Return the base64 <img> for a fertilizer bag image.
#' @param fert_type Internal type key (e.g. "NPK15_15_15").
#' @param bags      Bag count (numeric, may be 0.5, 1, 1.5, …, 10).
#' @return character(1) — <img> tag, or "" if file not found.
img_bag <- function(fert_type, bags) {
  color    <- if (fert_type %in% names(FERT_COLOUR)) FERT_COLOUR[[fert_type]] else "green"
  bag_file <- if (bags == 0.5) {
    "half.png"
  } else {
    paste0(gsub(".", "_", as.character(bags), fixed = TRUE), ".png")
  }
  path <- file.path(akpath, "net", color, bag_file)
  if (!file.exists(path)) return("")
  img_base64(path, alt = sprintf("%.1f bags", bags))
}

#' Return the base64 <img> for a cash-stack image.
#' @param ratio Integer 1–10 selecting net/cash/Picture{n}.png.
#' @return character(1)
img_cash <- function(ratio) {
  ratio <- max(1L, min(10L, as.integer(round(ratio))))
  path  <- file.path(akpath, "net", "cash", paste0("Picture", ratio, ".png"))
  if (!file.exists(path)) return("")
  img_base64(path, alt = paste0("cash ", ratio))
}

# ── Document open/close ───────────────────────────────────────────────────────

#' Open the HTML document with inlined CSS and optional banner.
#' @param title       Page <title>.
#' @param banner_path Absolute path to banner PNG (NULL = no banner).
#' @param css_path    Absolute path to akilimo_print.css.
#' @return character(1)
html_open <- function(title, banner_path = NULL,
                      css_path = file.path(akpath, "net", "akilimo_print.css")) {
  css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

  banner_html <- ""
  if (!is.null(banner_path) && file.exists(banner_path)) {
    banner_html <- sprintf(
      '<div class="banner">%s</div>\n',
      img_base64(banner_path, alt = "Akilimo banner", class = "")
    )
  }

  sprintf(
    '<!DOCTYPE html>\n<html lang="en">\n<head>\n<meta charset="UTF-8">\n<title>%s</title>\n<style>\n%s\n</style>\n</head>\n<body>\n%s',
    htmlEscape(title), css, banner_html
  )
}

#' Close the HTML document.
html_close <- function() "</body>\n</html>"

# ── Layout primitives ─────────────────────────────────────────────────────────

#' Wrap content in a <div class="section"> with an optional heading.
html_section <- function(heading, content) {
  head_html <- if (!is.null(heading) && nchar(heading) > 0)
    sprintf('<div class="section-title">%s</div>\n', htmlEscape(heading))
  else ""
  sprintf('<div class="section avoid-break">\n%s%s\n</div>\n', head_html, content)
}

#' Two-column grid.
html_two_col <- function(left, right) {
  sprintf('<div class="two-col">\n<div>%s</div>\n<div>%s</div>\n</div>\n', left, right)
}

#' Three-column grid.
html_three_col <- function(col1, col2, col3) {
  sprintf(
    '<div class="three-col">\n<div>%s</div>\n<div>%s</div>\n<div>%s</div>\n</div>\n',
    col1, col2, col3
  )
}

# ── PII masking ───────────────────────────────────────────────────────────────

#' Mask an email address for display, retaining the first character and domain.
#' E.g. "user@example.com" → "u***r@example.com".
mask_email <- function(x) {
  x <- trimws(as.character(x %||% ""))
  if (!grepl("@", x, fixed = TRUE)) return("***")
  at_pos <- regexpr("@", x, fixed = TRUE)
  local  <- substr(x, 1, at_pos - 1)
  domain <- substr(x, at_pos + 1, nchar(x))
  masked <- if (nchar(local) <= 2) {
    paste0(substr(local, 1, 1), "***")
  } else {
    paste0(substr(local, 1, 1), "***", substr(local, nchar(local), nchar(local)))
  }
  paste0(masked, "@", domain)
}

# ── Personal info ─────────────────────────────────────────────────────────────

#' Render the "What you told us" section.
#' @param user         List with $Name, $Email.
#' @param userField    Farm/field name.
#' @param area         Numeric farm size.
#' @param areaUnits    Unit string.
#' @param PD           Planting date (Date or character).
#' @param HD           Harvest date.
#' @param current_yield Numeric or NULL.
#' @param lang         Language code.
#' @return character(1)
html_personal_info <- function(user, userField, area, areaUnits,
                               PD, HD, current_yield = NULL, lang = "en") {
  fmt_date <- function(d) {
    tryCatch(format(as.Date(d), "%d-%b-%Y"), error = function(e) as.character(d))
  }
  unit_key      <- switch(areaUnits, ha = "area_unit_ha", acre = "area_unit_acre", NULL)
  display_units <- if (!is.null(unit_key)) html_label(unit_key, lang) else areaUnits

  # Area: show as integer if whole number, otherwise 1 decimal place
  area_fmt <- if (area == round(area)) sprintf("%.0f %s", area, display_units) else sprintf("%.1f %s", area, display_units)

  rows <- list(
    list(label = html_label("your_name",     lang), value = user$Name %||% ""),
    list(label = html_label("email_address", lang), value = mask_email(user$Email)),
    list(label = html_label("field_name",    lang), value = userField),
    list(label = html_label("field_area",    lang), value = area_fmt),
    list(label = html_label("planting_date", lang), value = fmt_date(PD)),
    list(label = html_label("harvest_date",  lang), value = fmt_date(HD))
  )

  if (!is.null(current_yield)) {
    rows <- c(rows, list(list(
      label = html_label("current_yield", lang),
      value = sprintf("%s %s",
                      formatC(as.numeric(current_yield), format = "f", big.mark = ",", digits = 0),
                      html_label("current_yield_unit", lang))
    )))
  }

  tds <- vapply(rows, function(r) {
    sprintf('<tr><td class="label">%s:</td><td class="value">%s</td></tr>',
            htmlEscape(r$label), htmlEscape(as.character(r$value)))
  }, character(1))

  inner <- sprintf('<table class="info-table">\n%s\n</table>', paste(tds, collapse = "\n"))
  html_section(html_label("what_you_told_us", lang), inner)
}

# ── Location map ──────────────────────────────────────────────────────────────

#' Render a location section for the PDF report.
#'
#' Resolution order:
#'   1. MAPBOX_TOKEN set → Mapbox Static Images API (streets-v12, pin marker)
#'   2. MAP_API_URL set  → generic static-map service
#'                         GET {MAP_API_URL}?lat=…&lon=…&zoom=10&size=800x{h}
#'   3. Neither set      → styled HTML coordinate card (offline, always works)
#'
#' On any network/HTTP error the function falls through to the next option.
#'
#' @param lat       Latitude.
#' @param lon       Longitude.
#' @param height_px Image height in pixels (width fixed at 800).
#' @param lang      Language code.
#' @return character(1)
html_location_map <- function(lat, lon, height_px = 300, lang = "en") {

  .fetch_map_png <- function(url) {
    map_png <- tp("map.png")
    ok <- tryCatch({
      resp <- httr::GET(url, httr::timeout(10),
                        httr::write_disk(map_png, overwrite = TRUE))
      httr::status_code(resp) == 200L && file.exists(map_png) && file.size(map_png) > 0
    }, error = function(e) {
      log_write("WARN", "html_location_map: map fetch failed:", conditionMessage(e))
      FALSE
    })
    if (ok) map_png else NULL
  }

  w <- 800L
  h <- as.integer(height_px)

  # ── 1. Mapbox Static Images API ──────────────────────────────────────────────
  mapbox_token <- Sys.getenv("MAPBOX_TOKEN", unset = "")
  if (nzchar(mapbox_token)) {
    # pin-s = small pin; ee4d5f = Akilimo red marker
    url <- sprintf(
      "https://api.mapbox.com/styles/v1/mapbox/streets-v12/static/pin-s+ee4d5f(%.6f,%.6f)/%.6f,%.6f,10,0/%dx%d@2x?access_token=%s",
      lon, lat, lon, lat, w, h, mapbox_token
    )
    png <- .fetch_map_png(url)
    if (!is.null(png)) {
      inner <- sprintf('<div class="map-image">%s</div>', img_base64(png, alt = "Farm location"))
      return(html_section(html_label("your_location", lang), inner))
    }
  }

  # ── 2. Generic static-map service ───────────────────────────────────────────
  map_api_url <- Sys.getenv("MAP_API_URL", unset = "")
  if (nzchar(map_api_url)) {
    url <- sprintf("%s?lat=%.6f&lon=%.6f&zoom=10&size=%dx%d",
                   map_api_url, lat, lon, w, h)
    png <- .fetch_map_png(url)
    if (!is.null(png)) {
      inner <- sprintf('<div class="map-image">%s</div>', img_base64(png, alt = "Farm location"))
      return(html_section(html_label("your_location", lang), inner))
    }
  }

  # ── 3. Offline fallback — coordinate card ───────────────────────────────────
  inner <- sprintf(
    '<div class="location-coords">
       <div class="coord-row"><span class="coord-label">%s</span><span class="coord-value">%.6f</span></div>
       <div class="coord-row"><span class="coord-label">%s</span><span class="coord-value">%.6f</span></div>
     </div>',
    html_label("coord_lat", lang), lat,
    html_label("coord_lon", lang), lon
  )
  html_section(html_label("your_location", lang), inner)
}

# ── Fertilizer table (price summary — left column) ────────────────────────────

#' Render the fertilizer prices + cassava price + max investment section.
#'
#' This goes in the left column. Fertilizer recommendation rows
#' (with bag images) are built inline in build_fr_pdf().
#'
#' @param fr        Data frame from calc_fertilizer_recom() (may have 0 rows).
#' @param area      Farm area (numeric).
#' @param areaUnits Area unit string.
#' @param currency  Currency string.
#' @param rootUP    Cassava root unit price (per kg).
#' @param cassPD    Cassava product destination string (e.g. "roots").
#' @param cassUW    Cassava unit weight (kg; 1000 = per tonne).
#' @param maxInv    Maximum investment (numeric or NA).
#' @param lang      Language code.
#' @return character(1)
html_fertilizer_table <- function(fr, area, areaUnits, currency,
                                  rootUP, cassPD, cassUW, maxInv, lang = "en") {
  parts <- character(0)

  # 1. Price table
  if (!is.null(fr) && nrow(fr) > 0) {
    price_rows <- vapply(seq_len(nrow(fr)), function(i) {
      ftype  <- fr$type[i]
      label  <- if (ftype %in% names(FERT_LABEL)) FERT_LABEL[[ftype]] else ftype
      cpb    <- formatC(as.numeric(fr$costPerBag[i]), format = "f", big.mark = ",", digits = 0)
      bw     <- fr$bagWeight[i]
      unit_str <- sprintf(html_label("bag_unit", lang), bw)
      sprintf('<tr><td class="label">%s</td><td class="value">@ %s %s %s %s</td></tr>',
              htmlEscape(label), htmlEscape(currency), htmlEscape(cpb),
              html_label("per_bag", lang),
              htmlEscape(unit_str))
    }, character(1))
    parts <- c(parts, sprintf('<table class="info-table">\n%s\n</table>',
                              paste(price_rows, collapse = "\n")))
  } else {
    parts <- c(parts, sprintf('<p><em>%s</em></p>', html_label("no_fert_rec", lang)))
  }

  # 2. Cassava price — "Cassava sold @ TZS 180,000 per tonne of roots"
  rootUP_fmt <- formatC(signif(as.numeric(rootUP), digits = 4),
                        format = "f", big.mark = ",", digits = 0)
  if (as.numeric(cassUW) == 1000) {
    unit_str <- paste(html_label("per_tonne", lang), htmlEscape(cassPD))
  } else {
    cw_fmt <- formatC(as.numeric(cassUW), format = "f", big.mark = ",", digits = 0)
    unit_str <- paste(sprintf(html_label("per_weight", lang), cw_fmt), htmlEscape(cassPD))
  }
  cassava_line <- sprintf('<p>%s: <strong>%s %s</strong> %s</p>',
                          html_label("cassava_price", lang),
                          htmlEscape(currency), htmlEscape(rootUP_fmt), unit_str)
  parts <- c(parts, cassava_line)

  # 3. Max investment — always shown; "NA" when zero or missing
  unit_key2     <- switch(areaUnits, ha = "area_unit_ha", acre = "area_unit_acre", NULL)
  display_units <- if (!is.null(unit_key2)) html_label(unit_key2, lang) else areaUnits
  area_fmt <- if (area == round(area)) sprintf("%.0f", area) else sprintf("%.1f", area)
  has_inv <- !is.na(maxInv) && as.numeric(maxInv) > 0
  inv_value <- if (has_inv) {
    sprintf("%s %s", htmlEscape(currency),
            htmlEscape(formatC(as.numeric(maxInv), format = "f", big.mark = ",", digits = 0)))
  } else {
    "NA"
  }
  inv_line <- sprintf('<p>%s: <strong>%s</strong> %s <strong>%s %s</strong></p>',
                      html_label("max_investment", lang), inv_value,
                      html_label("for_area", lang),
                      area_fmt, htmlEscape(display_units))
  parts <- c(parts, inv_line)

  html_section(html_label("fertilizer_prices", lang), paste(parts, collapse = "\n"))
}

# ── Cost-benefit ──────────────────────────────────────────────────────────────

#' Render the cost-benefit analysis section.
#'
#' @param sum_total     Numeric total fertilizer cost.
#' @param totalSale     Numeric total calculated revenue (TC + NR).
#' @param netRevenue    Numeric net revenue (NR).
#' @param currency      Currency string.
#' @param lang          Language code.
#' @return character(1)
html_cost_benefit <- function(sum_total, totalSale, netRevenue, currency, lang = "en") {
  # Compute cash-stack ratios
  if (min(sum_total, netRevenue) == sum_total) {
    ratio_cost    <- 1L
    ratio_sale    <- max(1L, min(10L, as.integer(round(totalSale   / sum_total))))
    ratio_revenue <- max(1L, min(10L, as.integer(round(netRevenue  / sum_total))))
  } else {
    ratio_revenue <- 1L
    ratio_cost    <- max(1L, min(10L, as.integer(round(sum_total   / netRevenue))))
    ratio_sale    <- max(1L, min(10L, as.integer(round(totalSale   / netRevenue))))
  }

  fmt <- function(x) formatC(as.numeric(x), format = "f", big.mark = ",", digits = 0)

  rows <- list(
    list(label = html_label("total_cost",    lang), amount = fmt(sum_total),  ratio = ratio_cost),
    list(label = html_label("total_revenue", lang), amount = fmt(totalSale),  ratio = ratio_sale),
    list(label = html_label("net_revenue",   lang), amount = fmt(netRevenue), ratio = ratio_revenue)
  )

  row_html <- vapply(rows, function(r) {
    sprintf(
      '<div class="cb-row">\n  <span class="cb-label">%s:</span>\n  <span class="cb-amount"><strong>%s %s,</strong></span>\n  <span class="cb-image">%s</span>\n</div>',
      htmlEscape(r$label), htmlEscape(currency), htmlEscape(r$amount),
      img_cash(r$ratio)
    )
  }, character(1))

  html_section(html_label("cost_benefit", lang),
               sprintf('<div class="cost-benefit">\n%s\n</div>', paste(row_html, collapse = "\n")))
}

# ── Recommendation text ───────────────────────────────────────────────────────

#' Render the full-width recommendation text at the bottom of the PDF.
#' The date heading is already shown inside the fertilizer rows section.
#' @param recText Recommendation text (newlines → <br>).
#' @param lang    Language code.
#' @return character(1)
html_recommendation <- function(recText, lang = "en") {
  # Collapse newlines into spaces so the text flows as a single paragraph
  flat_text <- gsub("\\s*\n\\s*", " ", trimws(as.character(recText)))
  body_html <- sprintf('<p class="recommendation-text">%s</p>', htmlEscape(flat_text))
  html_section(NULL, body_html)
}

# ── Generic data table ────────────────────────────────────────────────────────

#' Render a data frame as an HTML <table>.
#' @param df        Data frame.
#' @param col_names Optional column header overrides.
#' @return character(1)
html_table <- function(df, col_names = NULL) {
  if (is.null(col_names)) col_names <- colnames(df)

  header_cells <- paste(sprintf("<th>%s</th>", htmlEscape(col_names)), collapse = "")
  thead <- sprintf("<thead><tr>%s</tr></thead>", header_cells)

  body_rows <- apply(df, 1, function(row) {
    cells <- paste(sprintf("<td>%s</td>", htmlEscape(as.character(row))), collapse = "")
    sprintf("<tr>%s</tr>", cells)
  })
  tbody <- sprintf("<tbody>%s</tbody>", paste(body_rows, collapse = "\n"))

  sprintf('<table class="data-table">\n%s\n%s\n</table>', thead, tbody)
}

# ── Utilities ─────────────────────────────────────────────────────────────────

#' HTML-escape special characters.
htmlEscape <- function(x) {
  x <- gsub("&",  "&amp;",  as.character(x), fixed = TRUE)
  x <- gsub("<",  "&lt;",   x, fixed = TRUE)
  x <- gsub(">",  "&gt;",   x, fixed = TRUE)
  x <- gsub('"',  "&quot;", x, fixed = TRUE)
  x
}
