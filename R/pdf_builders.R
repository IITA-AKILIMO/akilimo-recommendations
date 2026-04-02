# pdf_builders.R — PDF assembly functions using WeasyPrint
#
# Entry points:
#   build_fr_pdf()  — Fertilizer Recommendation
#   build_ic_pdf()  — Intercropping (IC / CIS sub-types)
#   build_pp_pdf()  — Post-Planting tillage advice
#   build_sp_pdf()  — Scheduled Planting advice
#
# All builders call render_pdf() which writes HTML to the request's temp dir
# and calls WeasyPrint via system2().
#
# Dependencies: html_helpers.R (sourced by api.R / AkilimoMain.R)

# ── Core renderer ─────────────────────────────────────────────────────────────

#' Render an HTML string to a PDF file via WeasyPrint.
#'
#' WeasyPrint must be installed and on PATH.
#' See docs/PDF-GENERATION-PLAN.md §2 for installation instructions.
#'
#' @param html  Character(1) — full HTML document string.
#' @param path  Absolute output path for the PDF file.
#' @return Invisibly returns `path` on success; stops with an error on failure.
render_pdf <- function(html, path) {
  html_tmp <- tp("render_tmp.html")
  writeLines(html, html_tmp, useBytes = TRUE)

  result <- system2(
    "weasyprint",
    args   = c(shQuote(html_tmp), shQuote(path)),
    stdout = TRUE,
    stderr = TRUE
  )

  if (!file.exists(path) || file.size(path) == 0) {
    stop("WeasyPrint failed:\n", paste(result, collapse = "\n"))
  }

  invisible(path)
}

# ── FR — Tailored Fertilizer Recommendation ───────────────────────────────────

#' Build the FR (Fertilizer Recommendation) PDF.
#'
#' @param rr         Recommendation result data from process-FR.R.
#' @param fertilizers Fertilizer list (parsed, from process-FR.R return value).
#' @param user        User info list (name, phone_number, send_email, …).
#' @param country     Country code string (e.g. "TZ").
#' @param userField   Farm/field description string.
#' @param area        Farm area (numeric).
#' @param areaUnits   Area unit string.
#' @param PD          Planting date.
#' @param HD          Harvest date.
#' @param lat         Latitude (numeric).
#' @param lon         Longitude (numeric).
#' @param rootUP      Cassava root unit price.
#' @param cassPD      Cassava price destination.
#' @param cassUW      Cassava unit weight.
#' @param maxInv      Maximum investment amount.
#' @param recText     Recommendation text string.
#' @param currency    Currency string.
#' @param lang        Language code ("en" or "sw").
#' @param out_path    Absolute output path for PDF.
#' @return Invisibly returns `out_path`.
build_fr_pdf <- function(rr, fertilizers, user, country,
                         userField, area, areaUnits,
                         PD, HD, lat, lon,
                         rootUP, cassPD, cassUW, maxInv,
                         recText, currency,
                         lang = "en", out_path) {

  # 1. Fertilizer recommendation rows
  fr <- calc_fertilizer_recom(fertilizers, rr)

  # 2. Financial totals
  sum_total  <- if (nrow(fr) > 0) round(sum(fr$cost), digits = 0) else as.numeric(rr$data$TC)
  totalSale  <- as.numeric(rr$data$TC) + as.numeric(rr$data$NR)
  netRevenue <- totalSale - sum_total

  # 3. Expected production
  bags_total    <- round(as.numeric(rr$data$TargetY), digits = 1)
  bags_total100 <- round(bags_total * 10, digits = 0)

  # 4. Banner path (Swahili uses a different banner)
  banner_file <- if (lang == "sw") "Akilimo_Dashboard_FR_swa.png" else "Akilimo_Dashboard_FR.png"
  banner_path <- file.path(akpath, "net", banner_file)

  # 5. Fertilizer recommendation rows (go in left column below prices)
  area_fmt <- if (area == round(area)) sprintf("%.0f", area) else sprintf("%.1f", area)

  if (nrow(fr) > 0) {
    rec_heading <- paste(
      html_label("rec_generated_on", lang),
      format(Sys.Date(), if (lang == "sw") "%d %B, %Y" else "%B %d, %Y")
    )
    fert_rows_html <- vapply(seq_len(nrow(fr)), function(i) {
      ftype  <- fr$type[i]
      label  <- if (ftype %in% names(FERT_LABEL)) FERT_LABEL[[ftype]] else ftype
      kgs    <- round(fr$rate[i], digits = 0)
      bags   <- fr$bags[i]
      bw     <- fr$bagWeight[i]
      cost_f <- formatC(as.numeric(fr$cost[i]), format = "f", big.mark = ",", digits = 0)

      title_str <- sprintf(html_label("fert_row_fmt", lang),
                           label, kgs, area_fmt, bags, bw, currency, cost_f)

      bag_img <- img_bag(ftype, bags)
      sprintf(
        '<div class="fert-entry avoid-break">\n<p>%s</p>\n<div class="bags">%s</div>\n</div>',
        htmlEscape(title_str), bag_img
      )
    }, character(1))
    fert_left <- html_section(rec_heading, paste(fert_rows_html, collapse = "\n"))
  } else {
    fert_left <- ""
  }

  # 6. Left column: personal info + prices + fertilizer rows
  left_col <- paste(
    html_personal_info(user = user, userField = userField, area = area, areaUnits = areaUnits,
                       PD = PD, HD = HD,
                       current_yield = round(as.numeric(rr$data$CurrentY), digits = 0),
                       lang = lang),
    html_fertilizer_table(fr = fr, area = area, areaUnits = areaUnits, currency = currency,
                          rootUP = rootUP, cassPD = cassPD, cassUW = cassUW,
                          maxInv = maxInv, lang = lang),
    fert_left,
    sep = "\n"
  )

  # 7. Right column: map + expected gain + cost-benefit
  expected_gain_text <- sprintf(html_label("expected_gain_fmt", lang), bags_total, bags_total100)
  right_col <- paste(
    html_location_map(lat = lat, lon = lon, lang = lang),
    html_section(html_label("expected_gain", lang), sprintf("<p>%s</p>", expected_gain_text)),
    html_cost_benefit(sum_total = sum_total, totalSale = totalSale,
                      netRevenue = netRevenue, currency = currency, lang = lang),
    sep = "\n"
  )

  # 8. Assemble full document — wide-right two-column layout
  html <- paste(
    html_open(html_label("fr_title", lang), banner_path),
    sprintf('<div class="two-col-wide-right">\n<div>%s</div>\n<div>%s</div>\n</div>\n',
            left_col, right_col),
    '<hr style="margin: 4px 0;">',
    html_recommendation(recText, lang),
    html_close(),
    sep = "\n"
  )

  render_pdf(html, out_path)
}

# ── IC — Intercropping ────────────────────────────────────────────────────────

#' Build the IC / CIS (Intercropping) PDF.
#'
#' @param rr       Full result list from process_IC_NG / process_IC_TZ.
#'   Must contain: $data, $fertilizer_rates, $fertilizers, $subtype,
#'   and for IC-NG: $maizeUP, $maizeUW, $maizePD, $CMP
#'   and for CIS-TZ: $sweetPotatoUP, $sweetPotatoPD, $sweetPotatoUW, $tuberUP
#' @param user       User info list.
#' @param country    Country code string.
#' @param userField  Farm/field description.
#' @param area       Farm area (numeric).
#' @param areaUnits  Area unit string.
#' @param PD         Planting date.
#' @param HD         Harvest date.
#' @param lat        Latitude.
#' @param lon        Longitude.
#' @param rootUP     Cassava root unit price.
#' @param cassPD     Cassava product destination.
#' @param cassUW     Cassava unit weight.
#' @param maxInv     Maximum investment.
#' @param recText    Recommendation text string.
#' @param currency   Currency string.
#' @param lang       Language code.
#' @param out_path   Output PDF path.
#' @return Invisibly returns `out_path`.
build_ic_pdf <- function(rr, user, country,
                         userField, area, areaUnits,
                         PD, HD, lat, lon,
                         rootUP, cassPD, cassUW, maxInv,
                         recText, currency,
                         lang = "en", out_path) {

  subtype     <- rr$subtype %||% "IC"
  fertilizers <- rr$fertilizers

  # Fertilizer recommendation rows
  half_lo <- if (subtype == "CIS") 0.3  else 0.25
  half_hi <- if (subtype == "CIS") 0.65 else 0.75
  fr <- calc_fertilizer_recom(fertilizers, rr, half_lo = half_lo, half_hi = half_hi)

  # Financial totals (IC uses dTC / dNR, not TC / NR)
  sum_total  <- round(if (nrow(fr) > 0) sum(fr$cost) else as.numeric(rr$data$dTC), digits = 0)
  netRevenue <- as.numeric(rr$data$dNR)
  totalSale  <- sum_total + netRevenue

  # Banner
  banner_file <- if (subtype == "CIS") "Akilimo_Dashboard_CIS.png" else "Akilimo_Dashboard_IC.png"
  banner_path <- file.path(akpath, "net", banner_file)

  area_fmt <- if (area == round(area)) sprintf("%.0f", area) else sprintf("%.1f", area)

  # ── Left column ─────────────────────────────────────────────────────────────
  left_col <- html_personal_info(user = user, userField = userField,
                                 area = area, areaUnits = areaUnits,
                                 PD = PD, HD = HD, lang = lang)

  # Fertilizer price table + cassava/max-investment lines
  left_col <- paste(left_col,
                    html_fertilizer_table(fr = fr, area = area, areaUnits = areaUnits,
                                          currency = currency, rootUP = rootUP,
                                          cassPD = cassPD, cassUW = cassUW,
                                          maxInv = maxInv, lang = lang),
                    sep = "\n")

  # Extra crop price (maize or sweet potato)
  if (subtype == "IC") {
    maizeUP <- as.numeric(rr$maizeUP)
    maizeUW <- rr$maizeUW
    maizePD <- rr$maizePD
    maize_fmt <- formatC(maizeUP, format = "f", big.mark = ",", digits = 0)
    unit_str <- if (maizePD == "fresh_cob") {
      "per fresh cob"
    } else {
      sprintf("per %s kg of grain", maizeUW)
    }
    extra_price_html <- html_section(
      html_label("maize_price", lang),
      sprintf('<p>@ <strong>%s %s</strong> %s</p>',
              htmlEscape(currency), htmlEscape(maize_fmt), unit_str)
    )
  } else {
    spUP  <- as.numeric(rr$sweetPotatoUP)
    spUW  <- rr$sweetPotatoUW
    spPD  <- rr$sweetPotatoPD
    sp_fmt <- formatC(spUP, format = "f", big.mark = ",", digits = 0)
    unit_str <- if (as.numeric(spUW) == 1000) {
      paste(html_label("per_tonne", lang), htmlEscape(spPD))
    } else {
      paste(sprintf(html_label("per_weight", lang), spUW), htmlEscape(spPD))
    }
    extra_price_html <- html_section(
      html_label("sp_price", lang),
      sprintf('<p>@ <strong>%s %s</strong> %s</p>',
              htmlEscape(currency), htmlEscape(sp_fmt), unit_str)
    )
  }
  left_col <- paste(left_col, extra_price_html, sep = "\n")

  # Fertilizer recommendation rows
  if (nrow(fr) > 0) {
    rec_heading <- paste(html_label("rec_generated_on", lang),
                         format(Sys.Date(), if (lang == "sw") "%d %B, %Y" else "%B %d, %Y"))
    fert_rows_html <- vapply(seq_len(nrow(fr)), function(i) {
      ftype  <- fr$type[i]
      label  <- if (ftype %in% names(FERT_LABEL)) FERT_LABEL[[ftype]] else ftype
      kgs    <- round(fr$rate[i], digits = 0)
      bags   <- fr$bags[i]
      bw     <- fr$bagWeight[i]
      cost_f <- formatC(as.numeric(fr$cost[i]), format = "f", big.mark = ",", digits = 0)
      title_str <- sprintf(html_label("fert_row_fmt", lang), label, kgs, area_fmt, bags, bw, currency, cost_f)
      bag_img <- img_bag(ftype, bags)
      sprintf('<div class="fert-entry avoid-break">\n<p>%s</p>\n<div class="bags">%s</div>\n</div>',
              htmlEscape(title_str), bag_img)
    }, character(1))
    left_col <- paste(left_col,
                      html_section(rec_heading, paste(fert_rows_html, collapse = "\n")),
                      sep = "\n")
  }

  # ── Right column ─────────────────────────────────────────────────────────────
  # Extra production text
  if (subtype == "IC") {
    dMP <- as.numeric(rr$data$dMP)
    if (!is.null(rr$maizePD) && rr$maizePD == "grain") {
      extra_prod_val <- round(dMP / 7.64, digits = 0)
      extra_prod_str <- sprintf("%s kg extra maize grain", extra_prod_val)
    } else {
      extra_prod_str <- sprintf("%s extra maize cobs", round(dMP, digits = 0))
    }
    density_str <- if (isTRUE(rr$data$rec_D)) html_label("high_density_rec", lang) else html_label("low_density_rec", lang)
    right_extra <- html_section(html_label("extra_production", lang),
                                sprintf("<p>%s</p><p>%s</p>", extra_prod_str, htmlEscape(density_str)))
  } else {
    rec_ic_str <- if (isTRUE(rr$data$rec_IC)) {
      if (isTRUE(rr$data$rec_F)) "Intercropping with sweet potato is recommended." else "Intercropping is recommended but fertilizer is not profitable."
    } else {
      "Cassava monocrop is more profitable for this location."
    }
    right_extra <- html_section(html_label("extra_production", lang),
                                sprintf("<p>%s</p>", htmlEscape(rec_ic_str)))
  }

  right_col <- paste(
    html_location_map(lat = lat, lon = lon, lang = lang),
    right_extra,
    html_cost_benefit(sum_total = sum_total, totalSale = totalSale,
                      netRevenue = netRevenue, currency = currency, lang = lang),
    sep = "\n"
  )

  # ── Assemble ─────────────────────────────────────────────────────────────────
  title <- html_label(if (subtype == "CIS") "cis_title" else "ic_title", lang)
  html <- paste(
    html_open(title, banner_path),
    sprintf('<div class="two-col-wide-right">\n<div>%s</div>\n<div>%s</div>\n</div>\n',
            left_col, right_col),
    '<hr style="margin: 4px 0;">',
    html_recommendation(recText, lang),
    html_close(),
    sep = "\n"
  )

  render_pdf(html, out_path)
}

# ── PP — Post-Planting tillage advice ────────────────────────────────────────

#' Build the PP (Post-Planting / Tillage) PDF.
#'
#' @param rr       Full result list from process_PP.
#'   Must contain: $data (pp scenarios df), $costLMO, $ploughing, $ridging,
#'   $method_ploughing, $method_ridging.
#' @param user      User info list.
#' @param country   Country code string.
#' @param userField Farm/field description string.
#' @param area      Farm area (numeric).
#' @param areaUnits Area unit string.
#' @param PD        Planting date.
#' @param HD        Harvest date.
#' @param lat       Latitude (numeric).
#' @param lon       Longitude (numeric).
#' @param rootUP    Cassava root unit price.
#' @param cassPD    Cassava price destination.
#' @param cassUW    Cassava unit weight.
#' @param maxInv    Maximum investment.
#' @param recText   Recommendation text string.
#' @param currency  Currency string.
#' @param lang      Language code.
#' @param out_path  Absolute output path for PDF.
#' @return Invisibly returns `out_path`.
build_pp_pdf <- function(rr, user, country,
                         userField, area, areaUnits,
                         PD, HD, lat, lon,
                         rootUP, cassPD, cassUW, maxInv,
                         recText, currency,
                         lang = "en", out_path) {

  ds              <- rr$data
  costLMO         <- rr$costLMO
  ploughing       <- rr$ploughing
  ridging         <- rr$ridging
  method_ploughing <- rr$method_ploughing
  method_ridging   <- rr$method_ridging

  # Banner
  banner_file <- if (lang == "sw") "Akilimo_Dashboard_PP_swa.png" else "Akilimo_Dashboard_PP.png"
  banner_path <- file.path(akpath, "net", banner_file)

  # ── PP ggplot chart ─────────────────────────────────────────────────────────
  chart_path <- tp("pp_chart.png")
  tryCatch({
    ds2 <- ds
    ds2$ploughing_label <- ifelse(ds2$method_ploughing == "N/A", "No ploughing",
                            ifelse(ds2$method_ploughing == "manual", "Manual\nploughing", "Tractor\nploughing"))
    ds2$ridging_label   <- ifelse(ds2$method_ridging   == "N/A", "No ridging",
                            ifelse(ds2$method_ridging   == "manual", "Manual\nridging",   "Tractor\nridging"))
    pp_plot <- ggplot2::ggplot(data = ds2[!ds2$CP, ],
                               ggplot2::aes(x = ploughing_label, y = ridging_label)) +
      ggplot2::geom_tile(ggplot2::aes(fill = dNR), colour = "grey95", linewidth = 1, alpha = 0.8) +
      ggplot2::scale_fill_gradient2(low = "royalblue3", high = "green3", mid = "white", midpoint = 0,
                                    labels = scales::comma, name = currency) +
      ggplot2::geom_tile(data = ds2[ds2$CP, ], colour = "black", linewidth = 2, fill = NA) +
      ggplot2::geom_tile(data = ds2[1, ],      colour = "purple", linewidth = 2, fill = NA) +
      ggplot2::geom_text(data = ds2[ds2$CP, ], fontface = "bold",
                         ggplot2::aes(label = "Your current\npractice"), size = 3.5) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(
        ifelse(dNR > 0, "+ ", ifelse(dNR < 0, "- ", " ")),
        formatC(signif(abs(dNR), digits = 3), format = "f", big.mark = ",", digits = 0))),
        fontface = "bold", size = 3.5,
        colour = ifelse(ds2[!ds2$CP, ]$dNR > 0, "green3",
                 ifelse(ds2[!ds2$CP, ]$dNR < 0, "red", "grey50"))) +
      ggplot2::geom_text(data = ds2[1, ], colour = "purple", fontface = "bold",
                         ggplot2::aes(label = "Recommended\npractice\n\n"), size = 3.5) +
      ggplot2::xlab(html_label("ploughing_label", lang)) +
      ggplot2::ylab(html_label("ridging_label",   lang)) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.title = ggplot2::element_blank(),
                     axis.text  = ggplot2::element_text(size = 11, colour = "black", face = "bold"))
    ggplot2::ggsave(chart_path, pp_plot, width = 8, height = 5, units = "in", dpi = 150)
  }, error = function(e) {
    warning("PP chart generation failed: ", e$message)
    chart_path <<- NULL
  })

  # ── Current practice info ────────────────────────────────────────────────────
  plo_str <- if (!ploughing) "No ploughing" else paste(method_ploughing, "ploughing")
  rid_str <- if (!ridging)   "No ridging"   else paste(method_ridging, "ridging")
  practice_html <- html_section(
    html_label("current_practice", lang),
    sprintf('<table class="info-table"><tr><td class="label">%s:</td><td class="value">%s</td></tr>
            <tr><td class="label">%s:</td><td class="value">%s</td></tr></table>',
            htmlEscape(html_label("ploughing_label", lang)), htmlEscape(plo_str),
            htmlEscape(html_label("ridging_label",   lang)), htmlEscape(rid_str))
  )

  # Cassava price + max investment (reuse cassava_price / max_investment labels)
  rootUP_fmt <- formatC(signif(as.numeric(rootUP), digits = 4), format = "f", big.mark = ",", digits = 0)
  unit_key2  <- switch(areaUnits, ha = "area_unit_ha", acre = "area_unit_acre", NULL)
  disp_units <- if (!is.null(unit_key2)) html_label(unit_key2, lang) else areaUnits
  area_fmt   <- if (area == round(area)) sprintf("%.0f", area) else sprintf("%.1f", area)
  has_inv    <- !is.na(maxInv) && as.numeric(maxInv) > 0
  inv_value  <- if (has_inv) sprintf("%s %s", htmlEscape(currency),
                  htmlEscape(formatC(as.numeric(maxInv), format = "f", big.mark = ",", digits = 0))) else "NA"
  price_html <- html_section(
    html_label("fertilizer_prices", lang),
    sprintf('<p>%s: <strong>%s %s</strong> %s %s</p><p>%s: <strong>%s</strong> %s <strong>%s %s</strong></p>',
            html_label("cassava_price", lang), htmlEscape(currency), rootUP_fmt,
            html_label("per_tonne", lang), htmlEscape(cassPD),
            html_label("max_investment", lang), inv_value,
            html_label("for_area", lang), area_fmt, htmlEscape(disp_units))
  )

  # ── LMO cost table ───────────────────────────────────────────────────────────
  lmo_cols <- c("operation", "method", "costHa")
  lmo_names <- c("Operation", "Method", "Cost/ha")
  lmo_html  <- html_section(
    html_label("lmo_table_heading", lang),
    html_table(costLMO[, intersect(lmo_cols, colnames(costLMO))], lmo_names[seq_len(length(intersect(lmo_cols, colnames(costLMO))))])
  )

  # ── Assemble layout ──────────────────────────────────────────────────────────
  # Top: two-col (left = personal + practice + price, right = LMO table)
  top_left  <- paste(html_personal_info(user, userField, area, areaUnits, PD, HD, lang = lang),
                     practice_html, price_html, sep = "\n")
  top_row   <- html_two_col(top_left, lmo_html)

  # Bottom: two-col (left = map, right = PP chart)
  chart_section <- if (!is.null(chart_path) && file.exists(chart_path)) {
    html_section(NULL, sprintf('<div class="chart-image">%s</div>', img_base64(chart_path, alt = "PP matrix")))
  } else {
    html_section(NULL, "<p><em>Chart unavailable.</em></p>")
  }
  bot_row <- html_two_col(html_location_map(lat, lon, lang = lang), chart_section)

  html <- paste(
    html_open(html_label("pp_title", lang), banner_path),
    top_row, bot_row,
    '<hr style="margin: 4px 0;">',
    html_recommendation(recText, lang),
    html_close(),
    sep = "\n"
  )

  render_pdf(html, out_path)
}

# ── SP — Scheduled Planting advice ───────────────────────────────────────────

#' Build the SP (Scheduled Planting) PDF.
#'
#' @param rr       Full result list from process_SP.
#'   Must contain: $data (sp grid df or NULL), $recommendation.
#' @param user      User info list.
#' @param country   Country code string.
#' @param userField Farm/field description string.
#' @param area      Farm area (numeric).
#' @param areaUnits Area unit string.
#' @param PD        Requested planting date.
#' @param HD        Requested harvest date.
#' @param lat       Latitude.
#' @param lon       Longitude.
#' @param cassUP    Cassava unit price.
#' @param cassUW    Cassava unit weight.
#' @param cassPD    Cassava product destination.
#' @param saleSF    Logical: selling to starch factory.
#' @param nameSF    Starch factory name.
#' @param PD_window Planting date window (months).
#' @param HD_window Harvest date window (months).
#' @param recText   Recommendation text.
#' @param currency  Currency string.
#' @param lang      Language code.
#' @param out_path  Output PDF path.
#' @return Invisibly returns `out_path`.
build_sp_pdf <- function(rr, user, country,
                         userField, area, areaUnits,
                         PD, HD, lat, lon,
                         cassUP, cassUW, cassPD,
                         saleSF, nameSF,
                         PD_window, HD_window,
                         recText, currency,
                         lang = "en", out_path) {

  ds <- rr$data   # may be NULL or a data frame

  # Banner
  banner_file <- if (lang == "sw") "Akilimo_Dashboard_SP_swa.png" else "Akilimo_Dashboard_SP.png"
  banner_path <- file.path(akpath, "net", banner_file)

  # ── SP heatmap chart ─────────────────────────────────────────────────────────
  chart_path <- NULL
  if (!is.null(ds) && is.data.frame(ds) && nrow(ds) > 0) {
    chart_path <- tp("spgg.png")
    tryCatch({
      nameCurrency <- currency
      sp_plot <- ggplot2::ggplot(data = ds,
                   ggplot2::aes(x = as.factor(rPWnr), y = as.factor(rHWnr), fill = dGR)) +
        ggplot2::geom_tile(colour = "grey95", linewidth = 1) +
        ggplot2::scale_fill_gradient2(low = "red", high = "green3", mid = "linen", midpoint = 0,
                                      labels = scales::comma, name = nameCurrency) +
        ggplot2::geom_tile(data = ds[ds$CP, ],  colour = "black",  linewidth = 2, show.legend = FALSE) +
        ggplot2::geom_tile(data = ds[1, ],       colour = "purple", linewidth = 2, show.legend = FALSE) +
        ggplot2::geom_text(data = ds[ds$CP, ],   ggplot2::aes(label = "Current"),     size = 4) +
        ggplot2::geom_text(data = ds[1, ], colour = "purple", ggplot2::aes(label = "Recommended"), size = 4) +
        ggplot2::scale_x_discrete(
          breaks = seq(min(ds$rPWnr), max(ds$rPWnr), by = 2),
          labels = format(sort(unique(ds$PD)), "%d-%b")) +
        ggplot2::scale_y_discrete(
          breaks = seq(min(ds$rHWnr), max(ds$rHWnr), by = 2),
          labels = format(sort(unique(ds$HD)), "%d-%b")) +
        ggplot2::xlab("Planting date") +
        ggplot2::ylab("Harvest date") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          axis.title = ggplot2::element_text(size = 14, colour = "black", face = "bold"),
          axis.text.x = ggplot2::element_text(size = 11, colour = "black", angle = 45, hjust = 1),
          axis.text.y = ggplot2::element_text(size = 11, colour = "black"))
      ggplot2::ggsave(chart_path, sp_plot, width = 10, height = 6, units = "in", dpi = 150)
    }, error = function(e) {
      warning("SP chart generation failed: ", e$message)
      chart_path <<- NULL
    })
  }

  # ── Three-column header ───────────────────────────────────────────────────────
  # Col 1: personal info
  col1 <- html_personal_info(user, userField, area, areaUnits, PD, HD, lang = lang)

  # Col 2: current practice + planting/harvest windows
  pd_str <- tryCatch(format(as.Date(PD), "%d-%b-%Y"), error = function(e) as.character(PD))
  hd_str <- tryCatch(format(as.Date(HD), "%d-%b-%Y"), error = function(e) as.character(HD))
  col2 <- html_section(
    html_label("current_practice", lang),
    sprintf('<table class="info-table">
      <tr><td class="label">%s:</td><td class="value">%s</td></tr>
      <tr><td class="label">%s:</td><td class="value">%s %s</td></tr>
      <tr><td class="label">%s:</td><td class="value">%s</td></tr>
      <tr><td class="label">%s:</td><td class="value">%s %s</td></tr>
    </table>',
      htmlEscape(html_label("planting_date", lang)), htmlEscape(pd_str),
      htmlEscape(html_label("planting_window", lang)), PD_window, htmlEscape(html_label("months_label", lang)),
      htmlEscape(html_label("harvest_date",  lang)), htmlEscape(hd_str),
      htmlEscape(html_label("harvest_window", lang)), HD_window, htmlEscape(html_label("months_label", lang)))
  )

  # Col 3: price / factory info
  cassUP_fmt <- formatC(signif(as.numeric(cassUP), digits = 4), format = "f", big.mark = ",", digits = 0)
  if (isTRUE(saleSF) && !is.na(nameSF) && nzchar(as.character(nameSF))) {
    price_content <- sprintf('<p>%s: <strong>%s</strong></p><p>%s: <strong>%s %s</strong></p>',
                             htmlEscape(html_label("sold_to_sf", lang)), htmlEscape(as.character(nameSF)),
                             htmlEscape(html_label("cassava_price", lang)),
                             htmlEscape(currency), htmlEscape(cassUP_fmt))
  } else {
    price_content <- sprintf('<p>%s: <strong>%s %s</strong> %s %s</p>',
                             htmlEscape(html_label("cassava_price", lang)),
                             htmlEscape(currency), htmlEscape(cassUP_fmt),
                             html_label("per_tonne", lang), htmlEscape(cassPD))
  }
  col3 <- html_section(html_label("fertilizer_prices", lang), price_content)

  top_row <- html_three_col(col1, col2, col3)

  # ── Bottom two-column: map + chart ────────────────────────────────────────────
  map_section <- html_location_map(lat, lon, lang = lang)
  if (!is.null(chart_path) && file.exists(chart_path)) {
    chart_section <- html_section(
      html_label("expected_gain", lang),
      sprintf('<div class="chart-image">%s</div>', img_base64(chart_path, alt = "SP heatmap"))
    )
  } else {
    chart_section <- html_section(
      html_label("expected_gain", lang),
      sprintf('<p><em>%s</em></p>', html_label("no_sp_data", lang))
    )
  }
  bot_row <- html_two_col(map_section, chart_section)

  html <- paste(
    html_open(html_label("sp_title", lang), banner_path),
    top_row, bot_row,
    '<hr style="margin: 4px 0;">',
    html_recommendation(recText, lang),
    html_close(),
    sep = "\n"
  )

  render_pdf(html, out_path)
}
