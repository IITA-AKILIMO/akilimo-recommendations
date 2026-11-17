# PDF report generation: builds the report PDFs for a request. The PDFs are returned to the caller
# (returnPdf) and never emailed from here: the AKILIMO API sends any email.

# ── PDF generation ─────────────────────────────────────────────────────────────

#' Generate PDFs for all applicable recommendation types.
#' Returns the paths of the generated PDFs.
#' All types (FR, IC, PP, SP) use the WeasyPrint path.
#' Individual PDF failures are caught and logged; other PDFs are still attempted.
generate_pdfs <- function(user, FR, IC, PP, SP, country, result = NULL, params = NULL) {

  log_write("INFO", paste("Generating PDFs: FR=", FR, "IC=", IC, "PP=", PP, "SP=", SP, "country=", country))

  PDFs <- character(0)

  # .try_pdf runs expr and returns its value on success, or NULL on error.
  # fname must be assigned BEFORE calling .try_pdf so it lives in generate_pdfs's
  # frame — using <<- inside a passed expression does not work because R evaluates
  # promise arguments in their enclosing environment and <<- skips that frame.
  .try_pdf <- function(label, expr) {
    tryCatch(
      expr,
      error = function(e) {
        log_write("WARN", label, "PDF generation failed:", conditionMessage(e))
        warning(label, " PDF generation failed: ", conditionMessage(e))
        invisible(NULL)
      }
    )
  }

  # FR — WeasyPrint path
  if (FR && !IC && !is.null(result) && !is.null(result$data)) {
    fname <- tp("fertilizer_advice.pdf")
    path <- .try_pdf("FR", {
      build_fr_pdf(
        rr = result,
        fertilizers = result$fertilizers,
        user = params$user,
        country = params$country,
        userField = params$userField,
        area = params$area,
        areaUnits = params$areaUnits,
        PD = params$PD,
        HD = params$HD,
        lat = params$lat,
        lon = params$lon,
        rootUP = params$rootUP,
        cassPD = params$cassPD,
        cassUW = params$cassUW,
        maxInv = params$maxInv,
        recText = result$recommendation,
        currency = get_currency(params$country),
        lang = params$lang,
        out_path = fname,
        areaHa = params$areaHa,
        FCY = params$FCY
      )
      fname
    })
    if (!is.null(path)) PDFs <- c(PDFs, path)
  }

  # IC — WeasyPrint path
  if (IC && !is.null(result) && !is.null(result$data)) {
    fname <- tp("intercrop_advice.pdf")
    path <- .try_pdf("IC", {
      build_ic_pdf(
        rr = result,
        user = params$user,
        country = params$country,
        userField = params$userField,
        area = params$area,
        areaUnits = params$areaUnits,
        PD = params$PD,
        HD = params$HD,
        lat = params$lat,
        lon = params$lon,
        rootUP = params$rootUP,
        cassPD = params$cassPD,
        cassUW = params$cassUW,
        maxInv = params$maxInv,
        recText = result$recommendation,
        currency = get_currency(params$country),
        lang = params$lang,
        out_path = fname
      )
      fname
    })
    if (!is.null(path)) PDFs <- c(PDFs, path)
  }

  # PP — WeasyPrint path
  if (PP && !is.null(result) && !is.null(result$data)) {
    fname <- tp("PP_advice.pdf")
    path <- .try_pdf("PP", {
      build_pp_pdf(
        rr = result,
        user = params$user,
        country = params$country,
        userField = params$userField,
        area = params$area,
        areaUnits = params$areaUnits,
        PD = params$PD,
        HD = params$HD,
        lat = params$lat,
        lon = params$lon,
        rootUP = params$rootUP,
        cassPD = params$cassPD,
        cassUW = params$cassUW,
        maxInv = params$maxInv,
        recText = result$recommendation,
        currency = get_currency(params$country),
        lang = params$lang,
        out_path = fname
      )
      fname
    })
    if (!is.null(path)) PDFs <- c(PDFs, path)
  }

  # SP — WeasyPrint path
  if (SP && !is.null(result)) {
    fname <- tp("SP_advice.pdf")
    path <- .try_pdf("SP", {
      build_sp_pdf(
        rr = result,
        user = params$user,
        country = params$country,
        userField = params$userField,
        area = params$area,
        areaUnits = params$areaUnits,
        PD = params$PD,
        HD = params$HD,
        lat = params$lat,
        lon = params$lon,
        cassUP = params$cassUP,
        cassUW = params$cassUW,
        cassPD = params$cassPD,
        saleSF = params$saleSF,
        nameSF = params$nameSF,
        PD_window = params$PD_window,
        HD_window = params$HD_window,
        recText = result$recommendation,
        currency = get_currency(params$country),
        lang = params$lang,
        out_path = fname
      )
      fname
    })
    if (!is.null(path)) PDFs <- c(PDFs, path)
  }

  PDFs
}
