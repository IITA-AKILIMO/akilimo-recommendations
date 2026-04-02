# Required environment variables (see docs/SETUP.md for full details):
#
# SMS (Plivo):
#   PLIVO_AUTH_ID      — Plivo account AUTH ID
#   PLIVO_AUTH_TOKEN   — Plivo account AUTH token
#   PLIVO_SRC_NUMBER   — Sender phone number (with country code)
#
# Email — select provider with EMAIL_PROVIDER (default: smtp):
#
#   EMAIL_PROVIDER     — smtp | mailtrap | mailgun  (default: smtp)
#   EMAIL_FROM         — sender address (all providers)
#
#   smtp:
#     EMAIL_HOST       — SMTP host (e.g. smtp.gmail.com)
#     EMAIL_USER       — SMTP username / from address
#     EMAIL_PASSWORD   — SMTP password
#     EMAIL_PORT       — SMTP port (default: 587)
#
#   mailtrap (Sending API — https://send.api.mailtrap.io):
#     MAILTRAP_API_KEY — Mailtrap API key
#
#   mailgun:
#     MAILGUN_API_KEY  — Mailgun API key
#     MAILGUN_DOMAIN   — Mailgun sending domain (e.g. mg.example.com)
#     MAILGUN_REGION   — us | eu  (default: us)


sendSMSReport <- function(SMStext, dst) {
  AUTH_ID <- Sys.getenv("PLIVO_AUTH_ID")
  AUTH_TOKEN <- Sys.getenv("PLIVO_AUTH_TOKEN")
  src <- Sys.getenv("PLIVO_SRC_NUMBER")

  if (any(nchar(c(AUTH_ID, AUTH_TOKEN, src)) == 0)) {
    warning("SMS not sent: PLIVO_AUTH_ID, PLIVO_AUTH_TOKEN or PLIVO_SRC_NUMBER not set.")
    return(invisible(NULL))
  }

  url <- paste0("https://api.plivo.com/v1/Account/", AUTH_ID, "/Message/")

  for (txt in SMStext) {
    if (nchar(txt) > 1600) {
      txt <- paste0(substr(txt, 1, 1588), " [truncated]")
      message("text message truncated to the 1600 character limit")
    }
    httr::POST(url, httr::authenticate(AUTH_ID, AUTH_TOKEN),
               body = list(src = src, dst = dst, text = txt))
  }
}


# ── PDF generation ─────────────────────────────────────────────────────────────

#' Generate PDFs for all applicable recommendation types.
#' Always called regardless of the email flag. Returns paths of generated PDFs.
#' All types (FR, IC, PP, SP) use the WeasyPrint path.
#' Individual PDF failures are caught and logged; other PDFs are still attempted.
generate_pdfs <- function(user, FR, IC, PP, SP, country, result = NULL, params = NULL) {

  message(paste("Generating PDFs: FR=", FR, "IC=", IC, "PP=", PP, "SP=", SP, "country=", country))

  PDFs <- character(0)
  phone <- safe_filename_part(user$PhoneNr)

  # .try_pdf runs expr and returns its value on success, or NULL on error.
  # fname must be assigned BEFORE calling .try_pdf so it lives in generate_pdfs's
  # frame — using <<- inside a passed expression does not work because R evaluates
  # promise arguments in their enclosing environment and <<- skips that frame.
  .try_pdf <- function(label, expr) {
    tryCatch(
      expr,
      error = function(e) {
        warning(label, " PDF generation failed: ", conditionMessage(e))
        invisible(NULL)
      }
    )
  }

  # FR — WeasyPrint path
  if (FR && !IC && !is.null(result) && !is.null(result$data)) {
    fname <- tp(paste0("fertilizer_advice_", phone, ".pdf"))
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
        out_path = fname
      )
      fname
    })
    if (!is.null(path)) PDFs <- c(PDFs, path)
  }

  # IC — WeasyPrint path
  if (IC && !is.null(result) && !is.null(result$data)) {
    fname <- tp(paste0("intercrop_advice_", phone, ".pdf"))
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
    fname <- tp(paste0("PP_advice_", phone, ".pdf"))
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
    fname <- tp(paste0("SP_advice_", phone, ".pdf"))
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


# ── Email delivery ─────────────────────────────────────────────────────────────

#' Send generated PDFs by email.
#' Provider is selected by the EMAIL_PROVIDER env var (smtp | mailtrap | mailgun).
#' Only called when user$send_email is TRUE.
sendEmailReport <- function(user, PDFs) {
  if (is.null(PDFs) || length(PDFs) == 0) {
    log_write("DEBUG", "sendEmailReport: called with no PDFs — skipping.")
    return(invisible(NULL))
  }

  to       <- as.character(user$Email)
  from     <- Sys.getenv("EMAIL_FROM", unset = "")
  subject  <- "AKILIMO recommendation"
  body_txt <- "Please find attached your AKILIMO recommendation.\n\nBest regards,\nThe AKILIMO team"
  provider <- tolower(trimws(Sys.getenv("EMAIL_PROVIDER", unset = "smtp")))

  log_write("INFO", sprintf("Email: sending via '%s' to %s (%d attachment(s): %s)",
                            provider, to, length(PDFs),
                            paste(basename(PDFs), collapse = ", ")))

  tryCatch(
    switch(provider,
           mailtrap = .send_mailtrap(to, from, subject, body_txt, PDFs),
           mailgun  = .send_mailgun( to, from, subject, body_txt, PDFs),
           .send_smtp(to, from, subject, body_txt, PDFs)
    ),
    error = function(e) {
      log_write("ERROR", sprintf("Email delivery failed (%s): %s", provider, conditionMessage(e)))
      warning("Email delivery failed (", provider, "): ", conditionMessage(e))
    }
  )
  invisible(NULL)
}


# ── Provider implementations ──────────────────────────────────────────────────

.send_smtp <- function(to, from, subject, body_txt, PDFs) {
  host <- Sys.getenv("EMAIL_HOST")
  user <- Sys.getenv("EMAIL_USER")
  pass <- Sys.getenv("EMAIL_PASSWORD")
  port <- as.integer(Sys.getenv("EMAIL_PORT", unset = "587"))

  missing_vars <- names(which(nchar(c(EMAIL_HOST = host, EMAIL_USER = user, EMAIL_PASSWORD = pass)) == 0))
  if (length(missing_vars) > 0) {
    log_write("WARN", "SMTP email not sent: missing env vars:", paste(missing_vars, collapse = ", "))
    warning("SMTP email not sent: missing env vars: ", paste(missing_vars, collapse = ", "))
    return(invisible(NULL))
  }

  from_addr <- if (nzchar(from)) from else user
  log_write("DEBUG", sprintf("SMTP: host=%s port=%d from=%s to=%s", host, port, from_addr, to))

  mailR::send.mail(
    from         = from_addr,
    to           = to,
    subject      = subject,
    body         = body_txt,
    authenticate = TRUE,
    attach.files = PDFs,
    smtp         = list(host.name = host, port = port,
                        user.name = user, passwd = pass, tls = TRUE)
  )
  log_write("INFO", sprintf("SMTP: email sent to %s", to))
}


.send_mailtrap <- function(to, from, subject, body_txt, PDFs) {
  api_key <- Sys.getenv("MAILTRAP_API_KEY")
  host    <- Sys.getenv("EMAIL_HOST", unset = "https://send.api.mailtrap.io/api/send")

  if (!nzchar(api_key)) {
    log_write("WARN", "Mailtrap email not sent: MAILTRAP_API_KEY not set.")
    warning("Mailtrap email not sent: MAILTRAP_API_KEY not set.")
    return(invisible(NULL))
  }

  from_addr <- if (nzchar(from)) from else "akilimo@example.com"
  log_write("DEBUG", sprintf("Mailtrap: endpoint=%s from=%s to=%s", host, from_addr, to))

  attachments <- lapply(PDFs, function(f) {
    raw <- readBin(f, "raw", n = file.info(f)$size)
    list(
      content     = jsonlite::unbox(base64enc::base64encode(raw)),
      filename    = jsonlite::unbox(basename(f)),
      type        = jsonlite::unbox("application/pdf"),
      disposition = jsonlite::unbox("attachment")
    )
  })

  payload <- list(
    from        = list(email = jsonlite::unbox(from_addr)),
    to          = list(list(email = jsonlite::unbox(to))),
    subject     = jsonlite::unbox(subject),
    text        = jsonlite::unbox(body_txt),
    attachments = attachments
  )

  resp <- httr::POST(
    url    = host,
    httr::add_headers(
      Authorization  = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ),
    body   = jsonlite::toJSON(payload),
    encode = "raw"
  )

  status <- httr::status_code(resp)
  if (httr::http_error(resp)) {
    body <- httr::content(resp, as = "text", encoding = "UTF-8")
    log_write("ERROR", sprintf("Mailtrap API error: HTTP %d — %s", status, body))
    warning("Mailtrap API error: ", status, " — ", body)
  } else {
    log_write("INFO", sprintf("Mailtrap: email sent to %s (HTTP %d)", to, status))
  }
}


.send_mailgun <- function(to, from, subject, body_txt, PDFs) {
  api_key <- Sys.getenv("MAILGUN_API_KEY")
  domain  <- Sys.getenv("MAILGUN_DOMAIN")
  region  <- tolower(trimws(Sys.getenv("MAILGUN_REGION", unset = "us")))

  missing_vars <- names(which(nchar(c(MAILGUN_API_KEY = api_key, MAILGUN_DOMAIN = domain)) == 0))
  if (length(missing_vars) > 0) {
    log_write("WARN", "Mailgun email not sent: missing env vars:", paste(missing_vars, collapse = ", "))
    warning("Mailgun email not sent: missing env vars: ", paste(missing_vars, collapse = ", "))
    return(invisible(NULL))
  }

  from_addr <- if (nzchar(from)) from else paste0("akilimo@", domain)
  base_url  <- if (region == "eu") "https://api.eu.mailgun.net/v3" else "https://api.mailgun.net/v3"
  url       <- paste0(base_url, "/", domain, "/messages")

  log_write("DEBUG", sprintf("Mailgun: endpoint=%s from=%s to=%s", url, from_addr, to))

  body_parts <- list(from = from_addr, to = to, subject = subject, text = body_txt)
  if (length(PDFs) > 0) {
    body_parts[["attachment"]] <- lapply(PDFs, function(f)
      httr::upload_file(f, type = "application/pdf")
    )
  }

  resp <- httr::POST(
    url    = url,
    httr::authenticate("api", api_key),
    body   = body_parts,
    encode = "multipart"
  )

  status <- httr::status_code(resp)
  if (httr::http_error(resp)) {
    body <- httr::content(resp, as = "text", encoding = "UTF-8")
    log_write("ERROR", sprintf("Mailgun API error: HTTP %d — %s", status, body))
    warning("Mailgun API error: ", status, " — ", body)
  } else {
    log_write("INFO", sprintf("Mailgun: email sent to %s (HTTP %d)", to, status))
  }
}
