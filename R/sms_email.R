
#SHORT DEF:   Function to send SMS report.
#RETURNS:     Nothing. SMS report are sent.
#DESCRIPTION: Function using Plivo service to send SMS texts to phonenumber specified.
#INPUT:       SMStext: Vector of body text to be sent by SMS. Elements should not exceed 1600 character limit!
#             src: source phone number, starting with country code, e.g, 254727876796
#             dst: destination phone number, starting with country code, e.g., 234789123456
#
# Required environment variables:
#   PLIVO_AUTH_ID      — Plivo account AUTH ID
#   PLIVO_AUTH_TOKEN   — Plivo account AUTH token
#   PLIVO_SRC_NUMBER   — Sender phone number (with country code)
#   EMAIL_HOST         — SMTP host (e.g. smtp.gmail.com)
#   EMAIL_USER         — SMTP username / from address
#   EMAIL_PASSWORD     — SMTP password

sendSMSReport <- function(SMStext, dst) {
    AUTH_ID    <- Sys.getenv("PLIVO_AUTH_ID")
    AUTH_TOKEN <- Sys.getenv("PLIVO_AUTH_TOKEN")
    src        <- Sys.getenv("PLIVO_SRC_NUMBER")

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
        httr::POST(url, httr::authenticate(AUTH_ID, AUTH_TOKEN), body = list(src = src, dst = dst, text = txt))
    }
}


#' Generate PDFs for all applicable recommendation types.
#' Always called regardless of the email flag. Returns paths of generated PDFs.
generate_pdfs <- function(user, FR, IC, PP, SP, country) {

    message(paste("Generating PDFs: FR=", FR, "IC=", IC, "PP=", PP, "SP=", SP, "country=", country))

    PDFs       <- NULL
    add_pdf    <- function(f) { PDFs <<- c(PDFs, f); f }
    phone      <- safe_filename_part(user$PhoneNr)
    rmd_params <- list(params = list(temp_dir = file.path("..", temp_dir())))

    if (FR & (!IC) & file.exists(tp("FR_MarkDownText.csv"))) {
        if (country %in% c("NG", "GH")) {
            fname <- add_pdf(tp(paste0("fertilizer_advice_", phone, ".pdf")))
            webshot::rmdshot('./Rmd/FR_markdown_VFT.Rmd', file = fname, delay = 3, rmd_args = rmd_params)
        } else if (country == "TZ") {
            fname <- add_pdf(tp(paste0("fertilizer_advice_swa_", phone, ".pdf")))
            webshot::rmdshot('./Rmd/FR_markdown_swa.Rmd', file = fname, delay = 3, rmd_args = rmd_params)
        }
    }

    if (FR & IC & file.exists(tp("IC_MarkDownText.csv"))) {
        if (country == "NG") {
            fname <- add_pdf(tp(paste0("intercrop_advice_", phone, ".pdf")))
            webshot::rmdshot('./Rmd/IC_markdown_VFT.Rmd', file = fname, delay = 3, rmd_args = rmd_params)
        }
    }

    if (FR & IC & file.exists(tp("CIS_MarkDownText.csv"))) {
        if (country == "TZ") {
            fname <- add_pdf(tp(paste0("CIS_advice_", phone, ".pdf")))
            webshot::rmdshot('./Rmd/CIS_markdown_swa.Rmd', file = fname, delay = 3, rmd_args = rmd_params)
        }
    }

    if (PP & file.exists(tp("PP_MarkDownText.csv"))) {
        if (country == "NG") {
            fname <- add_pdf(tp(paste0("PP_advice_", phone, ".pdf")))
            webshot::rmdshot('./Rmd/PP_markdownVFT.Rmd', file = fname, delay = 3, rmd_args = rmd_params)
        } else if (country == "TZ") {
            fname <- add_pdf(tp(paste0("PP_advice_swa_", phone, ".pdf")))
            webshot::rmdshot('./Rmd/PP_markdown_swa.Rmd', file = fname, delay = 3, rmd_args = rmd_params)
        }
    }

    if (SP & file.exists(tp("SP_MarkDownText.csv"))) {
        if (country %in% c("NG", "GH")) {
            fname <- add_pdf(tp(paste0("SP_advice_", phone, ".pdf")))
            webshot::rmdshot('./Rmd/SP_markdownVFT.Rmd', file = fname, delay = 3, rmd_args = rmd_params)
            if (file.exists("spgg.png")) file.remove("spgg.png")
        } else if (country == "TZ") {
            fname <- add_pdf(tp(paste0("SP_advice_swa_", phone, ".pdf")))
            webshot::rmdshot('./Rmd/SP_markdown_swa.Rmd', file = fname, delay = 3, rmd_args = rmd_params)
            if (file.exists("spgg.png")) file.remove("spgg.png")
        }
    }

    PDFs
}


#' Send generated PDFs by email. Only called when user$send_email is TRUE.
sendEmailReport <- function(user, PDFs) {
    if (is.null(PDFs) || length(PDFs) == 0) return(invisible(NULL))

    email_host <- Sys.getenv("EMAIL_HOST")
    email_user <- Sys.getenv("EMAIL_USER")
    email_pass <- Sys.getenv("EMAIL_PASSWORD")

    if (any(nchar(c(email_host, email_user, email_pass)) == 0)) {
        warning("Email not sent: EMAIL_HOST, EMAIL_USER or EMAIL_PASSWORD not set.")
        return(invisible(NULL))
    }

    mailR::send.mail(
        from         = email_user,
        to           = as.character(user$Email),
        subject      = "AKILIMO recommendation",
        body         = "Please find attached the recommendation. \n Best Regards, \n AKILIMO",
        authenticate = TRUE,
        attach.files = PDFs,
        smtp         = list(host.name = email_host, port = 587,
                            user.name = email_user, passwd = email_pass, tls = TRUE)
    )
}
