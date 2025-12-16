
#SHORT DEF:   Function to send SMS report.
#RETURNS:     Nothing. SMS report are sent.
#             TODO: build in checks to log if SMS report was successfully sent.
#DESCRIPTION: Function using Plivo service to send SMS texts to phonenumber specified.
#             TODO: use scan function to read credentials from csv input file.
#INPUT:       SMStext: Vector of body text to be sent by SMS. Elements should not exceed 1600 character limit!
#             src: source phone number, starting with country code, e.g, 254727876796
#             dst: destination phone number, starting with country code, e.g., 234789123456


# POST returns
#  Could not verify your access level for that URL.
# perhaps need a new subscription

sendSMSReport <- function(SMStext, dst) {
    #plivio account details
	pw <- readRDS("passwords.rds")
    AUTH_ID <- pw$plivo$AUTH_ID
    AUTH_TOKEN <- pw$plivo$AUTH_TOKEN
	src <- pw$plivo$source
    url <- paste0("https://api.plivo.com/v1/Account/", AUTH_ID, "/Message/")

    for (txt in SMStext) {
		if (nchar(txt) > 1600) {
			txt <- paste0(substr(1, 1588, txt), " [truncated]")
			message("text message truncated to the 1600 character limit")
		}
        httr::POST(url, httr::authenticate(AUTH_ID, AUTH_TOKEN), body=list(src=src, dst=dst, text=txt))
	}
}


#' function to send mail

# this is not working. Perhaps replace mailR with sendmailR or blastula package to avoid Java dependency
# never reached??
sendEmailReport <- function(user, FR, IC, PP, SP, FRrecom, ICrecom, country, PPrecom, SPrecom) {

	message(paste("Running email generation FR=", FR, "IC=", IC, "PP=", PP, "SP=", SP, "FRrecom=", FRrecom, "ICrecom=", ICrecom))

	PDFs <- NULL
	add_pdf <- function(f) {PDFs <<- c(PDFs, f); f}

	if (FR & (!IC) & FRrecom) {
	# is that the correct file exists? Or is that the file that should be generated?
		if (country %in% c("NG", "GH") & file.exists(paste0("fertilizer_advice_", user$PhoneNr, ".pdf"))) {
			fname <- add_pdf(paste0('./temp/fertilizer_advice_', user$PhoneNr, ".pdf"))
			webshot::rmdshot('./Rmd/FR_markdown_VFT.Rmd', file=fname, delay = 3)
		} else if (country == "TZ") {
			#if (file.exists("fertilizer_advice_swa.pdf"))
			fname <- add_pdf(paste0('./temp/fertilizer_advice_swa_', user$PhoneNr, ".pdf"))
			webshot::rmdshot('./Rmd/FR_markdown_swa.Rmd', file = fname, delay = 3)
		}
	}

	if (FR & IC & ICrecom) {
		if (country == "NG" & file.exists("./temp/intercrop_advice_VFT.pdf")) {
			fname <- add_pdf(paste0('./temp/intercrop_advice_', user$PhoneNr, ".pdf"))
			webshot::rmdshot('./Rmd/IC_markdown_VFT.Rmd', file = fname, delay = 3)
		} else if (country == "TZ" & file.exists("CIS_VFT.pdf")) {
			fname <- add_pdf(paste0('./temp/CIS_advice_', user$PhoneNr, ".pdf"))
			webshot::rmdshot('./Rmd/CIS_markdown_swa.Rmd', file = fname, delay = 3)
		}
	}
	
	if (PP & PPrecom) {
		if (country == "NG" & file.exists("PP_advice_VFT.pdf")) {
			fname <- add_pdf(paste0('./temp/PP_advice_', user$PhoneNr, ".pdf"))
			webshot::rmdshot('./Rmd/PP_markdownVFT.Rmd', file = fname, delay = 3)
		} else if (country == "TZ" & file.exists("PP_advice_swa.pdf")) {
			fname <- add_pdf(paste0('./temp/PP_advice_swa_', user$PhoneNr, ".pdf"))
			webshot::rmdshot('./Rmd/PP_markdown_swa.Rmd', file = fname, delay = 3)
		} 
	}

	if (SP & SPrecom) {
		if (country %in% c("NG", "GH") & file.exists("SP_advice_VFT.pdf")) {
			fname <- add_pdf(paste0('./temp/SP_advice_', user$PhoneNr, ".pdf"))
			webshot::rmdshot('./Rmd/SP_markdownVFT.Rmd', file = fname, delay = 3)
			if (file.exists("spgg.png")) file.remove("spgg.png")
		} else if (country == "TZ" & file.exists("SP_advice_swa.pdf")) {
			fname <- add_pdf(paste0('./temp/SP_advice_swa_', user$PhoneNr, ".pdf"))
			webshot::rmdshot('./Rmd/SP_markdown_swa.Rmd', file = fname, delay = 3)
			if (file.exists("spgg.png")) file.remove("spgg.png")
		}
	}

	if (!is.null(listofPDFs)) {
		pwd <- readRDS("passwords.rds")
		mailR::send.mail(from = pwd$email$user.name,
              to = as.character(user$Email),
              subject = "AKILIMO recommendation",
              body = "Please find attached the recommendation. \n Best Regards, \n AKILIMO",
              authenticate = TRUE,
              attach.files = PDFs,
              smtp = list(host.name = pwd$email$host.name, port = 587,
                          user.name = pwd$email$user.name, passwd = pwd$email$password, tls = TRUE))
		try(file.remove(PDFs))
	}

}
