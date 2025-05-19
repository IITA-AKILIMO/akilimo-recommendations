
library(mailR)
library(rJava)


to <- "s.barasa@cgiar.org"
subject <- "Test this message"
msg <- "This is the body of the email"
send.mail(from = "AKILIMO@cgiar.org",
          to = to,
          subject = "AKILIMO recommendation",
          body = "Please find attached the recommendation. \n Best Regards, \n AKILIMO",
          authenticate = TRUE,
          smtp = list(host.name = "smtp.office365.com", port = 587,
                      user.name = "AKILIMO@cgiar.org", passwd = "ysyztfbyzglmmpdg", tls = TRUE))

