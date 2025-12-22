
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	akpath <- "C:/github/omilika/akilimo-recommendations"
} else {
	akpath <- "."
}
setwd(akpath)

x <- read.csv("tests/query_result_2025-12-20T19_34_01.582224662Z.csv")
x <- x[order(x$Created.At, decreasing=TRUE), ]
x <- x[!duplicated(x$RScript.Request), ]
x <- x[order(x$Created.At), ]

for (d in c("app", "request", "response")) dir.create(file.path("tests", d), FALSE, TRUE)

for (i in 1:nrow(x)) {
	writeLines(x[i,"App.Request"], paste0("tests/app/app_", i, ".json"))
	writeLines(x[i,"RScript.Request"], paste0("tests/request/request_", i, ".json"))
	writeLines(x[i,"RScript.Response"], paste0("tests/response/response_", i, ".json"))
}
