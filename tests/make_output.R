
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	akpath <- "C:/github/omilika/akilimo-recommendations"
} else {
	akpath <- "."
}
setwd(akpath)

srcdir <- file.path(akpath, "R")
testdir <- file.path(akpath, "tests")

run <- function(i) {
	if (i %% 10 == 0) {	
		cat("+--- ", i, " ---+\n"); flush.console()
	}
	json <- readLines(paste0(testdir, gsub("xxx", i, "/request/request_xxx.json")))
	r <- try(run_akilimo(json))
	flush.console()
	if (inherits(r, "try-error")) {
		cat("ERROR",  i, " ---+\n"); flush.console()	
	}
	r
}

for (f in grep("api", list.files(srcdir, pattern="\\.R$"), invert=TRUE, value=TRUE)) source(file.path(srcdir, f))

#inf <- list.files(file.path(testdir, "request"), full=TRUE)
out <- vector(mode="list", 3203)
for (i in 1:3203) { out[[i]] <- run(i) }

which(sapply(out, \(z) grepl("^Error", z)[1]))
saveRDS(out, file.path(testdir, "test_full.rds"))
