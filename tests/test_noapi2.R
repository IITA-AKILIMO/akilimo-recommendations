
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	akpath <- "C:/github/omilika/akilimo-recommendations"
} else {
	akpath <- "."
}
setwd(akpath)

srcdir <- file.path(akpath, "R")
testdir <- file.path(akpath, "tests")

run2 <- function(i) {
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

test2 <- function(i) {
	json <- readLines(paste0(testdir, gsub("xxx", i, "/request/request_xxx.json")))
	try(run_akilimo(json))
	json
}

for (f in grep("api", list.files(srcdir, pattern="\\.R$"), invert=TRUE, value=TRUE)) source(file.path(srcdir, f))

cmp2 <- vector(mode="list", 3203)
for (i in 1:3203) { cmp2[[i]] <- run2(i) }

#saveRDS(cmp2, file.path(testdir, "test_all1.rds"))
#cmp2 <- readRDS(file.path(testdir, "test_all1.rds"))
#rct <- sapply(cmp2, \(x) x$data$rec_type)
#s <- which(rct == "FR")

#for (j in s) {
r <- lapply(s, \(j) {
	x <- jsonlite::fromJSON(readLines(paste0(testdir, gsub("xxx", j, "/response/response_xxx.json"))))
	y <- cmp2[[j]]
	if (length(x) < 2) {
		#next
		return(TRUE)
	}
	if (is.data.frame(x)) {
		xr <- unlist(x[2,][[1]])
	} else {
		xr <- unlist(x[[2]])
	}
	names(xr) <- NULL
	yr <- as.character(y$data$recommendation)
	tst <- tinytest::expect_equal(gsub(" ", "", xr), gsub(" ", "", yr))
}
)
table(sapply(r, isTRUE))
#z <- jsonlite::fromJSON(readLines(paste0(testdir, gsub("xxx", i, "/response/response_xxx.json"))))

#out <- lapply(1:29, run)
