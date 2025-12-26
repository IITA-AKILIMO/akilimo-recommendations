
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	akpath <- "C:/github/omilika/akilimo-recommendations"
} else {
	akpath <- "."
}
setwd(akpath)

srcdir <- file.path(akpath, "R")
testdir <- file.path(akpath, "tests")

cmp <- readRDS(file.path(testdir, "test_full.rds"))

test <- function(i, x) {
	y <- cmp[[i]]
	a <- tinytest::expect_equal(x$recommendation, y$recommendation)
	if (!a) {
		print(a)
		return(FALSE)
	} 
	b <- tinytest::expect_equivalent(x$data, y$data, tolerance=0.1)
	if (!b) {
		print(b)
		return(FALSE)
	}
	TRUE
}

run <- function(i) {
	flush.console()
	if (i %% 10 == 0) {	cat("+--- ", i, " ---+\n")	}
	json <- readLines(paste0(testdir, gsub("xxx", i, "/request/request_xxx.json")))
	run_akilimo(json)
}

for (f in grep("api", list.files(srcdir, pattern="\\.R$"), invert=TRUE, value=TRUE)) source(file.path(srcdir, f))

n <- 3203
out <- lapply(1:n, \(i) {r <- run(i); test(i, r)})

