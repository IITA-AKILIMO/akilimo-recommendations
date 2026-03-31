
this <- system('hostname', TRUE)
if (this == "denovo") {
	akpath <- "C:/github/omilika/akilimo-recommendations"
} else {
	akpath <- "."
}
setwd(akpath)

srcdir <- file.path(akpath, "R")
testdir <- file.path(akpath, "tests")

cmp <- readRDS(file.path(testdir, "test_small.rds"))

test <- function(i, x) {
	y <- cmp[[i]]
	a <- tinytest::expect_equal(x$recommendation, y$recommendation)
	if (!a) print(a)
	b <- tinytest::expect_equivalent(x$data, y$data, tolerance=0.1)
	if (!b) print(b)
}

run <- function(i) {
	cat("+--- ", i, " ---+\n"); flush.console()
	json <- readLines(paste0(testdir, gsub("xxx", i, "/input/in_xxx.json")))
	run_akilimo(json)
}

for (f in list.files(srcdir, pattern="\\.R$", full=TRUE)) source(file.path(f))

out <- lapply(1:29, \(i) {r <- run(i); test(i, r); r})

#timeout <- sapply(1:29, \(i) system.time(run(i))["elapsed"])
#saveRDS(out, file.path(testdir, "test_out6.rds"))

