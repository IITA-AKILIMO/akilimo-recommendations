
this <- system('hostname', TRUE)
if (this == "LAPTOP-IVSPBGCA") {
	akpath <- "C:/github/omilika/akilimo-recommendations"
} else {
	akpath <- "."
}
setwd(akpath)

srcdir <- file.path(akpath, "R")
testdir <- file.path(akpath, "tests")

cmp <- readRDS(file.path(testdir, "test_out4_lonlat.rds"))

test <- function(i, new) {
#	x <- jsonlite::fromJSON(cmp[[i]])$data
#	x <- out[[i]]$data
	x <- new$data
	y <- cmp[[i]]$data
#	a <- tinytest::expect_equal(x$recommendation, y$recommendation[1])
	a <- tinytest::expect_equal(x$recommendation, y$recommendation)
	if (!a) print(a)
	if ((length(x$recommendations) > 0) & (length(y$recommendations) > 0)) {
		b <- tinytest::expect_equivalent(x$recommendations, y$recommendations, tolerance=0.1)
		if (!b) print(b)
	} 
}

run <- function(i) {
	cat("+--- ", i, " ---+\n"); flush.console()
	json <- readLines(paste0(testdir, gsub("xxx", i, "/input/in_xxx.json")))
	run_akilimo(json)
}


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

#cmp2 <- vector(mode="list", 3203)
#for (i in 1:3203) { cmp2[[i]] <- run2(i) }
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

out <- lapply(1:29, \(i) {r <- run(i); test(i, r); r})

#timeout <- sapply(1:29, \(i) system.time(run(i))["elapsed"])
saveRDS(out, file.path(testdir, "test_out4.rds"))


add_new <- function(js) {
	js$newFert1name = "My Product"
	js$newFert1N_cont = .1
	js$newFert1P2O5 = .1
	js$newFert1K2O = .1
	js$newFert1CostperBag = 100 	
	js$newFert1BagWt = 25
	js$newFert2name = "Your Product"
	js$newFert2N_cont = .12
	js$newFert2P2O5 = .15
	js$newFert2K2O = .15
	js$newFert2CostperBag = 70
	js$newFert2BagWt = 45
	js
}

for (i in 1:27) {
	js <- readLines(paste0(testdir, gsub("xxx", i, "/input/in_xxx.json")))
	bd <- tryCatch(jsonlite::fromJSON(js), error = function(e) NULL)
	bd <- add_new(bd)
	a <- get_fertilizers(bd, bd$country)
	a <- a[order(a$type), ]
	b <- get_fertilizers2(bd, bd$country)[, c("type", "N_cont", "P_cont", "K_cont", "costPerBag", "bagWeight", "price")]
	b$type[b$type=="urea"] <- "Urea"
	b <- b[order(b$type), ]
	if ((nrow(a) != nrow(b)) || (!isTRUE(all(a == b)))) {
		print(a)
		print(b)
	}
	cat("= ", i, " ", bd$country, " ------\n")
}

