
this <- system('hostname', TRUE)
if (this == "denovo") {
	setwd("C:/github/omilika/akilimo-recommendations")
} else {
	
}

cmd <- 'curl -X POST http://localhost:8000/compute --data "@./tests/input/in_xxx.json"'
#cmd <- 'curl --ssl-no-revoke -X POST https://plumbr.akilimo.org/compute --data "@./tests/input/in_1.json"'

curl --ssl-no-revoke -X POST https://localhost:8000/compute --data "@./tests/input/in_1.json"

jout <- lapply(1:33, \(i) {
	print(system.time(out <- system(gsub("xxx", i, cmd), intern=TRUE)[4]))
	print(out); flush.console()
	out
})

##saveRDS(outtst, "../akilimo-recommendations/test_out.rds")


compare <- function(i) {
	z <- readLines(gsub("xxx", i, "tests/output/out_xxx.json")) |> jsonlite::fromJSON()
	if (length(z) == 0) {
		print("no comparison data")
	} else {
		print(i)
		n <- jsonlite::fromJSON(jout[[i]])
		print(n$data)
		cat("-----------\n")
		print(c(as.list(z[[1]][[1]]), as.list(z[[2]])))
		cat("+============+\n")
	}
}



#out <- lapply(1:27, \(i) system(gsub("xxx", i, cmd), intern=TRUE)[4])
#which(sapply(out, \(x) grepl("recommendation", x)))
#[1] 14 15 16 18 19 22

#outtime <- lapply(1:27, \(i) system.time(system(gsub("xxx", i, cmd), intern=TRUE)[4]))
#sapply(outtime, \(x) x["elapsed"])
# 8.70   26.70   26.67   26.91   28.78   27.95   11.00    8.31    8.33    2.92   27.58   31.14   30.05    0.54    0.55    0.54   11.00    0.56    0.56    8.32    8.69    0.44    8.29   10.63    8.23    8.25   10.87 
#sapply(outtime, \(x) x["elapsed"]) |> sum()
#[1] 342.51
 
#curl -X POST http://localhost:8000/compute -H "Content-Type: application/json" --data "@./tests/input/in_10.json"
