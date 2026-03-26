

get_fertilizers2 <- function(js, country) {


	get_df <- function(js) {
		nms <- names(js)
		ava <- grep("available$", nms, value=TRUE)
		ava <- data.frame(type=gsub("available$", "", ava), available=unlist(js[ava]))
		ava$type <- gsub("DOLOMITEA", "DOLOMITE", ava$type)
		
		cost <- grep("CostperBag$", nms, value=TRUE)
		cost <- data.frame(type=gsub("CostperBag$", "", cost), costPerBag=unlist(js[cost]))
		
		wt <- grep("BagWt$", nms, value=TRUE)
		wt <- data.frame(type=gsub("BagWt$", "", wt), bagWeight=unlist(js[wt]))
		
		fert <- merge(ava, cost, by="type", all.x=TRUE)
		fert <- merge(fert, wt, by="type", all.x=TRUE)
		
		fert
	}
	d <- get_df(js)
	d <- d[d$available, ]
	d$costPerBag[is.na(d$costPerBag)] <- 0
	i <- d$costPerBag == 0

	if (any(i)) {
		# NPK201226 price needs to be added for TZ and NG
		# NPK151515, SSP also missing for TZ

		#RH: these prices are bag prices. For what weight? 50 kg? If so, 
		# if the user specified bag weight is not 50, this price needs to be adjusted. 
		# or is that not allowed (either price and weight or nothing?)
		default_prices <- get_data("default_prices")
		default_prices <- default_prices[default_prices$Country == country, ]		
		m <- match(d$type, default_prices$Item)
		d$costPerBag[i] <- default_prices$Price[m[i]] 
	}

	#RH this should go to a file. Alternatively could be computed for NPK
	content <- data.frame(
		type = c("urea", "MOP", "DAP", "NPK201010", "NPK151515", "TSP", "NPK171717", "NPK201226", "CAN", "SSP", "FOMIIMBURA", "FOMIBAGARA", "FOMITOTAHAZA", "NPK112221", "NPK251010", "NPK152020", "NPK201216", "NPK23105", "NPK123017"), 
		N_cont = c(0.46, 0, 0.18, 0.2, 0.15, 0, 0.17, 0.2, 0.27, 0, 0.09, 0.11, 0.21, 0.11, 0.25, 0.15, 0.2, 0.23, 0.12), 
		P_cont = c(0, 0, 0.2, 0.044, 0.07, 0.2, 0.074, 0.052, 0, 0.15, 0.0968, 0, 0, 0.1, 0.044, 0.088, 0.0520, 0.044, 0.132), 
		K_cont = c(0, 0.6, 0, 0.083, 0.125, 0, 0.15, 0.216, 0, 0, 0.0332, 0.1826, 0.0664, 0.17, 0.083, 0.166, 0.132, 0.0415, 0.14)
	)

	#NPK ought to be followed by 6 numbers
	#this needs to be fixed upstream
	#d$type[d$type == "NPK23105"] <- "NPK231005"

	fd <- merge(d, content, by="type", all.x=TRUE)
	
	fd$price <- fd$costPerBag / fd$bagWeight
	fd$available <- NULL

# if (!all(is.na(c(newFert1name, newFert2name, newFert3name, newFert4name, newFert5name)))) {

	get_new <- function(js) {
		nms <- names(js)

		ava <- grep("^newFert.name$", nms, value=TRUE)
		if (length(ava) == 0) return(NULL)
		ntype <- gsub("^newFert.", "", js[ava])

		N <- grep("^newFert.N_cont", nms, value=TRUE)
		P2O5 <- grep("^newFert.P2O5", nms, value=TRUE)
		K2O <- grep("^newFert.K2O", nms, value=TRUE)
		cost <- grep("^newFert.CostperBag", nms, value=TRUE)
		wt <- grep("^newFert.BagWt", nms, value=TRUE)

		new <- data.frame(type=ntype, N_cont=unlist(js[N]), P2O5=unlist(js[P2O5]), K2O=unlist(js[K2O]), costPerBag=unlist(js[cost]), bagWeight=unlist(js[wt]))

        new$P_cont <- round(0.44 * new$P2O5, digits = 3)
        new$K_cont <- round(0.83 * new$K2O, digits = 3)
		new$P2O5 <- new$K2O <- NULL

		new$price <- new$costPerBag / new$bagWeight
		new
	}

	d_new <- get_new(js)
	fd <- tryCatch(
		rbind(fd, d_new),
		error = function(e) {
			warning("Failed to merge custom fertilizer data: ", e$message,
					" — custom fertilizers ignored")
			fd
		}
	)
	rownames(fd) <- NULL
	na <- rowSums(is.na(fd)) > 0
	if (any(na)) {
		message("missing values for fertilizer type: ", paste(fd$type[na], collapse=", "))
		fd <- fd[!na, ]
	}

	# backward compatibility to keep results verbatim the same
	i <- fd$type=="urea"
	if (any(i)) {
		fd$type[i] <- "Urea" 
		fd <- rbind(fd[i,], fd[!i,])
	}
	i <- grep("^NPK", fd$type)
	fd$type[i] <- paste0(substr(fd$type[i], 1, 5), "_", substr(fd$type[i], 6, 7),  "_", substr(fd$type[i], 8, 9))

	#RH there could be some sanity checking on the prices, to assure they are not outside a reasonable range

	fd
}

