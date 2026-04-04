

get_fertilizers2 <- function(js, country) {

	# Normalise UREA prefix to lowercase so both "UREAavailable" and
	# "ureaavailable" (etc.) are accepted from callers.
	urea_idx <- grepl("^UREA", names(js))
	names(js)[urea_idx] <- sub("^UREA", "urea", names(js)[urea_idx])

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

	content <- get_data("fertilizer_npk")

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

	# ── Type-name normalisation ───────────────────────────────────────────────
	# Input names come from the JSON request fields (e.g. "ureaavailable") after
	# the prefix is stripped, which always yields lowercase "urea". Downstream
	# PDF output expects title-case "Urea", and by convention Urea is
	# displayed first in fertilizer tables.
	i <- fd$type == "urea"
	if (any(i)) {
		fd$type[i] <- "Urea"
		fd <- rbind(fd[i, ], fd[!i, ])
	}

	# NPK type names arrive as a compact 8–9 character string, e.g. "NPK201010"
	# (meaning N=20%, P=10%, K=10%). They are reformatted to "NPK20_10_10" for
	# readability in PDF labels and for backward compatibility with existing
	# response consumers. Transformation: chars 1–5 + "_" + chars 6–7 + "_" + chars 8–9.
	# NOTE: types with a single-digit final component (e.g. "NPK23105") produce
	# "NPK23_10_5"; this is a known upstream naming inconsistency (see commented-out
	# fix above) and is preserved here to avoid breaking existing integrations.
	i <- grep("^NPK", fd$type)
	fd$type[i] <- paste0(substr(fd$type[i], 1, 5), "_", substr(fd$type[i], 6, 7), "_", substr(fd$type[i], 8, 9))

	#RH there could be some sanity checking on the prices, to assure they are not outside a reasonable range

	fd
}

