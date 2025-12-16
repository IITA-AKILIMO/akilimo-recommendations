
		

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
	fd <- try(rbind(fd, d_new))
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


### BELOW IS NOT USED ANYMORE


#' function to creat a data frame with fertilizers
#' @return: data frame with (type, N_cont, P_cont, K_cont, price) The NPK is elemental concentration and price is per kg of fertilizer
#' @example
#TODO: price of fertilizers for tanzania is different so from GPS we need to define the zone and take the correct price accordingly. default is at the end of the script
fertilizerFunc <- function(NPK201216available = TRUE, NPK201216CostperBag = NA, NPK201216BagWt = 50,
                           ureaavailable = TRUE, ureaCostperBag = NA, ureaBagWt = 50,
                           MOPavailable = TRUE, MOPCostperBag = NA, MOPBagWt = 50,
                           DAPavailable = TRUE, DAPCostperBag = NA, DAPBagWt = 50,
                           NPK201010available = TRUE, NPK201010CostperBag = NA, NPK201010BagWt = 50,
                           NPK151515available = TRUE, NPK151515CostperBag = NA, NPK151515BagWt = 50,
                           TSPavailable = TRUE, TSPCostperBag = NA, TSPBagWt = 50,
                           NPK171717available = FALSE, NPK171717CostperBag = NA, NPK171717BagWt = 50,
                           CANavailable = TRUE, CANCostperBag = NA, CANBagWt = 50,
                           SSPavailable = TRUE, SSPCostperBag = NA, SSPBagWt = 50,
                           NPK112221available = FALSE, NPK112221CostperBag = NA, NPK112221BagWt = 50,
                           NPK251010available = FALSE, NPK251010CostperBag = NA, NPK251010BagWt = 50,
                           NPK152020available = FALSE, NPK152020CostperBag = NA, NPK152020BagWt = 50,
                           NPK23105available = FALSE, NPK23105CostperBag = NA, NPK23105BagWt = 50,
                           NPK123017available = FALSE, NPK123017CostperBag = NA, NPK123017BagWt = 50,

                           FOMIIMBURAavailable = FALSE, FOMIIMBURACostperBag = NA, FOMIIMBURABagWt = 50,
                           FOMIBAGARAavailable = FALSE, FOMIBAGARACostperBag = NA, FOMIBAGARABagWt = 50,
                           FOMITOTAHAZAavailable = FALSE, FOMITOTAHAZACostperBag = NA, FOMITOTAHAZABagWt = 50,


							Nafakaavailable = FALSE, NafakaCostperBag = 0, NafakaBagWt = 50,


                           newFert1name = NA, newFert1N_cont = NA, newFert1P2O5 = NA, newFert1K2O = NA, newFert1CostperBag = 0, newFert1BagWt = NA,
                           newFert2name = NA, newFert2N_cont = NA, newFert2P2O5 = NA, newFert2K2O = NA, newFert2CostperBag = 0, newFert2BagWt = NA,
                           newFert3name = NA, newFert3N_cont = NA, newFert3P2O5 = NA, newFert3K2O = NA, newFert3CostperBag = 0, newFert3BagWt = NA,
                           newFert4name = NA, newFert4N_cont = NA, newFert4P2O5 = NA, newFert4K2O = NA, newFert4CostperBag = 0, newFert4BagWt = NA,
                           newFert5name = NA, newFert5N_cont = NA, newFert5P2O5 = NA, newFert5K2O = NA, newFert5CostperBag = 0, newFert5BagWt = NA, country, ...) {
	dots <- list(...)
	if (length(dots) > 0) {
		message(paste("arguments ignored by fertilizerFunc:", paste(names(dots), collapse=", ")))
	}

	Default_prices <- get_data("default_prices")
  if (country == "NG") {
    if (ureaCostperBag == 0) { ureaCostperBag <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "urea",]$Price } else { ureaCostperBag <- as.numeric(ureaCostperBag) }
    if (MOPCostperBag == 0) { MOPCostperBag <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "MOP",]$Price }else { MOPCostperBag <- as.numeric(MOPCostperBag) }
    if (DAPCostperBag == 0) { DAPCostperBag <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "DAP",]$Price }else { DAPCostperBag <- as.numeric(DAPCostperBag) }
    if (NPK201010CostperBag == 0) { NPK201010CostperBag <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "NPK201010",]$Price }else { NPK201010CostperBag <- as.numeric(NPK201010CostperBag) }
    if (NPK151515CostperBag == 0) { NPK151515CostperBag <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "NPK151515",]$Price }else { NPK151515CostperBag <- as.numeric(NPK151515CostperBag) }
    if (TSPCostperBag == 0) { TSPCostperBag <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "TSP",]$Price }else { TSPCostperBag <- as.numeric(TSPCostperBag) }
    if (NPK171717CostperBag == 0) { NPK171717CostperBag <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "NPK171717",]$Price }else { NPK171717CostperBag <- as.numeric(NPK171717CostperBag) }
    if (CANCostperBag == 0) { CANCostperBag <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "CAN",]$Price }else { CANCostperBag <- as.numeric(CANCostperBag) }
    if (SSPCostperBag == 0) { SSPCostperBag <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "SSP",]$Price }else { SSPCostperBag <- as.numeric(SSPCostperBag) }
    if (NPK201216CostperBag == 0) { NPK201216CostperBag <- Default_prices[Default_prices$Country == "NG" & Default_prices$Item == "NPK201216",]$Price }else { NPK201216CostperBag <- as.numeric(NPK201216CostperBag) }
  }else if (country == "TZ") {
    if (ureaCostperBag == 0) { ureaCostperBag <- Default_prices[Default_prices$Country == "TZ" & Default_prices$Item == "urea",]$Price }else { ureaCostperBag <- as.numeric(ureaCostperBag) } ##65000
    if (MOPCostperBag == 0) { MOPCostperBag <- Default_prices[Default_prices$Country == "TZ" & Default_prices$Item == "MOP",]$Price }else { MOPCostperBag <- as.numeric(MOPCostperBag) } ##120000
    if (DAPCostperBag == 0) { DAPCostperBag <- Default_prices[Default_prices$Country == "TZ" & Default_prices$Item == "DAP",]$Price }else { DAPCostperBag <- as.numeric(DAPCostperBag) } ##85000
    if (NPK201010CostperBag == 0) { NPK201010CostperBag <- Default_prices[Default_prices$Country == "TZ" & Default_prices$Item == "NPK201010",]$Price }else { NPK201010CostperBag <- as.numeric(NPK201010CostperBag) }
    if (TSPCostperBag == 0) { TSPCostperBag <- Default_prices[Default_prices$Country == "TZ" & Default_prices$Item == "TSP",]$Price }else { TSPCostperBag <- as.numeric(TSPCostperBag) } #80000
    if (NPK171717CostperBag == 0) { NPK171717CostperBag <- Default_prices[Default_prices$Country == "TZ" & Default_prices$Item == "NPK171717",]$Price }else { NPK171717CostperBag <- as.numeric(NPK171717CostperBag) } ##61000
    if (CANCostperBag == 0) { CANCostperBag <- Default_prices[Default_prices$Country == "TZ" & Default_prices$Item == "CAN",]$Price }else { CANCostperBag <- as.numeric(CANCostperBag) } #65000
  }else if (country == "RW") {
    if (ureaCostperBag == 0) { ureaCostperBag <- Default_prices[Default_prices$Country == "RW" & Default_prices$Item == "urea",]$Price }else { ureaCostperBag <- as.numeric(ureaCostperBag) } #564*50
    if (MOPCostperBag == 0) { MOPCostperBag <- Default_prices[Default_prices$Country == "RW" & Default_prices$Item == "MOP",]$Price }else { MOPCostperBag <- as.numeric(MOPCostperBag) } #558*50
    if (DAPCostperBag == 0) { DAPCostperBag <- Default_prices[Default_prices$Country == "RW" & Default_prices$Item == "DAP",]$Price }else { DAPCostperBag <- as.numeric(DAPCostperBag) } #633*50
    if (NPK171717CostperBag == 0) { NPK171717CostperBag <- Default_prices[Default_prices$Country == "RW" & Default_prices$Item == "NPK171717",]$Price }else { NPK171717CostperBag <- as.numeric(NPK171717CostperBag) } #713 *50
  }else if (country == "GH") {
    if (ureaCostperBag == 0) { ureaCostperBag <- Default_prices[Default_prices$Country == "GH" & Default_prices$Item == "urea",]$Price }else { ureaCostperBag <- as.numeric(ureaCostperBag) }
    if (NPK112221CostperBag == 0) { NPK112221CostperBag <- Default_prices[Default_prices$Country == "GH" & Default_prices$Item == "NPK112221",]$Price }else { NPK112221CostperBag <- as.numeric(NPK112221CostperBag) }
    if (NPK251010CostperBag == 0) { NPK251010CostperBag <- Default_prices[Default_prices$Country == "GH" & Default_prices$Item == "NPK251010",]$Price }else { NPK251010CostperBag <- as.numeric(NPK251010CostperBag) }
    if (NPK152020CostperBag == 0) { NPK152020CostperBag <- Default_prices[Default_prices$Country == "GH" & Default_prices$Item == "NPK152020",]$Price }else { NPK152020CostperBag <- as.numeric(NPK152020CostperBag) }
    if (NPK201010CostperBag == 0) { NPK201010CostperBag <- Default_prices[Default_prices$Country == "GH" & Default_prices$Item == "NPK201010",]$Price }else { NPK201010CostperBag <- as.numeric(NPK201010CostperBag) }
    if (NPK23105CostperBag == 0) { NPK23105CostperBag <- Default_prices[Default_prices$Country == "GH" & Default_prices$Item == "NPK23105",]$Price46 }else { NPK23105CostperBag <- as.numeric(NPK23105CostperBag) }
    if (NPK123017CostperBag == 0) { NPK123017CostperBag <- Default_prices[Default_prices$Country == "GH" & Default_prices$Item == "NPK123017",]$Price }else { NPK123017CostperBag <- as.numeric(NPK123017CostperBag) }
  }else if (country == "BU") {
    if (FOMIIMBURACostperBag == 0) { FOMIIMBURACostperBag <- Default_prices[Default_prices$Country == "BU" & Default_prices$Item == "FOMIIMBURA",]$Price } else { FOMIIMBURACostperBag <- as.numeric(FOMIIMBURACostperBag) }
    if (FOMIBAGARACostperBag == 0) { FOMIBAGARACostperBag <- Default_prices[Default_prices$Country == "BU" & Default_prices$Item == "FOMIBAGARA",] }else { FOMIBAGARACostperBag <- as.numeric(FOMIBAGARACostperBag) }
    if (FOMITOTAHAZACostperBag == 0) { FOMITOTAHAZACostperBag <- Default_prices[Default_prices$Country == "BU" & Default_prices$Item == "FOMITOTAHAZA",] }else { FOMITOTAHAZACostperBag <- as.numeric(FOMITOTAHAZACostperBag) }
  }


  ureaFert <- data.frame(type = 'Urea', available = ureaavailable, N_cont = 0.46, P_cont = 0, K_cont = 0, costPerBag = ureaCostperBag, bagWeight = ureaBagWt)
  MOPFert <- data.frame(type = 'MOP', available = MOPavailable, N_cont = 0.00, P_cont = 0.00, K_cont = 0.60, costPerBag = MOPCostperBag, bagWeight = MOPBagWt)
  DAPFert <- data.frame(type = 'DAP', available = DAPavailable, N_cont = 0.18, P_cont = 0.20, K_cont = 0.0, costPerBag = DAPCostperBag, bagWeight = DAPBagWt)
  NPK201010Fert <- data.frame(type = 'NPK20_10_10', available = NPK201010available, N_cont = 0.20, P_cont = 0.044, K_cont = 0.083, costPerBag = NPK201010CostperBag, bagWeight = NPK201010BagWt)
  NPK151515Fert <- data.frame(type = 'NPK15_15_15', available = NPK151515available, N_cont = 0.15, P_cont = 0.07, K_cont = 0.125, costPerBag = NPK151515CostperBag, bagWeight = NPK151515BagWt)
  TSPFert <- data.frame(type = 'TSP', available = TSPavailable, N_cont = 0.0, P_cont = 0.2, K_cont = 0.0, costPerBag = TSPCostperBag, bagWeight = TSPBagWt)
  NPK171717Fert <- data.frame(type = 'NPK17_17_17', available = NPK171717available, N_cont = 0.17, P_cont = 0.074, K_cont = 0.15, costPerBag = NPK171717CostperBag, bagWeight = NPK171717BagWt) # TODO get price
  NPK201216Fert <- data.frame(type = 'NPK20_12_26', available = NPK201216available, N_cont = 0.20, P_cont = 0.052, K_cont = 0.216, costPerBag = NPK201216CostperBag, bagWeight = NPK201216BagWt)
  CANFert <- data.frame(type = 'CAN', available = CANavailable, N_cont = 0.27, P_cont = 0.00, K_cont = 0.00, costPerBag = CANCostperBag, bagWeight = CANBagWt) ## not correct value TODO check
  SSPFert <- data.frame(type = 'SSP', available = SSPavailable, N_cont = 0.00, P_cont = 0.15, K_cont = 0.00, costPerBag = SSPCostperBag, bagWeight = SSPBagWt) ## not correct value TODO check

  FOMIIMBURAFert <- data.frame(type = 'FOMI_IMBURA', available = FOMIIMBURAavailable, N_cont = 0.09, P_cont = 0.0968, K_cont = 0.0332, costPerBag = FOMIIMBURACostperBag, bagWeight = FOMIIMBURABagWt)
  FOMIBAGARAFert <- data.frame(type = 'FOMI_BAGARA', available = FOMIBAGARAavailable, N_cont = 0.11, P_cont = 0.00, K_cont = 0.1826, costPerBag = FOMIBAGARACostperBag, bagWeight = FOMIBAGARABagWt) ## not correct value TODO check
  FOMITOTAHAZAFert <- data.frame(type = 'FOMI_TOTAHAZA', available = FOMITOTAHAZAavailable, N_cont = 0.21, P_cont = 0.00, K_cont = 0.0664, costPerBag = FOMITOTAHAZACostperBag, bagWeight = FOMITOTAHAZABagWt) ## not correct value TODO check


  NPK112221Fert <- data.frame(type = 'NPK112221', available = NPK112221available, N_cont = 0.11, P_cont = 0.1, K_cont = 0.17, costPerBag = 160, bagWeight = 50)
  NPK251010Fert <- data.frame(type = 'NPK251010', available = NPK251010available, N_cont = 0.25, P_cont = 0.044, K_cont = 0.083, costPerBag = 150, bagWeight = 50)
  NPK152020Fert <- data.frame(type = 'NPK152020', available = NPK152020available, N_cont = 0.15, P_cont = 0.088, K_cont = 0.166, costPerBag = 156, bagWeight = 50)
  NPK201010Fert <- data.frame(type = 'NPK201010', available = NPK201010available, N_cont = 0.20, P_cont = 0.044, K_cont = 0.083, costPerBag = 146, bagWeight = 50)
  NPK23105Fert <- data.frame(type = 'NPK23105', available = NPK23105available, N_cont = 0.23, P_cont = 0.044, K_cont = 0.0415, costPerBag = 146, bagWeight = 50)
  NPK123017Fert <- data.frame(type = 'NPK123017', available = NPK123017available, N_cont = 0.12, P_cont = 0.132, K_cont = 0.14, costPerBag = 160, bagWeight = 50)


  if (country == "NG") {
    fd_cont <- rbind(ureaFert, MOPFert, DAPFert, CANFert, NPK171717Fert, NPK151515Fert, NPK201010Fert, TSPFert, SSPFert, NPK201216Fert)
  }else if (country == "TZ") {
    fd_cont <- rbind(ureaFert, MOPFert, DAPFert, CANFert, NPK171717Fert, NPK151515Fert, NPK201010Fert, TSPFert, SSPFert)
  }else if (country == "RW") {
    fd_cont <- rbind(ureaFert, MOPFert, DAPFert, NPK171717Fert)
  }else if (country == "GH") {
    fd_cont <- rbind(ureaFert, NPK112221Fert, NPK251010Fert, NPK152020Fert, NPK201010Fert, NPK23105Fert, NPK123017Fert)
  }else if (country == "BU") {
    fd_cont <- rbind(FOMIIMBURAFert, FOMIBAGARAFert, FOMITOTAHAZAFert)
  }


  fd_cont <- droplevels(fd_cont[fd_cont$available == TRUE,])
  fd_cont$costPerBag <- as.numeric(fd_cont$costPerBag)
  fd_cont$price <- fd_cont$costPerBag / fd_cont$bagWeight
  fd_cont <- subset(fd_cont, select = -c(available))

  if (any(!is.na(c(newFert1name, newFert2name, newFert3name, newFert4name, newFert5name)))) {
  
    newfert <- data.frame(
		type = c(newFert1name, newFert2name, newFert3name, newFert4name, newFert5name),
        N_cont = as.numeric(c(newFert1N_cont, newFert2N_cont, newFert3N_cont, newFert4N_cont, newFert5N_cont)),
        P_cont = as.numeric(c(newFert1P2O5, newFert2P2O5, newFert3P2O5, newFert4P2O5, newFert5P2O5)),
        K_cont = as.numeric(c(newFert1K2O, newFert2K2O, newFert3K2O, newFert4K2O, newFert5K2O)),
        costPerBag = as.numeric(c(newFert1CostperBag, newFert2CostperBag, newFert3CostperBag, newFert4CostperBag, newFert5CostperBag)),
        bagWeight = as.numeric(c(newFert1BagWt, newFert2BagWt, newFert3BagWt, newFert4BagWt, newFert5BagWt)),
		price = 0
 	)


	notna <- rowSums(is.na(newfert)) == 0
	newfert <- newfert[notna, ]

	newfert$N_cont[is.na(newfert$N_cont)] <- 0
	newfert$P_cont <- round(0.44 * newfert$P_cont, 3)
	newfert$K_cont <- round(0.83 * newfert$K_cont, 3)
	newfert$P_cont[is.na(newfert$P_cont)] <- 0
	newfert$K_cont[is.na(newfert$K_cont)] <- 0
    newfert$price <- newfert$costPerBag / newfert$bagWeight
	
    fd_cont <- rbind(fd_cont, newfert)
  }
  return(fd_cont)
}




get_fertilizers <- function(body, country) {


	fert <- list(
		country = country,
		# UREA
		ureaavailable = process_json_value("ureaavailable", body),
		ureaCostperBag = process_json_value("ureaCostperBag", body),
		ureaBagWt = process_json_value("ureaBagWt", body, default_value = 50),

		# MOP
		MOPavailable = process_json_value("MOPavailable", body),
		MOPCostperBag = process_json_value("MOPCostperBag", body),
		MOPBagWt = process_json_value("MOPBagWt", body, default_value = 50),

		# DAP
		DAPavailable = process_json_value("DAPavailable", body),
		DAPCostperBag = process_json_value("DAPCostperBag", body),
		DAPBagWt = process_json_value("DAPBagWt", body, default_value = 50),

		# NPK 20-10-10
		NPK201010available = process_json_value("NPK201010available", body),
		NPK201010CostperBag = process_json_value("NPK201010CostperBag", body),
		NPK201010BagWt = process_json_value("NPK201010BagWt", body, default_value = 50),

		# NPK 20-12-16
		NPK201216available = process_json_value("NPK201216available", body),
		NPK201216CostperBag = process_json_value("NPK201216CostperBag", body),
		NPK201216BagWt = process_json_value("NPK201216BagWt", body, default_value = 50),

		# NPK 15-15-15
		NPK151515available = process_json_value("NPK151515available", body),
		NPK151515CostperBag = process_json_value("NPK151515CostperBag", body),
		NPK151515BagWt = process_json_value("NPK151515BagWt", body, default_value = 50),

		# TSP
		TSPavailable = process_json_value("TSPavailable", body),
		TSPCostperBag = process_json_value("TSPCostperBag", body),
		TSPBagWt = process_json_value("TSPBagWt", body, default_value = 50),

		# NPK 17-17-17
		NPK171717available = process_json_value("NPK171717available", body),
		NPK171717CostperBag = process_json_value("NPK171717CostperBag", body),
		NPK171717BagWt = process_json_value("NPK171717BagWt", body, default_value = 50),

		# Nafaka
		Nafakaavailable = process_json_value("Nafakaavailable", body),
		NafakaCostperBag = process_json_value("NafakaCostperBag", body),
		NafakaBagWt = process_json_value("NafakaBagWt", body, default_value = 50),

		# CAN
		CANavailable = process_json_value("CANavailable", body),
		CANCostperBag = process_json_value("CANCostperBag", body),
		CANBagWt = process_json_value("CANBagWt", body, default_value = 50),

		# SSP
		SSPavailable = process_json_value("SSPavailable", body),
		SSPCostperBag = process_json_value("SSPCostperBag", body),
		SSPBagWt = process_json_value("SSPBagWt", body, default_value = 50),

		# NPK 11-22-21
		NPK112221available = process_json_value("NPK112221available", body),
		NPK112221CostperBag = process_json_value("NPK112221CostperBag", body),
		NPK112221BagWt = process_json_value("NPK112221BagWt", body, default_value = 50),

		# NPK 25-10-10
		NPK251010available = process_json_value("NPK251010available", body),
		NPK251010CostperBag = process_json_value("NPK251010CostperBag", body),
		NPK251010BagWt = process_json_value("NPK251010BagWt", body, default_value = 50),

		# NPK 15-20-20
		NPK152020available = process_json_value("NPK152020available", body),
		NPK152020CostperBag = process_json_value("NPK152020CostperBag", body),
		NPK152020BagWt = process_json_value("NPK152020BagWt", body, default_value = 50),

		# NPK 23-10-5
		NPK23105available = process_json_value("NPK23105available", body),
		NPK23105CostperBag = process_json_value("NPK23105CostperBag", body),
		NPK23105BagWt = process_json_value("NPK23105BagWt", body, default_value = 50),

		# NPK 12-30-17
		NPK123017available = process_json_value("NPK123017available", body),
		NPK123017CostperBag = process_json_value("NPK123017CostperBag", body),
		NPK123017BagWt = process_json_value("NPK123017BagWt", body, default_value = 50),

		# FOMI IMBURA
		FOMIIMBURAavailable = process_json_value("FOMIIMBURAavailable", body),
		FOMIIMBURACostperBag = process_json_value("FOMIIMBURACostperBag", body),
		FOMIIMBURABagWt = process_json_value("FOMIIMBURABagWt", body, default_value = 50),

		# FOMI BAGARA
		FOMIBAGARAavailable = process_json_value("FOMIBAGARAavailable", body),
		FOMIBAGARACostperBag = process_json_value("FOMIBAGARACostperBag", body),
		FOMIBAGARABagWt = process_json_value("FOMIBAGARABagWt", body, default_value = 50),

		# FOMI TOTAHAZA
		FOMITOTAHAZAavailable = process_json_value("FOMITOTAHAZAavailable", body),
		FOMITOTAHAZACostperBag = process_json_value("FOMITOTAHAZACostperBag", body),
		FOMITOTAHAZABagWt = process_json_value("FOMITOTAHAZABagWt", body, default_value = 50),

		# newFert1
		newFert1name = process_json_value("newFert1name", body),
		newFert1N_cont = process_json_value("newFert1N_cont", body, NA),
		newFert1P2O5 = process_json_value("newFert1P2O5", body, NA),
		newFert1K2O = process_json_value("newFert1K2O", body, NA),
		#newFertCostperBag = process_json_value("newFertCostperBag", body), 
# should be this ?
		newFert1CostperBag = process_json_value("newFert1CostperBag", body, NA),
		newFert1BagWt = process_json_value("newFert1BagWt", body, default_value = 50),

		# newFert2
		newFert2name = process_json_value("newFert2name", body),
		newFert2N_cont = process_json_value("newFert2N_cont", body, NA),
		newFert2P2O5 = process_json_value("newFert2P2O5", body, NA),
		newFert2K2O = process_json_value("newFert2K2O", body, NA),
		newFert2CostperBag = process_json_value("newFert2CostperBag", body, NA),
		newFert2BagWt = process_json_value("newFert2BagWt", body, default_value = 50),

		# newFert3
		newFert3name = process_json_value("newFert3name", body),
		newFert3N_cont = process_json_value("newFert3N_cont", body, NA),
		newFert3P2O5 = process_json_value("newFert3P2O5", body, NA),
		newFert3K2O = process_json_value("newFert3K2O", body, NA),
		newFert3CostperBag = process_json_value("newFert3CostperBag", body, NA),
		newFert3BagWt = process_json_value("newFert3BagWt", body, default_value = 50),

		# newFert4
		newFert4name = process_json_value("newFert4name", body),
		newFert4N_cont = process_json_value("newFert4N_cont", body, NA),
		newFert4P2O5 = process_json_value("newFert4P2O5", body, NA),
		newFert4K2O = process_json_value("newFert4K2O", body, NA),
		newFert4CostperBag = process_json_value("newFert4CostperBag", body, NA),
		newFert4BagWt = process_json_value("newFert4BagWt", body, default_value = 50),

		# newFert5
		newFert5name = process_json_value("newFert5name", body),
		newFert5N_cont = process_json_value("newFert5N_cont", body, NA),
		newFert5P2O5 = process_json_value("newFert5P2O5", body, NA),
		newFert5K2O = process_json_value("newFert5K2O", body, NA),
		newFert5CostperBag = process_json_value("newFert5CostperBag", body, NA),
		newFert5BagWt = process_json_value("newFert5BagWt", body, default_value = 50)
	)

	do.call(fertilizerFunc, fert)
}
