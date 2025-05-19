process_FR <- function(FR, lat, lon, pd, pw, HD, had, maxInv, fertilizers, rootUP, areaHa, country, FCY, riskAtt,
                       userName, userPhoneNr, userField, area, areaUnits, PD, email, userPhoneCC,
                       cassPD, cassUW, recText, plumberRes, frnotrec_ng, frnotrec_tz, frnotrec_rw) {

  no_fr_recommendation_countries <- c("NG", "GH", "TZ", "RW")
  no_recommendation_msg <- "We do not have fertilizer recommendation for your location because your location is out of the recommendation domain AKILIMO is currently serving."
  FRrecom <- NULL

  cat(paste("Processing FR", FR))
  print("Processing FR section now")

  plumberRes$FR <- getFRrecommendations(
    lat = lat, lon = lon, pd = pd, pw = pw, HD = HD, had = had, maxInv = maxInv,
    fertilizers = fertilizers, rootUP = rootUP, areaHa = areaHa, country = country,
    FCY = FCY, riskAtt = riskAtt
  )

  cat("Finished processing")

  if (all(plumberRes$FR == no_recommendation_msg)) {
    if (country %in% no_fr_recommendation_countries) {
      FRrecom <- FALSE
      recText[["FR"]] <- plumberRes$FR
    }
  } else {
    if (plumberRes[["FR"]]$rec$NR > 0) {
      FRrecom <- TRUE
      recText[["FR"]] <- getFRrecText(
        ds = plumberRes$FR,
        country = country,
        fertilizers = fertilizers,
        rootUP = rootUP
      )
      write.csv(recText$FR, 'FR_recText.csv', row.names = FALSE)

      FR_MarkdownText(
        rr = plumberRes$FR, fertilizers = fertilizers, userName = userName,
        country = country, userPhoneNr = userPhoneNr, userField = userField,
        area = area, areaUnits = areaUnits, PD = PD, HD = HD, email = email,
        lat = lat, lon = lon, userPhoneCC = userPhoneCC,
        rootUP = rootUP, cassPD = cassPD, cassUW = cassUW, maxInv = maxInv
      )

      fertilizerAdviseTable(FR = TRUE, IC = FALSE, country = country, areaUnits = areaUnits)
    } else {
      FRrecom <- FALSE
      recText[["FR"]] <- switch(
        country,
        "NG" = frnotrec_ng,
        "GH" = frnotrec_ng,  # Assuming GH shares with NG
        "RW" = frnotrec_rw,
        "TZ" = frnotrec_tz,
        "No recommendation available"
      )
    }
  }

  return(list(FRrecom = FRrecom, recText = recText, plumberRes = plumberRes))
}
