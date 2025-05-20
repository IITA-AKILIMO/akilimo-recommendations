# process_IC.R

# Process recommendations for Nigeria (NG)
process_IC_NG <- function(
  IC, country, areaHa, CMP, cobUP, fertilizers, riskAtt,
  maizePD, userName, userPhoneNr, userField, area,
  areaUnits, PD, HD, email, lat, lon, userPhoneCC,
  maizeUW, cassUW, saleSF, nameSF, rootUP, cassPD, maxInv,
  maizeUP, res, recText
) {
  print(paste("Processing IC for", country))

  # Generate IC recommendations
  res[["IC"]] <- getICrecommendations(
    areaHa = areaHa,
    CMP = CMP,
    cobUP = cobUP,
    fertilizers = fertilizers,
    riskAtt = riskAtt
  )

  if (nrow(res$IC[[2]]) > 0) {
    recText[["IC"]] <- getICrecText(ds = res$IC, maizePD)

    write.csv(res$IC, 'IC_rec.csv', row.names = FALSE)
    write.csv(recText$IC, 'IC_recText.csv', row.names = FALSE)

    IC_MarkdownText(
      rr = res$IC,
      fertilizers = fertilizers,
      userName = userName,
      country = country,
      userPhoneNr = userPhoneNr,
      userField = userField,
      area = area,
      areaUnits = areaUnits,
      PD = PD,
      HD = HD,
      email = email,
      lat = lat,
      lon = lon,
      userPhoneCC = userPhoneCC,
      maizeUW = maizeUW,
      maizePD = maizePD,
      cassUW = cassUW,
      saleSF = saleSF,
      nameSF = nameSF,
      rootUP = rootUP,
      cassPD = cassPD,
      maxInv = maxInv,
      CMP = CMP,
      maizeUP = maizeUP,
      riskAtt = riskAtt
    )

    fertilizerAdviseTable(
      FR = FALSE,
      IC = TRUE,
      country = country,
      areaUnits = areaUnits
    )

    ICrecom <- TRUE
  } else {
    recText[["IC"]] <- res[["IC"]]$rec$reason_F
    ICrecom <- FALSE
  }

  return(list(ICrecom = ICrecom, res = res, recText = recText))
}

# Process recommendations for Tanzania (TZ)
process_IC_TZ <- function(
  IC, country, areaHa, FCY, tuberUP, rootUP, fertilizers, riskAtt,
  userName, userPhoneNr, userPhoneCC, email,
  userField, area, areaUnits, PD, HD, lat, lon,
  sweetPotatoUP, sweetPotatoPD, sweetPotatoUW,
  cassUW, cassPD, maxInv,
  res, recText_input
) {
  recText <- recText_input
  print(paste("Processing IC for", country))

  # Generate CIS recommendations
  res[["IC"]] <- getCISrecommendations(
    areaHa = areaHa,
    FCY = FCY,
    tuberUP = tuberUP,
    rootUP = rootUP,
    fertilizers = fertilizers,
    riskAtt = riskAtt
  )

  if (nrow(res$IC[[2]]) > 0) {
    recText[["IC"]] <- getCISrecText(ds = res$IC)

    write.csv(recText$IC, 'CIS_recText.csv', row.names = FALSE)

    CIS_MarkdownText(
      rr = res$IC,
      fertilizers = fertilizers,
      userName = userName,
      country = country,
      userPhoneNr = userPhoneNr,
      userField = userField,
      area = area,
      areaUnits = areaUnits,
      PD = PD,
      HD = HD,
      email = email,
      lat = lat,
      lon = lon,
      userPhoneCC = userPhoneCC,
      sweetPotatoUP = sweetPotatoUP,
      sweetPotatoPD = sweetPotatoPD,
      sweetPotatoUW = sweetPotatoUW,
      rootUP = rootUP,
      cassUW = cassUW,
      cassPD = cassPD,
      maxInv = maxInv,
      tuberUP = tuberUP
    )

    fertilizerAdviseTable(
      FR = FALSE,
      IC = TRUE,
      country = "TZ",
      areaUnits = areaUnits
    )

    ICrecom <- TRUE
  } else {
    recText[["IC"]] <- res[["IC"]]$rec$reason_F
    ICrecom <- FALSE
  }

  return(list(ICrecom = ICrecom, plumberRes = res, recText = recText))
}