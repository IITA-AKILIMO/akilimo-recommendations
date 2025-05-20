process_PP <- function(
  PP, country,
  areaHa, costLMO, ploughing, ridging,
  method_ploughing, method_ridging,
  FCY, rootUP, riskAtt,
  userName, userPhoneNr, userPhoneCC,
  userField, area, areaUnits, PD, HD, email, lat, lon,
  cassPD, cassUW, maxInv,
  res, recText_input
) {
  recText <- recText_input
  print(paste("Processing PP for", country))

  # Generate PP recommendations
  res[["PP"]] <- getPPrecommendations(
    areaHa = areaHa,
    costLMO = costLMO,
    ploughing = ploughing,
    ridging = ridging,
    method_ploughing = method_ploughing,
    method_ridging = method_ridging,
    FCY = FCY,
    rootUP = rootUP,
    riskAtt = riskAtt
  )

  # Generate recommendation text
  recText[["PP"]] <- getPPrecText(ds = res$PP, country = country)

  # Write output files
  write.csv(res$PP, 'PP_rec.csv', row.names = FALSE)
  write.csv(recText$PP, 'PP_recText.csv', row.names = FALSE)

  # Generate markdown output
  PP_MarkdownText(
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
    rootUP = rootUP,
    cassPD = cassPD,
    cassUW = cassUW,
    maxInv = maxInv,
    ploughing = ploughing,
    ridging = ridging,
    method_ploughing = method_ploughing,
    method_ridging = method_ridging,
    userPhoneCC = userPhoneCC
  )

  return(list(PPrecom = TRUE, plumberRes = res, recText = recText))
}
