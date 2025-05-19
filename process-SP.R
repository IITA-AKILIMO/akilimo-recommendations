process_SP <- function(
  SPP, SPH, country, PD_window, HD_window, areaHa, lat, lon, PD, HD, saleSF, nameSF, FCY,
  rootUP, rootUP_m1, rootUP_m2, rootUP_p1, rootUP_p2, userName, userPhoneNr, userPhoneCC, userField,
  area, areaUnits, email, maxInv, ploughing, ridging, method_ploughing, method_ridging, CMP, riskAtt,
  cassPD, cassUW, cassUP, cassUP_m1, cassUP_m2, cassUP_p1, cassUP_p2, res, recText
) {
  SPrecom <- NULL

  if (PD_window == 0 && HD_window == 0) {
    SPrecom <- FALSE
    recText[["SP"]] <- if (country %in% c("NG", "GH")) {
      "AKILIMO provides advice for schedule planting if only at least your planting or harvest time or both are flexible. Please provide this information and you will be advised when the best time is for your location."
    } else {
      "AKILIMO hutoa ushauri wa upandaji wa ratiba ikiwa angalau wakati wako wa upandaji au wakati wa kuvuna au zote mbili zinabadilika. Tafadhali toa habari hii na utashauriwa wakati mzuri wa kupanda na kuvuna kwa eneo lako"
    }
  } else {
    print("Processing SP")

    res[["SP"]] <- getSPrecommendations(
      areaHa = areaHa,
      country = country,
      lat = lat,
      lon = lon,
      PD = PD,
      HD = HD,
      PD_window = PD_window,
      HD_window = HD_window,
      saleSF = saleSF,
      nameSF = nameSF,
      FCY = FCY,
      rootUP = rootUP,
      rootUP_m1 = rootUP_m1,
      rootUP_m2 = rootUP_m2,
      rootUP_p1 = rootUP_p1,
      rootUP_p2 = rootUP_p2
    )

    if (!is.data.frame(res[["SP"]])) {
      SPrecom <- FALSE
      recText[["SP"]] <- res$SP
    } else {
      SPrecom <- TRUE
      recText[["SP"]] <- getSPrecText(
        ds = res$SP,
        country = country,
        PD = PD,
        HD = HD
      )

      write.csv(recText$SP, 'SP_recText.csv', row.names = FALSE)

      SP_MarkdownText(
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
        saleSF = saleSF,
        nameSF = nameSF,
        maxInv = maxInv,
        ploughing = ploughing,
        ridging = ridging,
        method_ploughing = method_ploughing,
        method_ridging = method_ridging,
        userPhoneCC = userPhoneCC,
        CMP = CMP,
        riskAtt = riskAtt,
        PD_window = PD_window,
        HD_window = HD_window,
        cassPD = cassPD,
        cassUW = cassUW,
        cassUP = cassUP,
        cassUP_m1 = cassUP_m1,
        cassUP_m2 = cassUP_m2,
        cassUP_p1 = cassUP_p1,
        cassUP_p2 = cassUP_p2
      )
    }
  }

  return(list(SPrecom = SPrecom, plumberRes = res, recText = recText))
}
