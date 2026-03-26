
get_currency <- function(country) {
	m <- matrix(c("NG", "NGN", "RW", "RWF", "GH", "GHS", "BI", "BIF", "TZ", "TZS"), ncol=2, byrow=TRUE)
	i <- match(country, m[,1])
	m[i,2]
}


#SHORT DEF:   Function to convert root DM yield into root fresh matter yield (RFY)
#RETURNS:     RFY: root fresh yield in the same units as root DM yield input
#DESCRIPTION: Function to predict root FM yield based on date of harvest and country, using data from gravimetric starch measurements conducted across ACAI trials.
#INPUT:       HD: harvest date (Date format)
#             RDY: root dry matter yield (user's units)
#             country = c("NG", "TZ")

getRFY <- function(HD, RDY, country) {

  d <- as.numeric(strftime(HD, format = "%j"))
  #data.frame with day of the year (dayNr = [1..366]) and %DM (DMCont = [0..100], by country)
  fd <- get_data("dry_matter")
  DC <- merge(data.frame(dayNr = d), fd[fd$country == country,], sort = FALSE)$DMCont
  RFY <- RDY / DC * 100
  return(RFY)

}


#SHORT DEF:   Function to convert root FM yield into root dry matter yield (RDY): user define CY in FM in ton/ha, QUEFTS require Cy in DM kg/ha
#RETURNS:     RDY: root dry yield in the same units as root FM yield input
#INPUT:       HD: harvest date (Date format)
#             RFY: root fresh matter yield (user's units)
#             country = c("NG", "TZ")
## current yield is given by the user as FM ton/ha, we need to change it to DM in Kg/ha for QUEFTS

getRDY <- function(HD, RFY, country) {
  if (HD > 366) {
    HD <- HD - 366
  }
  fd <- get_data("dry_matter")
  DC <- merge(data.frame(dayNr = HD), fd[fd$country == country,], sort = FALSE)$DMCont
  RDY <- (RFY * DC) / 100
  return(RDY)
}



