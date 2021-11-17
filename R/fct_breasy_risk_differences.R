breasy_risk_differences <- function(data) {
  for (a in 1:dim(data)[1]) {
         riskCI <- fmsb::riskdifference(data[a,]$NUMBER_EVENTS_VERUM, data[a,]$NUMBER_EVENTS_COMP, data[a,]$NUMBER_PATIENTS_VERUM, data[a,]$NUMBER_PATIENTS_COMP)
         data[a,]$EFFECT <- riskCI[["estimate"]]
         data[a,which(names(data) %in% c("LOWER95", "UPPER95"))] <- c(riskCI[["conf.int"]])
  }
  return(data)
}
