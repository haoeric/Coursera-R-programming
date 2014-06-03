best <- function(state, outcome) {
  ## Read outcome data
  outdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  diseases <- c("heart attack", "heart failure", "pneumonia")
  disID <- c(11,17,23)
  states <- unique(outdata$State)
   
  ## Check that state and outcome are valid
  if (!(state %in% states)) stop("invalid state")
  if (!(outcome %in% diseases)) stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death rate
  choseDis <- disID[diseases == outcome]
  outdata[,choseDis] <- as.numeric(outdata[,choseDis])
  cleanOut <- outdata[which(!(is.na(outdata[,choseDis])) & outdata$State == state),]
  
  bestHos <- sort(cleanOut$Hospital.Name[which(cleanOut[,choseDis] == min(cleanOut[,choseDis]))])[1]
  return (bestHos)
}
