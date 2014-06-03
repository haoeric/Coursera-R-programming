rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  diseases <- c("heart attack", "heart failure", "pneumonia")
  disID <- c(11,17,23)
  states <- unique(outdata$State)
  
  ## Check that state and outcome are valid
  if (!(state %in% states)) stop("invalid state")
  if (!(outcome %in% diseases)) stop("invalid outcome")

  ## Return hospital name in that state with the given rank 30-day death rate
  choseDis <- disID[diseases == outcome]
  outdata[,choseDis] <- as.numeric(outdata[,choseDis])
  cleanOut <- outdata[which(!(is.na(outdata[,choseDis])) & outdata$State == state),]
  cleanOut <- cleanOut[order(cleanOut[[choseDis]], cleanOut$Hospital.Name),]
  
  selectNum <- nrow(cleanOut)
  
  if (num == "best"){
    return (cleanOut$Hospital.Name[1])
  }else if (num == "worst"){
    return (tail(cleanOut$Hospital.Name,1))
  }else if (num > 0 & num <= selectNum){
    return (cleanOut$Hospital.Name[num])
  } else{
    return (NA)
  }
}