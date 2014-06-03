rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  outdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome are valid
  diseases <- c("heart attack", "heart failure", "pneumonia")
  disID <- c(11,17,23)
  if (!(outcome %in% diseases)) stop("invalid outcome")
  choseDis <- disID[diseases == outcome]
  
  ## For each state, find the hospital of the given rank
  states <- unique(outdata$State)
  hos <- character()
  sta <- character()
  i <- 1
  for (state in sort(states)){
    sta[i] <- state
    outdata[,choseDis] <- as.numeric(outdata[,choseDis])
    cleanOut <- outdata[which(!(is.na(outdata[,choseDis])) & outdata$State == state),]
    cleanOut <- cleanOut[order(cleanOut[[choseDis]], cleanOut$Hospital.Name),]   
    selectNum <- nrow(cleanOut)   
    if (num == "best"){
      hos[i] <- cleanOut$Hospital.Name[1]
    }else if (num == "worst"){
      hos[i] <- tail(cleanOut$Hospital.Name,1)
    }else if (num > 0 & num <= selectNum){
      hos[i] <- cleanOut$Hospital.Name[num]
    } else{
      hos[i] <- NA
    }
    i <- i + 1
  } 
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  rankout <- data.frame(hos, sta)
  colnames(rankout) <- c("hospital", "state")
  return (rankout)  
}
