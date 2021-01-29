rankall<- function(outcome, num = "best")
{
  library(dplyr)
  library(magrittr)
  outcome2 <- read.csv("outcome-of-care-measures.csv",
                       colClasses = "character")
  if((outcome %in% c("heart attack", "heart failure",
                     "pneumonia")) == FALSE) {
    stop(print("invalid outcome"))
  }
  if (outcome == "heart attack") {
    colnum <- 11
  }
  else if (outcome == "heart failure") {
    colnum <- 17
  }
  else {
    colnum <- 23
  }
  outcome2[ ,colnum] <- as.numeric(outcome2[ ,colnum])
  
  outcome2 = outcome2[!is.na(outcome2[,colnum]),]
  
  splited = split(outcome2, outcome2$State)
  # print(summary(splited))
  
  ans = lapply(splited, function(x, num) {
    x = x[order(x[,colnum], x$Hospital.Name),]
    
    if(class(num) == "character") {
      if(num == "best") {
        return (x$Hospital.Name[1])
      }
      else if(num == "worst") {
        return (x$Hospital.Name[nrow(x)])
      }
    }
    else {
      return (x$Hospital.Name[num])
    }
  }, num)
  
  #Return data.frame with format
  return ( data.frame(hospital=unlist(ans), state=names(ans)) )
}
head(rankall("heart failure"),1)
