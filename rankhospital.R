rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!any(state == data$State)) {
    stop(print("Invalid State"))
  } else if (outcome %in% c("heart attack", "heart failure","pneumonia") == FALSE) {
    stop(print("Invalid outcome"))
  }
  
  ## determine column number
  if (outcome == "heart attack") {
    colnum <- 11 
  } else if (outcome == "heart failure") {
    colnum <- 17
  } else{
    colnum <- 23
  }
  
  # select state
  data <- subset(data, State == state)
  
  # covert to number
  data[, colnum] <- as.numeric(data[, colnum])
  
  srl <- order(data[,colnum], data[,2]) # index of ordered row
  data_ordered <- data[srl, ] # ordered rows asc
  data_ordered <- data_ordered[!is.na(data_ordered[,colnum]),] # exclude na
  
  
  if (num == "best") {
    num <- 1
  } else if (num == "worst") {
    num <- nrow(data_ordered)
  }
  

  return(data_ordered[num,2])
}

#rankhospital("TX", "heart failure", 4)
