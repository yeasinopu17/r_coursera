best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!any(state == data$State)) {
    stop("Invalid State")
  } else if (outcome %in% c("heart attack", "heart failure","pneumonia") == FALSE) {
    stop("Invalid Outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  data <- subset(data, State == state)
  if (outcome == "heart attack") {
    colnum <- 11 
  }
  else if (outcome == "heart failure") {
    colnum <- 17
  }
  else if (outcome == "pneumonia") {
    colnum <- 23
  }
  
  min_row <- which(as.numeric(data[, colnum]) == 
                     min(as.numeric(data[, colnum]), na.rm = TRUE ))
  
  hospital <- data[min_row,2]
  hospital <- sort(hospital)
  return(hospital[1])
}
best("SC", "heart attack")

