corr <- function(directory, threshold = 0) {
  fileList <- list.files(directory, pattern = ".csv", full.names = TRUE);
  df <- complete(directory)
  # print(str(df)) # 332 obs
  ids <- df[df["nobs"] > threshold,]$id # select dataframe rows , > threshold
  # print(ids)
  # str(ids) # 323 obs
  
  corrr <- numeric()
  
  for (i in ids) {
    data <- read.csv(fileList[i])
    complete_data <- data[complete.cases(data),]
    corrr <- c(corrr , cor(complete_data$sulfate, complete_data$nitrate))
  }
  # print(complete_data)
  return(corrr)
}
cr <- corr("specdata" , 150)
cr
# summary(cr)

