pollutantmean <- function(directory, pollutant, id = 1:132) {
  # path <- paste(getwd(),"/",directory,sep = "") # no need this
  fileList = list.files(directory, pattern = ".csv", full.names = TRUE)
  # paste(fileList)
  values <- vector()
  # paste(value)
  
  for (i in id) {
      data <- read.csv(fileList[i])
      values <- c(values, data[[pollutant]])
  }
  mean(values, na.rm = TRUE)
}
pollutantmean("specdata", "sulfate", 1:50)
