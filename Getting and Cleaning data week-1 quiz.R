########1######
library(data.table)
data <- data.table::fread("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv")
data[VAL == 24, .N]


#########3########
library(xlsx)
dat <- read.xlsx("data/getdata_data_DATA.gov_NGAP.xlsx", sheetIndex = 1, rowIndex = 18:23, colIndex = 7:15)
dim(dat)
sum(dat$Zip*dat$Ext,na.rm=T)
