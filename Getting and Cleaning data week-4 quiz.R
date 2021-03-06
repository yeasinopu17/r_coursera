#####1
df <- read.csv("data/getdata_data_ss06hid.csv")
str(df)
names(df)
strsplit(names(df),"wgtp")[[123]]

######2
library(dplyr)
gdp <- read.csv("data/gdp.csv",skip = 5, header = F , nrows = 190)
dim(gdp)
head(gdp,10)
gdp <- select(gdp, c(1,2,4,5))
col.names=c("CountryCode", "Rank", "Economy", "Total")
gdp1 <- rename(gdp, CountryCode = V1, Rank = V2, Economy = V4, Total = V5)
str(gdp1)
gdp1$Total2 <- as.numeric(gsub(",","", gdp1$Total))
gdp1[is.na(gdp1$Total2),]
mean(gdp1$Total2)


######3
grep("^United",gdp1$Economy)

#####4
country <- read.csv("data/country.csv")
country <- select(country, c(1,2,3,10))
names(country)
str(country)
merge_df <- merge(gdp1,country, by = "CountryCode")
merge_df
str(merge_df)
fiscialJune <- grep("Fiscal year end: June",merge_df$Special.Notes)
NROW(fiscialJune)


######5
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
head(amzn)
sampleTimes = index(amzn)
sampleTimes
class(sampleTimes)

library(lubridate)
amzn2021 <- sampleTimes[grep("^2012", sampleTimes)]
amzn2021
NROW(amzn2021)
weekdays(amzn2021) == "Monday"
NROW(amzn2021[weekdays(amzn2021) == "Monday"])
