#####1
df <- read.csv("data/getdata_data_ss06hid.csv")
dim(df)
head(df)
agricultureLogical <- df$ACR == 3 & df$AGS == 6
agricultureLogical
which(agricultureLogical)


####2
library(jpeg)
imgUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(imgUrl, destfile = "data/w3q2.jpg", method = "auto")
readImg <- readJPEG("data/w3q2.jpg", native = T)
readImg
class(readImg)
quantile(readImg, probs = c(0.3,0.8))

####3
url1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(url1, destfile = "data/gdp.csv", method = "auto")
download.file(url2, destfile = "data/country.csv", method = "auto")
library(dplyr)


gdp <- read.csv("data/gdp.csv",skip = 5, header = F , nrows = 190)
dim(gdp)
head(gdp,10)
gdp <- select(gdp, c(1,2,4,5))
col.names=c("CountryCode", "Rank", "Economy", "Total")
gdp1 <- rename(gdp, CountryCode = V1, Rank = V2, Economy = V4, Total = V5)
str(gdp1)

country <- read.csv("data/country.csv")
str(country)
country <- select(country, c(1,2,3))

merge_df <- merge(gdp1,country, by = "CountryCode")
str(merge_df)
merge_df <- arrange(merge_df, desc(Rank))
head(merge_df)
nrow(merge_df)
merge_df[13,"Economy"]

#####4 depends on 3
merge_df$Income.Group <- factor(merge_df$Income.Group)
table(merge_df$Income.Group)

group_by(merge_df, Income.Group) %>% summarise(Average = mean(Rank, na.rm = T)) %>%
  arrange(Income.Group)



#####4 depends on 3, 4
merge_df$Rank.Group<-cut(merge_df$Rank, breaks = 5) 
head(merge_df)
table(merge_df$Rank.Group,merge_df$Income.Group)

      