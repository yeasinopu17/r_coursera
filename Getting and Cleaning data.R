######################### Week -1 ####################################
## download file
#------------------
fileUrl <- "https://opendata.arcgis.com/datasets/7055dbb02f0c4f14ab7ea3eb5ebfda42_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"
download.file(fileUrl, destfile = "data/cameras.csv", method = "auto")
list.files("data")

## read local file
#------------------
cameraData <- read.table("data/cameras.csv", sep = ",", header = TRUE)
head(cameraData)


## read Excel file
#------------------
if (!file.exists("data")) {# check directory ; if not exists then create
  dir.create("data")
}

fileUrl <- "https://opendata.arcgis.com/datasets/7055dbb02f0c4f14ab7ea3eb5ebfda42_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"
download.file(fileUrl, destfile = "data/cameras.xlsx", method = "auto")
list.files("data")

#Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre6') # for 32-bit version
# xlsx library er jonno r and java version bit same hote hobe
library(xlsx)
camxl <- read.xlsx("data/cameras.xlsx", sheetIndex = 1)
str(camxl)

# reading specific rows and column
colIndx <- 2:5
rowIndx <- 1:10
camxl <- read.xlsx("data/cameras.xlsx", sheetIndex = 1, colIndex = colIndx, rowIndex = rowIndx)
str(camxl)# now i have 1 header + 9 row = 10 row from

## read XML file
#------------------
library(XML)
fileUrl <- "https://www.w3schools.com/xml/simple.xml"
doc <- xmlTreeParse("food.xml", useInternal = T)
doc
rootNode <- xmlRoot(doc)
rootNode
xmlName(rootNode)
names(rootNode)
rootNode[[1]]
rootNode [[1]][[1]]
xmlSApply(rootNode, xmlValue)

xpathSApply(rootNode, "//name", xmlValue)
xpathSApply(rootNode, "//price", xmlValue)

#read html file
filehtml <- "https://www.espn.in/nfl/team/_/name/bal/baltimore-ravens"
download.file(filehtml, destfile = "data/baltimore-ravens.html", method = "auto")
doc <- htmlParse("data/baltimore-ravens.html", useInternalNodes = TRUE)## dowloaded file chara kaj kortesena
doc
scores <- xpathSApply(doc, "//td[@class='right']", xmlValue)
scores


## read JSON file
#------------------
library(jsonlite)
jsonData <- fromJSON("https://api.github.com/users/jtleek/repos")
names(jsonData)
jsonData$owner$login

#writing a data frame to JSON
myJson <- toJSON(iris, pretty = T)
myJson
iris4 <- fromJSON(myJson)
iris4



## data.table package
#---------------------
library(data.table)
dt <- data.table(x = rnorm(9), y = rep(c("a", "b", "c"), each = 3), z = rnorm(9))
df <- data.frame(x = rnorm(9), y = rep(c("a", "b", "c"), each = 3), z = rnorm(9))
dt
tables()


#subsetting
dt[2,]
dt[dt$y == "a"]
dt[1:3]
dt[,1:2]
dt[, c(1,2)]


k = {print(10); 5}
k


dt[, list(mean(x),sum(x), mean(z))]
table(dt$y)


#adding new column
dt[, w:= z +1]
dt


#multiple operation
dt[, a:= {tmp <- (x+1); log2(tmp+5)}]
dt


#plyr like operator
dt[, b:= z < 0]
dt
dt[, d:= mean(a+w), by=b]
dt

#special variable
set.seed(123)
dt2 <- data.table(x = sample(letters[1:3], 1000, TRUE))
dt2
dt2[, .N]
dt2[, .N, by=x]


#keys
dt3 <- data.table(x = rep(c("a", "b", "c"), each = 100), int=runif(300,1,100))
dt3
setkey(dt3, x)
dt3['b']

######################### Week -2 ####################################

library(rhdf5)
created <- h5createFile("example.h5")
created
created <- h5createGroup("example.h5","foo")
created

created <- h5createGroup("example.h5","baa")
created <- h5createGroup("example.h5","baa/foobaa")

h5ls("example.h5")

A <- matrix(1:10, nrow = 5, ncol = 2)
A

h5write(A, "example.h5", "foo/A")
h5ls("example.h5")

B = array(seq(0.1, 2, by = 0.1), dim = c(5,2,2))
B

attr(B, "scale") <- "liter"
B

h5write(B, "example.h5", "foo/foobaa/B")


df <- data.frame(1L:5L, seq(0,1, length.out = 5), c("a","b","c","d","e"), stringsAsFactors = F)
df

h5write(df, "example.h5","df")
h5ls("example.h5")



con <- url("https://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en")
htmlcode <- readLines(con,10)
close(con)
htmlcode

library(XML)
url <- "https://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en"
html <- htmlTreeParse(url, useInternalNodes=T)
xpathSApply(html, "//title", xmlValue)

library(httr)
url <- "https://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en"
html2 <- GET(url)
content2 = content(html2, as="text")
content2
parsedHtml <- htmlParse(content2, asText = T)
parsedHtml
xpathSApply(parsedHtml, "//title",xmlValue)


goo <- handle("https://www.google.com/")
goo
p1 <- GET(handle = goo, path="/")
p2 <- GET(handle = goo, path="search")
p2

######################### Week -3 ####################################
set.seed(13435)
x <- data.frame("var1" = sample(1:5), "var2" = sample(6:10), "var3" = sample(11:15))
x
x$var2[c(1,3)] <- NA
x
x[x$var1 <= 3 & x$var3 > 11,]
x[x$var2 > 8,] # without which NA soho ase
x[which(x$var2 > 8 &  x$var1 >= 1 ) ,] # without which NA soho ase
which(c(T,F,T))

sort(x$var2) ## na exclude
sort(x$var2, na.last = T) ## now na asche

x[order(x$var1),]
order(x$var2)


if (!file.exists("data")) {dir.create("data")}
fileUrl <- "https://opendata.arcgis.com/datasets/53319332a909407e8ee52ae8ea79663d_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"
download.file(fileUrl, destfile = "data/restaurants.csv", method = "auto")
resData <- read.csv("data/restaurants.csv")
names(resData)
head(resData,10)
dim(resData)
library(dplyr)
resData <- select(resData, c(name,zipcode,nghbrhd,cncldst,plcdst, address) )
str(resData)
resData$plcdst <- factor(resData$plcdst)


table(resData$zipcode)
table(c(1,2,2,2,1,NA), useNA = "always")

sum(is.na(resData$cncldst))
any(is.na(resData$cncldst)) # Given a set of logical vectors, is at least one of the values true
any(c(F,F)) # return F
any(c(F,F,T)) # return T
all(c(F,F,T)) # return F ;Given a set of logical vectors, are all of the values true
all(c(T,T,T)) # return T ; because all are True

is.na(resData) # all column er T or F return korbe
colSums(is.na(resData)) #Every col er T or F sum hobe
class(colSums(is.na(resData))) # retrun numberic vector
colSums(is.na(resData)) == 0
all(colSums(is.na(resData)) == 0) # all value T kina check kortese


data(UCBAdmissions)
df <- as.data.frame(UCBAdmissions)
df
summary(df)
xt <- xtabs(Freq ~ Gender + Admit, data = df)
xt


warpbreaks
xt <- xtabs(breaks ~. , data = warpbreaks)
xt
ftable(xt)


resData$nearMe <- resData$nghbrhd %in% c("Roland Park","Homeland")
table(resData$nearMe)

resData$zipWrong <- ifelse(resData$zipcode < 0 , T, F)
table(resData$zipWrong)

resData$zipGroups <- cut(as.numeric(resData$zipcode), breaks = quantile(resData$zipcode))
table(resData$zipGroups)
table(resData$zipGroups,resData$zipcode)


library(Hmisc)
resData$zipGroups <- cut2(resData$zipcode, g=4)
table(resData$zipGroups)

yesno <- sample(c("Yes", "No"), size = 10, replace = T)
yesno
yesnofac <- factor(yesno)
yesnofac
relevel(yesnofac, ref = "Yes")#Reorder Levels of Factor

# reshaping
library(reshape2)
head(mtcars)
mtcars2 <- mtcars

row.names(mtcars2)# this return row names
rownames(mtcars2)# this return row names

mtcars2$carname <- rownames(mtcars2); 
head(mtcars2,3)
dim(mtcars2)

# here measure.vars er karone variable and value name a col create hoise
# and variable value hisabe gese mpg and hp ; and value te gese mpg and hp col er value
# row number double hoise
# see melt.data.frame {reshape2}
carMelt <- melt(mtcars2, id=c("carname","gear","cyl"), measure.vars = c("mpg","hp"))
carMelt # now row num double hoise, compare to "mtcars2" data set
carMelt[carMelt$carname == "Mazda RX4",]
mtcars2[mtcars2$carname == "Mazda RX4",]

#dcast
# onk ta group by er motho, here group by cyl, count(variable)
# by default count korbe, jodi fun.aggregate use na kori
# data frame ta molten hote hobe
cyldata <- dcast(carMelt, cyl ~ variable)
carMelt[carMelt$cyl == 6,] # cyl 6 a mpg er ase 7 ta and hp te ase 7 ta

cyldata <- dcast(carMelt, cyl ~ variable, mean)
cyldata
carMelt[carMelt$cyl == 6,]

head(InsectSprays)
str(InsectSprays)
tapply(InsectSprays$count, InsectSprays$spray, sum)#InsectSprays$spray is a factor

sprIns <- split(InsectSprays$count, InsectSprays$spray)
sprIns
sprCount <- lapply(sprIns, sum)
sprCount
unlist(sprCount) # list ke unlist kora hoise



######################### Week -4 ####################################
camera <- read.csv("data/cameras.csv")
dim(camera)
names(camera)
toupper(names(camera))

names(camera)
splitNames <- strsplit(names(camera), "\\.")
splitNames


sub("_","*","name_id_yeasin") #only first er ta replace hobe
gsub("_","*","name_id_yeasin") # all element are replaced

names(camera)
camera[,"intersecti"]
grep("Alameda", camera$intersecti)# kon kon index a Alameda ase
grep("Alameda", camera$intersecti, value = T) # here value para er karone value return korche
grepl("Alameda", camera$intersecti) # Return True and False


library(stringr)
nchar("Yeasin Opu")
str_trim("Yeasin  ")

d1 <- date()
d1
class(d1)

d2 <- Sys.Date()
d2
class(d2)

library(lubridate)
ymd("20110721")
class(ymd("20110721"))
Sys.timezone()

dmYY <- dmy(c("1jan2021","1feb2021"))
wday(dmYY, label = T)

