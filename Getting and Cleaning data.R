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
