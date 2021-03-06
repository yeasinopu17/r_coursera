# chapter 5 #
#===========
library(tidyverse)
ozone <- read_csv("data/hourly_42101_2014.csv",n_max = 100000, col_types = cols(
  `State Code` = col_character(),
  `County Code` = col_character(),
  `Site Num` = col_character(),
  `Parameter Code` = col_double(),
  POC = col_double(),
  Latitude = col_double(),
  Longitude = col_double(),
  Datum = col_character(),
  `Parameter Name` = col_character(),
  `Date Local` = col_date(format = ""),
  `Time Local` = col_character(),
  `Date GMT` = col_date(format = ""),
  `Time GMT` = col_character(),
  `Sample Measurement` = col_double(),
  `Units of Measure` = col_character(),
  MDL = col_double(),
  Uncertainty = col_double(),
  Qualifier = col_double(),
  `Method Type` = col_character(),
  `Method Code` = col_character(),
  `Method Name` = col_character(),
  `State Name` = col_character(),
  `County Name` = col_character(),
  `Date of Last Change` = col_date(format = ""))
)
names(ozone) <- make.names(names(ozone))
spec(ozone) # see full col specification
dim(ozone)
str(ozone)

head(ozone[, c(6:7,10)])
tail(ozone[, c(6:7,10)])

table(ozone$Time.Local)

names(ozone)
filter(ozone, Time.Local == "13:00") %>% 
  select(State.Name,County.Name,Date.Local,Sample.Measurement,Time.Local)

filter(ozone, State.Code == "01" & County.Code == "073" & Date.Local == "2014-09-30") %>%
  select(Date.Local,Time.Local,Sample.Measurement) %>% 
  as.data.frame() # data frame use korlei full data show kore

select(ozone, State.Name) %>% unique

summary(ozone$Sample.Measurement)

quantile(ozone$Sample.Measurement, probs = seq(0,1,0.10))

ranking <- group_by(ozone, State.Name, County.Name) %>% 
  summarise(ozon = mean(Sample.Measurement)) %>%
  as.data.frame %>% # here df a convert kore arrange kora hoise
  arrange(desc(ozon))
ranking

class(ozone$Date.Local)
ozone <- mutate(ozone, month = factor(months(Date.Local), levels = month.name))


str(ozone)
table(ozone$State.Name,ozone$County.Name)

filter(ozone, State.Name == "Alaska" & County.Name == "Fairbanks North Star") %>%
  group_by(month) %>%
  summarise(ozo = mean(Sample.Measurement))


set.seed(10234)
N <- nrow(ozone)
idx <- sample(N,N,replace = T)


# chapter 7 #
#===========
colType <- c("numeric","character","factor","numeric","numeric")
pollution <- read.csv("data/avgpm25.csv", colClasses = colType)
head(pollution)
str(pollution)

summary(pollution$pm25)
fivenum(pollution$pm25)

boxplot(pollution$pm25, col = "blue")
abline(h=12)
filter(pollution, pm25 > 15)

library(maps)
names(pollution)
map("county", "california")
with(filter(pollution, pm25 > 15), points(longitude,latitude))

hist(pollution$pm25, col = "green")
rug(pollution$pm25)

hist(pollution$pm25, col = "green", breaks = 100)
rug(pollution$pm25)
abline(v = 12, lwd = 2, col = "yellow")
abline(v = mean(pollution$pm25), col = "pink", lwd = 2)

table(pollution$region) %>% barplot(col= "wheat")

boxplot(pm25 ~ region, data= pollution, col = "red")# here use formula

par(mfrow = c(2,1), mar = c(4,4,2,1))# see help
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "red")

plot(pollution$latitude, pollution$pm25)# aie vabeo use kora jabe
with(pollution, plot(latitude, pm25))
abline(h = 12, lwd = 2, lty = 2)

with(pollution, plot(latitude, pm25, col= region))# region onushare col hobe
abline(h = 12, lwd = 2, lty = 4)

levels(pollution$region)#levels provides access to the levels attribute of a variable

#main = a main title for the plot; individual color assign kora jai
par(mfrow = c(1,2), mar = c(5,4,2,1))
with(subset(pollution, region = "west"), plot(latitude,pm25, col = "green", main = "West"))
with(subset(pollution, region = "east"), plot(latitude,pm25, col = "red", main = "East"))

library(lattice)
xyplot(pm25 ~ latitude | region, data = pollution)# help

library(ggplot2)
qplot(latitude, pm25, data = pollution, facets = .~ region) # here group by para = facets 

# chapter 8 #
#===========
data(airquality)
names(airquality)
with(airquality, {
  plot(Temp, Ozone)
  lines(loess.smooth(Temp, Ozone))
})

data(cars)
head(cars)
with(cars, plot(speed, dist))
title("Speed vs Stop")

head(state.x77)
state <- data.frame(state.x77, region = state.region)
head(state)
library(lattice)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4,1) )# layout = 4 col, 1 row

library(ggplot2)
data(mpg)
head(mpg)
qplot(displ, hwy, data = mpg)


# chapter 9 #
#===========
?Devices
library(ggplot2)
library(lattice)
faithful
with(faithful, plot(eruptions, waiting))
title("Old Faithful Geyser Data")

pdf(file = "myPlot.pdf") #starts the graphics device driver for producing PDF graphics
with(faithful, plot(eruptions, waiting))
title("Old Faithful Geyser Data")
dev.off()#see help

dev.cur()#see help
dev.copy()
dev.copy2pdf()

with(faithful, plot(eruptions, waiting))
title("Old Faithful Geyser Data")
dev.copy(png, file = "test.png")
dev.off()

# chapter 10 #
#=============
airquality
str(airquality)
airquality2 <- transform(airquality, Month = factor(Month))# month ke factor a transform korse
str(airquality2)
boxplot(Ozone ~ Month, data = airquality2, xlab = "Month",
        ylab = "Ozone(ppb)") #xlab and ylab is : x- and y-axis annotation


with(airquality2, plot(Wind, Ozone))#scatter plot a with use kortei hobe

par("oma")

with(airquality, plot(Wind, Ozone, 
                      main = "Ozone and Wind in New York City"))
with(subset(airquality, Month == 5), 
     points(Wind, Ozone, col = "blue"))#points is a generic function to draw a sequence of points at the specified coordinates

#here first without point create plot, then pointing
with(airquality, plot(Wind, Ozone, type = "n", main = "Wind and Ozone"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1, col = c("blue","red"),legend = c("MAY","Other Month"))

with(airquality, plot(Wind, Ozone, main = "Wind and Ozone", pch = 20))
model <- lm(Ozone ~ Wind, airquality)
model
summary(model)
abline(model, lwd = 2)

par(mfrow = c(1,3), mar=c(4,4,2,1), oma = c(0,0,2,0))
with(airquality, {
  plot(Wind, Ozone, main = "Ozone & Wind")
  plot(Solar.R, Ozone, main = "Ozone & Solar")
  plot(Temp, Ozone, main = "Ozone & Temp")
  mtext("Ozone and Weather in New York City",outer = T)#Write Text into the Margins of a Plot; Outer = T imp
})

# chapter 11 #
#=============
#11.1
set.seed(19)
x <- rnorm(30)
y <- rnorm(30)
plot(x,y, col = rep(1:3, each = 10), pch = 19)
legend("bottomright", legend = paste("Group", 1:3), col = 1:3, 
       pch = 19, bty = "n")

head(volcano)
image(volcano)
heat.colors(10)
par(mfrow = c(1,2))
image(volcano, col = heat.colors(10), main = "heat.colors()")
image(volcano, col = topo.colors(10), main = "topo.colors()")
#------
colors()
pal <- colorRamp(c("red","blue"))
pal(1)

pal <- colorRampPalette(c("red", "yellow"))
pal(20)
pal <- colorRampPalette(c("red", "yellow","green","orange"))
pal(20)

rgb(0,0,234, maxColorValue = 255)

library(RColorBrewer)
display.brewer.all()
display.brewer.pal(n = 5, name = 'Dark2')
brewer.pal(3, "BuGn")
display.brewer.pal(n = 9, name = 'BuGn')

cols = brewer.pal(3, "BuGn")
cols
pal <- colorRampPalette(cols)
image(volcano, col = pal(20))

set.seed(1)
x <- rnorm(10000)
y <- rnorm(10000)
smoothScatter(x,y)

plot(x , y, pch = 19, col = rgb(0,0,0,0.15))

# chapter 12 #
#=============
#https://www.analyticsvidhya.com/blog/2019/05/beginners-guide-hierarchical-clustering/

set.seed(1234)
x <- rnorm(12, rep(1:3, each = 4), 0.2)# second para is mean
x
mean(x)
y <- rnorm(12, rep(c(1,2,1), each = 4), 0.2)
plot(x,y, col = "blue", pch = 19, cex = 2)# cex = dot in plot kotho boro hobe, see help
text(x + 0.05, y + 0.05, labels = as.character(1:12)) #text draws the strings given in the vector labels at the coordinates given by x and y

dataFrame <- data.frame(x = x, y = y)
dist(dataFrame)# see help 
dataFrame # calculation of first distance formula below
sqrt((0.7585869 - 1.0554858)^2 + (0.8447492 - 1.0128918)^2)#sqrt((x1-x2)^2 + (y1-y2)^2)

rdistxy <- as.matrix(dist(dataFrame))
rdistxy
diag(rdistxy) <- diag(rdistxy) + 100000
rdistxy

class(rdistxy)#"matrix" "array"
min(rdistxy)#0.08150268
rdistxy == min(rdistxy)#whole matrix ke min value shathe compare kortese
idn <- which(rdistxy == min(rdistxy), arr.ind = T) #arr.ind : logical; should array indices be returned when x is an array
idn

plot(x,y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))
idn[1,] #6 5
x[idn[1,]] # return 5 and 6 no index value
y[idn[1,]] # return 5 and 6 no index value
points(x[idn[1,]],y[idn[1,]], pch = 19, col = "orange",cex = 2)# now orange color kortesi x and y er 5 and 6 coordinate, because ader distance is low

par(mfrow = c(1,2))
plot(x, y, col = "blue", pch = 19, cex = 2, main = "Data")
text(x + 0.05, y + 0.05, labels = as.character(1:12))
points(x[idn[1,]], y[idn[1,]], col = "orange", pch = 19, cex = 2)

library(dbplyr)
hcluster <- dist(dataFrame) %>% hclust # Hierarchical cluster analysis 
hcluster#return different types of value ase
unclass(hcluster)
dendro <- as.dendrogram(hcluster)
dendro
unclass(dendro)
hcluster$height
hcluster$height[1]
cutDendro <- cut(dendro, h = hcluster$height[1] + 0.00001)
cutDendro

cutDendro$lower[[11]]
plot(cutDendro$lower[[11]], yaxt = "n", 
     main = "Begin Building Tree")#yaxt : A character which specifies the y axis type. Specifying "n" suppresses plotting.

t <- c(3,5,7,0)
order(t)

rdistxy
order(rdistxy)# asec order kora hoise, return index
NROW(order(rdistxy))#12*12 value
nextmin <- rdistxy[order(rdistxy)][3] # select 3 value

rdistxy == nextmin # whole data frame er every value k compare kortese
which(rdistxy == nextmin, arr.ind = T)#arr.ind na dile vector return kore
which(rdistxy == nextmin)# return vector, and check the value
ind <- which(rdistxy == nextmin, arr.ind = T)
ind
ind[1,]

hClustering <- data.frame(x = x, y = y) %>% dist %>% hclust
plot(hClustering)


dataMatrix <- data.frame(x=x, y=y) %>% data.matrix
heatmap(dataMatrix)


# chapter 13 #
#=============
set.seed(1234)
x <- rnorm(12, rep(1:3, each = 4), 0.2)# second para is mean
x
mean(x)
y <- rnorm(12, rep(c(1,2,1), each = 4), 0.2)
plot(x,y, col = "blue", pch = 19, cex = 2)# cex = dot in plot kotho boro hobe, see help
text(x + 0.05, y + 0.05, labels = as.character(1:12)) #text draws the strings given in the vector labels at the coordinates given by x and y

dataFrame <- data.frame(x = x, y = y)
kmeansObj <- kmeans(dataFrame, centers = 3)
names(kmeansObj)
class(kmeansObj)#kmeans
kmeansObj
unclass(kmeansObj)#here lots of item ase which is important for analysis


#13.4
set.seed(1234)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
dataMatrix

t(dataMatrix)#Given a matrix or data.frame x, t returns the transpose of 
nrow(dataMatrix)#12
nrow(dataMatrix):1#12-1
t(dataMatrix)[, nrow(dataMatrix):1]
kmeansObj$cluster#A vector of integers (from 1:k) indicating the cluster to which each point is allocated
order(kmeansObj$cluster)

par(mfrow = c(1,2))
image(t(dataMatrix)[,nrow(dataMatrix):1], yaxt = "n", main = "orginal data")
image(t(dataMatrix)[,order(kmeansObj$cluster)], yaxt = "n", main = "cluster data")




# chapter 14 #
#=============
set.seed(12345)
dataMatrix <- matrix(rnorm(400), nrow = 40, ncol = 10)#def ncol = 10 nibe
dim(dataMatrix)
image(1:10, 1:40, t(dataMatrix)[,nrow(dataMatrix):1])
heatmap(dataMatrix)


set.seed(678910)
for (i in 1:40){
  coinFlip <- rbinom(1,size = 1, prob = 0.5)#rbinom(n, size, prob)
  if (coinFlip) {
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,3), each = 5)
  }
}
rbinom(1,size = 1, prob = 0.5)
rep(c(0,3), each = 5)
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])

heatmap(dataMatrix)

#14.2
library(dplyr)
hh <- dist(dataMatrix) %>% hclust
hh$order
dataMatrix[c(33,39),]
dataMatrix[hh$order,]
dataMatrixOrdered <- dataMatrix[hh$order,]

par(mfrow = c(1,3))
t(dataMatrixOrdered)[,nrow(dataMatrix):1]
image(t(dataMatrixOrdered)[,nrow(dataMatrix):1])
rowMeans(dataMatrixOrdered)
plot(rowMeans(dataMatrixOrdered),40:1, xlab = "Row Mean",#y cordinate = 40:1 
     ylab = "Row", pch = 19)
plot(colMeans(dataMatrixOrdered),xlab = "column", ylab = "Column Mean", pch = 19)
#--------
head(dataMatrixOrdered)
head(scale(dataMatrixOrdered))#scale is generic function whose default method centers and/or scales the columns of a numeric matrix.
a <-c(1,2,3)
b <- c(3,4,5)
scale(data.frame(a = a, b = b))#here a col er mean 2 & b mean = 4

svd(scale(dataMatrixOrdered))
svd1 <- svd(scale(dataMatrixOrdered))
svd1$u#40 by 10
dim(svd1$u)
dim(svd1$v)
svd1$v# 10 by 10

par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1], main = "Original Data")
plot(svd1$u[, 1], 40:1, , ylab = "Row", xlab = "First left singular vector", pch = 19)
plot(svd1$v[, 1], xlab = "Column", ylab = "First right singular vector", pch = 19)

#14.6
svd1$u[,1]#40 item
svd1$v[,1]#10 item
approx <- with(svd1, outer(u[,1], v[,1]))
approx
dim(approx)
par(mfrow = c(1,2))
image(t(dataMatrixOrdered)[,nrow(dataMatrix):1], main = "orginal Matrix")
image(t(approx)[,nrow(approx):1], main = "approx Matrix")

#14.7
constantMatrix <- dataMatrixOrdered * 0
constantMatrix
for (i in 1:dim(dataMatrixOrdered)[1]) {
  constantMatrix[i,] <- rep(c(0,1), each = 5)
}
svd1 <- svd(constantMatrix)
svd1$d#10 items
par(mfrow = c(1,3))
image(t(constantMatrix)[, nrow(constantMatrix):1], main = "orginal data")
plot(svd1$d, xlab = "column", ylab = "singluar Value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "column", pch = 19,
     ylab = "prop. of varience Expalined")

par(mfrow = c(1,2))
svd1 <- svd(scale(dataMatrixOrdered))
plot(svd1$d, xlab = "column", ylab = "Singular Value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "column", 
     ylab = "prop. of varience Expalined", pch = 19)

#14.8
dim(dataMatrixOrdered)
svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered, scale = T)
prcomp(dataMatrixOrdered, scale = T)
dim(pca1$rotation)#10 by 10
pca1$rotation[,1]
plot(pca1$rotation[,1], svd1$v[,1], pch = 19, xlab = "Pca1", 
     ylab = "Right Singular vactor")
abline(c(0,1))

#14.9
dim(dataMatrix)
set.seed(678910)
for (i in 1:40){
  coinFlip1 <- rbinom(1, size = 1, prob = 0.5)
  coinFlip2 <- rbinom(1, size = 1, prob = 0.5)
  
  if (coinFlip1) {
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,5), each = 5)
  }
  if (coinFlip2) {
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,5), each = 5)
  }
}

hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order,]

svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1], main = "Data")
plot(rep(c(0, 1), each = 5), pch = 19, xlab = "Column", ylab = "Pattern 1", main = "Block pattern")
plot(rep(c(0, 1), 5), pch = 19, xlab = "Column", ylab = "Pattern 2", main = "Alternating pattern")

svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd2$v[, 1], pch = 19, xlab = "Column", ylab = "First right singular vector")
plot(svd2$v[, 2], pch = 19, xlab = "Column", ylab = "Second right singular vector")


svd1$d
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", 
     ylab = "Percent of variance explained", pch = 19)


#14.10
dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100, size = 40, replace = F)] <- NA
dataMatrix2
dim(dataMatrix2)

library(impute)
dataMatrix2 <- impute.knn(dataMatrix2)$data
svd1 <- svd(scale(dataMatrixOrdered))
svd2 <- svd(scale(dataMatrix2))
par(mfrow = c(1, 2))
plot(svd1$v[, 1], pch = 19, main = "Original dataset")
plot(svd2$v[, 1], pch = 19, main = "Imputed dataset")

#14.11
load("data/face.rda")
image(t(faceData)[, nrow(faceData):1])
svd1 <- svd(scale(faceData))
plot(svd1$d^2/sum(svd1$d^2), pch = 19, xlab = "Singular vector", ylab = "variance Explain")

dim(svd1$u)
dim(svd1$v)
svd1$d
approx1 <- svd1$u[,1] %*% t(svd1$v[,1]) * svd1$d[1]

approx5 <- svd1$u[,1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[,1:5])
approx10 <- svd1$u[,1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[,1:10])

par(mfrow = c(1, 4))
image(t(approx1)[, nrow(approx1):1], main = "1 vector")
image(t(approx5)[, nrow(approx5):1], main = "5 vectors")
image(t(approx10)[, nrow(approx10):1], main = "10 vectors")
image(t(faceData)[, nrow(faceData):1], main = "Original data")

# chapter 15 #
#=============
library(ggplot2)

#geom_smooth = Aids the eye in seeing patterns in the presence of overplotting
#se = Display confidence interval around smooth? TRUE by default
#geom_point = pointing geom
#aes = Aesthetic mappings describe how variables in the data are mapped to visual properties (aesthetics) of geoms
ggplot(airquality, aes(Temp, Ozone)) +
  geom_point() +
  geom_smooth(method = "loess", se = F)


str(mpg)
qplot(displ, hwy, data = mpg)#x,y,data

unique(mpg$drv)
qplot(displ, hwy, data = mpg, col = drv)

qplot(displ, hwy, data = mpg,geom = c("point", "smooth"))
qplot(displ, hwy, data = mpg, col = drv, geom = c("point", "smooth")) # here diff color a line draw hoy

qplot(hwy, data = mpg, binwidth = 2, fill = drv)#binwidth The width of the bins

qplot(drv ,hwy, data = mpg, geom = "boxplot")

qplot(hwy, data = mpg, facets = drv ~., binwidth = 2)#here drv group by ta right a asbe
qplot(hwy, data = mpg, facets = .~ drv, binwidth = 2)#here drv group by ta top a asbe

qplot(displ, hwy, data = mpg, facets = .~drv)
qplot(displ, hwy, data = mpg, facets = drv~.)

qplot(displ, hwy, data = mpg, facets = .~drv) + geom_smooth()
qplot(displ, hwy, data = mpg, facets = .~drv, geom = c("point", "smooth"))# same a above

#save(count, age, circumference, file = "mydata.rda")
load(file = "data/maacs.Rda")
head(maacs)
names(maacs)

#log without base means ln
qplot(log(eno), data = maacs)
qplot(log(eno), data = maacs, col = mopos)
qplot(log(eno), data = maacs, fill = mopos)

qplot(log(eno), data = maacs, geom = "density")

qplot(log(eno), data = maacs, geom = "density", color = mopos)


library(dplyr)
data1 <- mutate(maacs, logpm = log(pm25))
head(select(data1, pm25, logpm))

qplot(log(pm25), log(eno), data = maacs, geom = c("point","smooth"))

qplot(log(pm25), log(eno), data = maacs, shape = mopos)
qplot(log(pm25), log(eno), data = maacs, col = mopos)
qplot(log(pm25), log(eno), data = maacs, col = mopos, shape = mopos)

qplot(log(pm25), log(eno), data = maacs, col = mopos, shape = mopos) +
  geom_smooth(method = "lm")

qplot(log(pm25), log(eno), data = maacs, facets = .~mopos) + 
  geom_smooth(method = "lm")


# chapter 16 #
#=============
library(ggplot2)
load(file = "data/maacs.Rda")
head(maacs)
names(maacs)

g <- ggplot(maacs, aes(logpm25, NocturnalSympt))
summary(g)
class(g)

g + geom_point()

g + geom_point() + geom_smooth()

g + geom_point() + geom_smooth(method = "lm")

g + geom_point() + geom_smooth(method = "lm") + facet_grid(.~bmicat)

g + geom_point(col = "steelblue", size = 4, alpha = 1/2)

g + geom_point(aes(col = bmicat), size = 2, alpha = 0.5)

g + geom_point(aes(col = bmicat)) + labs(title = "MAACS Cohort") +
  labs(x = expression("log" * PM[2.5]), y = "Nocturnal Symptoms") 
  #labs(x = "pm25")

g + geom_point(aes(col = bmicat), size = 2, alpha = 0.5) +
  geom_smooth(method = "lm", size = 2, linetype = 3, se = F, color = "black")
  #here color o add kora jabe

g + geom_point(aes(col = bmicat)) + theme_bw()
g + geom_point(aes(col = bmicat)) + theme_bw(base_family = "Times")


#16.11
maacs$logno2_new
range(maacs$logno2_new, na.rm = T)
seq(0,1, length.out =4)# desired length of the sequence, length.out == length
quantile(maacs$logno2_new, seq(0,1, length.out =4), na.rm = T)
cutpoints <- quantile(maacs$logno2_new, seq(0,1,length.out = 4), na.rm = T)
cutpoints
maacs$no2tert <- cut(maacs$logno2_new,cutpoints)
table(maacs$no2tert)#here 3 ta range hoise , according to the cutpoint value
levels(maacs$no2tert)

names(maacs)
g <- ggplot(maacs, aes(logpm25, NocturnalSympt))

#here akta akta value deye deye execute kore dhekte hobe
g + 
  geom_point(alpha = 1/3)+ # alpha = opacity
  facet_wrap(bmicat ~ no2tert, nrow = 2, ncol = 4)+ # search facet_wrap
  geom_smooth(method = "lm", se = F, col = "steelblue")+
  theme_bw(base_family = "Arial" ,base_size = 10) + #base_size = fontsize
  labs(title = "MAACS", x = expression("log" * PM[2.5]), y = "NOCSYM")


#16.12
testdat <- data.frame(x = 1:100, y = rnorm(100))
testdat
testdat[50,2] <- 100 # outlier

plot(testdat$x, testdat$y, type = "l", ylim = c(-3,3)) # ylim = yaxis ke limit kortese

g <- ggplot(testdat, aes(x = x, y = y))
g + geom_line()
g + geom_line() + ylim(-3,3) # outlier ke delete kore dese, plot faka ase
g + geom_line() + coord_cartesian(ylim = c(-3,3)) #coord_cartesian = Cartesian coordinates; akhen a thik ase



# chapter 17 #
#=============
#17.2
library(readr)
pm0 <- read_delim("pm25data/RD_501_88101_1999-0.txt",
                  delim = "|",
                  comment = "#",
                  col_names = F,
                  na = "")
dim(pm0)
head(pm0[,1:13])

cnames <- readLines("pm25data/RD_501_88101_1999-0.txt",1)
cnames <- strsplit(cnames, "|", fixed = T)#logical. If TRUE match split exactly, otherwise use regular expressions
class(cnames)
names(pm0) <- make.names(cnames[[1]])#class(cnames) == list , tai [[1]] use kora hoise
head(pm0[,1:13])

x0 <- pm0$Sample.Value
summary(x0)

mean(is.na(x0))



pm1 <- read_delim("pm25data/RD_501_88101_2012-0.txt",
                  delim = "|",
                  comment = "#",
                  col_names = F,
                  na = "")
dim(pm1)
head(pm1[,1:13])
names(pm1) <- make.names(cnames[[1]])

library(dplyr)
pm <- rbind(pm0,pm1)
dim(pm)

rep(c(1992,2012),c(2,3))#here 1992 2 bar and 2012 3 bar repeat korbe
pm <- mutate(pm, year = factor(rep(c(1992,2012), c(nrow(pm0),nrow(pm1))))) %>% rename(PM = Sample.Value)
str(pm)
rm(pm1)#remove from env
rm(pm0)

#17.3
library(ggplot2)
set.seed(2015)
idx <- sample(nrow(pm),1000)
idx
qplot(year, log2(PM), data = pm[idx,], geom = "boxplot")

with(pm, tapply(PM,year,summary))

filter(pm, year == "2012") %>% summarise(negative = mean(PM < 0, na.rm = T), 
                                         total = sum(PM < 0, na.rm = T))# total col a count korlam
library(lubridate)
names(pm)
head(pm$Date)#date ase 19990103 aie format a

negative <- filter(pm, year == "2012") %>% 
  mutate(negative = PM < 0, date = ymd(Date)) %>% 
  select(date,negative)

str(negative)
month.name
month.name[2]
month("2012-03-13")#return month as a number
month("2012-03-13", label = T)#def label = F
month("2012-03-13", label = T,abbr = F)#abbr def = T, abber shathe label para relation ase

month("2012-03-13")#return = 3
month.name[month("2012-03-13")]#3 ke march a convert korse

mutate(negative, month = factor(month.name[month(date)], levels = month.name)) %>%
  group_by(month) %>%
  summarise(pct.negtive = mean(negative, na.rm = T)*100)

names(pm)
sites <- filter(pm, State.Code == 36) %>% 
  select(County.Code,Site.ID,year) %>%
  unique %>% data.frame #data.frame use korle annoying attr specfication gula chole jai

str(sites)
sites<- mutate(sites, site.code = paste(County.Code,Site.ID, sep = "."))

site.year <- with(sites, split(site.code, year))
both <- intersect(site.year[[1]],site.year[[2]]) 
both

count <- mutate(pm, site.code = paste(County.Code,Site.ID, sep = ".")) %>%
  filter(site.code %in% both)
group_by(count, site.code) %>% summarise(n = n())


library(lubridate)
library(ggplot2)
library(dplyr)
names(pm)
pmsub <- filter(pm, State.Code == "36" & County.Code == "063" & Site.ID == "2008") %>%
  select(Date, year, PM) %>%
  mutate(Date = ymd(Date), yday = yday(Date))

qplot(yday, PM, data = pmsub, facets = .~year, xlab = "Day of the year")

mn <- group_by(pm, year, State.Code) %>% summarize(PM = mean(PM, na.rm = T))

head(mn)
tail(mn)
str(mn)

mutate(mn, xyear = as.numeric(as.character(year)))
qplot(xyear, PM, 
      data = mutate(mn, xyear = as.numeric(as.character(year))),
      color = factor(State.Code),
      geom = c("point","line"))
