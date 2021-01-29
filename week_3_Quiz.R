#1
library(datasets)
data(iris)
names(iris)
speies <- iris["Species"] == "virginica"
speies
virginica <- iris[speies,]
round(mean(virginica$Sepal.Length))


#2
apply(iris[, 1:4], 2, mean)


#3
library(datasets)
data(mtcars)
names(mtcars)
sapply(split(mtcars$mpg, mtcars$cyl), mean)
tapply(mtcars$mpg, mtcars$cyl, mean)


#4
mpgAvg <- tapply(mtcars$hp, mtcars$cyl, mean)
mpgAvg
round(abs(mpgAvg["4"] - mpgAvg["8"]))


#Execution of 'ls' will suspend at the beginning of the function 
#and you will be in the browser.

