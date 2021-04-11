library(kernlab)
data("spam")

set.seed(3435)
trainIndicator <- rbinom(4601, size = 1, prob = 0.5)
trainIndicator
table(trainIndicator)

trainSpam <- spam[trainIndicator == 1,]
testSpam <- spam[trainIndicator == 0,]

head(names(trainSpam),20)

head(trainSpam[,1:10])

table(trainSpam$type)

names(trainSpam)

trainSpam[,"capitalAve"]
boxplot(capitalAve ~ type, data = trainSpam)
boxplot(log10(capitalAve) ~ type, data = trainSpam)

pairs(log10(trainSpam[,1:4] + 1))

names(trainSpam)
dim(t(trainSpam[,1:57]))
hCluster = hclust(dist(t(trainSpam[,1:57])))
plot(hCluster)

hClusterUpdated <- hclust(dist(t(log10(trainSpam[,1:57] + 1))))
plot(hClusterUpdated)

as.numeric(trainSpam$type)#return 1 and 2
trainSpam$numType <- as.numeric(trainSpam$type) - 1 #nonspam = 0 ; spam = 1
table(trainSpam$numType )

costFunction <- function(x,y) sum(x != (y > 0.5))
cvError <- rep(NA,55)
library(boot)
dim(trainSpam)

for (i in 1:55) {
  lmFormula <- reformulate(names(trainSpam)[i], response = "numType")#create formula
  #print(lmFormula)
  glmFit <- glm(lmFormula, family = "binomial", data = trainSpam)
  #print(glmFit)
  cvError[i] <- cv.glm(trainSpam, glmFit, costFunction, 2)$delta
}
print(cvError)

which.min(cvError)#53
names(trainSpam)[which.min(cvError)]

predictionModel <- glm(numType ~ charDollar, family = "binomial", data = trainSpam)
predictionModel
unclass(predictionModel)

predictionTest <- predict(predictionModel, testSpam)
str(predictionTest)
predictedSpam = rep("nonspam", dim(testSpam)[1])
predictedSpam[predictionModel$fitted > 0.5] <- "spam"
predictedSpam
table(predictedSpam,testSpam$type)
table(testSpam$type)
str(predictedSpam)

#PAGE = 46
# winnowed DOWN = to reduce the size of a group of people or things so that you only keep the best or most useful ones.

sessionInfo()
