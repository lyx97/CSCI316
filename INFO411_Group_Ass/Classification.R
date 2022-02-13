library(RSNNS)
library(splitTools)
library(ranger)
#Load dataset
fullDataSet <- read.csv("creditworthiness.csv")

#select all entries for which the credit rating is known
knownData <- subset(fullDataSet, fullDataSet[,46] > 0)

#select all entries for which the credit rating is unknown
unknownData <- subset(fullDataSet, fullDataSet[,46] == 0)
tpart <- partition(knownData$credit.rating,p = c(train = 0.8,test = 0.2))

#separate value from targets
trainValues <- knownData[,c(1,2,3,6)]
trainTargets <- decodeClassLabels(knownData[,46])
unknownsValues <- unknownData[,c(1,2,3,6)]

#split dataset into traing and test set
trainset <- splitForTrainingAndTest(trainValues, trainTargets, ratio=0.20)
trainset <- normTrainingAndTestSet(trainset)

model <- mlp(trainset$inputsTrain, trainset$targetsTrain, size=100, learnFuncParams=c(0.005), maxit=250, inputsTest=trainset$inputsTest, targetsTest=trainset$targetsTest, shufflePatterns = FALSE)
predictTestSet <- predict(model,trainset$inputsTest)

confusionMatrix(trainset$targetsTrain,fitted.values(model))
confusionMatrix(trainset$targetsTest,predictTestSet)

par(mfrow=c(2,2))
plotIterativeError(model)

plotRegressionError(predictTestSet[,2], trainset$targetsTest[,2])
plotROC(fitted.values(model)[,2], trainset$targetsTrain[,2])
plotROC(predictTestSet[,2], trainset$targetsTest[,2])

#confusion matrix with 402040-method
confusionMatrix(trainset$targetsTrain, encodeClassLabels(fitted.values(model),method="402040", l=0.4, h=0.6))


#show detailed information of the model
summary(model)
model
weightMatrix(model)
extractNetInfo(model)
 
##########################################
##########################################
##########################################
##########################################
##########################################
##########################################
##########################################
#Load dataset
fullDataSet <- read.csv("creditworthiness.csv")
#select all entries for which the credit rating is known
knownData <- subset(fullDataSet, fullDataSet[,46] > 0)

#select all entries for which the credit rating is unknown
unknownData <- subset(fullDataSet, fullDataSet[,46] == 0)

#separate value from targets
trainValues <- knownData[,1:45]
trainTargets <- decodeClassLabels(knownData[,46])
unknownsValues <- unknownData[,1:45]
hist(trainValues)
#split dataset into traing and test set
trainset <- splitForTrainingAndTest(trainValues, trainTargets, ratio=0.5)
trainset <- normTrainingAndTestSet(trainset)

model <- mlp(trainset$inputsTrain, trainset$targetsTrain, size=5, learnFuncParams=c(0.01), maxit=250, inputsTest=trainset$inputsTest, targetsTest=trainset$targetsTest)
predictTestSet <- predict(model,trainset$inputsTest)

confusionMatrix(trainset$targetsTrain,fitted.values(model))
confusionMatrix(trainset$targetsTest,predictTestSet)

par(mfrow=c(2,2))
plotIterativeError(model)
plotRegressionError(predictTestSet[,2], trainset$targetsTest[,2])
plotROC(fitted.values(model)[,2], trainset$targetsTrain[,2])
plotROC(predictTestSet[,2], trainset$targetsTest[,2])

#confusion matrix with 402040-method
confusionMatrix(trainset$targetsTrain, encodeClassLabels(fitted.values(model),method="402040", l=0.4, h=0.6))


#show detailed information of the model
summary(model)
model
weightMatrix(model)
extractNetInfo(model)

