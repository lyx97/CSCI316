library(rpart)
DT.rpart <- rpart(Species ~ Petal.Length + Petal.Width, iris)
print(DT.rpart)

plot(DT.rpart)
text(DT.rpart)

DTpredict <- predict(DT.rpart, iris, type = "class")
table(iris$Species, DTpredict)

weather.train <- read.csv("weather.train.csv")
weather.test <- read.csv("weather.test.csv")
weather.train$Date <- NULL
weather.test$Date <- NULL
for(name in names(weather.test))
  if(is.factor(weather.test[[name]])) {
    weather.test[[name]] <- factor(weather.test[[name]],
                                   levels=levels(weather.train[[name]]))
  }
weather.train$RainTomorrow <-factor(weather.train$RainTomorrow)
## Train and predict with decision tree
DecTree <- rpart(RainTomorrow ~ ., weather.train)
pred1 <- predict(DecTree, newdata = weather.test, type = "prob")
## Train and predict with random forest
library(randomForest)
RF <- randomForest(RainTomorrow ~ ., weather.train)
pred2 <- predict(RF, newdata = weather.test, type="prob")

## Generate ROC curves
library(ROCR)
truth <- weather.test$RainTomorrow
perf1 <- performance(prediction(pred1[,2],truth),
                     "tpr", "fpr")
perf2 <- performance(prediction(pred2[,2],truth),
                     "tpr", "fpr")

plot(perf1, col = 2)
plot(perf2, add = TRUE, col = 3)
abline(0,1,lty=3)
legend("bottomright", c("Decision Tree", "Random Forest"),
       lty = 1, col = 2:3)

