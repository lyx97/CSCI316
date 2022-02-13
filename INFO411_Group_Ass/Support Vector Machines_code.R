############ INFO411/INFO911 Week 7 Lecture, Autumn 2020 ############
### Please see lecture slides for context and output ###

library(kernlab)
library(ROCR)
library(e1071)

data(iris)

pairs(iris[,-5],cex.labels=2,col=as.numeric(iris$Species)+1)

iris2 <- transform(subset(iris, Species!="setosa", c("Species","Sepal.Length","Sepal.Width")), Species=factor(Species))

plot(iris2[,2:3],col=as.numeric(iris$Species)+2)

legend("topleft", levels(iris$Species)[-1],fill=3:4, ncol=1)

svm.linear <- svm(Species~.,data=iris2,kernel="linear")
plot(svm.linear, data=iris2)

svm.linear10 <- svm(Species~.,data=iris2,kernel="linear",cost=10)
plot(svm.linear10, data=iris2)

svm.linear100 <- svm(Species~.,data=iris2,kernel="linear",cost=100)
plot(svm.linear100, data=iris2)

svm.linear1000 <- svm(Species~.,data=iris2,kernel="linear",cost=1000)
plot(svm.linear1000, data=iris2)

svm.radial <- svm(Species~.,data=iris2,kernel="radial")
plot(svm.radial, data=iris2)

svm.radial1 <- svm(Species~., data=iris2,kernel="radial",gamma=1)
plot(svm.radial1, data=iris2)

svm.radial1C100 <- svm(Species~., data=iris2,kernel="radial",gamma=1,cost=100)
plot(svm.radial1C100, data=iris2)

svm.radial1C1000 <- svm(Species~., data=iris2,kernel="radial",gamma=1,cost=1000)
plot(svm.radial1C1000, data=iris2)


svm.radial10 <- svm(Species~., data=iris2,kernel="radial",gamma=10)
plot(svm.radial10, data=iris2)

svm.radial100 <- svm(Species~., data=iris2,kernel="radial",gamma=100)
plot(svm.radial100, data=iris2)

svm.radialC100 <- svm(Species~., data=iris2,kernel="radial",cost=100)
plot(svm.radialC100, data=iris2)

table(iris2$Species,fitted(svm.radial))

table(iris2$Species,fitted(svm.radial100))

summary(svm.radial <- svm(Species~.,data=iris2,kernel="radial", cross=10))

summary(svm.radial100 <- svm(Species~.,data=iris2,kernel="radial", gamma=100, cross=10))

summary(tune.svm(Species~.,data=iris2,kernel="radial", gamma = 10^(-1:1), cost = 10^(-1:1)))

svm3.radial <- svm(Species~Sepal.Length+Sepal.Width,data=iris,cross=10)
plot(svm3.radial, data=iris, Sepal.Length~Sepal.Width)

summary(svm3.radial)

predict(svm3.radial, newdata=data.frame(Sepal.Width=3.4, Sepal.Length=6), decision.values=TRUE)

pred.radial <- predict(svm.radial,newdata=iris2,decision.values = TRUE)
y.hat.radial <- attr(pred.radial,"decision.values")

hist(y.hat.radial <- attr(predict(svm.radial, newdata=iris2, decision.values=TRUE),"decision.values"), breaks=40,main="",xlab="")

#library(ROCR)
pred.radial <- prediction(y.hat.radial, iris2$Species=="versicolor")
performance(pred.radial, "auc")@y.values[1]
perf.radial <- performance(pred.radial, "tpr", "fpr")
plot(perf.radial)
abline(0,1,lty=3)

y.hat.linear <- attr(predict(svm.linear, newdata=iris2, decision.values=TRUE),"decision.values")
pred.linear <- prediction(y.hat.linear, iris2$Species=="versicolor")
perf.linear<- performance(pred.linear, "tpr", "fpr")
plot(perf.radial, col=2)
plot(perf.linear, col=3, add=TRUE)
abline(0,1,lty=3)
legend("bottomright",c("RBF SVM", "Linear SVM"),lty=1,col=2:3)
