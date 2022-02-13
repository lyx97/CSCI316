############ INFO411/911 Lecture 9 R Code, SIM22 ############

# 
## Categorical predictors

data(iris)
summary(iris.fit<-lm(Petal.Length~Species, data=iris))

## Swiss Fertility data

data(swiss)

# pairs(swiss, panel=panel.smooth)
# pairs(swiss)

summary(swiss.fit <- lm(Fertility~., data=swiss))

## Stepwise regression

step(swiss.fit, data=swiss)

library(leaps)
regsub <- regsubsets(Fertility~.,data=swiss)
summary(regsub)

summary(regsub)$cp
with(summary(regsub), which[which.min(cp),])

## inreractions
summary(swiss.fit <- lm(Fertility~(Agriculture+Examination+Education+Catholic+Infant.Mortality)^2, data=swiss))

swiss.fit2 <- lm(Fertility~(Agriculture+Examination+Education+Catholic+Infant.Mortality)^2, data=swiss)
swiss.fit2.steps <- step(swiss.fit2, data=swiss)

summary(swiss.fit2.steps)

## Logistic regression

iris2 <- transform(subset(iris, Species!="setosa", c("Species","Sepal.Length","Sepal.Width")), Species=factor(Species))

pairs(iris2,col=as.numeric(iris$Species)+2)
legend("topleft", levels(iris$Species)[-1],       fill=3:4, ncol=1)

summary(glm(I(Species=="virginica")~.,data=iris2,family=binomial("logit")))

## Regression trees

library(rpart)
(swiss.tree <- rpart(Fertility~.,data=swiss))

plot(swiss.tree)
text(swiss.tree, digits=4)


