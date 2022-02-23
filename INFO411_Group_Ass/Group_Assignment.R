# Preprocessing

### LOAD LIBRARIES - install with:
#install.packages(c("kohonen", "dummies", "ggplot2", "maptools", "sp", "reshape2", "rgeos"))
library(kohonen)
library(dummies)
library(ggplot2)
library(sp)
library(maptools)
library(dplyr)
library(reshape2)
library(rgeos)
library(Hmisc)
library(randomForest)
library(RSNNS)
library(splitTools)
# Colour palette definition
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

data_raw <- read.csv("./annual_aqi_by_county_2020.csv")
clean_data <- data_raw %>% filter(data_raw$Days.with.AQI > 80)
clean_data$performance <- (clean_data$Good.Days + clean_data$Moderate.Days) / clean_data$Days.with.AQI
boxplot(clean_data$performance~clean_data$State,data=clean_data)
correlation_data <- clean_data[,c(4:20)]
cormat <- round(cor(correlation_data),2)
clean_data$stateCode <- state.abb[match(clean_data$State,state.name)]
boxplot(clean_data$performance~clean_data$stateCode,data=clean_data,ylim = c(0.25,1))
boxplot(clean_data$Days.PM2.5,main='Days > 2.5PM concentrations',sub = paste("outlier rows:" ,boxplot.stats(clean_data$Days.PM2.5)$out))
boxplot(clean_data$performance,main='Performance',sub = paste("outlier rows:" ,boxplot.stats(clean_data$performance)$out))
##shift target to end of the table and drop the year column
clean_data = subset(clean_data,select = -c(3))
clean_data = clean_data %>% relocate(Days.PM2.5,.after = last_col())
clean_cormat <- melt(round(cor(correlation_data),2)) %>% filter(Var2 == "Days.PM2.5" )
# ### Heatmap
ggplot(data = melt(clean_cormat), aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))+
  coord_fixed()
clean_data = subset(clean_data, select = -c(stateCode))
clean_cormat[order(clean_cormat$value),]
# ------------------- SOM TRAINING ---------------------------

data_train <- clean_data[,c(4:19)]

# now train the SOM using the Kohonen method
data_train_matrix <- as.matrix(data_train)
names(data_train_matrix) <- names(data_train)
require(kohonen)
x_dim=3
y_dim=3
som_grid <- somgrid(xdim = x_dim, ydim=y_dim, topo="hexagonal")  
# Train the SOM model!
if (packageVersion("kohonen") < 3){
  system.time(som_model <- som(data_train, 
                               grid=som_grid, 
                               rlen=5000, 
                               alpha=c(0.05,0.01),
                               n.hood = "circular",
                               keep.data = TRUE ))
}else{
  system.time(som_model <- som(data_train_matrix, 
                               grid=som_grid, 
                               rlen=1000, 
                               alpha=c(0.9,0.01),
                               mode="online",
                               normalizeDataLayers=false,
                               keep.data = TRUE ))
}



summary(som_model)
rm(som_grid, data_train_matrix)
# -------------------- SOM VISUALISATION -----------------

#Visualise the SOM model results
# Plot of the training progress - how the node distances have stabilised over time.

## custom palette as per kohonen package (not compulsory)
source('./coolBlueHotRed.R')

plot(som_model, type = "changes")
#counts within nodes
plot(som_model, type = "counts", main="Node Counts", palette.name=coolBlueHotRed)
#map quality
plot(som_model, type = "quality", main="Node Quality/Distance", palette.name=coolBlueHotRed)
#neighbour distances
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances", palette.name=grey.colors)
#code spread
plot(som_model, type = "codes")
#plot a variable from the original data set (will be uncapped etc.)
# This function produces a menu for multiple heatmaps.
source('./plotHeatMap.R')
plotHeatMap(som_model, valid_data, variable=0)
# ------------------ Clustering SOM results -------------------

# show the WCSS metric for kmeans for different clustering sizes.
# Can be used as a "rough" indicator of the ideal number of clusters
mydata <- matrix(unlist(som_model$codes), ncol = length(data_train), byrow = FALSE)

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
par(mar=c(5.1,4.1,4.1,2.1))
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main="Within cluster sum of squares (WCSS)")

# Form clusters on grid
## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(mydata)), 3)

# Show the map with different colours for every cluster						  
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)

### split data to 70 30 
data.train <- clean_data[1:(nrow(clean_data)/2), ]
data.test <- clean_data[-(1:(nrow(clean_data)/2)),]
train = sample(1:nrow(clean_data),700)
##### random forest
rdmfrst = randomForest(Days.PM2.5~.,data = clean_data,  type = "regression")
rdmfrst
sqrt(sum((rdmfrst$predicted - clean_data$Days.PM2.5)^2)/ nrow(clean_data))
#### attempt to improve performance by reducing noisy features
oob.err=double(13)
test.err=double(13)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:18) 
{
  rf=randomForest(Days.PM2.5 ~ . , data = clean_data, subset = train,mtry=mtry,ntree=10000) 
  oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
  
  pred<-predict(rf,clean_data[-train,]) #Predictions on Test Set for each Tree
  test.err[mtry]= with(clean_data[-train,], mean( (Days.PM2.5 - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}
matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))
###use mtry = 5 as parameter based off the MSE plot
rdmfrst_tuned = randomForest(Days.PM2.5~.,data = clean_data,  type = "regression", mtry = 6, ntree = 10000)
rdmfrst_tuned
sqrt(sum((rdmfrst_tuned$predicted - clean_data$Days.PM2.5)^2)/ nrow(clean_data))



# plot a regression line
linearModel <- lm(Days.PM2.5~Days.Ozone + Moderate.Days + Days.with.AQI, data = data.train, na.action = na.omit)
plot(linearModel)
summary(linearModel)
test <- lm(Days.PM2.5~Moderate.Days,data=data.train)
test

boxplot(x=data.train$Days.PM2.5, y=data.train$Days.with.AQI,
        xlab="Days PM 2.5", ylab="Days with AQI", main = "Days PM 2.5 vs Days with AQI")

#### Linear regression
### split data to 80 20 
set.seed(100)
trainingRowIndex <- sample(1:nrow(clean_data), 0.8*nrow(clean_data))

data.train <- clean_data[trainingRowIndex, ] # model training data
data.test <- clean_data[-trainingRowIndex, ] # test data

# Build the model on training data
goodlmMod <- lm(Days.PM2.5 ~ Moderate.Days + Days.Ozone + Max.AQI + Days.with.AQI, data=data.train)  # build the model
goodPM2.5Pred <- predict(goodlmMod, data.test)  # predict PM2.5
summary (goodlmMod)
badlmMod <- lm(Days.PM2.5 ~ Days.NO2 + Good.Days, data=data.train)  # build the model
badPM2.5Pred <- predict(badlmMod, data.test)  # predict PM2.5
summary (badlmMod)


#### SVM
# find the best parameters
summary(tune.svm((Days.PM2.5 > 179) ~ + Days.Ozone + Max.AQI + Moderate.Days,
                 data = data.train,
                 kernel = "radial",cost = 10^c(-2:2), gamma = 10^c(-4:1),
                 type='C'))

# regression with SVM
svmfit = svm((Days.PM2.5)~ + Days.Ozone + Max.AQI + Moderate.Days, 
             data = data.train, 
             cost = 10,
             gamma = 0.1,
             kernel="radial")

# summary of the svm
summary(svmfit)

# get the predictions of the svm
svmfitpredict = predict(svmfit, data.train, decision.values =TRUE)

# summary of the prediction
summary(svmfitpredict)

ggplot(data.train, aes(x = Days.PM2.5, y = Moderate.Days, colour = Days.Ozone)) +
  geom_point(aes(size = Max.AQI)) + 
  geom_smooth(col = "red")

# ploting the predictions
plot(svmfitpredict)