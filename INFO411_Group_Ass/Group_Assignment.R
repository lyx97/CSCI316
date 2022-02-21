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

##shift target to end of the table and drop the year column
clean_data = subset(clean_data,select = -c(3))
clean_data = clean_data %>% relocate(Days.PM2.5,.after = last_col())
clean_cormat <- melt(round(cor(correlation_data),2)) %>% filter(Var2 == "performance" | Var2 == "Days.PM2.5" )
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

### split data to 70 30 
data.train <- clean_data[1:(nrow(clean_data)/2), ]
data.test <- clean_data[-(1:(nrow(clean_data)/2)),]
##### random forest
rdmfrst = randomForest(Days.PM2.5~.,data = data.train, na.action = na.omit)
treepred = predict(rdmfrst, data.test[,-20], type="class")

