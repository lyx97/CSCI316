# Preprocessing

### LOAD LIBRARIES - install with:
#install.packages(c("kohonen", "dummies", "ggplot2", "maptools", "sp", "reshape2", "rgeos"))
library(kohonen)
library(dummies)
library(ggplot2)
library(sp)
library(maptools)
library(reshape2)
library(rgeos)

# Colour palette definition
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')
library(Hmisc)
library(dplyr)
### DATA PREPARATION

# Census data comes in counts of people per area. 
# To compare areas, we will convert a number of the
# stats collected into percentages. Without this, 
# the primary differentiator between the different 
# areas would be population size.

# Load the data into a data frame
# Get the map of these areas and filter for Dublin areas.

data_raw <- read.csv("./creditworthiness.csv")
#idcol="functionary"
#names(data_raw)[1] <- "functionary"
valid_data <- data_raw %>% filter(credit.rating != 0)
useful_data <- valid_data[,c(1,2,3,6,46)]
uniquedata <- unique(valid_data)
#get a correlation matrix of all the features in the data set
cormat <- round(cor(valid_data),2) 
#plot correlation matrix for data exploration
ggplot(data = melt(credit_rating_cormat),aes(x=Var1, y=Var2, fill=value)) + geom_raster()
### Melt
melted_cormat <- melt(upper_tri, na.rm = TRUE)
credit_rating_cormat <- melt(round(cor(knownData),2)) %>% filter(Var2 == "credit.rating" ) %>% filter(value > 0.1 | value < -0.1)
### Heatmap
ggplot(data = credit_rating_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
