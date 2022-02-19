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

data_raw <- read.csv("./annual_aqi_by_county_2020.csv")
clean_data <- data_raw %>% filter(data_raw$Days.with.AQI > 80)
clean_data$performance <- (clean_data$Good.Days + clean_data$Moderate.Days) / clean_data$Days.with.AQI
boxplot(clean_data$performance~clean_data$State,data=clean_data)
correlation_data <- clean_data[,c(4:20)]
cormat <- round(cor(correlation_data),2)
boxplot(clean_data$performance~clean_data$stateCode,data=clean_data,ylim = c(0.25,1))
clean_data$stateCode <- state.abb[clean_data$State]


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