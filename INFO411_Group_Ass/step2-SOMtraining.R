# ------------------- SOM TRAINING ---------------------------

data_train <- valid_data[,c(1,2,3,6,46)]

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

