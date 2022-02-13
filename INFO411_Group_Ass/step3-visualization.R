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


# Plot the heatmap for a variable at scaled / normalised values
var <- 1
if (packageVersion("kohonen") < 3){
   plot(som_model, type = "property", property = som_model$codes[,var], main=names(som_model$data)[var], palette.name=coolBlueHotRed)
} else{
   plot(som_model, type = "property", property = som_model$codes[[1]][,var], main=names(som_model$data)[var], palette.name=coolBlueHotRed)
}

# Plot the original scale heatmap for a variable from the training set:
var <- 5 #define the variable to plot
var_unscaled <- aggregate(as.numeric(data_train[,var]), by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2]
plot(som_model, type = "property", property=var_unscaled, main=names(data_train)[var], palette.name=coolBlueHotRed)
rm(var_unscaled, var)

#plot a variable from the original data set (will be uncapped etc.)
# This function produces a menu for multiple heatmaps.
source('./plotHeatMap.R')
plotHeatMap(som_model, valid_data, variable=0)

