library(raster)
library(spatialEco)

rm(list=ls())

setwd('~/src/MATH561_final_project')

load("./data/High/high_train.RData")
load("./data/Medium/med_train.RData")
load("./data/Low/low_train.RData")
load("./data/High/high_validate.RData")
load("./data/Medium/med_validate.RData")
load("./data/Low/low_validate.RData")

ds_list <- list(high_train, med_train, low_train, high_validate, med_validate, low_validate)

df <- data.frame()
# loop data sets
for (i in 1:length(ds_list)) {
  print(paste('dataset ', i, '/', length(ds_list)))
  
  dataset.i <- ds_list[[i]]$mat
  
  length.i <- length(dataset.i)
  
  # create feature placeholders
  tri_min_feature <- rep(NA, length.i)
  tri_max_feature <- rep(NA, length.i)
  tri_mean_feature <- rep(NA, length.i)
  tri_sd_feature <- rep(NA, length.i)
  tri_range_feature <- rep(NA, length.i)

  for(j in 1:length.i){
    print(paste('image ', j, '/', length.i))

    # select image j in set i
    image.ij <- ds_list[[i]]$mat[[j]]

    # surface roughness
    # - convert to raster datatype
    raster_dataset = raster(image.ij)
    
    # - calculate Terrain Ruggedness Index
    tri_raster_dataset = tri(raster_dataset)
    
    # - convert back to matrix
    tri_dataset = as.matrix(tri_raster_dataset[2:287, 2:191])
    
    # - calculate descriptive statistics
    tri_min_feature[j] <- min(tri_dataset)
    tri_max_feature[j] <- max(tri_dataset)
    tri_mean_feature[j] <- mean(tri_dataset)
    tri_sd_feature[j] <- sd(tri_dataset)
    tri_range_feature[j] <- abs(max(tri_dataset) - min(tri_dataset))
  }
  
  df <- rbind(df, data.frame(
    "tri_min"=tri_min_feature,
    "tri_max"=tri_max_feature,
    "tri_mean"=tri_mean_feature,
    "tri_sd"=tri_sd_feature,
    "tri_range"=tri_range_feature
  ))
}

# write to CSV
write.csv(df, 'data/tri_features.csv', row.names=TRUE)
