library(automap)

rm(list=ls())

setwd('~/src/MATH561_final_project')

#######################
# process training data
#######################

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
  var_sill_feature <- rep(NA, length.i)
  var_range_feature <- rep(NA, length.i)
  var_kappa_feature <- rep(NA, length.i)

  for(j in 1:length.i){
    print(paste('image ', j, '/', length.i))

    # select image j in set i
    image.ij <- ds_list[[i]]$mat[[j]]

    # variogram
    # - convert to raster datatype
    raster_dataset = raster(image.ij)
    
    # - convert to spatial dataset
    points_dataset = rasterToPoints(raster_dataset, spatial=TRUE)
    
    # - fit variogram
    variogram = autofitVariogram(layer~1, points_dataset)
    
    # - extract parameters
    var_sill_feature[j] <- variogram$var_model$psill[2]
    var_range_feature[j] <- variogram$var_model$range[2]
    var_kappa_feature[j] <- variogram$var_model$kappa[2]

  }
  
  df <- rbind(df, data.frame(
    "classification"=rep(classification, length.i),
    "var_sill"=var_sill_feature,
    "var_range"=var_range_feature,
    "var_kappa"=var_kappa_feature
  ))
}

# write to CSV
write.csv(df, 'data/variogram_features.csv', row.names=TRUE)

###################
# process test data
###################

load("./data/test.RData")

dataset.i <- test_df$mat

length.i <- length(dataset.i)

# create feature placeholders
var_sill_feature <- rep(NA, length.i)
var_range_feature <- rep(NA, length.i)
var_kappa_feature <- rep(NA, length.i)

df <- data.frame()
# loop data sets
for(j in 1:length.i){
  print(paste('image ', j, '/', length.i))
  
  # select image j in set i
  image.ij <- dataset.i[[j]]
  
  # variogram
  # - convert to raster datatype
  raster_dataset = raster(image.ij)
  
  # - convert to spatial dataset
  points_dataset = rasterToPoints(raster_dataset, spatial=TRUE)
  
  # - fit variogram
  variogram = autofitVariogram(layer~1, points_dataset)
  
  # - extract parameters
  var_sill_feature[j] <- variogram$var_model$psill[2]
  var_range_feature[j] <- variogram$var_model$range[2]
  var_kappa_feature[j] <- variogram$var_model$kappa[2]
  
}

df <- rbind(df, data.frame(
  "var_sill"=var_sill_feature,
  "var_range"=var_range_feature,
  "var_kappa"=var_kappa_feature
))

# write to CSV
write.csv(df, 'data/var_features_test.csv', row.names=TRUE)
