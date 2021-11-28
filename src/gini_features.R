library(DescTools)
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
  g_feature <- rep(NA, length.i)

  for(j in 1:length.i){
    print(paste('image ', j, '/', length.i))
    
    # select image j in set i
    image.ij <- ds_list[[i]]$mat[[j]]
    
    # calculate Gini coefficient
    g_features[j] = Gini(image.ij)
  }
  
  df <- rbind(df, data.frame(
    "min"=min_feature,
    "max"=max_feature,
    "mean"=mean_feature,
    "sd"=sd_feature,
    "range"=range_feature,
    "sum"=sum_feature,
    "zeroes"=zero_feature
  ))
}

# write to CSV
write.csv(df, 'gini_features.csv', row.names=TRUE)
