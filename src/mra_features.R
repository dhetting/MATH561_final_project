library(wavelets)

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
  mra_D1_min_feature <- rep(NA, length.i)
  mra_D1_max_feature <- rep(NA, length.i)
  mra_D1_mean_feature <- rep(NA, length.i)
  mra_D1_sd_feature <- rep(NA, length.i)
  mra_D1_range_feature <- rep(NA, length.i)
  mra_D1_zero_feature <- rep(NA, length.i)
  
  mra_D2_min_feature <- rep(NA, length.i)
  mra_D2_max_feature <- rep(NA, length.i)
  mra_D2_mean_feature <- rep(NA, length.i)
  mra_D2_sd_feature <- rep(NA, length.i)
  mra_D2_range_feature <- rep(NA, length.i)
  mra_D2_zero_feature <- rep(NA, length.i)
  
  mra_D3_min_feature <- rep(NA, length.i)
  mra_D3_max_feature <- rep(NA, length.i)
  mra_D3_mean_feature <- rep(NA, length.i)
  mra_D3_sd_feature <- rep(NA, length.i)
  mra_D3_range_feature <- rep(NA, length.i)
  mra_D3_zero_feature <- rep(NA, length.i)
  
  for(j in 1:length.i){
    print(paste('image ', j, '/', length.i))

    # select image j in set i
    image.ij <- ds_list[[i]]$mat[[j]]

    # wavlets
    # - calculate coefficients
    mra.dataset = mra(image.ij, n.levels = 3)
    
    # - extract coefficients
    mra.dataset.D1 = matrix(unlist(mra.dataset@D[[1]]), nrow = 288, ncol = 192)
    mra.dataset.D2 = matrix(unlist(mra.dataset@D[[2]]), nrow = 288, ncol = 192)
    mra.dataset.D3 = matrix(unlist(mra.dataset@D[[3]]), nrow = 288, ncol = 192)
    
    # - calculate descriptive statistics
    mra_D1_min_feature[j] <- min(mra.dataset.D1)
    mra_D1_max_feature[j] <- max(mra.dataset.D1)
    mra_D1_mean_feature[j] <- mean(mra.dataset.D1)
    mra_D1_sd_feature[j] <- sd(mra.dataset.D1)
    mra_D1_range_feature[j] <- abs(max(mra.dataset.D1) - min(mra.dataset.D1))
    mra_D1_zero_feature[j] <- sum(mra.dataset.D1 == 0)
    
    mra_D2_min_feature[j] <- min(mra.dataset.D2)
    mra_D2_max_feature[j] <- max(mra.dataset.D2)
    mra_D2_mean_feature[j] <- mean(mra.dataset.D2)
    mra_D2_sd_feature[j] <- sd(mra.dataset.D2)
    mra_D2_range_feature[j] <- abs(max(mra.dataset.D2) - min(mra.dataset.D2))
    mra_D2_zero_feature[j] <- sum(mra.dataset.D2 == 0)
    
    mra_D3_min_feature[j] <- min(mra.dataset.D3)
    mra_D3_max_feature[j] <- max(mra.dataset.D3)
    mra_D3_mean_feature[j] <- mean(mra.dataset.D3)
    mra_D3_sd_feature[j] <- sd(mra.dataset.D3)
    mra_D3_range_feature[j] <- abs(max(mra.dataset.D3) - min(mra.dataset.D3))
    mra_D3_zero_feature[j] <- sum(mra.dataset.D3 == 0)
    
  }

  df <- rbind(df, data.frame(
    "mra_D1_min"=mra_D1_min_feature,
    "mra_D1_max"=mra_D1_max_feature,
    "mra_D1_mean"=mra_D1_mean_feature,
    "mra_D1_sd"=mra_D1_sd_feature,
    "mra_D1_range"=mra_D1_range_feature,
    "mra_D1_zero"=mra_D3_zero_feature,
    
    "mra_D2_min"=mra_D2_min_feature,
    "mra_D2_max"=mra_D2_max_feature,
    "mra_D2_mean"=mra_D2_mean_feature,
    "mra_D2_sd"=mra_D2_sd_feature,
    "mra_D2_range"=mra_D2_range_feature,
    "mra_D2_zero"=mra_D3_zero_feature,
    
    "mra_D3_min"=mra_D3_min_feature,
    "mra_D3_max"=mra_D3_max_feature,
    "mra_D3_mean"=mra_D3_mean_feature,
    "mra_D3_sd"=mra_D3_sd_feature,
    "mra_D3_range"=mra_D3_range_feature,
    "mra_D3_zero"=mra_D3_zero_feature
  ))
}

# write to CSV
write.csv(df, 'data/mra_features.csv', row.names=TRUE)

###################
# process test data
###################

load("./data/test.RData")

dataset.i <- test_df$mat

length.i <- length(dataset.i)

# create feature placeholders
mra_D1_min_feature <- rep(NA, length.i)
mra_D1_max_feature <- rep(NA, length.i)
mra_D1_mean_feature <- rep(NA, length.i)
mra_D1_sd_feature <- rep(NA, length.i)
mra_D1_range_feature <- rep(NA, length.i)
mra_D1_zero_feature <- rep(NA, length.i)

mra_D2_min_feature <- rep(NA, length.i)
mra_D2_max_feature <- rep(NA, length.i)
mra_D2_mean_feature <- rep(NA, length.i)
mra_D2_sd_feature <- rep(NA, length.i)
mra_D2_range_feature <- rep(NA, length.i)
mra_D2_zero_feature <- rep(NA, length.i)

mra_D3_min_feature <- rep(NA, length.i)
mra_D3_max_feature <- rep(NA, length.i)
mra_D3_mean_feature <- rep(NA, length.i)
mra_D3_sd_feature <- rep(NA, length.i)
mra_D3_range_feature <- rep(NA, length.i)
mra_D3_zero_feature <- rep(NA, length.i)
df <- data.frame()
# loop data sets
for(j in 1:length.i){
  print(paste('image ', j, '/', length.i))
  
  # select image j in set i
  image.ij <- dataset.i[[j]]
  
  # wavlets
  # - calculate coefficients
  mra.dataset = mra(image.ij, n.levels = 3)
  
  # - extract coefficients
  mra.dataset.D1 = matrix(unlist(mra.dataset@D[[1]]), nrow = 288, ncol = 192)
  mra.dataset.D2 = matrix(unlist(mra.dataset@D[[2]]), nrow = 288, ncol = 192)
  mra.dataset.D3 = matrix(unlist(mra.dataset@D[[3]]), nrow = 288, ncol = 192)
  
  # - calculate descriptive statistics
  mra_D1_min_feature[j] <- min(mra.dataset.D1)
  mra_D1_max_feature[j] <- max(mra.dataset.D1)
  mra_D1_mean_feature[j] <- mean(mra.dataset.D1)
  mra_D1_sd_feature[j] <- sd(mra.dataset.D1)
  mra_D1_range_feature[j] <- abs(max(mra.dataset.D1) - min(mra.dataset.D1))
  mra_D1_zero_feature[j] <- sum(mra.dataset.D1 == 0)
  
  mra_D2_min_feature[j] <- min(mra.dataset.D2)
  mra_D2_max_feature[j] <- max(mra.dataset.D2)
  mra_D2_mean_feature[j] <- mean(mra.dataset.D2)
  mra_D2_sd_feature[j] <- sd(mra.dataset.D2)
  mra_D2_range_feature[j] <- abs(max(mra.dataset.D2) - min(mra.dataset.D2))
  mra_D2_zero_feature[j] <- sum(mra.dataset.D2 == 0)
  
  mra_D3_min_feature[j] <- min(mra.dataset.D3)
  mra_D3_max_feature[j] <- max(mra.dataset.D3)
  mra_D3_mean_feature[j] <- mean(mra.dataset.D3)
  mra_D3_sd_feature[j] <- sd(mra.dataset.D3)
  mra_D3_range_feature[j] <- abs(max(mra.dataset.D3) - min(mra.dataset.D3))
  mra_D3_zero_feature[j] <- sum(mra.dataset.D3 == 0)
  
}

df <- rbind(df, data.frame(
  "mra_D1_min"=mra_D1_min_feature,
  "mra_D1_max"=mra_D1_max_feature,
  "mra_D1_mean"=mra_D1_mean_feature,
  "mra_D1_sd"=mra_D1_sd_feature,
  "mra_D1_range"=mra_D1_range_feature,
  "mra_D1_zero"=mra_D3_zero_feature,
  
  "mra_D2_min"=mra_D2_min_feature,
  "mra_D2_max"=mra_D2_max_feature,
  "mra_D2_mean"=mra_D2_mean_feature,
  "mra_D2_sd"=mra_D2_sd_feature,
  "mra_D2_range"=mra_D2_range_feature,
  "mra_D2_zero"=mra_D3_zero_feature,
  
  "mra_D3_min"=mra_D3_min_feature,
  "mra_D3_max"=mra_D3_max_feature,
  "mra_D3_mean"=mra_D3_mean_feature,
  "mra_D3_sd"=mra_D3_sd_feature,
  "mra_D3_range"=mra_D3_range_feature,
  "mra_D3_zero"=mra_D3_zero_feature
))

# write to CSV
write.csv(df, 'data/mra_features_test.csv', row.names=TRUE)
