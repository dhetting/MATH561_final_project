rm(list=ls())

setwd('~/src/MATH561_final_project')

load("./data/High/high_train.RData")
load("./data/Medium/med_train.RData")
load("./data/Low/low_train.RData")
load("./data/High/high_validate.RData")
load("./data/Medium/med_validate.RData")
load("./data/Low/low_validate.RData")

ds_list <- list(high_train, med_train, low_train, high_validate, med_validate, low_validate)

classifications = c("high", "med", "low")
class_index = rep(c(1, 2, 3), 2)

df <- data.frame()

# loop data sets
for (i in 1:length(ds_list)) {
  print(paste('dataset ', i, '/', length(ds_list)))
  
  classification <- classifications[class_index[i]]
  
  dataset.i <- ds_list[[i]]$mat
  
  length.i <- length(dataset.i)
  print(length.i)
  df <- rbind(df, data.frame(
    "classification"=rep(classification, length.i)
  ))
}
df$classification <- as.factor(df$classification)

# write to CSV
write.csv(df, 'classification.csv', row.names=TRUE)
