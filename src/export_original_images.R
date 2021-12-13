rm(list=ls())

setwd('~/src/MATH561_final_project/src')

load("../data/High/high_train.RData")
load("../data/Medium/med_train.RData")
load("../data/Low/low_train.RData")
load("../data/High/high_validate.RData")
load("../data/Medium/med_validate.RData")
load("../data/Low/low_validate.RData")
load("../data/test.RData")

train_idx = read.csv('../data/train_X.csv')$image_id
validate_idx = read.csv('../data/validate_X.csv')$image_id
test_idx = read.csv('../data/test_X.csv')$image_id

get_group = function(i){
  if (i %in% train_idx) { 
    group = 'train'
  } else if (i %in% validate_idx) {
    group = 'validate'
  } else if  (i %in% test_idx) {
    group = 'test'
  } else {
    group = NULL
  }
  
  return(group)
}

# loop data sets
image_id = 0
for (i in high_train$mat) {
  image_id = image_id + 1
  group = get_group(image_id)
  print(image_id)
  write.csv(i, file=paste('../data/original_images/', group, '/high/', image_id, '.csv', sep=''), row.names = FALSE, col.names = FALSE)
}

for (i in med_train$mat) {
  image_id = image_id + 1
  print(image_id)
  group = get_group(image_id)
  write.csv(i, file=paste('../data/original_images/', group, '/med/', image_id, '.csv', sep=''), row.names = FALSE, col.names = FALSE)
}

for (i in low_train$mat) {
  image_id = image_id + 1
  print(image_id)
  group = get_group(image_id)
  write.csv(i, file=paste('../data/original_images/', group, '/low/', image_id, '.csv', sep=''), row.names = FALSE, col.names = FALSE)
}

for (i in high_validate$mat) {
  image_id = image_id + 1
  print(image_id)
  group = get_group(image_id)
  write.csv(i, file=paste('../data/original_images/', group, '/high/', image_id, '.csv', sep=''), row.names = FALSE, col.names = FALSE)
}

for (i in med_validate$mat) {
  image_id = image_id + 1
  print(image_id)
  group = get_group(image_id)
  write.csv(i, file=paste('../data/original_images/', group, '/med/', image_id, '.csv', sep=''), row.names = FALSE, col.names = FALSE)
}

for (i in low_validate$mat) {
  image_id = image_id + 1
  print(image_id)
  group = get_group(image_id)
  write.csv(i, file=paste('../data/original_images/', group, '/low/', image_id, '.csv', sep=''), row.names = FALSE, col.names = FALSE)
}

# export test set
image_id = 0
for (i in test_df$mat) {
  image_id = image_id + 1
  print(image_id)
  write.csv(i, file=paste('../data/original_images/final_test/', image_id, '.csv', sep=''), row.names = FALSE, col.names = FALSE)
}
