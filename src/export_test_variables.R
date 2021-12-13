rm(list=ls())

setwd('~/src/MATH561_final_project')

load("./data/test.RData")

vars <- test_df$var

df <- data.frame(variable=vars)

# write to CSV
write.csv(df, 'data/test_variables.csv', row.names=TRUE)
