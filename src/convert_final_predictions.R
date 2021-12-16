setwd('~/src/MATH561_final_project/')

final_predictions = read.csv(file = './data/final_predictions.csv')
final_predictions = subset(final_predictions, select = c(variable, pred))
save(final_predictions, file = "./data/final_predictions.RData")
