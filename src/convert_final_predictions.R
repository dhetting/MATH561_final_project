final_predictions = read.csv(file = 'final_predictions.csv')
final_predictions = subset(final_predictions, select = c(variable,pred) )
save(final_predictions, file = "final_predictions.RData")
