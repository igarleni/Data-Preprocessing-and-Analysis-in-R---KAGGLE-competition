#

library(party)

###############################################
## Conditional inference Classification Tree ##
###############################################
for(i in 1:10){
  cat("Fold iteration number", i, "... \n")
  #generate model
  model <- ctree(formulaClass, trainDataImputed[trainPartitions[[i]], ])
  #predict over test fold
  predictions <- predict(model, newdata = trainDataImputed[testPartitions[[i]], -n])
  #Save statistics
  hits[i] <- sum(trainDataImputed[testPartitions[[i]], n] == predictions)
  errors[i] <- length(trainDataImputed[testPartitions[[i]], n]) - hits[i]
}
#Predict on KAGGLE test data
model <- ctree(formulaClass, trainDataImputed)
kagglePrediction <- predict(model, newdata = testDataImputed)