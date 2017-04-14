#
library(randomForest)

###################
## Random Forest ##
###################
for(i in 1:10){
  #generate model
  cat("Fold iteration number", i, "... \n")
  model <- randomForest::randomForest(formulaClass, data=trainDataImputed[trainPartitions[[i]], ],
                                      ntree=ntrees)
  #predict over test fold
  predictions <- predict(model, newdata = trainDataImputed[testPartitions[[i]], -n])
  #Save statistics
  hits[i] <- sum(trainDataImputed[testPartitions[[i]], n] == predictions)
  errors[i] <- length(trainDataImputed[testPartitions[[i]], n]) - hits[i]
}
#Predict on KAGGLE test data
model <- randomForest::randomForest(formulaClass, data=trainDataImputed, ntree=ntrees)
kagglePrediction <- predict(model, newdata = testDataImputed)
