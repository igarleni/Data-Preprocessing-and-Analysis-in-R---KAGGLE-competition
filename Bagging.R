#
library(rpart)
library(adabag)

#############
## Bagging ##
#############
for(i in 1:10){
  cat("Fold iteration number", i, "... \n")
  #generate model
  model <- adabag::bagging(formulaClass, 
                           data = trainDataImputed[trainPartitions[[i]], ], 
                           control=rpart::rpart.control(maxdepth=maxdp, minsplit=minsplt))
  #predict over test fold
  predictions <- adabag::predict.bagging(model, newdata = trainDataImputed[testPartitions[[i]], -n])
  #Save statistics
  hits[i] <- sum(trainDataImputed[testPartitions[[i]], n] == predictions$class)
  errors[i] <- length(trainDataImputed[testPartitions[[i]], n]) - hits[i]
}
#Predict on KAGGLE test data
model <- adabag::bagging(formulaClass, data = trainDataImputed, 
                         control=rpart::rpart.control(maxdepth=maxdp, minsplit=minsplt))
baggingPrediction <- adabag::predict.bagging(model, newdata = as.data.frame(testDataImputed))
kagglePrediction <- baggingPrediction$class