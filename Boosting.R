#
library(rpart)
library(adabag)

##############
## Boosting ##
##############
#Algorithm
for(i in 1:10){
  cat("Fold iteration number", i, "... \n")
  #generate model
  model <- adabag::boosting(formulaClass, 
                            data = trainDataImputed[trainPartitions[[i]], ],
                            mfinal = finalm, 
                            control = rpart::rpart.control(maxdepth = maxdp))
  #predict over test fold
  predictions <- adabag::predict.boosting(model, newdata = trainDataImputed[testPartitions[[i]], -n])
  #Save statistics
  hits[i] <- sum(trainDataImputed[testPartitions[[i]], n] == predictions$class)
  errors[i] <- length(trainDataImputed[testPartitions[[i]], n]) - hits[i]
}
#Predict on KAGGLE test data
model <- adabag::boosting(formulaClass, data = trainDataImputed, mfinal = finalm, 
                          control = rpart::rpart.control(maxdepth = maxdp))
boostingPrediction <- adabag::predict.boosting(model, newdata = as.data.frame(testDataImputed))
kagglePrediction <- boostingPrediction$prob