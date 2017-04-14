#
library(caret)

############################
## 10cfv Validation setup ##
############################
n <- length(names(trainDataImputed))

indexes <- seq(1,nrow(trainDataImputed),by=1)
trainPartitions <- createFolds(indexes, k = 10,
                               returnTrain = TRUE)
testPartitions <- list()
for(i in 1:10){
  testPartitions[[i]] <- indexes[-trainPartitions[[i]]]
}
errors <- c()
hits <- c()

getAccuracy <- function(errors, hits)
{
  accuracy <- mean(hits/(errors+hits))
  cat("The accuracy is", accuracy, "\n")
  return(accuracy)
}
