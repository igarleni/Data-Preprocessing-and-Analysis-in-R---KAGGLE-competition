#
source("utilsFunctions.R")

##Read files
trainData <- readDataTrain()
#testData <- readDataTest()


##################
## NA treatment ##
##################

##get NA count
NAvalues <- countNA(trainData)

##Delete items with a min of 40% of NA variables
trainData <- deleteNA(trainData, 40)

##Impute data with MICE
# All variables to impute are Factor, so MICE is the best way to impute them
# There are too many data, so we have to impute them 1 by 1
for (i in 1:ncol(trainData))
{
  if(NAvalues[i,2] != 0)
  {
    cat("Imputing variable", NAvalues[i,1], "...")
    imputed <- mice::mice(trainData[,1:i], m=5, method = "pmm")
    imputedData <- mice::complete(imputed)
    trainData[,i] <- imputedData[,i]
  }
}

##Save imputed data (for future uses)
write.csv(trainData, "imputedData.csv", row.names = F)
