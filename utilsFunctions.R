#

#########
## I/O ##
#########

#Data test and training readers
library(readr)
readDataTrain <- function()
{
  dataset <- read_csv("accidentes-kaggle.csv")
  return(dataset)
}

readDataTest <- function()
{
  library(readr)
  dataset <- read_csv("accidentes-kaggle-test.csv")
  return(dataset)
}

#KAGGLE output generator
writeKAGGLEData <- function(results)
{
  write.csv(results, "kagglePrediction.csv", row.names = F)
}


###################
## Visualization ##
###################



################################
## missing values' imputation ##
################################



#########################
## Anomalies detection ##
#########################



#########################
## Data transformation ##
#########################



#