#
library(outliers)
library(ggplot2)
library(mice)
library(mvoutlier)
library(NoiseFiltersR)

#################################
## Anomalies and noise dection ##
#################################

########
## Library outliers
########

#this method considers only one variable, not its interaction with other variables.
# It analyze the variable looking for deviations.
outliersAnomalies <- function(dataset, columns)
{
  anomalies <- outlier(dataset[,columns])
  return(anomalies)
}
##visualize var1 mean and distribution
#mean(dataset[,var1])
#ggplot(data = dataset) + geum_bar(mapping = aes(x = var1))

#########
## Library mvoutlier
#########

mvoutlierAnomalies <- function(dataset)
{
  require(mvoutlier)
  anomalies <- uni.plot(dataset)
  anomalies.final <- anomalies$outliers
  return(anomalies.final)
}
#dataset <- dataset[!anomalies.final, ]

###########
## library NoiseFiltersR
###########

# set.seed(1)
# noise <- IPF(class~., data = dataset, s = 2)
# identical(noise$cleanData, dataset[setdiff(1:nrow(dataset), out$remIdx),])