#
library(mice)
library(VIM)
library(robCompositions)
library(ggplot2)

###################
## NA processing ##
###################

## This script delete items with >40% NA and apply mice imputation

########
## Functions
########
countNA <- function(dataset)
{
  n <- dim(dataset)
  NAvalues <- c(colnames(dataset))
  NAvalues <- cbind(NAvalues,c(1:n[2]))
  for (i in 1:n[2])
  {
    NAvalues[i,2] <- sum(is.na(dataset[i]))
  }
  return(NAvalues)
}

# minPercent = percent of NA on item
deleteNA <- function(dataset, minPercent)
{
  percentNA <- apply (dataset, 1, function(x) sum(is.na(x))) / ncol(dataset) * 100
  badInstances <- (percentNA > minPercent)
  cat(sum(badInstances), "deleted instances with more than", minPercent,"% NA values.")
  filteredInstances <- dataset[!badInstances,]
  return(filteredInstances)
}


#############
## Info about imputations
#############

###########
#MICE data imputation
#
##get imputed data
#methodC = "pmm" | "mean" | ... (see methods(mice))
#  imputed <- mice::mice(dataset, m=5, method = methodC)
#
##generate imputed dataset
#  imputedData <- mice::complete(imputed)
#
##Imputed mice data visualization
# library(lattice)
#
#Show imputation of a variable
# imputed$imp$var1
#
#Graphic plot to compare Var1 distribution vs other variables
# lattice::xyplot(imputed,var1 ~ var2+var3+var4,pch=18,cex=1)
#
#Density plot of imputed data vs not imputed
# lattice::densxityplot(imputed)
#
#Barplot ofimputed data
# lattice::wplot(imputed)


#############
## robCompositions imputation
#
#  imputed <- robCompositions::impKNNa(dataset)
#  imputados$xImp
#  plot ( imputed , which=2)

###############
## missing values' Visualization
###############

#VIM for NA distribution and pattern
visualizeVIM <- function(dataset)
{
  aggr_plot <- VIM::agrr(dataset, col = c ('blue','red'), numbers = T,
                         sortVars = T, labels = names(dataset), cex.axis= .5,
                         gap = 1, ylab = c("NA Graphic", "Pattern"))
  return (aggr_plot)
}

#NA var1 vs NA var2
NAconfrontation <- function(dataset, var1, var2)
{
  VIM::marginplot(dataset[,var1],dataset[,var2])
}


#############
## Imputation process
#############

##get NA count
NAvalues <- countNA(trainData)
NAvaluesTest <- countNA(testData)

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

for (i in 1:ncol(testData))
{
  if(NAvaluesTest[i,2] != 0)
  {
    cat("Imputing variable", NAvaluesTest[i,1], "...")
    imputedTest <- mice::mice(testData[,1:i], m=5, method = "pmm")
    imputedDataTest <- mice::complete(imputedTest)
    testData[,i] <- imputedDataTest[,i]
  }
}
write.csv(testData, "imputedDataTest.csv", row.names = F)

##Save imputed data (for future uses)
write.csv(trainData, "imputedData.csv", row.names = F)