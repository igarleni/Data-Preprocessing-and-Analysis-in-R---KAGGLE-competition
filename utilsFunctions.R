#
library(ggplot2)
library(Hmisc)
library(mice)

#########
## I/O ##
#########
#Data test and training readers
readDataTrain <- function()
{
  dataset <- read.csv("accidentes-kaggle.csv", dec = ",")
  return(dataset)
}

readDataTest <- function()
{
  dataset <- read.csv("accidentes-kaggle-test.csv", dec = ",")
  return(dataset)
}

#KAGGLE output generator
writeKAGGLEData <- function(results)
{
  write.csv(results, "kagglePrediction.csv", row.names = F)
}


################################
## missing values' imputation ##
################################

#Delete missing Values with a minPercent of NA
# minPercent = 5 as default
deleteNA <- function(dataset, minPercent)
{
  percentNA <- apply (dataset, 1, function(x) sum(is.na(x))) / ncol(dataset) * 100
  badInstances <- (percentNA > minPercent)
  filteredInstances <- dataset[!badInstances,]
  return(filteredInstances)
}

#MICE data imputation
#methodC = "pmm" | "mean" | ... (see methods(mice))
miceImputation <- function(dataset, methodC)
{
  pattern <- mice::md.pattern(x = dataset)
  imputed <- mice::mice(datos, m=5, method = methodC)
  imputedData <- mice::complete(imputed)
  return(imputedData)
}
#watch sample script imputationMice.r for plotting results


#########################
## Anomalies detection ##
#########################



###################
## Visualization ##
###################
#Var1 vs Var2
simpleVisualization <- function(dataset, var1, var2)
{
  ggplot(data = dataset) + geom_point(mapping = aes(x = var1, y = var2))
}

#Var1 vs Var2 + class color
colorClassVisualization <- function(dataset, var1, var2, class)
{
  ggplot(data = mpg) +  
    geom_point(mapping = aes(x = var1, y = var2, color = class))
}

#Var1 vs Var2 + divided by class
dividedClassVisualization <- function(dataset, var1, var2, class)
{
  ggplot(data = dataset) + 
    geom_point(mapping = aes(x = var1, y = var2)) + 
    facet_wrap(~ class, nrow = 2)
}

#Var1 vs Var2 + geom_smooth class (show data trends)
geomClassVisualization <- function(dataset, var1, var2, class)
{
  
  ggplot(data = dataset) + 
    geom_smooth(mapping = aes(x = var1, y = var2, linetype = class))
}

#Show variable item count + class
variableCountVisualization <- function(dataset, var, class)
{
  
  ggplot(data = dataset) +
    geom_bar(mapping = aes(x=var, fill=class))
}

#Class depending on a variable
classVariableVisualization <- function(dataset, var, class)
{
  ggplot(data = dataset, mapping = aes(x = class, y = var)) +
  geom_boxplot() +
  coord_flip()
}


###################
## Data overview ##
###################






#