#
library(ggplot2)
library(Hmisc)
library(mice)
library(VIM)
library(robCompositions)
library(outliers)
library(NoiseFiltersR)

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

#########
## Visualization
#########

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

#mice for incomplete vs complete
visualizeMice <- function(dataset)
{
  complete <- mice::ccn(dataset)
  complete
  incomplete <- mice::icn(dataset)
  incomplete
}

####################
#Delete missing Values with a minPercent of NA
####################

# minPercent = 5 as default
deleteNA <- function(dataset, minPercent)
{
  percentNA <- apply (dataset, 1, function(x) sum(is.na(x))) / ncol(dataset) * 100
  badInstances <- (percentNA > minPercent)
  filteredInstances <- dataset[!badInstances,]
  return(filteredInstances)
}

###########
#MICE data imputation
###########

#get imputed data
#methodC = "pmm" | "mean" | ... (see methods(mice))
getMiceImputation <- function(dataset, methodC)
{
  imputed <- mice::mice(datos, m=5, method = methodC)
  return(imputed)
}
#generate imputed dataset
imputeMice <- function(imputed)
{
  imputedData <- mice::complete(imputed)
  return(imputedData)
}

#Imputed mice data visualization
# library(lattice)

#Show imputation of a variable
# imputed$imp$var1

#Graphic plot to compare Var1 distribution vs other variables
# lattice::xyplot(imputed,var1 ~ var2+var3+var4,pch=18,cex=1)

#Density plot of imputed data vs not imputed
# lattice::densxityplot(imputed)

#Barplot ofimputed data
# lattice::wplot(imputed)

#############
## robCompositions imputation
#############

getRobImputation <- function(dataset)
{
  imputed <- robCompositions::impKNNa(dataset)
  return(imputed)
}
#plot ( imputed , which=2)



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
  anomalies <- uni.plot(dataset)
  anomalies.final <- anomalies$outliers
  return(anomalies.final)
}

###########
## library NoiseFiltersR
###########

noiseFilter <- function(dataset, class)
{
  noise <- IPF(dataset[,class]-., data = dataset, s = 2)
  return (noise)
}
#identical(noise$cleanData, dataset[setdiff(1:nrow(dataset), out$remIdx),])



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



#######################################
## Classification methods with 10CFV ##
#######################################



###############################
## Performance meassurements ##
###############################

#recall = TP/(TP+FN)
#specificity = TN/(TN+FP)
#accuracy = TP/(TP+FP)
