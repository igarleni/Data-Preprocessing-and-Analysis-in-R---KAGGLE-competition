#
library(ggplot2)
library(Hmisc)
library(mice)
library(VIM)
library(robCompositions)
library(outliers)
library(mvoutlier)
library(NoiseFiltersR)
library(FSelector)
library(caret)
library(party)
library(rpart)
library(adabag)
library(randomForest)
library(kernlab)

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
  results <- cbind(1:length(results),results)
  results <- rbind(c("Id","Prediccion"))
  write.csv(results, "kagglePrediction.csv", row.names = F, quote = F)
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

#count NA values
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


####################
#Delete missing Values with a minPercent of NA
####################

# minPercent = percent of NA on item
deleteNA <- function(dataset, minPercent)
{
  percentNA <- apply (dataset, 1, function(x) sum(is.na(x))) / ncol(dataset) * 100
  badInstances <- (percentNA > minPercent)
  cat(sum(badInstances), "deleted instances with more than", minPercent,"% NA values.")
  filteredInstances <- dataset[!badInstances,]
  return(filteredInstances)
}


###########
#MICE data imputation
###########

##get imputed data
#methodC = "pmm" | "mean" | ... (see methods(mice))
#  imputed <- mice::mice(dataset, m=5, method = methodC)

##generate imputed dataset
#  imputedData <- mice::complete(imputed)

##Imputed mice data visualization
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

#  imputed <- robCompositions::impKNNa(dataset)
#  imputados$xImp
#  plot ( imputed , which=2)



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



########################
## Variable selection ##
########################

############
## Approximation filters
############

filterSelection <- function(dataset, type)
{
  switch(type,
         ##Chi-squared
         chiSquared =
         {
           weights <- FSelector::chi.squared(TIPO_ACCIDENTE~.,dataset)
           return(weights)
         },
         ##Correlation
         #only for continuous data
         correlation =
         {
           weights <- FSelector::linear.correlation(TIPO_ACCIDENTE~.,dataset)
           return(weights)
         },
         ##Entropy based - information gain
         #only for discrete data
         entropInfo =
         {
           weights <- FSelector::information.gain(TIPO_ACCIDENTE~.,dataset)
           return(weights)
         },
         ##Entropy based - ratio information gain
         #only for discrete data
         entropInfoRatio =
         {
           weights <- FSelector::gain.ratio(TIPO_ACCIDENTE~.,dataset)
           return(weights)
         },
         ##Entropy based - symmetrical uncertainty
         #only for discrete data
         entropSymm =
         {
           weights <- FSelector::symmetrical.uncertainty(TIPO_ACCIDENTE~.,dataset)
           return(weights)
         },
         ##oneR
         #only for discrete data
         oneR =
         {
           weights <- FSelector::oneR(TIPO_ACCIDENTE~.,dataset)
           return(weights)
         }
         ##relief needs distance formula
         #relief =
         #{
         #  weights <- FSelector::relief(TIPO_ACCIDENTE~.,dataset)
         #   return(weights)
         #}
  )
}
############
## Approximation wrapper
############

##best.first.search


##exhaustive.search


##greedy.search


##hill.climbing.search


##cfs


##consistency


############
## Approximation embedded
############

##random.forest.importance
# type 1 = Predictive reliability reduction
# type 2 = mean impure node reduction
approximationRFI <- function(dataset, type)
{
  weights <- FSelector::random.forest.importance(TIPO_ACCIDENTE~.,dataset, type)
  return(weights)
}

############
## caret package
############


############
## boruta package
############



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
## Classification methods with 5CFV ##
#######################################


######
## Conditional inference Classification Tree
######
classificationTree <- function(dataset, kaggleTest, trainPartitions, testPartitions)
{
  errors <- c()
  hits <- c()
  #Algorithm
  n <- ncol(dataset)
  variableClass <- names(dataset)[n]
  formulaClass <- as.formula(paste(variableClass,"~TOT_VEHICULOS_IMPLICADOS * ZONA_AGRUPADA * CARRETERA * ZONA * TIPO_VIA",sep=""))
  for(i in 1:5){
    #generate model
    model <- ctree(formulaClass, dataset[trainPartitions[[i]], ])
    #predict over test fold
    predictions <- predict(model, newdata = dataset[testPartitions[[i]], -n])
    #Save statistics
    hits[i] <- sum(dataset[testPartitions[[i]], n] == predictions)
    errors[i] <- length(dataset[testPartitions[[i]], n]) - hits[i]
  }
  accuracy <- mean(hits/(errors+hits))
  cat("Accuracy:", accuracy)
  #Predict on KAGGLE test data
  model <- ctree(formulaClass, dataset)
  kagglePrediction <- predict(model, newdata = kaggleTest[, -1])
}

######
## Bagging
######
#Variables input
maxdp = 5
minsplt = 15
baggingAlgorithm <- function(dataset, kaggleTest, trainPartitions, testPartitions, maxdp, minsplt)
{
  errors <- c()
  hits <- c()
  #Algorithm
  for(i in 1:5){
    #generate model
    model <- adabag::bagging(TIPO_ACCIDENTE ~ ., 
                             data = dataset[trainPartitions[[i]], ], 
                             control=rpart::rpart.control(maxdepth=maxdp, minsplit=minsplt))
    #predict over test fold
    predictions <- adabag::predict.bagging(model, newdata = dataset[testPartitions[[i]], -n])
    #Save statistics
    hits[i] <- sum(dataset[testPartitions[[i]], n] == predictions$class)
    errors[i] <- length(dataset[testPartitions[[i]], n]) - hits[i]
  }
  accuracy <- mean(hits/(errors+hits))
  cat("Accuracy:", accuracy)
  #Predict on KAGGLE test data
  model <- adabag::bagging(TIPO_ACCIDENTE ~ ., data = dataset, 
                           control=rpart::rpart.control(maxdepth=maxdp, minsplit=minsplt))
  baggingPrediction <- adabag::predict.bagging(model, newdata = as.data.frame(kaggleTest[, -1]))
  kagglePrediction <- baggingPrediction$class
  return(kagglePrediction)
}

######
## Boosting
######
maxdp = 2
finalm = 10
boostingAlgorithm <- function(dataset, kaggleTest, trainPartitions, testPartitions, maxdp, finalm)
{
  errors <- c()
  hits <- c()
  #Algorithm
  for(i in 1:5){
    #generate model
    model <- adabag::boosting(TIPO_ACCIDENTE ~ ., 
                              data = dataset[trainPartitions[[i]], ],
                              mfinal = finalm, 
                              control = rpart::rpart.control(maxdepth = maxdp))
    #predict over test fold
    predictions <- adabag::predict.boosting(model, newdata = dataset[testPartitions[[i]], -n])
    #Save statistics
    hits[i] <- sum(dataset[testPartitions[[i]], n] == predictions$class)
    errors[i] <- length(dataset[testPartitions[[i]], n]) - hits[i]
  }
  accuracy <- mean(hits/(errors+hits))
  cat("Accuracy:", accuracy)
  #Predict on KAGGLE test data
  model <- adabag::boosting(TIPO_ACCIDENTE ~ ., data = dataset, mfinal = finalm, 
                            control = rpart::rpart.control(maxdepth = maxdp))
  boostingPrediction <- adabag::predict.boosting(model, newdata = as.data.frame(kaggleTest[, -1]))
  kagglePrediction <- boostingPrediction$class
  return(kagglePrediction)
}

######
## RandomForest
######
ntrees = 100
randomForestAlgorithm <- function(dataset, kaggleTest, trainPartitions, testPartitions, ntrees)
{
  errors <- c()
  hits <- c()
  #Algorithm
  for(i in 1:5){
    #generate model
    model <- randomForest::randomForest(TIPO_ACCIDENTE ~ TIPO_ACCIDENTE ~ TOT_VEHICULOS_IMPLICADOS * ZONA_AGRUPADA * ZONA * TIPO_VIA, data=dataset[trainPartitions[[i]], ],
                                        ntree=ntrees)
    #predict over test fold
    predictions <- predict(model, newdata = dataset[testPartitions[[i]], -n])
    #Save statistics
    hits[i] <- sum(dataset[testPartitions[[i]], n] == predictions)
    errors[i] <- length(dataset[testPartitions[[i]], n]) - hits[i]
  }
  accuracy <- mean(hits/(errors+hits))
  cat("Accuracy:", accuracy)
  #Predict on KAGGLE test data
  model <- randomForest::randomForest(TIPO_ACCIDENTE ~ TOT_VEHICULOS_IMPLICADOS * ZONA_AGRUPADA * ZONA * TIPO_VIA, data=dataset, ntree=ntrees)
  kagglePrediction <- predict(model, newdata = kaggleTest[, -1])
  return(kagglePrediction)
}

######
## SVM
######
SVMalgorithm <- function(dataset, kaggleTest, trainPartitions, testPartitions)
{
  errors <- c()
  hits <- c()
  #Algorithm
  for(i in 1:5){
    #generate model
    model <- train(TIPO_ACCIDENTE ~ ., data = dataset[testPartitions[[i]], ], method = "svmRadial",
                   preProc = c("center", "scale"))
    #predict over test fold
    predictions <- predict(model, newdata = dataset[testPartitions[[i]], -n])
    #Save statistics
    hits[i] <- sum(dataset[testPartitions[[i]], n] == predictions)
    errors[i] <- length(dataset[testPartitions[[i]], n]) - hits[i]
  }
  accuracy <- mean(hits/(errors+hits))
  cat("Accuracy:", accuracy)
  model <- train(TIPO_ACCIDENTE ~ ., 
                 data = dataset, 
                 method = "svmRadial", preProc = c("center", "scale"))
  kagglePrediction <- predict(model, newdata = kaggleTest[, -1])
  return(kagglePrediction)
}


###############################
## Performance meassurements ##
###############################

#recall = TP/(TP+FN)
#specificity = TN/(TN+FP)
#accuracy = TP/(TP+FP)
