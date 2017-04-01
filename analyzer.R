#
source("utilsFunctions.R")

##Read files
trainData <- readDataTrain()
testData <- readDataTest()


##################
## NA treatment ##
##################

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

########################
## Variable selection ##
########################

weightchiSquared <- filterSelection(trainData,"chiSquared")
weightentropInfo <- filterSelection(trainData,"entropInfo")
weightentropInfoRatio <- filterSelection(trainData,"entropInfoRatio")
weightentropSymm <- filterSelection(trainData,"entropSymm")

final.weight <- (weightchiSquared + weightentropInfo + weightentropInfoRatio + weightentropSymm)/4

final.weight.ordered <- cbind(final.weight, rownames(final.weight))
final.weight.ordered <- final.weight.ordered[order(final.weight, decreasing = T),]

final.weight.ordered

###variables selected:
# TOT_VEHICULOS_IMPLICADOS, ZONA_AGRUPADA, CARRETERA, ZONA, TIPO_VIA, TRAZADO_NO_INTERSEC,
# RED_CARRETERA, ACERAS




#Reset workspace



#
source("utilsFunctions.R")

##Read imputed data (for future uses)
trainDataImputed <- read.csv("imputedData.csv")
testDataImputed <- read.csv("imputedDataTest.csv")

##############################
## Classification algorithm ##


##5 cross fold validation setup
indexes <- seq(1,nrow(trainDataImputed),by=1)
trainPartitions <- createFolds(indexes, k = 5,
                               returnTrain = TRUE)
testPartitions <- list()
for(i in 1:5){
  testPartitions[[i]] <- indexes[-trainPartitions[[i]]]
}

#Test1
model <- randomForest::randomForest(TIPO_ACCIDENTE ~ TOT_VEHICULOS_IMPLICADOS * 
                                      ZONA_AGRUPADA * ZONA * TIPO_VIA, data=trainDataImputed,
                                    ntree=2000)
kagglePrediction <- predict(model, newdata = testDataImputed)
#0.81930

#Test 2
model <- randomForest::randomForest(TIPO_ACCIDENTE ~ TOT_VEHICULOS_IMPLICADOS * 
                                      ZONA_AGRUPADA * ZONA * TIPO_VIA, data=trainDataImputed,
                                    ntree=2000)
kagglePrediction <- predict(model, newdata = testDataImputed)
#0.81930

#Test 3
trainDataImputed.randomForest <- trainDataImputed[,-15]
testData.randomForest <- testDataImputed[,-15]

model <- randomForest::randomForest(TIPO_ACCIDENTE ~ TOT_VEHICULOS_IMPLICADOS * ZONA_AGRUPADA * ZONA * TIPO_VIA * TRAZADO_NO_INTERSEC *
                                     RED_CARRETERA * ACERAS, data=trainDataImputed.randomForest,
                                     ntree=500)
kagglePrediction <- predict(model, newdata = testData.randomForest)
#0.82158

#Test 4
model <- randomForest::randomForest(TIPO_ACCIDENTE ~. -CARRETERA - ISLA - MEDIDAS_ESPECIALES, data=trainDataImputed,
                                     ntree=500)
kagglePrediction <- predict(model, newdata = testDataImputed)
#0.82859

#Test 5
n <- ncol(trainDataImputed)
variableClass <- names(trainDataImputed)[n]
formulaClass <- as.formula(paste(variableClass,"~ TOT_VEHICULOS_IMPLICADOS * ZONA_AGRUPADA * ZONA * TIPO_VIA * TRAZADO_NO_INTERSEC",sep=""))
model <- ctree(formulaClass, trainDataImputed)
kagglePrediction <- predict(model, newdata = testDataImputed)
#0.81891

#Test 6
n <- ncol(trainDataImputed)
variableClass <- names(trainDataImputed)[n]
formulaClass <- as.formula(paste(variableClass,"~TOT_VEHICULOS_IMPLICADOS * ZONA_AGRUPADA*ZONA *TIPO_VIA *TRAZADO_NO_INTERSEC* RED_CARRETERA*ACERAS *PRIORIDAD *TIPO_INTERSEC *PROVINCIA * TOT_HERIDOS_LEVES * SUPERFICIE_CALZADA ",sep=""))
model <- ctree(formulaClass, trainDataImputed)
kagglePrediction <- predict(model, newdata = testDataImputed)
#0.82573

#Test 7
maxdp = 2
finalm = 10
model <- adabag::boosting(TIPO_ACCIDENTE ~
                            TOT_VEHICULOS_IMPLICADOS + 
                            ZONA_AGRUPADA + 
                            ZONA + 
                            TIPO_VIA +
                            TRAZADO_NO_INTERSEC +
                            RED_CARRETERA,
                          data = trainDataImputed, mfinal = finalm, 
                          control = rpart::rpart.control(maxdepth = maxdp))
boostingPrediction <- adabag::predict.boosting(model, newdata = as.data.frame(testDataImputed))
kagglePrediction <- boostingPrediction$class
#0.81891

#Test 8
library(party)
library(caret)
variableClass <- names(trainDataImputed)[30]
formulaClass <- as.formula(paste(variableClass, "~TOT_VEHICULOS_IMPLICADOS * ZONA_AGRUPADA * ZONA * TIPO_VIA * TRAZADO_NO_INTERSEC * RED_CARRETERA"))
model <- ctree(formulaClass, trainDataImputed)
kagglePrediction <- predict(model, newdata = testDataImputed)
#0.81891

#Test 9
model <- randomForest::randomForest(TIPO_ACCIDENTE ~ TOT_VEHICULOS_IMPLICADOS * ZONA_AGRUPADA * ZONA * TIPO_VIA * TRAZADO_NO_INTERSEC * RED_CARRETERA, data=trainDataImputed, ntree=500)
kagglePrediction <- predict(model, newdata = testDataImputed)
#0.81930

#Test 10
maxdp = 5
minsplt = 15
model <- adabag::bagging(TIPO_ACCIDENTE ~ 
                           TOT_VEHICULOS_IMPLICADOS + 
                           ZONA_AGRUPADA + 
                           ZONA + 
                           TIPO_VIA +
                           TRAZADO_NO_INTERSEC +
                           RED_CARRETERA,
                         data = trainDataImputed, 
                         control=rpart::rpart.control(maxdepth=maxdp, minsplit=minsplt))
baggingPrediction <- adabag::predict.bagging(model, newdata = testDataImputed)
kagglePrediction <- baggingPrediction$class
#0.81891

#write data
writeKAGGLEData(kagglePrediction)
