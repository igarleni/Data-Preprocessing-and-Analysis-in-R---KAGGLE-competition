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
testData <- readDataTest()

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

model <- randomForest::randomForest(TIPO_ACCIDENTE ~ TOT_VEHICULOS_IMPLICADOS * 
                                      ZONA_AGRUPADA * ZONA * TIPO_VIA, data=trainDataImputed,
                                    ntree=100)
kagglePrediction <- predict(model, newdata = testData[, -1])

kagglePrediction <- cbind(1:length(kagglePrediction),kagglePrediction)
colnames(kagglePrediction) <- c("Id","Prediccion")
write.csv(kagglePrediction, "kagglePrediction.csv", row.names = F, quote = F)

writeKAGGLEData(kagglePrediction)

