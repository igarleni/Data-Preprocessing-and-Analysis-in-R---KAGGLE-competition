#

#Clear workspace
rm(list = ls())

##Read files
source("DataReader.R")


##################
## NA treatment ##
##################
source("NAprocessing.R")
#Imputed data is now saved on 2 different csv's and in testDataImputed/trainDataImputed variable


########################
## Variable selection ##
########################
source("VariableSelection.R")
final.weight.ordered
###variables selected:
# TOT_VEHICULOS_IMPLICADOS, ZONA_AGRUPADA, CARRETERA, ZONA, TIPO_VIA, TRAZADO_NO_INTERSEC,
# RED_CARRETERA, ACERAS
idClass <- length(names(trainData))
classVariable <- names(trainData)[idClass]
formulaClassSelected <- as.formula(paste(classVariable, "~ TOT_VEHICULOS_IMPLICADOS + ZONA_AGRUPADA +
                                         CARRETERA + ZONA + TIPO_VIA + TRAZADO_NO_INTERSEC + RED_CARRETERA
                                         + ACERAS", sep = ""))
formulaClassAll <- as.formula(paste(classVariable, "~.", sep = ""))
#choose between variable selection or full variables
formulaClass <- formulaClassAll


###############################
## Classification algorithms ##
###############################
#Clear workspace for using imputedData use (impute data takes too long to do on each model generation)
rm(list = ls())
##Read imputed data (for future uses)
trainDataImputed <- read.csv("imputedData.csv")
testDataImputed <- read.csv("imputedDataTest.csv")


##5 cross fold validation setup
source("CrossFoldSetup.R")
idClass <- length(names(trainDataImputed))
classVariable <- names(trainDataImputed)[idClass]
#use function getAccuracy(errors,hits) to get model's accuracy

#Test1
formulaClass <- as.formula(paste(classVariable, "~ TOT_VEHICULOS_IMPLICADOS * 
                                      ZONA_AGRUPADA * ZONA * TIPO_VIA", sep = ""))
ntrees <- 100
source("RandomForest.R")
#Accuracy on 10cfv -> 0.8203448
#Accuracy on KAGGLE -> 0.81930

#Test 2
formulaClass <- as.formula(paste(classVariable, "~ TOT_VEHICULOS_IMPLICADOS * 
                                      ZONA_AGRUPADA * ZONA * TIPO_VIA", sep = ""))
ntrees <- 2000
source("RandomForest.R")
#Accuracy on 10cfv -> 0.8202781
#Accuracy on KAGGLE -> 0.81930

#Test 3
formulaClass <- as.formula(paste(classVariable, "~ TOT_VEHICULOS_IMPLICADOS * 
                                 ZONA_AGRUPADA * ZONA * TIPO_VIA * TRAZADO_NO_INTERSEC
                                 * RED_CARRETERA * ACERAS", sep = ""))
ntrees <- 500
source("RandomForest.R")
#Accuracy on 10cfv -> 0.8216112
#Accuracy on KAGGLE -> 0.82158

#Test 4
formulaClass <- as.formula(paste(classVariable, "~. -CARRETERA - ISLA - MEDIDAS_ESPECIALES", sep = ""))
ntrees <- 500
source("RandomForest.R")
#Accuracy on 10cfv -> (takes too long)
#Accuracy on KAGGLE -> 0.82158

#Test 5
formulaClass <- as.formula(paste(classVariable, "~. -CARRETERA - ISLA - MEDIDAS_ESPECIALES", sep = ""))
ntrees <- 50
source("RandomForest.R")
#Accuracy on 10cfv -> 0.8281116
#Accuracy on KAGGLE -> 0.82859

#Test 6
formulaClass <- as.formula(paste(variableClass, "~ TOT_VEHICULOS_IMPLICADOS * ZONA_AGRUPADA *
                                 ZONA * TIPO_VIA * TRAZADO_NO_INTERSEC",sep=""))
source("CicTree.R")
#Accuracy on 10cfv -> 0.8214445
#Accuracy on KAGGLE -> 0.81891

#Test 7
formulaClass <- as.formula(paste(variableClass, "~ TOT_VEHICULOS_IMPLICADOS * ZONA_AGRUPADA*ZONA
                                 * TIPO_VIA * TRAZADO_NO_INTERSEC * RED_CARRETERA*ACERAS *
                                 PRIORIDAD *TIPO_INTERSEC *PROVINCIA * TOT_HERIDOS_LEVES *
                                 SUPERFICIE_CALZADA ",sep=""))
source("CicTree.R")
#Accuracy on 10cfv -> 0.8250111
#Accuracy on KAGGLE -> 0.82573

#Test 8
maxdp = 2
finalm = 10
formulaClass <- as.formula(paste(variableClass, "~ TOT_VEHICULOS_IMPLICADOS + ZONA_AGRUPADA + ZONA +
                                  TIPO_VIA + TRAZADO_NO_INTERSEC + RED_CARRETERA",sep=""))
source("Boosting.R")
#Accuracy on 10cfv -> 0.8191449
#Accuracy on KAGGLE -> 0.81891

#Test 9
formulaClass <- as.formula(paste(variableClass, "~ TOT_VEHICULOS_IMPLICADOS * ZONA_AGRUPADA * ZONA *
                                 TIPO_VIA * TRAZADO_NO_INTERSEC * RED_CARRETERA"))
source("CicTree.R")
#Accuracy on 10cfv -> 0.8214445
#Accuracy on KAGGLE -> 0.81891

#Test 10
ntrees <- 500
formulaClass <- as.formula(paste(variableClass, "~ TOT_VEHICULOS_IMPLICADOS * ZONA_AGRUPADA * ZONA *
                                 TIPO_VIA * TRAZADO_NO_INTERSEC * RED_CARRETERA"))
source("RandomForest.R")
#Accuracy on 10cfv -> 0.8209446
#Accuracy on KAGGLE -> 0.81930

#Test 11
maxdp = 5
minsplt = 15
formulaClass <- as.formula(paste(variableClass, "~ TOT_VEHICULOS_IMPLICADOS + ZONA_AGRUPADA + ZONA + 
                                 TIPO_VIA + TRAZADO_NO_INTERSEC + RED_CARRETERA"))
source("Bagging.R")
#Accuracy on 10cfv -> 0.8191449
#Accuracy on KAGGLE -> 0.81891


#Generate KAGGLE output
write.csv(results, "kagglePrediction.csv", quote = F)
