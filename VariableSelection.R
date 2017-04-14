#
library(FSelector)
library(rpart)

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
#
##best.first.search
#
##exhaustive.search
#
##greedy.search
#
##hill.climbing.search
#
##cfs
#
##consistency


############
## Approximation embedded
#
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
## boruta package


#############
## Selection process
#############
weightchiSquared <- filterSelection(trainData,"chiSquared")
weightentropInfo <- filterSelection(trainData,"entropInfo")
weightentropInfoRatio <- filterSelection(trainData,"entropInfoRatio")
weightentropSymm <- filterSelection(trainData,"entropSymm")

#Normalize results
weightchiSquared <- (weightchiSquared - min(weightchiSquared)) / (max(weightchiSquared) - min(weightchiSquared))
weightentropInfo <- (weightentropInfo - min(weightentropInfo)) / (max(weightentropInfo) - min(weightentropInfo))
weightentropInfoRatio <- (weightentropInfoRatio - min(weightentropInfoRatio)) / (max(weightentropInfoRatio) - min(weightentropInfoRatio))
weightentropSymm <- (weightentropSymm - min(weightentropSymm)) / (max(weightentropSymm) - min(weightentropSymm))

final.weight <- (weightchiSquared + weightentropInfo + weightentropInfoRatio + weightentropSymm)/4

final.weight.ordered <- cbind(final.weight, rownames(final.weight))
final.weight.ordered <- final.weight.ordered[order(final.weight, decreasing = T),]