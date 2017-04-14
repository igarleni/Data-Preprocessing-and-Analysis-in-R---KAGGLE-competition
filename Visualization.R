#
library(ggplot2)
library(Hmisc)

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

#plot all vs class variable
#temp <- dataset
plotY<-function (x) {
  hist(temp[,x], main = names(temp)[x], xlab = paste(names(temp)[x]," X",x,sep=""), breaks = (max(temp[,x])-min(temp[,x])+2) )
}
#par(mfrow=c(6,5))
#x <-sapply(1:(dim(temp)[2]-1), plotY)
