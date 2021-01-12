#Title: Script2_DeleteDataFunction_V1.0.R
#Version: 1.0
#Dependencies: none
#Author: Thomas F. Johnson
#Date: 19/02/2019


#Core deletion strategies
#Random sampling
guaranteedSampling <- function(DataFrame, ColumnStart, ColumnEnd, ProportionRemove) {
  TrimmedDF = DataFrame[ColumnStart:ColumnEnd]
  RecordsToRemove <- round(nrow(TrimmedDF) * ProportionRemove, digits = 0)
  RowsToDelete <- replicate(ncol(TrimmedDF), sample(nrow(TrimmedDF), size=RecordsToRemove), simplify=FALSE)
  DeletedDF <- mapply(`[<-`, TrimmedDF, RowsToDelete, MoreArgs=list(NA), SIMPLIFY=FALSE)
  Comb = cbind(DataFrame[1:(ColumnStart-1)], DeletedDF, DataFrame[(ColumnEnd+1): length(DataFrame)])
  return(Comb)
}

#Remove from 1-side of the distribution
OrderedRemoval <- function(DataFrame, ColumnStart, ColumnEnd, ProportionRemove, RemoveBy) {
  NumberColumnsPastEnd = length(DataFrame) - length(ColumnEnd)
  OrderedDF = DataFrame[order(RemoveBy),]
  NumberOfRowsToRemove = length(DataFrame[,1])*ProportionRemove
  Remove = head(OrderedDF[ColumnStart:ColumnEnd],-(NumberOfRowsToRemove))
  Temp = matrix(ncol=4, nrow = NumberOfRowsToRemove)
  colnames(Temp) = c("V2", "V3", "V4", "V5")
  RemoveJoin = rbind(Remove, Temp)
  Comb = cbind.fill(OrderedDF[1:(ColumnStart-1)], RemoveJoin, OrderedDF[(ColumnEnd+1):(NumberColumnsPastEnd+1)])
  return(Comb)
}


#Sample 1 side of the distribution more than the other
OrderedSample <- function(DataFrame,ColumnStart, ColumnEnd, ProportionRemove, OrderBy, Classifier, SubsetBy){
  Temp = DataFrame[order(OrderBy),]
  Temp$MegaBranch = c((rep(Classifier[1], ((length(Temp[,1]))*SubsetBy))), rep(Classifier[2], ((length(Temp[,1]))*(1-SubsetBy))))
  Top = guaranteedSampling(subset(Temp, MegaBranch == Classifier[1]), ColumnStart, ColumnEnd, (ProportionRemove * (0.8)))
  Bottom = guaranteedSampling(subset(Temp, MegaBranch == Classifier[2]), ColumnStart, ColumnEnd, (ProportionRemove * (1.066667)))
  Join = rbind.fill(Top, Bottom)
  OrderedJoin = Join[order(Join$TreeOrder),]
  return(OrderedJoin)
}

#Sample by categories
SampleByCharacter <- function(DataFrame, ColumnStart, ColumnEnd, ProportionRemove, ByCharacterColumn){
  LoopThrough = unique(ByCharacterColumn)
  Temp = DataFrame
  Join=NULL
  for (i in 1:length(LoopThrough)){
    SubsettedSample = guaranteedSampling(subset(Temp, ByCharacterColumn == LoopThrough[i]) , ColumnStart, ColumnEnd, ProportionRemove)
    Join = rbind.fill(Join, SubsettedSample)
  }
  OrderedJoin = Join[order(Join$TreeOrder),]
  return(OrderedJoin)
}

#Specific function for deleting data#

#Missing completely at random - remove data points from the predictors at random
MCAR = function(DataFrame, ColumnStart, ColumnEnd, ProportionRemove){
  DataFrame$FunctionName = "MCAR"
guaranteedSampling(DataFrame, ColumnStart, ColumnEnd, ProportionRemove)
}


#Missing on purpose phylogeny - Sample nodes proportionately to number of species per node
MOPP = function(DataFrame, ColumnStart, ColumnEnd, ProportionRemove){
  DataFrame$FunctionName = "MOPP"
  ByCharacterColumn = DataFrame$Node
  SampleByCharacter(DataFrame, ColumnStart, ColumnEnd, ProportionRemove, ByCharacterColumn)
}


#Missing on purpose trait - Sample trait proportionately to number of species per node
MOPT = function(DataFrame, ColumnStart, ColumnEnd, ProportionRemove){
  DataFrame$FunctionName = "MOPT"
  ByCharacterColumn = DataFrame$TraitBreaks
  SampleByCharacter(DataFrame, ColumnStart, ColumnEnd, ProportionRemove, ByCharacterColumn)
}



#Weak bias phylogeny - 4/5 records are removed from the top 75% of the phylogeny, 1/5 records removed from the bottom 25%
WBP = function(DataFrame, ColumnStart, ColumnEnd, ProportionRemove){
  DataFrame$FunctionName = "WBP"
  OrderBy = DataFrame$TreeOrder
  Classifier = c("Top", "Bottom")
  SubsetBy = 0.25
  OrderedSample(DataFrame,ColumnStart, ColumnEnd, ProportionRemove, OrderBy, Classifier, SubsetBy)
}

#Weak bias trait (Predictor) - 4/5 records are removed from the bottom 75% of trait1 values, 1/5 records removed from the top 25%
WBTP = function(DataFrame, ColumnStart, ColumnEnd, ProportionRemove){
  DataFrame$FunctionName = "WBTP"
  OrderBy = DataFrame$V2
  Classifier = c("Top", "Bottom")
  SubsetBy = 0.25
  OrderedSample(DataFrame,ColumnStart, ColumnEnd, ProportionRemove, OrderBy, Classifier, SubsetBy)
}


#Weak bias trait (Response) - 4/5 records are removed from the bottom 75% of response values, 1/5 records removed from the top 25%
WBTR = function(DataFrame, ColumnStart, ColumnEnd, ProportionRemove){
  DataFrame$FunctionName = "WBTR"
  OrderBy = DataFrame$V1
  Classifier = c("Top", "Bottom")
  SubsetBy = 0.25
  OrderedSample(DataFrame,ColumnStart, ColumnEnd, ProportionRemove, OrderBy, Classifier, SubsetBy)
}


#Weak bias trait (Predictor*Response) - 4/5 records are removed from the bottom 75% of predictor*response values, 1/5 records removed from the top 25%
WBTPR = function(DataFrame,ColumnStart, ColumnEnd, ProportionRemove){
  DataFrame$FunctionName = "WBTPR"
  OrderBy1 = DataFrame$V1
  OrderBy2 = DataFrame$V2
  Classifier = c("Top", "Bottom")
  SubsetBy = 0.25
  DataFrame$OrderComb = (OrderBy1 + OrderBy2)
  OrderedSample(DataFrame,ColumnStart, ColumnEnd, ProportionRemove, DataFrame$OrderComb, Classifier, SubsetBy)
}


#Severe bias phylogeny - Records are removed gradully from the top of the tree to the bottom
SBP = function(DataFrame, ColumnStart, ColumnEnd, ProportionRemove){
  DataFrame$FunctionName = "SBP"
  RemoveBy = DataFrame$TreeOrder
  OrderedRemoval(DataFrame, ColumnStart, ColumnEnd, ProportionRemove, RemoveBy)
}


#Severe bias trait (Predictor) - Records gradually removed from the lowest trait value
SBTP = function(DataFrame, ColumnStart, ColumnEnd, ProportionRemove){
  DataFrame$FunctionName = "SBTP"
  RemoveBy = DataFrame$V2
  OrderedRemoval(DataFrame, ColumnStart, ColumnEnd, ProportionRemove, RemoveBy)
}


#Severe bias trait (Response) - Records gradually removed from the species with lowest response values
SBTR = function(DataFrame, ColumnStart, ColumnEnd, ProportionRemove){
  DataFrame$FunctionName = "SBTR"
  RemoveBy = DataFrame$V1
  OrderedRemoval(DataFrame, ColumnStart, ColumnEnd, ProportionRemove, RemoveBy)
}


#Severe bias trait (Predictor*Reponse) - Records gradually removed from the smallest species with low extinction risk
SBTPR = function(DataFrame,ColumnStart, ColumnEnd, ProportionRemove){
  DataFrame$FunctionName = "SBTPR"
  OrderBy1 = DataFrame$V1
  OrderBy2 = DataFrame$V2
  DataFrame$OrderComb = (OrderBy1 + OrderBy2)
  OrderedRemoval(DataFrame,ColumnStart, ColumnEnd, ProportionRemove, DataFrame$OrderComb)
}


FunctionList = c(MCAR, MOPP, MOPT, SBP, SBTP, SBTR, SBTPR, WBP, WBTP, WBTR, WBTPR)

