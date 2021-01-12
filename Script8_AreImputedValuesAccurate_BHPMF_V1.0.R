


#Title: Script8_AreImputedValuesAccurate_BHPMF_V1.0.R
#Version: 1.0
#Dependencies: NULL
#Author: Thomas F. Johnson
#Date: 22/02/2019


#Functions
rowVar <- function(x, ...) {
  rowSums((x - rowMeans(x, ...))^2, ...)/(dim(x)[2] - 1)
}



ExtractImputedPointsBHPMF = function(TrueDataFrame, DeletedDataFrame, ImputedDataFrame, VariableName){
  TrueVarNum = which(colnames(TrueDataFrame) == VariableName)
  DeletedVarNum = which(colnames(DeletedDataFrame) == VariableName)
  ImputedVarNum1 = which(colnames(ImputedDataFrame) == VariableName)
  ImputedVarNum2 = which(colnames(ImputedDataFrame) == paste(VariableName,"_Var",sep=""))
  TrueDataFrame = TrueDataFrame[order(TrueDataFrame$TreeOrder),]
  DeletedDataFrame = DeletedDataFrame[order(DeletedDataFrame$TreeOrder),]
  ImputedDataFrame = ImputedDataFrame[order(ImputedDataFrame$TreeOrder),]
  True = TrueDataFrame[is.na(DeletedDataFrame[,DeletedVarNum]),c(2,TrueVarNum)]
  Imputed = ImputedDataFrame[is.na(DeletedDataFrame[,DeletedVarNum]),c(ImputedVarNum1, ImputedVarNum2)]
  return(cbind(True, Imputed))
}

DescribeDistribution = function(CompleteCaseData, ImputedData, PhyloTree, CompleteCaseSlope, ImputedSlope){
  PhyloTemp = CompleteCaseData[,c("TreeTips", "V2")]
  PhyloTemp$V2 = ifelse(PhyloTemp$V2 == "NA", 0, 1)
  PhyloTemp[is.na(PhyloTemp)] = 0
  PhyloClustering = as.numeric((phylo.d(PhyloTemp, PhyloTree, names.col = TreeTips, binvar = V2, permut = 1000))[[1]])
  MeanImp = mean(ImputedData$V2)
  MeanCC = mean(CompleteCaseData$V2, na.rm = T)
  RangeImp = (range(ImputedData$V2)[2] - range(ImputedData$V2)[1])
  RangeCC = (range(CompleteCaseData$V2, na.rm=T)[2] - range(CompleteCaseData$V2, na.rm=T)[1])
  Descriptors = cbind(StartingTrend[1,1], StartingTrend[1,2], StartingTrend[1,3], StartingTrend[1,4], ImputedData$Function[1], ImputedData$Missing[1], ImpName, PhyloClustering, MeanImp, MeanCC, RangeImp, RangeCC)
  return(Descriptors)
}

SaveResiduals = function(DataFrame, Model){
  if((DataFrame$Missing[1] == 0.4 | DataFrame$Missing[1] == 0.8) & (StartingTrend[1,1] ==  "1" | StartingTrend[1,1] ==  "2")){
    Residuals = as.data.frame(Model$residuals)
    Residuals = cbind(
      Seed  = StartingTrend[1,1],
      CorrelationLevel = StartingTrend[1,2],
      ResponsePresent = StartingTrend[1,3],
      Trend = StartingTrend[1,4],
      FunctionName = DataFrame$FunctionName[1],
      Missing = DataFrame$Missing[1],
      Residuals)
    rownames(Residuals) <- c()
  } else {
    Residuals = NULL
  }
  return(Residuals)
}


BasicRegression = function(DataFrame){
  ModelTemp = lm(V1 ~ V2, data = DataFrame)
  ModelSummary = summary(ModelTemp)
  ModelConfint = confint(ModelTemp)
  ModelOutput = c(StartingTrend[1,1], StartingTrend[1,2], StartingTrend[1,3], StartingTrend[1,4], DataFrame$Function[1], DataFrame$Missing[1],  t(ModelSummary$coefficients[1,1:4]), t(ModelConfint[1,1:2]), t(ModelSummary$coefficients[2,1:4]), t(ModelConfint[2,1:2]))
  Residuals = SaveResiduals(DataFrame, ModelTemp)
  return(list(ModelOutput, Residuals))
}






#Load data
Files = list.files("../Data/RawData")
Nodes = readRDS("../Data/TemporaryData/BHPMFNodeList.rds")
Obvs = readRDS("../Data/TemporaryData/BHPMFObvList.rds")
ImpReport = readRDS("../Data/TemporaryData/ImpReport.rds")

CombinedStartingTrend = NULL
CombinedImputationErrors = NULL
SummarisedImputationErrors = NULL
CombinedRegressionErrors = NULL
RegressionErrorStructure = c("Seed", "CorrelationLevel", "ResponsePresent", "Trend", "FunctionName", "Missing", "InterceptParameterEstimate", "InterceptSE", "InterceptT-Value", "InterceptP-Value", "InterceptLowerCI", "InterceptUpperCI", "TraitParameterEstimate", "TraitSE", "TraitT-Value", "TraitP-Value", "TraitLowerCI", "TraitUpperCI","ImputationApproach")
CombinedResiduals = NULL
CombinedDescriptors = NULL

#Compile all true and imputed records
for(M in Files){
  message(paste(Sys.time(),"File:", M))
  DataList = readRDS(paste("../Data/RawData/",M, sep = ""))
  BaseData = DataList[[1]]
  
  #Combine reports for each step of data flow
  Tree = DataList[[2]]
  StartingTrend = DataList[[4]]
  DeletedData = DataList[[6]]
  rm(DataList)
  
  #Calculate complete-case regression errors
  pb <- progress_bar$new(total = 176)
  for(Z in seq(1,176,1)){
    pb$tick()
    Sys.sleep(1/100)
    
    DF = DeletedData[[Z]]
    CompleteCase = BasicRegression(DF)
    CompleteCaseModel = as.data.frame(t(CompleteCase[[1]]))
    CompleteCaseModel$ImputationApproach = "Complete-case"
    colnames(CompleteCaseModel) = RegressionErrorStructure
    CombinedRegressionErrors = rbind(CombinedRegressionErrors, CompleteCaseModel)
    Residuals = as.data.frame(CompleteCase[[2]])
    if(length(Residuals) > 1){
      Residuals$ImputationApproach = "Complete-case"
      CombinedResiduals = rbind(CombinedResiduals, Residuals)
    } else {
    } 
  }
  
  
  ImpName =  "BHPMFNode"
  FullImpList = Nodes[[substr(M,1,nchar(M)-4)]]
  #Run within each imputation approach
  for(O in seq(1,176,1)){
    
    #Calculate imputation errors
    ImpDF = FullImpList[[O]]
    if(length(ImpDF) > 2){
      colnames(ImpDF)[(length(ImpDF)-4):length(ImpDF)] = c("V1_Var","V2_Var","V3_Var","V4_Var","V5_Var")
      CompareTrueImp_V2 = ExtractImputedPointsBHPMF(BaseData, DeletedData[[O]], ImpDF,"V2")
      CompareTrueImp_V2 = cbind(StartingTrend[1,1], StartingTrend[1,2], StartingTrend[1,3], StartingTrend[1,4], ImpDF$FunctionName[1], ImpDF$Missing[1],ImpName, CompareTrueImp_V2)
      colnames(CompareTrueImp_V2) = c("Seed","CorrelationLevel","ResponsePresent","Trend","FunctionName","Missing","ImputationApproach","TreeTips", "True", "Imputed", "ImputedVariance")
      CombinedImputationErrors = rbind(CombinedImputationErrors,CompareTrueImp_V2)
      
      #Summarise imputation errors
      SumImputationErrors = CompareTrueImp_V2 %>%
        dplyr::group_by(Seed, CorrelationLevel, ResponsePresent, Trend, FunctionName, Missing, ImputationApproach) %>%
        dplyr::summarise(RMSE = sqrt(mean((Imputed - True)^2)), 
                         MeanAE = mean(abs(Imputed - True)), 
                         MedianAE = median(abs(Imputed - True)), 
                         MeanVarOfObv = mean(abs(ImputedVariance^2)))
      SummarisedImputationErrors = rbind(SummarisedImputationErrors, SumImputationErrors)
      
      
      
      #Calculate regression errors
      ImpDF = FullImpList[[O]]
      ImpRegression = BasicRegression(ImpDF)
      ImpRegressionModel = as.data.frame(t(ImpRegression[[1]]))
      ImpRegressionModel$ImputationApproach = ImpName
      colnames(ImpRegressionModel) = RegressionErrorStructure
      CombinedRegressionErrors = rbind(CombinedRegressionErrors, ImpRegressionModel)
      Residuals = as.data.frame(ImpRegression[[2]])
      if(length(Residuals) > 1){
        Residuals$ImputationApproach = ImpName
        CombinedResiduals = rbind(CombinedResiduals, Residuals)
        
        
        #Calculate distribution errors
        Descriptors = DescribeDistribution(DF, ImpDF, Tree, CompleteCaseModel[1,13], ImpRegressionModel[1,13])
        CombinedDescriptors = rbind(CombinedDescriptors, Descriptors)
      } else {
      }
    } else {
    }
  }
  
  
  ImpName =  "BHPMFObv"
  FullImpList = Obvs[[substr(M,1,nchar(M)-4)]]
  #Run within each imputation approach
  for(O in seq(1,176,1)){
    
    #Calculate imputation errors
    ImpDF = FullImpList[[O]]
    if(length(ImpDF) > 2){
      colnames(ImpDF)[(length(ImpDF)-4):length(ImpDF)] = c("V1_Var","V2_Var","V3_Var","V4_Var","V5_Var")
      CompareTrueImp_V2 = ExtractImputedPointsBHPMF(BaseData, DeletedData[[O]], ImpDF,"V2")
      CompareTrueImp_V2 = cbind(StartingTrend[1,1], StartingTrend[1,2], StartingTrend[1,3], StartingTrend[1,4], ImpDF$FunctionName[1], ImpDF$Missing[1],ImpName, CompareTrueImp_V2)
      colnames(CompareTrueImp_V2) = c("Seed","CorrelationLevel","ResponsePresent","Trend","FunctionName","Missing","ImputationApproach","TreeTips", "True", "Imputed", "ImputedVariance")
      CombinedImputationErrors = rbind(CombinedImputationErrors,CompareTrueImp_V2)
      
      #Summarise imputation errors
      SumImputationErrors = CompareTrueImp_V2 %>%
        dplyr::group_by(Seed, CorrelationLevel, ResponsePresent, Trend, FunctionName, Missing, ImputationApproach) %>%
        dplyr::summarise(RMSE = sqrt(mean((Imputed - True)^2)), 
                         MeanAE = mean(abs(Imputed - True)), 
                         MedianAE = median(abs(Imputed - True)), 
                         MeanVarOfObv = mean(abs(ImputedVariance^2)))
      SummarisedImputationErrors = rbind(SummarisedImputationErrors, SumImputationErrors)
      
      
      
      #Calculate regression errors
      ImpDF = FullImpList[[O]]
      ImpRegression = BasicRegression(ImpDF)
      ImpRegressionModel = as.data.frame(t(ImpRegression[[1]]))
      ImpRegressionModel$ImputationApproach = ImpName
      colnames(ImpRegressionModel) = RegressionErrorStructure
      CombinedRegressionErrors = rbind(CombinedRegressionErrors, ImpRegressionModel)
      Residuals = as.data.frame(ImpRegression[[2]])
      if(length(Residuals) > 1){
        Residuals$ImputationApproach = ImpName
        CombinedResiduals = rbind(CombinedResiduals, Residuals)
        
        
        #Calculate distribution errors
        Descriptors = DescribeDistribution(DF, ImpDF, Tree, CompleteCaseModel[1,13], ImpRegressionModel[1,13])
        CombinedDescriptors = rbind(CombinedDescriptors, Descriptors)
      } else {
      }
    } else {
    }
  }
}


saveRDS(SummarisedImputationErrors, "../Data/SummarisedData/SummarisedImputationErrors_BHPMF.rds")
saveRDS(CombinedImputationErrors, "../Data/SummarisedData/CombinedImputationErrors_BHPMF.rds")
saveRDS(CombinedDescriptors, "../Data/SummarisedData/CombinedDescriptors_BHPMF.rds")
saveRDS(CombinedResiduals, "../Data/SummarisedData/CombinedResiduals_BHPMF.rds")
CombinedRegressionErrors2 = subset(CombinedRegressionErrors, ImputationApproach != "Complete-case")
saveRDS(CombinedRegressionErrors2, "../Data/SummarisedData/CombinedRegressionErrors_BHPMF.rds")
saveRDS(ImpReport, "../Data/SummarisedData/CombinedImputationReport_BHPMF.rds")
