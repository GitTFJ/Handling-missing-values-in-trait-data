#Title: Script6_AreImputedValuesAccurate_V1.0.R
#Version: 1.0
#Dependencies: NULL
#Author: Thomas F. Johnson
#Date: 22/02/2019


#Functions
rowVar <- function(x, ...) {
  rowSums((x - rowMeans(x, ...))^2, ...)/(dim(x)[2] - 1)
}


AverageMiceImputations = function(IterationList, VariableName){
  CombineImps = NULL
  for(Q in IterationList){
    Imps = Q[,VariableName]
    CombineImps = as.data.frame(cbind(CombineImps, Imps))
  }
  AverageImp = rowMeans(CombineImps[,1:length(CombineImps)])
  VarImp = rowVar(CombineImps[,1:length(CombineImps)])
  ImpOutput = cbind(IterationList[[1]], AverageImp, VarImp)
  return(ImpOutput)
}


ExtractImputedPointsMice = function(TrueDataFrame, DeletedDataFrame, ImputedDataFrame, VariableName){
  TrueVarNum = which(colnames(TrueDataFrame) == VariableName)
  DeletedVarNum = which(colnames(DeletedDataFrame) == VariableName)
  ImputedVarNum = which(colnames(ImputedDataFrame) == "AverageImp")
  TrueDataFrame = TrueDataFrame[order(TrueDataFrame$TreeOrder),]
  DeletedDataFrame = DeletedDataFrame[order(DeletedDataFrame$TreeOrder),]
  ImputedDataFrame = ImputedDataFrame[order(ImputedDataFrame$TreeOrder),]
  True = TrueDataFrame[is.na(DeletedDataFrame[,DeletedVarNum]),c(2,TrueVarNum)]
  Imputed = ImputedDataFrame[is.na(DeletedDataFrame[,DeletedVarNum]),ImputedVarNum:(ImputedVarNum+1)]
  return(cbind(True, Imputed))
}

ExtractImputedPointsRphylopars = function(TrueDataFrame, DeletedDataFrame, ImputedDataFrame, VariableName){
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

MiceRegression = function(DataFrameList){
  CombinedPE = list()
  CombinedSE = list()
  for(X in seq(1, length(DataFrameList), 1)){
    DF = DataFrameList[[X]]
    ModelTemp = lm(V1 ~ V2, data = DF)
    ParamterEstimate = summary(ModelTemp)$coefficients[1:2,1]
    StandardError = summary(ModelTemp)$coefficients[1:2,2]
    CombinedPE[[X]] <- ParamterEstimate
    CombinedSE[[X]] <- StandardError
  }
  AveragedModel = mi.inference(est = CombinedPE, std.err = CombinedSE, confidence = 0.95) 
  #mi.inference cant estimate t-values
  ModelOutput = c(StartingTrend[1,1], StartingTrend[1,2], StartingTrend[1,3], StartingTrend[1,4], DF$Function[1], DF$Missing[1], AveragedModel$est[1], AveragedModel$std.err[1], NA,  round(AveragedModel$signif[1],5), AveragedModel$lower[1], AveragedModel$upper[1], AveragedModel$est[2], AveragedModel$std.err[2], NA,  AveragedModel$signif[2], AveragedModel$lower[2], AveragedModel$upper[2])
  Residuals = SaveResiduals(DF, ModelTemp)
  return(list(ModelOutput, Residuals))
}



#Load data
Files = list.files("../Data/RawData")

CombinedStartingTrend = NULL
CombinedDeletionReport = NULL
CombinedEigenReport = NULL
CombinedImputationReport = NULL
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
  CombinedStartingTrend = rbind(CombinedStartingTrend, StartingTrend)
  DeletionReport = as.data.frame(DataList[[5]])
  CombinedDeletionReport = rbind(CombinedDeletionReport, DeletionReport)
  DeletedData = DataList[[6]]
  EigenReport = as.data.frame(DataList[[7]])
  CombinedEigenReport = rbind(CombinedEigenReport, EigenReport)
  ImputationReport = as.data.frame(DataList[[9]])
  CombinedImputationReport = rbind(CombinedImputationReport, ImputationReport)
  Imputation = DataList[[10]]
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
  
  
  
  #Run through each imputation approach
  for(N in seq(1,7,1)){
    message(paste("Imputation approach:", N))
    FullImpList = Imputation[[N]]
    ImpName = names(Imputation)[N]
    
    #Run within each imputation approach
    pb <- progress_bar$new(total = 176)
    for(O in seq(1,176,1)){
      pb$tick()
      Sys.sleep(1/100)
      
      #Calculate imputation errors
      if(grepl("Mice",ImpName)){
        ImpList = FullImpList[[O]]
        ImpDF = AverageMiceImputations(ImpList, "V2")
        CompareTrueImp_V2 = ExtractImputedPointsMice(BaseData, DeletedData[[O]], ImpDF,"V2")
      } else {
        ImpDF = FullImpList[[O]]
        CompareTrueImp_V2 = ExtractImputedPointsRphylopars(BaseData, DeletedData[[O]], ImpDF,"V2")
      }
      CompareTrueImp_V2 = cbind(StartingTrend[1,1], StartingTrend[1,2], StartingTrend[1,3], StartingTrend[1,4], ImpDF$FunctionName[1], ImpDF$Missing[1],ImpName, CompareTrueImp_V2)
      colnames(CompareTrueImp_V2) = c("Seed","CorrelationLevel","ResponsePresent","Trend","FunctionName","Missing","ImputationApproach","TreeTips", "True", "Imputed", "ImputedVariance")
      CombinedImputationErrors = rbind(CombinedImputationErrors,CompareTrueImp_V2)
      
      #Summarise imputation errors
      SumImputationErrors = CompareTrueImp_V2 %>%
        dplyr::group_by(Seed, CorrelationLevel, ResponsePresent, Trend, FunctionName, Missing, ImputationApproach) %>%
        dplyr::summarise(RMSE = sqrt(mean((Imputed - True)^2)), 
                         MeanAE = mean(abs(Imputed - True)), 
                         MedianAE = median(abs(Imputed - True)), 
                         MeanVarOfObv = mean(abs(ImputedVariance)))
      SummarisedImputationErrors = rbind(SummarisedImputationErrors, SumImputationErrors)
      
      
      
      #Calculate regression errors
      if(grepl("Mice",ImpName)){
        ImpList = FullImpList[[O]]
        ImpDF = AverageMiceImputations(ImpList, "V2")
        ImpRegression = MiceRegression(ImpList)
        ImpRegressionModel = as.data.frame(t(ImpRegression[[1]]))
        ImpRegressionModel$ImputationApproach = ImpName
        colnames(ImpRegressionModel) = RegressionErrorStructure
        CombinedRegressionErrors = rbind(CombinedRegressionErrors, ImpRegressionModel)
        Residuals = as.data.frame(ImpRegression[[2]])
        if(length(Residuals) > 1){
          Residuals$ImputationApproach = ImpName
          CombinedResiduals = rbind(CombinedResiduals, Residuals)
        } else {
        } 
      } else {
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
        } else {
        } 
      }
      
      #Calculate distribution errors
      Descriptors = DescribeDistribution(DF, ImpDF, Tree, CompleteCaseModel[1,13], ImpRegressionModel[1,13])
      CombinedDescriptors = rbind(CombinedDescriptors, Descriptors)
      
    }
  }
}

saveRDS(CombinedStartingTrend, "../Data/TemporaryData/CombinedStartingTrend.rds")
saveRDS(CombinedDeletionReport, "../Data/TemporaryData/CombinedDeletionReport.rds")
saveRDS(CombinedEigenReport, "../Data/TemporaryData/CombinedEigenReport.rds")
saveRDS(CombinedImputationReport, "../Data/TemporaryData/CombinedImputationReport.rds")
saveRDS(CombinedImputationErrors, "../Data/TemporaryData/CombinedImputationErrors.rds")
saveRDS(SummarisedImputationErrors, "../Data/TemporaryData/SummarisedImputationErrors.rds")
saveRDS(CombinedRegressionErrors, "../Data/TemporaryData/CombinedRegressionErrors.rds")
saveRDS(CombinedResiduals, "../Data/TemporaryData/CombinedResiduals.rds")
saveRDS(CombinedDescriptors, "../Data/TemporaryData/CombinedDescriptors.rds")
