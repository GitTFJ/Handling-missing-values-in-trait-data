#Title: Script4_CalculateEigenvectors_V1.0.R
#Version: 1.0
#Dependencies: BaseData, DeletedData, Tree
#Author: Thomas F. Johnson
#Date: 19/02/2019


RobustPEM = function(Response, TGraph){
  PEM = tryCatch(
    {
      TrimmedPEM = PEM.fitSimple(
        y = Response,
        x = NULL,
        w = TGraph,
        d = "distance", sp = "species")
      Error_In_La.svd = "No"
      PEM = list(TrimmedPEM, Error_In_La.svd)
    },
    error = function(cond){
      BasePEM = PEM.fitSimple(
        y = BaseData$V2,
        x = NULL,
        w = TreeGraph,
        d = "distance", sp="species")
      Error_In_La.svd = "Yes"
      PEM = list(TrimmedPEM, Error_In_La.svd)
    }
  )
  return(PEM)
} 

EigenReport = NULL

#Base eigen calculation
StartTime = Sys.time()

TreeGraph = Phylo2DirectedGraph(Tree)
BasePEM = PEM.fitSimple(
  y = BaseData$V2,
  x = NULL,
  w = TreeGraph,
  d = "distance", sp="species")
BasePEM_DF = as.data.frame(BasePEM)

BasePEM_LM = lmforwardsequentialsidak(
  y = BaseData$V2,
  object = BasePEM,
  alpha = 0.01)

EndTime = Sys.time()

BaseEigens = as.data.frame(BasePEM_LM$qr$qr)[,-1]
TreeTips = as.character(rownames(BaseEigens))
BaseEigens = cbind(TreeTips, BaseEigens)

saveRDS(BaseEigens, file = "../Data/TemporaryData/BaseEigens.rds")

MinutesElapsed = (as.numeric(EndTime - StartTime)/60)
EigenvectorN = length(BaseEigens) - 1
RSquared = (summary(BasePEM_LM))$r.squared
Steepness = BasePEM$a
EvoRate = BasePEM$psi
TempEigenReport = cbind(a,b,c,d,"BaseData",0,MinutesElapsed,EigenvectorN,RSquared,Steepness[1],EvoRate[1], "No")
EigenReport = rbind(EigenReport, TempEigenReport)

#Eigens of deleted data


pb <- progress_bar$new(total = 176)
Eigens = list()
for(A in DeletedData){
  
  pb$tick()
  Sys.sleep(1/100)
  
  assign("last.warning", NULL, envir = baseenv())
  StartTime = Sys.time()
  

  TrimmedRows = A[complete.cases(A[,4]),]
  spmatch = match(Tree$tip.label, TrimmedRows[,2])
  TrimmedTree = drop.tip(Tree,Tree$tip.label[is.na(spmatch)])
  TrimmedTreeGraph = Phylo2DirectedGraph(TrimmedTree)
  
  TrimmedPEM = RobustPEM(TrimmedRows$V2, TrimmedTreeGraph)
  ErrorPresent = TrimmedPEM[[2]]
  TrimmedPEM = TrimmedPEM[[1]]

  TrimmedPEM_LM = lmforwardsequentialsidak(
    y = TrimmedRows$V2,
    object = TrimmedPEM,
    alpha = 0.01)
  
  BuildTrimmedPEM = PEM.build(
    TreeGraph,
    d = "distance", sp = "species",
    a = TrimmedPEM$a, psi = TrimmedPEM$psi)
  TrimmedPEM_DF = as.data.frame(BuildTrimmedPEM)

  if (length(TrimmedPEM_LM$coefficients) < 2){
    SelectedColumns = data.frame(TreeTips = A$TreeTips)#No eigen vectors selected
  } else {
  EigenColumns = as.data.frame(TrimmedPEM_LM$coefficients)
  EigenColumns = rownames(EigenColumns)[2:length(EigenColumns[,1])]
  SelectedColumns = NULL
  
  for (B in EigenColumns){
  SelectedColumns = cbind(SelectedColumns, assign(B, TrimmedPEM_DF[,B]))
}
  colnames(SelectedColumns) = c(EigenColumns)
  SelectedColumns = as.data.frame(SelectedColumns)
  TreeTips =  as.character(A$TreeTips) 
  SelectedColumns = cbind(TreeTips, SelectedColumns)
  }
  
  EndTime = Sys.time()
  MinutesElapsed = (as.numeric(EndTime - StartTime)/60)
  EigenvectorN = length(SelectedColumns) - 1
  RSquared = (summary(TrimmedPEM_LM))$r.squared
  Steepness = TrimmedPEM$a
  EvoRate = TrimmedPEM$psi
  TempEigenReport = cbind(a,b,c,d,A$FunctionName[1],A$Missing[1],MinutesElapsed,EigenvectorN,RSquared,Steepness[1],EvoRate[1], ErrorPresent)
  EigenReport = rbind(EigenReport, TempEigenReport)

  
  Eigens[[paste0("Eigens", A$Categorisation[1])]] <- SelectedColumns
}

colnames(EigenReport) = c("Seed", "CorrelationLevel", "ResponsePresent", "Trend", "FunctionName", "Missing", "MinutesElapsed", "EigenvectorN", "RSquared", "PhyloSteepness", "PhyloEvolutionRate", "ErrorPresent")
CombinedEigenReport = rbind(CombinedEigenReport, EigenReport)

saveRDS(list(Eigens, EigenReport), file = "../Data/TemporaryData/Eigens.rds")

rm(list=setdiff(ls(), c("a", "b", "c", "d", "CombinedStartingTrend", "CombinedDeletionReport", "CombinedEigenReport", "CombinedImputationReport", "StartingTrend", "DeletionReport", "EigenReport", "ImputationReport", "Seed", "CorrelationLevel", "ResponsePresent", "Trend", "BaseData", "Tree", "CollapsedTree", "FunctionList", "DeletedData", "BaseEigens", "Eigens")))


