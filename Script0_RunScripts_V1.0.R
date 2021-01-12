#Install all packages
#install.packages("Rphylopars")
#install.packages("phytools")
#install.packages("norm")
#install.packages("caper")
#install.packages("rowr")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("mice")
#install.packages("MPSEM")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("progress")
#install.packages("randomForest")
#install.packages("lme4")
#install.packages("lsmeans")
#install.packages("lmerTest")
#install.packages("stringr")
#install.packages("xlsx")
#install.packages("arm")


#Attach all packages
require(Rphylopars)
require(phytools)
require(norm)
require(caper)
require(rowr)
require(plyr)
require(dplyr)
require(mice)
require(MPSEM)
require(tidyr)
require(ggplot2)
require(ggridges)
require(ggpubr)
require(progress)
require(randomForest)
require(lme4)
require(lsmeans)
require(lmerTest)
require(stringr)
require(xlsx)
require(arm)
library(pdftools)


#Create directory. Required space 22gb
dir.create("../Data")
dir.create("../Data/TemporaryData")
dir.create("../Data/RawData")
dir.create("../Data/SummarisedData")
dir.create("../Results")
dir.create("../Results/ResidualPlots")
dir.create("../Results/RidgePlots")
dir.create("../Results/ModelOutputs")
dir.create("../Results/ModelOutputs/ResidualPlots")
dir.create("../Results/SummaryPlots")


#Save session
LoadedPackages = sessionInfo()
saveRDS(LoadedPackages,"../Results/SessionInfo.rds")


#Create files to fill
CombinedStartingTrend = NULL
CombinedDeletionReport = NULL
CombinedEigenReport = NULL
CombinedImputationReport = NULL


#Run scripts

Seed = seq(1,10,1)
CorrelationLevel = list(0.2,0.6)
ResponsePresent = list("Yes", "No")
Trend = list("NoTrend", "PositiveTrend")

for(a in Seed){
  set.seed(a)
  message(paste("Current seed: ",a, sep = ""))
  for(b in CorrelationLevel){
    message(paste(" Correlation level: ",b, sep = ""))
    for(c in ResponsePresent){
      message(paste("  Response present: ",c, sep = ""))
      for(d in Trend){
        message(paste("   Trend type: ",d, sep = ""))
        message(paste("    ",Sys.time()))
        
      
      
      #Simulate datasets. Output: BaseData, Tree, CollapsedTree
      print("Step 1: Simulate datasets")
      source(file="Script1_SimulateData_V1.0.R")

      
      
      #Develop functions to delete data: Output: FunctionList
      print(" Step 2: Develop functions to delete data")
      source(file="Script2_DeleteDataFunction_V1.0.R") 

      
      
      #Delete data. Output: DeletedData
      print("  Step 3: Delete data")
      source(file="Script3_DeleteData_V1.0.R")


      
      #Develop phylogenetic eigenvectors. Output: BaseEigens, Eigens
      print("    Step 4: Eigens")
      source(file="Script4_CalculateEigenvectors_V1.0.R")

      

      #Imputation without the response: Output: MMMList, MRList, MMMControlList, MRControlList, MMMPList, MRPList, RRPList, ImputationErrors
      print("     Step 5: Impute data")
      source(file="Script5_ImputeData_V1.0.R")
      
      CompileData = list(BaseData, Tree, CollapsedTree, StartingTrend, DeletionReport, DeletedData, EigenReport, Eigens, ImputationReport, Imputation)
      saveRDS(CompileData, paste("../Data/RawData/","Seed",a,"_Correlation",b,"_Response",c,d,".rds",sep=""))
      }
    }
  }
}

 
saveRDS(CombinedStartingTrend, "../Data/RawData/StartingTrend.rds")
saveRDS(CombinedDeletionReport, "../Data/RawData/DeletionReport.rds")
saveRDS(CombinedEigenReport, "../Data/RawData/EigenReport.rds")
saveRDS(CombinedImputationReport, "../Data/RawData/ImputationReport.rds")

