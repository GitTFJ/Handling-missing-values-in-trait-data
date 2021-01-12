#Title: Script3_DeleteData_V1.0.R
#Version: 1.0
#Dependencies: BaseData, FunctionList
#Author: Thomas F. Johnson
#Date: 19/02/2019

DeletionReport = NULL
#******************************************************#
pb <- progress_bar$new(total = 176)
DeletedData = list()
#Run loop to create 176 elements with different bias types and degrees of missing data. 
for(i in seq(0.05, 0.8, 0.05)) {#Missingness
  for(j in FunctionList) {#Bias types
    pb$tick()
    Sys.sleep(1/100)
      Temp = j(BaseData, 4,7, i)
      Temp$Missing = i
      Temp$FunctionName = as.character(Temp$FunctionName)
      Temp$Categorisation = paste(Temp$Trend, Temp$Missing, Temp$FunctionName, sep="")
      TempDeletionReport = cbind(a,b,c,d,Temp$FunctionName[1],i,sum(is.na(Temp$V2)),sum(is.na(Temp$V3)),sum(is.na(Temp$V4)),sum(is.na(Temp$V5)),500*i)
      DeletionReport = rbind(DeletionReport, TempDeletionReport)
      DeletedData[[paste0("Data", Temp$Categorisation[1])]] <- Temp
      
    }
  }



rm(Temp, i, j,MCAR, MOPP, MOPT, SBP, SBTP, SBTR, SBTPR, WBP, WBTP, WBTR, WBTPR, guaranteedSampling, OrderedRemoval, OrderedSample, SampleByCharacter)

colnames(DeletionReport) = c("Seed", "CorrelationLevel", "ResponsePresent", "Trend", "FunctionName", "Missing", "RecordsRemoved_V1", "RecordsRemoved_V2", "RecordsRemoved_V3", "RecordsRemoved_V4", "ExpectedRecordsRemoved")
CombinedDeletionReport = rbind(CombinedDeletionReport, DeletionReport)

saveRDS(list(DeletedData,DeletionReport), file = "../Data/TemporaryData/Deletion.rds")

if(a == 1 & b == 0.6 & c == "Yes" & d == "PositiveTrend"){
  saveRDS(DeletedData, "../Data/TemporaryData/DeletedData_Seed1_CorHigh_RespYes_Pos.rds")
} else {
}
