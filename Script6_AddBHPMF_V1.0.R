#Title: Script6_AddBHPMF_V1.0.R
#Version: 1.0
#Dependencies: NULL
#Author: Thomas F. Johnson
#Date: 22/02/2019


library(ape)
library(dplyr)
library(BHPMF)
library(R.utils)


Seed = seq(1,10,1)
CorrelationLevel = list(0.2,0.6)
ResponsePresent = list("Yes", "No")
Trend = list("NoTrend", "PositiveTrend")

BHPMFNodeList = list()
BHPMFObvList = list()
CombinedImputationReport = NULL
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
        List = readRDS(paste("../Data/RawData/Seed", a, "_Correlation", b, "_Response", c, d, ".rds", sep = ""))
        DeletedData = List[[6]]
        
        l = nodepath(List[[2]])
        Comb = NULL
        for(e in 1:length(l)){
          Temp = data.frame(N = length(l[[e]]))
          Comb = rbind(Comb, Temp)
        }
        Nodes = min(Comb$N)
        
        Comb2 = NULL
        for(e in 1:length(l)){
          Temp = l[[e]][1:Nodes]
          Comb2 = rbind(Comb2, Temp)
        }
        Comb2=Comb2[,order(ncol(Comb2):1)]
        Comb2 = as.data.frame(Comb2)
        colnames(Comb2) = (gsub("V", "Level", colnames(Comb2)))
        HeiMatrix = cbind(List[[2]]$tip.label, Comb2)
        colnames(HeiMatrix)[1] = "species"
        
        #####BHPMFNode#####
        BHPMFNode = list()
        ImputationReport = NULL
        #pb <- progress_bar$new(total = 176)
        for(D in seq(1,176,1)){
          message(D)
          #pb$tick()
          Sys.sleep(1/100)
          StartTime = Sys.time()
          IncompleteData = DeletedData[[D]]
          names(IncompleteData)[2] = "species"
          
          ######ResponsePresent#####
          if(c == "Yes"){
            Comb = left_join(IncompleteData, HeiMatrix, by = "species")
            Comb[,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1))] <- sapply(Comb[,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1))] ,as.character)
            NumberOfColumns = length((Comb[,3:7])) + length((Comb[,c(2,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1)))]))
            StartImp = Sys.time()
            tryCatch(
              expr = {
                withTimeout({
                  Imp = GapFilling(X = as.matrix(Comb[,3:7]), 
                                   hierarchy.info = (Comb[,c(2,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1)))]),
                                   mean.gap.filled.output.path = "../Data/TemporaryData/mean_gap_filled2.txt",
                                   std.gap.filled.output.path="../Data/TemporaryData/std_gap_filled2.txt",
                                   num.samples = 1000,
                                   burn = 200)
                }, timeout = 180)
              }, 
              TimeoutException = function(ex) cat("Timeout. Skipping.\n")
            )
            EndImp = Sys.time()
            if(difftime(EndImp, StartImp, units = "secs")<180){
              ImputedMeans <- read.delim("../Data/TemporaryData/mean_gap_filled2.txt")
              ImputedSD <- read.delim("../Data/TemporaryData/std_gap_filled2.txt")
              IndexV2 = which(is.na(IncompleteData$V2))
              IndexV3 = which(is.na(IncompleteData$V3))
              IndexV4 = which(is.na(IncompleteData$V4))
              IndexV5 = which(is.na(IncompleteData$V5))
              Construct = IncompleteData[,3:7]
              Construct[IndexV2,2] = ImputedMeans[IndexV2,2]
              Construct[IndexV3,3] = ImputedMeans[IndexV3,3]
              Construct[IndexV4,4] = ImputedMeans[IndexV4,4]
              Construct[IndexV5,5] = ImputedMeans[IndexV5,5]
              Construct$V1_Var = 0
              Construct$V2_Var = 0
              Construct$V3_Var = 0
              Construct$V4_Var = 0
              Construct$V5_Var = 0
              Construct[IndexV2,7] = ImputedSD[IndexV2,2]
              Construct[IndexV3,8] = ImputedSD[IndexV3,3]
              Construct[IndexV4,9] = ImputedSD[IndexV4,4]
              Construct[IndexV5,10] = ImputedSD[IndexV5,5]
              Construct$species = IncompleteData$species
              
              
              ImputedDF = left_join(IncompleteData[,c("Node", "species", "TreeOrder", "Trend", "FunctionName", "Missing", "Categorisation")], Construct, by = "species")
              BHPMFNode[[paste(IncompleteData$Categorisation[1])]] <- ImputedDF
            } else {
              BHPMFNode[[D]] = "0"
            }
            if(difftime(EndImp, StartImp, units = "secs")<180){
              EndTime = Sys.time()
              MinutesElapsed = difftime(EndImp, StartImp, units = "mins")
              ColumnsRemoved = NA
              ColumnsInFinalImputation =  paste(colnames((Comb[,c(2,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1)))])), collapse = ", ")
              NumberOfColumnsInInitialImputation = NA
              NumberOfColumnsInFinalImputation = NumberOfColumns
              NumberOfImputationAttempts = NA
              MiceComputationallySingular = NA
              BHPMFError = NA
              TempImputationReport = as.data.frame(cbind(a,b,c,d,IncompleteData$FunctionName[1],IncompleteData$Missing[1], "BHPMFNode",MinutesElapsed,ColumnsRemoved, ColumnsInFinalImputation, NumberOfColumnsInInitialImputation, NumberOfColumnsInFinalImputation, NumberOfImputationAttempts, MiceComputationallySingular, BHPMFError))
              ImputationReport = rbind(ImputationReport,TempImputationReport)
            } else {
              EndTime = Sys.time()
              MinutesElapsed = NA
              ColumnsRemoved = NA
              ColumnsInFinalImputation =  paste(colnames((Comb[,c(2,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1)))])), collapse = ", ")
              NumberOfColumnsInInitialImputation = NA
              NumberOfColumnsInFinalImputation = NumberOfColumns
              NumberOfImputationAttempts = NA
              MiceComputationallySingular = NA
              BHPMFError = "Time limit exceeded"
              TempImputationReport = as.data.frame(cbind(a,b,c,d,IncompleteData$FunctionName[1],IncompleteData$Missing[1], "BHPMFNode",MinutesElapsed,ColumnsRemoved, ColumnsInFinalImputation, NumberOfColumnsInInitialImputation, NumberOfColumnsInFinalImputation, NumberOfImputationAttempts, MiceComputationallySingular, BHPMFError))
              ImputationReport = rbind(ImputationReport,TempImputationReport)
            }
          } else {
            #####Response absent##########
            Comb = left_join(IncompleteData, HeiMatrix, by = "species")
            Comb[,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1))] <- sapply(Comb[,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1))] ,as.character)
            message(Sys.time())
            NumberOfColumns = length((Comb[,3:7])) + length((Comb[,c(2,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1)))]))
            
            
            #When response is absent, BHPMF will fail if any species have no data
            #####DummyVariableRequired#####
            if(any(rowSums(is.na(Comb[,4:7]))==4)){
              StartImp = Sys.time()
              tryCatch(
                expr = {
                  withTimeout({GapFilling(X = as.matrix(cbind(Comb[,4:7], rnorm(500,0,1))), 
                                          hierarchy.info = (Comb[,c(2,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1)))]),
                                          mean.gap.filled.output.path = "../Data/TemporaryData/mean_gap_filled2.txt",
                                          std.gap.filled.output.path="../Data/TemporaryData/std_gap_filled2.txt",
                                          num.samples = 1000,
                                          burn = 200)
                  }, timeout = 180)
                }, 
                TimeoutException = function(exit) cat("Timeout. Skipping.\n")
              )
              EndImp = Sys.time()
              if(difftime(EndImp, StartImp, units = "secs")<180){
                ImputedMeans <- read.delim("../Data/TemporaryData/mean_gap_filled2.txt")
                ImputedSD <- read.delim("../Data/TemporaryData/std_gap_filled2.txt")
                IndexV2 = which(is.na(IncompleteData$V2))
                IndexV3 = which(is.na(IncompleteData$V3))
                IndexV4 = which(is.na(IncompleteData$V4))
                IndexV5 = which(is.na(IncompleteData$V5))
                Construct = IncompleteData[,4:7]
                Construct[IndexV2,1] = ImputedMeans[IndexV2,1]
                Construct[IndexV3,2] = ImputedMeans[IndexV3,2]
                Construct[IndexV4,3] = ImputedMeans[IndexV4,3]
                Construct[IndexV5,4] = ImputedMeans[IndexV5,4]
                Construct$V2_Var = 0
                Construct$V3_Var = 0
                Construct$V4_Var = 0
                Construct$V5_Var = 0
                Construct[IndexV2,6] = ImputedSD[IndexV2,1]
                Construct[IndexV3,7] = ImputedSD[IndexV3,2]
                Construct[IndexV4,8] = ImputedSD[IndexV4,3]
                Construct[IndexV5,9] = ImputedSD[IndexV5,4]
                Construct$species = IncompleteData$species
                ImputedDF = left_join(IncompleteData[,c("Node", "species", "TreeOrder", "Trend", "FunctionName", "Missing", "Categorisation","V1")], Construct, by = "species")
                BHPMFNode[[paste(IncompleteData$Categorisation[1])]] <- ImputedDF
              } else {
                BHPMFNode[[D]] <- "0"
              }
              if(difftime(EndImp, StartImp, units = "secs")<180){
                EndTime = Sys.time()
                MinutesElapsed = difftime(EndImp, StartImp, units = "mins")
                ColumnsRemoved = NA
                ColumnsInFinalImputation =  paste(colnames((Comb[,c(2,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1)))])), collapse = ", ")
                NumberOfColumnsInInitialImputation = NA
                NumberOfColumnsInFinalImputation = NumberOfColumns
                NumberOfImputationAttempts = NA
                MiceComputationallySingular = NA
                BHPMFError = "Dummy used"
                TempImputationReport = as.data.frame(cbind(a,b,c,d,IncompleteData$FunctionName[1],IncompleteData$Missing[1], "BHPMFNode",MinutesElapsed,ColumnsRemoved, ColumnsInFinalImputation, NumberOfColumnsInInitialImputation, NumberOfColumnsInFinalImputation, NumberOfImputationAttempts, MiceComputationallySingular, BHPMFError))
                ImputationReport = rbind(ImputationReport,TempImputationReport)
              } else {
                EndTime = Sys.time()
                MinutesElapsed = NA
                ColumnsRemoved = NA
                ColumnsInFinalImputation =  paste(colnames((Comb[,c(2,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1)))])), collapse = ", ")
                NumberOfColumnsInInitialImputation = NA
                NumberOfColumnsInFinalImputation = NumberOfColumns
                NumberOfImputationAttempts = NA
                MiceComputationallySingular = NA
                BHPMFError = "Time limit exceeded"
                TempImputationReport = as.data.frame(cbind(a,b,c,d,IncompleteData$FunctionName[1],IncompleteData$Missing[1], "BHPMFNode",MinutesElapsed,ColumnsRemoved, ColumnsInFinalImputation, NumberOfColumnsInInitialImputation, NumberOfColumnsInFinalImputation, NumberOfImputationAttempts, MiceComputationallySingular, BHPMFError))
                ImputationReport = rbind(ImputationReport,TempImputationReport)
              }
              
            } else {
              StartImp = Sys.time()
              tryCatch(
                expr = {
                  withTimeout({GapFilling(X = as.matrix(Comb[,4:7]), 
                                          hierarchy.info = (Comb[,c(2,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1)))]),
                                          mean.gap.filled.output.path = "../Data/TemporaryData/mean_gap_filled2.txt",
                                          std.gap.filled.output.path="../Data/TemporaryData/std_gap_filled2.txt",
                                          num.samples = 1000,
                                          burn = 200)
                  }, timeout = 180)
                }, 
                TimeoutException = function(exit) cat("Timeout. Skipping.\n")
              )
              EndImp = Sys.time()
              if(difftime(EndImp, StartImp, units = "secs")<180){
                ImputedMeans <- read.delim("../Data/TemporaryData/mean_gap_filled2.txt")
                ImputedSD <- read.delim("../Data/TemporaryData/std_gap_filled2.txt")
                IndexV2 = which(is.na(IncompleteData$V2))
                IndexV3 = which(is.na(IncompleteData$V3))
                IndexV4 = which(is.na(IncompleteData$V4))
                IndexV5 = which(is.na(IncompleteData$V5))
                Construct = IncompleteData[,4:7]
                Construct[IndexV2,1] = ImputedMeans[IndexV2,1]
                Construct[IndexV3,2] = ImputedMeans[IndexV3,2]
                Construct[IndexV4,3] = ImputedMeans[IndexV4,3]
                Construct[IndexV5,4] = ImputedMeans[IndexV5,4]
                Construct$V2_Var = 0
                Construct$V3_Var = 0
                Construct$V4_Var = 0
                Construct$V5_Var = 0
                Construct[IndexV2,6] = ImputedSD[IndexV2,1]
                Construct[IndexV3,7] = ImputedSD[IndexV3,2]
                Construct[IndexV4,8] = ImputedSD[IndexV4,3]
                Construct[IndexV5,9] = ImputedSD[IndexV5,4]
                Construct$species = IncompleteData$species
                ImputedDF = left_join(IncompleteData[,c("Node", "species", "TreeOrder", "Trend", "FunctionName", "Missing", "Categorisation","V1")], Construct, by = "species")
                BHPMFNode[[paste(IncompleteData$Categorisation[1])]] <- ImputedDF
              } else {
                BHPMFNode[[D]] <- "0"
              }
              if(difftime(EndImp, StartImp, units = "secs")<180){
                EndTime = Sys.time()
                MinutesElapsed = difftime(EndImp, StartImp, units = "mins")
                ColumnsRemoved = NA
                ColumnsInFinalImputation =  paste(colnames((Comb[,c(2,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1)))])), collapse = ", ")
                NumberOfColumnsInInitialImputation = NA
                NumberOfColumnsInFinalImputation = NumberOfColumns
                NumberOfImputationAttempts = NA
                MiceComputationallySingular = NA
                BHPMFError = NA
                TempImputationReport = as.data.frame(cbind(a,b,c,d,IncompleteData$FunctionName[1],IncompleteData$Missing[1], "BHPMFNode",MinutesElapsed,ColumnsRemoved, ColumnsInFinalImputation, NumberOfColumnsInInitialImputation, NumberOfColumnsInFinalImputation, NumberOfImputationAttempts, MiceComputationallySingular, BHPMFError))
                ImputationReport = rbind(ImputationReport,TempImputationReport)
              } else {
                EndTime = Sys.time()
                MinutesElapsed = NA
                ColumnsRemoved = NA
                ColumnsInFinalImputation =  paste(colnames((Comb[,c(2,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1)))])), collapse = ", ")
                NumberOfColumnsInInitialImputation = NA
                NumberOfColumnsInFinalImputation = NumberOfColumns
                NumberOfImputationAttempts = NA
                MiceComputationallySingular = NA
                BHPMFError = "Time limit exceeded"
                TempImputationReport = as.data.frame(cbind(a,b,c,d,IncompleteData$FunctionName[1],IncompleteData$Missing[1], "BHPMFNode",MinutesElapsed,ColumnsRemoved, ColumnsInFinalImputation, NumberOfColumnsInInitialImputation, NumberOfColumnsInFinalImputation, NumberOfImputationAttempts, MiceComputationallySingular, BHPMFError))
                ImputationReport = rbind(ImputationReport,TempImputationReport)
              }#Time limit not exceeded
            }#At least one value per row
          }#Response absent
        }
        colnames(ImputationReport) = c("Seed", "CorrelationLevel", "ResponsePresent", "Trend", "FunctionName", "Missing", "ImputationApproach", "MinutesElapsed", "ColumnsRemoved", "ColumnsInFinalImputation", "NumberOfColumnsInInitialImputation", "NumberOfColumnsInFinalImputation", "NumberOfImputationAttempts", "MiceComputationallySingular",  "BHPMFError")
        CombinedImputationReport = rbind(CombinedImputationReport, ImputationReport)
        BHPMFNodeList[[paste("Seed", a, "_Correlation", b, "_Response", c, d, sep = "")]] = BHPMFNode
        
        BHPMFObv = list()
        ImputationReport = NULL
        #pb <- progress_bar$new(total = 176)
        for(D in seq(1,176,1)){
          message(D)
          #pb$tick()
          Sys.sleep(1/100)
          StartTime = Sys.time()
          IncompleteData = DeletedData[[D]]
          names(IncompleteData)[2] = "species"
          
          ######ResponsePresent#####
          if(c == "Yes"){
            Comb = left_join(IncompleteData, HeiMatrix, by = "species")
            Comb[,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1))] <- sapply(Comb[,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1))] ,as.character)
            NumberOfColumns = length((Comb[,3:7])) + length((Comb[,c(2,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1)))]))
            StartImp = Sys.time()
            tryCatch(
              expr = {
                withTimeout({
                  Imp = GapFilling(X = as.matrix(Comb[,3:7]), 
                                   hierarchy.info = (Comb[,c(2,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1)))]),
                                   used.num.hierarchy.levels = 1,
                                   mean.gap.filled.output.path = "../Data/TemporaryData/mean_gap_filled2.txt",
                                   std.gap.filled.output.path="../Data/TemporaryData/std_gap_filled2.txt",
                                   num.samples = 1000,
                                   burn = 200)
                }, timeout = 180)
              }, 
              TimeoutException = function(ex) cat("Timeout. Skipping.\n")
            )
            EndImp = Sys.time()
            if(difftime(EndImp, StartImp, units = "secs")<180){
              ImputedMeans <- read.delim("../Data/TemporaryData/mean_gap_filled2.txt")
              ImputedSD <- read.delim("../Data/TemporaryData/std_gap_filled2.txt")
              IndexV2 = which(is.na(IncompleteData$V2))
              IndexV3 = which(is.na(IncompleteData$V3))
              IndexV4 = which(is.na(IncompleteData$V4))
              IndexV5 = which(is.na(IncompleteData$V5))
              Construct = IncompleteData[,3:7]
              Construct[IndexV2,2] = ImputedMeans[IndexV2,2]
              Construct[IndexV3,3] = ImputedMeans[IndexV3,3]
              Construct[IndexV4,4] = ImputedMeans[IndexV4,4]
              Construct[IndexV5,5] = ImputedMeans[IndexV5,5]
              Construct$V1_Var = 0
              Construct$V2_Var = 0
              Construct$V3_Var = 0
              Construct$V4_Var = 0
              Construct$V5_Var = 0
              Construct[IndexV2,7] = ImputedSD[IndexV2,2]
              Construct[IndexV3,8] = ImputedSD[IndexV3,3]
              Construct[IndexV4,9] = ImputedSD[IndexV4,4]
              Construct[IndexV5,10] = ImputedSD[IndexV5,5]
              Construct$species = IncompleteData$species
              
              
              ImputedDF = left_join(IncompleteData[,c("Node", "species", "TreeOrder", "Trend", "FunctionName", "Missing", "Categorisation")], Construct, by = "species")
              BHPMFObv[[paste(IncompleteData$Categorisation[1])]] <- ImputedDF
            } else {
              BHPMFObv[[D]] = "0"
            }
            if(difftime(EndImp, StartImp, units = "secs")<180){
              EndTime = Sys.time()
              MinutesElapsed = difftime(EndImp, StartImp, units = "mins")
              ColumnsRemoved = NA
              ColumnsInFinalImputation =  paste(colnames((Comb[,c(2,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1)))])), collapse = ", ")
              NumberOfColumnsInInitialImputation = NA
              NumberOfColumnsInFinalImputation = NumberOfColumns
              NumberOfImputationAttempts = NA
              MiceComputationallySingular = NA
              BHPMFError = NA
              TempImputationReport = as.data.frame(cbind(a,b,c,d,IncompleteData$FunctionName[1],IncompleteData$Missing[1], "BHPMFObv",MinutesElapsed,ColumnsRemoved, ColumnsInFinalImputation, NumberOfColumnsInInitialImputation, NumberOfColumnsInFinalImputation, NumberOfImputationAttempts, MiceComputationallySingular, BHPMFError))
              ImputationReport = rbind(ImputationReport,TempImputationReport)
            } else {
              EndTime = Sys.time()
              MinutesElapsed = NA
              ColumnsRemoved = NA
              ColumnsInFinalImputation =  paste(colnames((Comb[,c(2,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1)))])), collapse = ", ")
              NumberOfColumnsInInitialImputation = NA
              NumberOfColumnsInFinalImputation = NumberOfColumns
              NumberOfImputationAttempts = NA
              MiceComputationallySingular = NA
              BHPMFError = "Time limit exceeded"
              TempImputationReport = as.data.frame(cbind(a,b,c,d,IncompleteData$FunctionName[1],IncompleteData$Missing[1], "BHPMFObv",MinutesElapsed,ColumnsRemoved, ColumnsInFinalImputation, NumberOfColumnsInInitialImputation, NumberOfColumnsInFinalImputation, NumberOfImputationAttempts, MiceComputationallySingular, BHPMFError))
              ImputationReport = rbind(ImputationReport,TempImputationReport)
            }
          } else {
            #####Response absent##########
            Comb = left_join(IncompleteData, HeiMatrix, by = "species")
            Comb[,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1))] <- sapply(Comb[,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1))] ,as.character)
            message(Sys.time())
            NumberOfColumns = length((Comb[,3:7])) + length((Comb[,c(2,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1)))]))
            
            
            #When response is absent, BHPMF will fail if any species have no data
            #####DummyVariableRequired#####
            if(any(rowSums(is.na(Comb[,4:7]))==4)){
              StartImp = Sys.time()
              tryCatch(
                expr = {
                  withTimeout({GapFilling(X = as.matrix(cbind(Comb[,4:7], rnorm(500,0,1))), 
                                          hierarchy.info = (Comb[,c(2,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1)))]),
                                          used.num.hierarchy.levels = 1,
                                          mean.gap.filled.output.path = "../Data/TemporaryData/mean_gap_filled2.txt",
                                          std.gap.filled.output.path="../Data/TemporaryData/std_gap_filled2.txt",
                                          num.samples = 1000,
                                          burn = 200)
                  }, timeout = 180)
                }, 
                TimeoutException = function(exit) cat("Timeout. Skipping.\n")
              )
              EndImp = Sys.time()
              if(difftime(EndImp, StartImp, units = "secs")<180){
                ImputedMeans <- read.delim("../Data/TemporaryData/mean_gap_filled2.txt")
                ImputedSD <- read.delim("../Data/TemporaryData/std_gap_filled2.txt")
                IndexV2 = which(is.na(IncompleteData$V2))
                IndexV3 = which(is.na(IncompleteData$V3))
                IndexV4 = which(is.na(IncompleteData$V4))
                IndexV5 = which(is.na(IncompleteData$V5))
                Construct = IncompleteData[,4:7]
                Construct[IndexV2,1] = ImputedMeans[IndexV2,1]
                Construct[IndexV3,2] = ImputedMeans[IndexV3,2]
                Construct[IndexV4,3] = ImputedMeans[IndexV4,3]
                Construct[IndexV5,4] = ImputedMeans[IndexV5,4]
                Construct$V2_Var = 0
                Construct$V3_Var = 0
                Construct$V4_Var = 0
                Construct$V5_Var = 0
                Construct[IndexV2,6] = ImputedSD[IndexV2,1]
                Construct[IndexV3,7] = ImputedSD[IndexV3,2]
                Construct[IndexV4,8] = ImputedSD[IndexV4,3]
                Construct[IndexV5,9] = ImputedSD[IndexV5,4]
                Construct$species = IncompleteData$species
                ImputedDF = left_join(IncompleteData[,c("Node", "species", "TreeOrder", "Trend", "FunctionName", "Missing", "Categorisation","V1")], Construct, by = "species")
                BHPMFObv[[paste(IncompleteData$Categorisation[1])]] <- ImputedDF
              } else {
                BHPMFObv[[D]] <- "0"
              }
              if(difftime(EndImp, StartImp, units = "secs")<180){
                EndTime = Sys.time()
                MinutesElapsed = difftime(EndImp, StartImp, units = "mins")
                ColumnsRemoved = NA
                ColumnsInFinalImputation =  paste(colnames((Comb[,c(2,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1)))])), collapse = ", ")
                NumberOfColumnsInInitialImputation = NA
                NumberOfColumnsInFinalImputation = NumberOfColumns
                NumberOfImputationAttempts = NA
                MiceComputationallySingular = NA
                BHPMFError = "Dummy used"
                TempImputationReport = as.data.frame(cbind(a,b,c,d,IncompleteData$FunctionName[1],IncompleteData$Missing[1], "BHPMFObv",MinutesElapsed,ColumnsRemoved, ColumnsInFinalImputation, NumberOfColumnsInInitialImputation, NumberOfColumnsInFinalImputation, NumberOfImputationAttempts, MiceComputationallySingular, BHPMFError))
                ImputationReport = rbind(ImputationReport,TempImputationReport)
              } else {
                EndTime = Sys.time()
                MinutesElapsed = NA
                ColumnsRemoved = NA
                ColumnsInFinalImputation =  paste(colnames((Comb[,c(2,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1)))])), collapse = ", ")
                NumberOfColumnsInInitialImputation = NA
                NumberOfColumnsInFinalImputation = NumberOfColumns
                NumberOfImputationAttempts = NA
                MiceComputationallySingular = NA
                BHPMFError = "Time limit exceeded"
                TempImputationReport = as.data.frame(cbind(a,b,c,d,IncompleteData$FunctionName[1],IncompleteData$Missing[1], "BHPMFObv",MinutesElapsed,ColumnsRemoved, ColumnsInFinalImputation, NumberOfColumnsInInitialImputation, NumberOfColumnsInFinalImputation, NumberOfImputationAttempts, MiceComputationallySingular, BHPMFError))
                ImputationReport = rbind(ImputationReport,TempImputationReport)
              }
              
            } else {
              StartImp = Sys.time()
              tryCatch(
                expr = {
                  withTimeout({GapFilling(X = as.matrix(Comb[,4:7]), 
                                          hierarchy.info = (Comb[,c(2,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1)))]),
                                          used.num.hierarchy.levels = 1,
                                          mean.gap.filled.output.path = "../Data/TemporaryData/mean_gap_filled2.txt",
                                          std.gap.filled.output.path="../Data/TemporaryData/std_gap_filled2.txt",
                                          num.samples = 1000,
                                          burn = 200)
                  }, timeout = 180)
                }, 
                TimeoutException = function(exit) cat("Timeout. Skipping.\n")
              )
              EndImp = Sys.time()
              if(difftime(EndImp, StartImp, units = "secs")<180){
                ImputedMeans <- read.delim("../Data/TemporaryData/mean_gap_filled2.txt")
                ImputedSD <- read.delim("../Data/TemporaryData/std_gap_filled2.txt")
                IndexV2 = which(is.na(IncompleteData$V2))
                IndexV3 = which(is.na(IncompleteData$V3))
                IndexV4 = which(is.na(IncompleteData$V4))
                IndexV5 = which(is.na(IncompleteData$V5))
                Construct = IncompleteData[,4:7]
                Construct[IndexV2,1] = ImputedMeans[IndexV2,1]
                Construct[IndexV3,2] = ImputedMeans[IndexV3,2]
                Construct[IndexV4,3] = ImputedMeans[IndexV4,3]
                Construct[IndexV5,4] = ImputedMeans[IndexV5,4]
                Construct$V2_Var = 0
                Construct$V3_Var = 0
                Construct$V4_Var = 0
                Construct$V5_Var = 0
                Construct[IndexV2,6] = ImputedSD[IndexV2,1]
                Construct[IndexV3,7] = ImputedSD[IndexV3,2]
                Construct[IndexV4,8] = ImputedSD[IndexV4,3]
                Construct[IndexV5,9] = ImputedSD[IndexV5,4]
                Construct$species = IncompleteData$species
                ImputedDF = left_join(IncompleteData[,c("Node", "species", "TreeOrder", "Trend", "FunctionName", "Missing", "Categorisation","V1")], Construct, by = "species")
                BHPMFObv[[paste(IncompleteData$Categorisation[1])]] <- ImputedDF
              } else {
                BHPMFObv[[D]] <- "0"
              }
              if(difftime(EndImp, StartImp, units = "secs")<180){
                EndTime = Sys.time()
                MinutesElapsed = difftime(EndImp, StartImp, units = "mins")
                ColumnsRemoved = NA
                ColumnsInFinalImputation =  paste(colnames((Comb[,c(2,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1)))])), collapse = ", ")
                NumberOfColumnsInInitialImputation = NA
                NumberOfColumnsInFinalImputation = NumberOfColumns
                NumberOfImputationAttempts = NA
                MiceComputationallySingular = NA
                BHPMFError = NA
                TempImputationReport = as.data.frame(cbind(a,b,c,d,IncompleteData$FunctionName[1],IncompleteData$Missing[1], "BHPMFObv",MinutesElapsed,ColumnsRemoved, ColumnsInFinalImputation, NumberOfColumnsInInitialImputation, NumberOfColumnsInFinalImputation, NumberOfImputationAttempts, MiceComputationallySingular, BHPMFError))
                ImputationReport = rbind(ImputationReport,TempImputationReport)
              } else {
                EndTime = Sys.time()
                MinutesElapsed = NA
                ColumnsRemoved = NA
                ColumnsInFinalImputation =  paste(colnames((Comb[,c(2,grep("Level1", colnames(Comb)):(grep("Level1", colnames(Comb))+(Nodes-1)))])), collapse = ", ")
                NumberOfColumnsInInitialImputation = NA
                NumberOfColumnsInFinalImputation = NumberOfColumns
                NumberOfImputationAttempts = NA
                MiceComputationallySingular = NA
                BHPMFError = "Time limit exceeded"
                TempImputationReport = as.data.frame(cbind(a,b,c,d,IncompleteData$FunctionName[1],IncompleteData$Missing[1], "BHPMFObv",MinutesElapsed,ColumnsRemoved, ColumnsInFinalImputation, NumberOfColumnsInInitialImputation, NumberOfColumnsInFinalImputation, NumberOfImputationAttempts, MiceComputationallySingular, BHPMFError))
                ImputationReport = rbind(ImputationReport,TempImputationReport)
              }#Time limit not exceeded
            }#At least one value per row
          }#Response absent
        }
        colnames(ImputationReport) = c("Seed", "CorrelationLevel", "ResponsePresent", "Trend", "FunctionName", "Missing", "ImputationApproach", "MinutesElapsed", "ColumnsRemoved", "ColumnsInFinalImputation", "NumberOfColumnsInInitialImputation", "NumberOfColumnsInFinalImputation", "NumberOfImputationAttempts", "MiceComputationallySingular",  "BHPMFError")
        CombinedImputationReport = rbind(CombinedImputationReport, ImputationReport)
        BHPMFObvList[[paste("Seed", a, "_Correlation", b, "_Response", c, d, sep = "")]] = BHPMFObv
      }#Trend
    }#Response
  }#Correlation
}#Seed

saveRDS(BHPMFNodeList, "C:/Users/mn826766/OneDrive - University of Reading/PhDResearch/UnderstandingDeclinesInLargeCarnivores/Chapters/Imputation/MS/Data/TemporaryData/BHPMFNodeList.rds")

saveRDS(BHPMFObvList, "C:/Users/mn826766/OneDrive - University of Reading/PhDResearch/UnderstandingDeclinesInLargeCarnivores/Chapters/Imputation/MS/Data/TemporaryData/BHPMFObvList.rds")

saveRDS(CombinedImputationReport, "C:/Users/mn826766/OneDrive - University of Reading/PhDResearch/UnderstandingDeclinesInLargeCarnivores/Chapters/Imputation/MS/Data/TemporaryData/ImpReport.rds")
