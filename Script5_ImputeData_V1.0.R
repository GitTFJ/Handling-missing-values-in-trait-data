#Title: Script5_ImputeData_V1.0.R
#Version: 1.0
#Dependencies: DeletedData, Eigens, Tree
#Author: Thomas F. Johnson
#Date: 19/02/2019




Missingness = c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8)
Iterations = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
Missingness_Iterations = data.frame(Missingness, Iterations)

#########################

#Define functions
ImputationFunction = function(DataFrame){
  MiceRun = tryCatch(
    {
      MiceOutput = mice(DataFrame, m=Iteration, method = rep(Method, length(DataFrame)), maxit = 10, printFlag = F)
      MiceComputationallySingular = "No"
      list(MiceOutput,MiceComputationallySingular, DataFrame)
    },
    error = function(cond){
      DataFrame = DataFrame[,1:(length(DataFrame)-10)]
      MiceOutput = mice(DataFrame, m=Iteration, method = rep(Method, length(DataFrame)), maxit = 10, printFlag = F)
      MiceComputationallySingular =  "Yes"
      list(MiceOutput,MiceComputationallySingular, DataFrame)
    }# need to note that we have dropped variables because error was present
  )
  return(MiceRun)
}  

WriteData = function(MiceOutput){
  for (E in seq(1, Iteration, 1)){
    ImputedDF =  mice::complete(MiceRun,E)
    if(c == "Yes"){
      ImputedDF = cbind(IncompleteData[,1:3],ImputedDF[,2:5],IncompleteData[,8:(length(IncompleteData))])
    } else {
      ImputedDF = cbind(IncompleteData[,1:3],ImputedDF[,1:4],IncompleteData[,8:(length(IncompleteData))])
    }
    IterationList[[paste("Iteration: ", E)]] <- ImputedDF
  }
  return(IterationList) 
}

RemoveExcessVariable = function(DataFrame,MiceOutput){
  FullVariables = colnames(DataFrame)
  RemoveVariables = strsplit(as.character(MiceOutput$loggedEvents[1,5]), ", ")[[1]]
  NewPredictors = FullVariables[!FullVariables %in% RemoveVariables]
  Join = subset(DataFrame, select = NewPredictors)
  return(Join)
}

ImputationReportFunction = function(DataFrame, Failures){
  EndTime = Sys.time()
  MinutesElapsed = (as.numeric(EndTime - StartTime)/60)
  ColumnsRemoved = paste(setdiff(colnames(FullDF),colnames(DataFrame)), collapse = ", ")
  ColumnsInFinalImputation = paste(colnames(DataFrame), collapse = ", ")
  NumberOfColumnsInInitialImputation = length(FullDF)
  NumberOfColumnsInFinalImputation = length(DataFrame)
  NumberOfImputationAttempts = Failures
  MiceComputationallySingular = MiceComputationallySingular[1]
  TempImputationReport = cbind(a,b,c,d,IncompleteData$FunctionName[1],IncompleteData$Missing[1],C,MinutesElapsed,ColumnsRemoved, ColumnsInFinalImputation, NumberOfColumnsInInitialImputation, NumberOfColumnsInFinalImputation, NumberOfImputationAttempts, MiceComputationallySingular)
  return(TempImputationReport)
}


#########################

ImputationApproaches = list(
  "Mice: mean matching",
  "Mice: regression",
  "Mice: random forest",
  "Mice: mean matching + phylogeny",
  "Mice: regression + phylogeny",
  "Mice: random forest + phylogeny",
  "Rphylopars"
)

ImputationReport = NULL


################################################
Imputation = list()
for(C in ImputationApproaches){
  print(C)
  
  #################################################
  if(C == "Mice: mean matching" | 
     C ==  "Mice: regression" |
     C ==  "Mice: random forest") {
    if(C ==  "Mice: mean matching"){
      Method = "pmm"
      MiceMeanMatching = list()
    } else if(C ==  "Mice: regression"){
      Method = "norm"
      MiceRegression = list()
    } else {
      Method = "rf"
      MiceRandomForest = list()
    }
    
    pb <- progress_bar$new(total = 176)
    for(D in seq(1,176,1)){
      
      pb$tick()
      Sys.sleep(1/100)
      
      StartTime = Sys.time()
      IncompleteData = DeletedData[[D]]
      if(c == "Yes"){
        Impute = IncompleteData[,2:7]
      } else {
        Impute = IncompleteData[,c(2,4:7)]
      }
      Join = Impute[,-1]
      FullDF = Join
      IterationList = list()
      Iteration = subset(Missingness_Iterations, Missingness == as.character(IncompleteData$Missing[1]))[[2]]
      MiceRun = ImputationFunction(Join)
      MiceComputationallySingular = MiceRun[[2]]
      MiceRun = MiceRun[[1]]
      IterationList = WriteData(MiceRun)
      ImputationReport = rbind(ImputationReport,ImputationReportFunction(Join,1))
      if(C ==  "Mice: mean matching"){
        MiceMeanMatching[[paste(IncompleteData$Categorisation[1])]] = IterationList
      } else if(C ==  "Mice: regression"){
        MiceRegression[[paste(IncompleteData$Categorisation[1])]] = IterationList
      } else {
        MiceRandomForest[[paste(IncompleteData$Categorisation[1])]] = IterationList
      }
    }
    if(C ==  "Mice: mean matching"){
      Imputation[[C]] = MiceMeanMatching
    } else if(C ==  "Mice: regression"){
      Imputation[[C]] = MiceRegression
    } else {
      Imputation[[C]] = MiceRandomForest
    }
  
  ################################################################
  } else if (C == "Mice: mean matching + phylogeny" | 
             C ==  "Mice: regression + phylogeny" |
             C ==  "Mice: random forest + phylogeny"){
      if(C ==  "Mice: mean matching + phylogeny"){
        Method = "pmm"
        MiceMeanMatchingPhylo = list()
      } else if(C ==  "Mice: regression + phylogeny"){
        Method = "norm"
        MiceRegressionPhylo = list()
      } else {
        Method = "rf"
        MiceRandomForestPhylo = list()
      }
    

    pb <- progress_bar$new(total = 176)  
    for(D in seq(1,176,1)){
      
      pb$tick()
      Sys.sleep(1/100)
      
      StartTime = Sys.time()  
      IncompleteData = DeletedData[[D]]
        if(c == "Yes"){
          Impute = IncompleteData[,2:7]
        } else {
          Impute = IncompleteData[,c(2,4:7)]
        }
        AssociatedEigens = Eigens[[D]]
        Join = left_join(Impute, AssociatedEigens, by = "TreeTips")
        Join = Join[,-1]
        FullDF = Join
        IterationList = list()
        Iteration = subset(Missingness_Iterations, Missingness == as.character(IncompleteData$Missing[1]))[[2]]
        MiceRun = ImputationFunction(Join)
        Join = MiceRun[[3]]
        MiceComputationallySingular = MiceRun[[2]]
        MiceRun = MiceRun[[1]]
          if (length(MiceRun$loggedEvents)[1] < 1) {#Are warnings present?
            IterationList = WriteData(MiceRun)
            ImputationReport = rbind(ImputationReport,ImputationReportFunction(Join,1))
          } else {
            Join = RemoveExcessVariable(Join, MiceRun)
            MiceRun = ImputationFunction(Join)
            Join = MiceRun[[3]]
            MiceComputationallySingular = c(MiceComputationallySingular, MiceRun[[2]])
            MiceRun = MiceRun[[1]]
              if (length(MiceRun$loggedEvents)[1] < 1) {#Are warnings present?
                IterationList = WriteData(MiceRun)
                ImputationReport = rbind(ImputationReport,ImputationReportFunction(Join,2))
              } else {
                Join = RemoveExcessVariable(Join, MiceRun)
                MiceRun = ImputationFunction(Join)
                Join = MiceRun[[3]]
                MiceComputationallySingular = MiceRun[[2]]
                MiceRun = MiceRun[[1]]
                if (length(MiceRun$loggedEvents)[1] < 1) {#Are warnings present?
                  IterationList = WriteData(MiceRun)
                  ImputationReport = rbind(ImputationReport,ImputationReportFunction(Join,3))
                } else {
                  Join = RemoveExcessVariable(Join, MiceRun)
                  MiceRun = ImputationFunction(Join)
                  Join = MiceRun[[3]]
                  MiceComputationallySingular = MiceRun[[2]]
                  MiceRun = MiceRun[[1]]
                  if (length(MiceRun$loggedEvents)[1] < 1) {#Are warnings present?
                    IterationList = WriteData(MiceRun)
                    ImputationReport = rbind(ImputationReport,ImputationReportFunction(Join,4))
                  } else {
                    Join = RemoveExcessVariable(Join, MiceRun)
                    MiceRun = ImputationFunction(Join)
                    Join = MiceRun[[3]]
                    MiceComputationallySingular = MiceRun[[2]]
                    MiceRun = MiceRun[[1]]
                    if (length(MiceRun$loggedEvents)[1] < 1) {#Are warnings present?
                      IterationList = WriteData(MiceRun)
                      ImputationReport = rbind(ImputationReport,ImputationReportFunction(Join,5))
                    } else {
                      Join = RemoveExcessVariable(Join, MiceRun)
                      MiceRun = ImputationFunction(Join)
                      Join = MiceRun[[3]]
                      MiceComputationallySingular = MiceRun[[2]]
                      MiceRun = MiceRun[[1]]
                      IterationList = WriteData(MiceRun)
                      ImputationReport = rbind(ImputationReport,ImputationReportFunction(Join,6))
                             }
                          }
                        }
                      }
          }
        if(C ==  "Mice: mean matching + phylogeny"){
          MiceMeanMatchingPhylo[[paste(IncompleteData$Categorisation[1])]] = IterationList
        } else if(C ==  "Mice: regression + phylogeny"){
          MiceRegressionPhylo[[paste(IncompleteData$Categorisation[1])]] = IterationList
        } else {
          MiceRandomForestPhylo[[paste(IncompleteData$Categorisation[1])]] = IterationList
        }
      }
    if(C ==  "Mice: mean matching + phylogeny"){
      Imputation[[C]] = MiceMeanMatchingPhylo
    } else if(C ==  "Mice: regression + phylogeny"){
      Imputation[[C]] = MiceRegressionPhylo
    } else {
      Imputation[[C]] = MiceRandomForestPhylo
    }
  }


    else {
      Rphylopars = list()
      pb <- progress_bar$new(total = 176)
      for(D in seq(1,176,1)){

        pb$tick()
        Sys.sleep(1/100)
        
        StartTime = Sys.time()
        IncompleteData = DeletedData[[D]]
        names(IncompleteData)[2] = "species"
        if(c == "Yes"){
          PhyloRun = phylopars(trait_data = IncompleteData[,2:7], tree = Tree, phylo_correlated = TRUE, pheno_correlated = FALSE)
          ImputedDF = as.data.frame(cbind(PhyloRun$anc_recon[1:500,], PhyloRun$anc_var[1:500,]))
          ImputedDF$species = rownames(ImputedDF)
          colnames(ImputedDF)[1:11] <- c("V1", "V2", "V3", "V4", "V5", "V1_Var", "V2_Var", "V3_Var", "V4_Var", "V5_Var", "species")
          rownames(ImputedDF) <- c()
          ImputedDF = left_join(IncompleteData[,c("Node", "species", "TreeOrder", "Trend", "FunctionName", "Missing", "Categorisation")], ImputedDF, by = "species")
          Rphylopars[[paste(IncompleteData$Categorisation[1])]] <- ImputedDF
          
        } else {
          PhyloRun = phylopars(trait_data = IncompleteData[,c(2,4:7)], tree = Tree, phylo_correlated = TRUE, pheno_correlated = FALSE)
          ImputedDF = as.data.frame(cbind(PhyloRun$anc_recon[1:500,], PhyloRun$anc_var[1:500,]))
          ImputedDF$species = rownames(ImputedDF)
          colnames(ImputedDF)[1:9] <- c("V2", "V3", "V4", "V5", "V2_Var", "V3_Var", "V4_Var", "V5_Var", "species")
          rownames(ImputedDF) <- c()
          ImputedDF = left_join(IncompleteData[,c("Node", "species", "TreeOrder", "Trend", "FunctionName", "Missing", "Categorisation","V1")], ImputedDF, by = "species")

          
          Rphylopars[[paste(IncompleteData$Categorisation[1])]] <- ImputedDF

        }
        EndTime = Sys.time()
        MinutesElapsed = (as.numeric(EndTime - StartTime)/60)
        ColumnsRemoved = NA
        ColumnsInFinalImputation = NA
        NumberOfColumnsInInitialImputation = NA
        NumberOfColumnsInFinalImputation = NA
        NumberOfImputationAttempts = NA
        MiceComputationallySingular = NA
        TempImputationReport = cbind(a,b,c,d,IncompleteData$FunctionName[1],IncompleteData$Missing[1],C,MinutesElapsed,ColumnsRemoved, ColumnsInFinalImputation, NumberOfColumnsInInitialImputation, NumberOfColumnsInFinalImputation, NumberOfImputationAttempts, MiceComputationallySingular)
        ImputationReport = rbind(ImputationReport,TempImputationReport)
      }
      Imputation[[C]] <- Rphylopars
    }
}

colnames(ImputationReport) = c("Seed", "CorrelationLevel", "ResponsePresent", "Trend", "FunctionName", "Missing", "ImputationApproach", "MinutesElapsed", "ColumnsRemoved", "ColumnsInFinalImputation", "NumberOfColumnsInInitialImputation", "NumberOfColumnsInFinalImputation", "NumberOfImputationAttempts", "MiceComputationallySingular")
CombinedImputationReport = rbind(CombinedImputationReport, ImputationReport)

saveRDS(list(Imputation, ImputationReport), file = "../Data/TemporaryData/Imputations.rds")
