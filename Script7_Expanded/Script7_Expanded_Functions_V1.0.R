#**************Function
DataFormat = function(DataFrame){
  DataFrame[,1] = as.factor(DataFrame[,1]) 
  DataFrame[,2] = as.factor(DataFrame[,2]) 
  DataFrame[,3] = as.factor(DataFrame[,3]) 
  DataFrame[,4] = as.factor(DataFrame[,4]) 
  DataFrame[,5] = as.factor(DataFrame[,5]) 
  DataFrame[,6] = as.numeric(as.character(DataFrame[,6]))
  return(DataFrame)
}

Factor2Num = function(Variable){
  Variable = as.numeric(as.character(Variable))
  return(Variable)
}


residuals_plots <- function(m,x,title, y) {
  if(length(y) < 2){
    Colour = rgb(red=0, green=0, blue=0, alpha= 0.02)
    par(mfrow=c(1,2))
    qqnorm(residuals(m, type="response"), main = title, cex.main = 0.8, col = Colour)
    qqline(residuals(m, type="response"))
    plot(sqrt(abs(residuals(m, type="response")))~ fitted(m), main = " ", xlab = x, col = Colour, ylab = "Residuals")
    panel.smooth(fitted(m),sqrt(abs(residuals(m, type="response"))), col = Colour)
  } else {
    Colour = rgb(red=0, green=0, blue=0, alpha= 0.02)
    par(mfrow=c(1,2))
    qqnorm(residuals(m, type="response"), main = title, cex.main = 0.8, col = Colour)
    qqline(residuals(m, type="response"))
    plot(sqrt(abs(residuals(m, type="response")))~ fitted(m), main = " ", xlab = x, col = Colour, ylab = "Residuals")
    panel.smooth(fitted(m),sqrt(abs(residuals(m, type="response"))), col = Colour)
  }
}

vif.mer <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)] }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam 
  v}

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

MixedModel2Excel = function(MixedModel, ModelName){
  ModelCall = MixedModel@call
  AnovaOutput = anova(MixedModel)
  AnovaOutput$Relationship = rownames(AnovaOutput)
  SummaryOutput = as.data.frame((summary(MixedModel))$coefficients)
  SummaryOutput$Relationship = rownames(SummaryOutput)
  CI = confint(MixedModel, method = "Wald")
  SummaryOutput = cbind(SummaryOutput,CI[(1 + length(CI[,1]) - length(SummaryOutput[,1])):(length(CI[,1])),])
  write.xlsx(paste(ModelCall), file= paste("../Results/ModelOutputs/Model", ModelName, ".xlsx", sep = ""), sheetName="Model structure", row.names=FALSE)
  write.xlsx(AnovaOutput, file=paste("../Results/ModelOutputs/Model", ModelName, ".xlsx", sep = ""), sheetName="Model anova", append=TRUE, row.names=FALSE)
  write.xlsx(SummaryOutput, file=paste("../Results/ModelOutputs/Model", ModelName, ".xlsx", sep = ""), sheetName="Model summary", append=TRUE, row.names=FALSE)
}


MixedModel2Excel_FactorsPresent = function(MixedModel, ModelName){
  ModelCall = MixedModel@call
  AnovaOutput = anova(MixedModel)
  AnovaOutput$Relationship = rownames(AnovaOutput)
  SummaryOutput = as.data.frame((summary(MixedModel))$coefficients)
  SummaryOutput$Relationship = rownames(SummaryOutput)
  CI = confint(MixedModel, method = "Wald")
  SummaryOutput = cbind(SummaryOutput,CI[(1 + length(CI[,1]) - length(SummaryOutput[,1])):(length(CI[,1])),])
  TempDF = difflsmeans(MixedModel)
  TempDF$Relationship <- rownames(TempDF)
  rownames(TempDF) = NULL
  DiffMeansAllOutput = TempDF
  DiffMeansRefinedOutputTemp = {
    Comb = NULL
    BiasTerms = c("MCAR:", "MOPP:", "MOPT:", "WBP:", "WBTP:", "WBTR:", "WBTPR:", "SBP:", "SBTP:", "SBTR:", "SBTPR:")
    for(X in BiasTerms){
      MatchNumber = as.data.frame(str_count(TempDF$Relationship, X))
      Temp = cbind(TempDF, MatchNumber)
      colnames(Temp)[11] = "MatchNumber"
      Temp = subset(Temp, MatchNumber == 2)
      Comb = rbind(Comb, Temp)
    }
    ImputationTerms = c("Rphylopars:", "Mice: mean matching:", "Mice: mean matching_phylogeny:", "Mice: regression:", "Mice: regression_phylogeny:", "Mice: random forest:", "Mice: random forest_phylogeny:", "BHPMF:", "BHPMF_phylogeny")

    TempDF$Rel2 = gsub(" \\+ ", "_",TempDF$Relationship)
    for(X in ImputationTerms){
      MatchNumber = as.data.frame(str_count(TempDF$Rel2, X))
      Temp = cbind(TempDF, MatchNumber)
      colnames(Temp)[12] = "MatchNumber"
      Temp = subset(Temp, MatchNumber == 2)
      Temp$Rel2 = NULL
      Comb = rbind(Comb, Temp)
    }
    TempDF$Rel2 = NULL
    if(grepl("Imputation", ModelName)){
    Temp = TempDF[c(1:10,12,16,21,25,56:94),]
    } else {
    Temp = TempDF[c(1:10,12,16,21,25,56:101),]  
    }
    Comb = plyr::rbind.fill(Temp, Comb)
    Comb$P = Comb$`Pr(>|t|)`
    Comb$`Pr(>|t|)` = ifelse(Comb$P > 0.05, ".", 
                             (ifelse(Comb$Estimate > 0, "+", "-")))
    Comb$RoundedP = Comb$P
    Comb$RoundedP = ifelse(Comb$RoundedP < 0.0001, 0.0001, Comb$RoundedP)
    Comb$term = NULL
    Comb$levels = NULL
    Comb$MatchNumber = NULL
    
    DiffMeansRefinedOutput = Comb
  }
  
  
  write.xlsx(paste(ModelCall), file= paste("../Results/ModelOutputs/Model", ModelName, ".xlsx", sep = ""), sheetName="Model structure", row.names=FALSE)
  write.xlsx(AnovaOutput, file=paste("../Results/ModelOutputs/Model", ModelName, ".xlsx", sep = ""), sheetName="Model anova", append=TRUE, row.names=FALSE)
  write.xlsx(SummaryOutput, file=paste("../Results/ModelOutputs/Model", ModelName, ".xlsx", sep = ""), sheetName="Model summary", append=TRUE, row.names=FALSE)
  write.xlsx(DiffMeansAllOutput, file=paste("../Results/ModelOutputs/Model", ModelName, ".xlsx", sep = ""), sheetName="Full paired summary", append=TRUE, row.names=FALSE)
  write.xlsx(DiffMeansRefinedOutput, file=paste("../Results/ModelOutputs/Model", ModelName, ".xlsx", sep = ""), sheetName="Refined paired summary", append=TRUE, row.names=FALSE)
}


RibbonData = function(BasePlot){
  TempPlot = ggplot_build(BasePlot)
  DF <- data.frame(
    Missing = TempPlot$data[[2]]$x,
    Min = TempPlot$data[[2]]$y,
    Max = TempPlot$data[[3]]$y,
    ImputationApproach = TempPlot$data[[2]]$linetype,
    FunctionName = TempPlot$data[[2]]$colour)
  if(length(unique(Temp$ImputationApproach))<5){
    levels(DF$ImputationApproach) = c(
      "Rphylopars",
      "Mice: mean matching + phylogeny",
      "Mice: regression + phylogeny",
      "Mice: random forest + phylogeny"
    )
  } else {
    levels(DF$ImputationApproach) = c(
      "Complete-case",
      "Rphylopars",
      "Mice: mean matching + phylogeny",
      "Mice: regression + phylogeny",
      "Mice: random forest + phylogeny"
    )
  }
  levels(DF$FunctionName) = c(
    "MCAR",
    "MOPP",
    "MOPT",
    "SBP",
    "SBTP",
    "SBTPR",
    "SBTR",
    "WBP",
    "WBTP",
    "WBTPR",
    "WBTR"
  )
  
  DF$FunctionReName = DF$FunctionName
  levels(DF$FunctionReName) = list('Random' = "MCAR", 'Phylogeny' = "MOPP", 'Phylogeny ' = "WBP", 'Phylogeny  ' = "SBP", 'Trait' = "MOPT", 'Trait ' = "WBTP", 'Trait  ' = "SBTP", 'Trait*Response' = "WBTPR", 'Trait*Response ' = "SBTPR", 'Response' = "WBTR", 'Response ' = "SBTR")
  return(DF)
}
