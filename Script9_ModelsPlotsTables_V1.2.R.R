#Title: Script9_ModelsPlotsTables_V1.0.R
#Version: 1.0
#Dependencies: NULL
#Author: Thomas F. Johnson
#Date: 13/03/2019

#Functions
source(file = "Script7_Expanded/Script7_Expanded_Functions_V1.0.R")


#Load data and ensure consistent column names (<16gb ram recommended)
CombinedStartingTrend = readRDS("../Data/SummarisedData/CombinedStartingTrend.rds")
CombinedDeletionReport = DataFormat(readRDS("../Data/SummarisedData/CombinedDeletionReport.rds"))
CombinedEigenReport = DataFormat(readRDS("../Data/SummarisedData/CombinedEigenReport.rds"))
CombinedImputationReport = DataFormat(readRDS("../Data/SummarisedData/CombinedImputationReport.rds"))
CombinedImputationReport2 = DataFormat(readRDS("../Data/SummarisedData/CombinedImputationReport_BHPMF.rds"))
CombinedImputationReport = rbind.fill(CombinedImputationReport, CombinedImputationReport2)
rm(CombinedImputationReport2)

CombinedImputationErrors = DataFormat(readRDS("../Data/SummarisedData/CombinedImputationErrors.rds"))
CombinedImputationErrors2 = DataFormat(readRDS("../Data/SummarisedData/CombinedImputationErrors_BHPMF.rds"))
CombinedImputationErrors = rbind.fill(CombinedImputationErrors, CombinedImputationErrors2)
rm(CombinedImputationErrors2)

SummarisedImputationErrors = DataFormat(as.data.frame(readRDS("../Data/SummarisedData/SummarisedImputationErrors.rds")))
SummarisedImputationErrors2 = DataFormat(as.data.frame(readRDS("../Data/SummarisedData/SummarisedImputationErrors_BHPMF.rds")))
SummarisedImputationErrors = rbind.fill(SummarisedImputationErrors, SummarisedImputationErrors2)
rm(SummarisedImputationErrors2)

CombinedRegressionErrors = DataFormat(readRDS("../Data/SummarisedData/CombinedRegressionErrors.rds"))
CombinedRegressionErrors2 = DataFormat(readRDS("../Data/SummarisedData/CombinedRegressionErrors_BHPMF.rds"))
CombinedRegressionErrors = rbind.fill(CombinedRegressionErrors, CombinedRegressionErrors2)
rm(CombinedRegressionErrors2)

CombinedResiduals = DataFormat(readRDS("../Data/SummarisedData/CombinedResiduals.rds"))
CombinedResiduals2 = DataFormat(readRDS("../Data/SummarisedData/CombinedResiduals_BHPMF.rds"))
CombinedResiduals2 = subset(CombinedResiduals2, ImputationApproach != "Complete-case")
CombinedResiduals = rbind(CombinedResiduals, CombinedResiduals2)
rm(CombinedResiduals2)

CombinedDescriptors = DataFormat(as.data.frame(readRDS("../Data/SummarisedData/CombinedDescriptors.rds")))
CombinedDescriptors2 = DataFormat(as.data.frame(readRDS("../Data/SummarisedData/CombinedDescriptors_BHPMF.rds")))
CombinedDescriptors = rbind(CombinedDescriptors, CombinedDescriptors2)
rm(CombinedDescriptors2)


#**************Remove any records were errors occured, from Eigen first, then imputation
#Eigen
RecordsWithEigenErrors = subset(CombinedEigenReport, ErrorPresent == "Yes", select = c(Seed, CorrelationLevel, ResponsePresent,Trend, FunctionName, Missing, ErrorPresent))
colnames(RecordsWithEigenErrors)[7] = "EigenError"

CombinedImputationErrors = left_join(CombinedImputationErrors, RecordsWithEigenErrors)
CombinedImputationErrors = subset(CombinedImputationErrors, is.na(EigenError))
SummarisedImputationErrors = left_join(SummarisedImputationErrors, RecordsWithEigenErrors)
SummarisedImputationErrors = subset(SummarisedImputationErrors, is.na(EigenError))
CombinedRegressionErrors = left_join(CombinedRegressionErrors, RecordsWithEigenErrors)
CombinedRegressionErrors = subset(CombinedRegressionErrors, is.na(EigenError))

#Imputation
CombinedImputationReport$NumberOfImputationAttempts = as.numeric(as.character(CombinedImputationReport$NumberOfImputationAttempts))
RecordsWithImputationErrors = subset(CombinedImputationReport, MiceComputationallySingular == "Yes" | NumberOfImputationAttempts > 1, select = c(Seed, CorrelationLevel, ResponsePresent, Trend, FunctionName, Missing, ImputationApproach, MiceComputationallySingular, NumberOfImputationAttempts))
colnames(RecordsWithImputationErrors)[8] = "ImputationError"

CombinedImputationErrors = left_join(CombinedImputationErrors, RecordsWithImputationErrors)
CombinedImputationErrors = subset(CombinedImputationErrors, ImputationError == "No" | is.na(ImputationError))
SummarisedImputationErrors = left_join(SummarisedImputationErrors, RecordsWithImputationErrors)
source(file = "Script7_Expanded/Script7_Expanded_EigenVectors_V1.0.R")
SummarisedImputationErrors = subset(SummarisedImputationErrors, ImputationError == "No" | is.na(ImputationError))
CombinedRegressionErrors = left_join(CombinedRegressionErrors, RecordsWithImputationErrors)
CombinedRegressionErrors = subset(CombinedRegressionErrors, ImputationError == "No" | is.na(ImputationError))

#**************Format data for analysis
#RemoveExtremes
SummarisedImputationErrorsDelete = subset(SummarisedImputationErrors, RMSE > 10)
length(SummarisedImputationErrorsDelete[,1]) #Delete records where the RMSE is producing clearly inaccurate values (e.g. double expected), not just for imputation error, but also slope error
SummarisedImputationErrors = subset(SummarisedImputationErrors, RMSE < 10)


CombinedRegressionErrors = left_join(CombinedRegressionErrors, SummarisedImputationErrorsDelete)
CombinedRegressionErrors = subset(CombinedRegressionErrors, is.na(RMSE))

CombinedRegressionErrors = left_join(CombinedRegressionErrors, CombinedStartingTrend)
CombinedRegressionErrors$TraitParameterEstimate = Factor2Num(CombinedRegressionErrors$TraitParameterEstimate)
CombinedRegressionErrors$Estimate = Factor2Num(CombinedRegressionErrors$Estimate)
CombinedRegressionErrors$AbsDiff = abs(CombinedRegressionErrors$TraitParameterEstimate - CombinedRegressionErrors$Estimate)
CombinedRegressionErrors$Seed = as.factor(CombinedRegressionErrors$Seed)
CombinedRegressionErrors$FunctionName = as.factor(CombinedRegressionErrors$FunctionName)
CombinedRegressionErrors$ImputationApproach = as.factor(CombinedRegressionErrors$ImputationApproach)

#Set Rphylopars as the reference
SummarisedImputationErrors$ImputationApproach = factor(SummarisedImputationErrors$ImputationApproach, levels = c("Rphylopars", "Mice: regression", "Mice: regression + phylogeny", "Mice: mean matching", "Mice: mean matching + phylogeny", "Mice: random forest", "Mice: random forest + phylogeny", "BHPMFObv", "BHPMFNode"))
SummarisedImputationErrors$ImputationApproach = plyr::revalue(SummarisedImputationErrors$ImputationApproach, c("BHPMFObv"="BHPMF", "BHPMFNode"="BHPMF + phylogeny"))
SummarisedImputationErrors = DataFormat(SummarisedImputationErrors)

CombinedImputationErrors$ImputationApproach = plyr::revalue(CombinedImputationErrors$ImputationApproach, c("BHPMFObv"="BHPMF", "BHPMFNode"="BHPMF + phylogeny"))

CombinedRegressionErrors$ImputationApproach = factor(CombinedRegressionErrors$ImputationApproach, levels = c("Complete-case", "Rphylopars", "Mice: regression", "Mice: regression + phylogeny", "Mice: mean matching", "Mice: mean matching + phylogeny", "Mice: random forest", "Mice: random forest + phylogeny", "BHPMFObv", "BHPMFNode"))
CombinedRegressionErrors$ImputationApproach = plyr::revalue(CombinedRegressionErrors$ImputationApproach, c("BHPMFObv"="BHPMF", "BHPMFNode"="BHPMF + phylogeny"))


SummarisedImputationErrors$FunctionReName = SummarisedImputationErrors$FunctionName
levels(SummarisedImputationErrors$FunctionReName) = list('Random' = "MCAR", 'Phylogeny' = "MOPP", 'Phylogeny ' = "WBP", 'Phylogeny  ' = "SBP", 'Trait' = "MOPT", 'Trait ' = "WBTP", 'Trait  ' = "SBTP", 'Trait*Response' = "WBTPR", 'Trait*Response ' = "SBTPR", 'Response' = "WBTR", 'Response ' = "SBTR")
SummarisedImputationErrors$BiasLevel = SummarisedImputationErrors$FunctionName
levels(SummarisedImputationErrors$BiasLevel) = list('No Bias' = c("MCAR"), 'Controlled Bias' =  c("MOPP", "MOPT"), 'Weak Bias' = c("WBP", "WBTP", "WBTPR", "WBTR"), 'Severe Bias' = c("SBP", "SBTPR", "SBTP", "SBTR"))

SummarisedImputationErrors$FunctionBiasCombine = paste(SummarisedImputationErrors$BiasLevel, SummarisedImputationErrors$FunctionReName, sep = ": ")
SummarisedImputationErrors$FunctionBiasCombine = factor(SummarisedImputationErrors$FunctionBiasCombine, levels = c("No Bias: Random", "Controlled Bias: Phylogeny", "Controlled Bias: Trait", "Weak Bias: Phylogeny ", "Weak Bias: Trait ", "Weak Bias: Response", "Weak Bias: Trait*Response", "Severe Bias: Phylogeny  ", "Severe Bias: Trait  ", "Severe Bias: Response ", "Severe Bias: Trait*Response "))

CombinedRegressionErrors$FunctionReName = CombinedRegressionErrors$FunctionName
levels(CombinedRegressionErrors$FunctionReName) = list('Random' = "MCAR", 'Phylogeny' = "MOPP", 'Phylogeny ' = "WBP", 'Phylogeny  ' = "SBP", 'Trait' = "MOPT", 'Trait ' = "WBTP", 'Trait  ' = "SBTP", 'Trait*Response' = "WBTPR", 'Trait*Response ' = "SBTPR", 'Response' = "WBTR", 'Response ' = "SBTR")
CombinedRegressionErrors$BiasLevel = CombinedRegressionErrors$FunctionName
levels(CombinedRegressionErrors$BiasLevel) = list('No Bias' = c("MCAR"), 'Controlled Bias' =  c("MOPP", "MOPT"), 'Weak Bias' = c("WBP", "WBTP", "WBTPR", "WBTR"), 'Severe Bias' = c("SBP", "SBTPR", "SBTP", "SBTR"))


#**************Produce residual plots showing that response-trait residuals are somewhat normal, regardless of seed, missingnness or bias type

#PaletteOne <- rep("black",10)
#PaletteTwo <- rep(NA,10)
#CombinedResiduals$Categorisation = paste("Seed(",CombinedResiduals$Seed, ") - Missing(", CombinedResiduals$Missing, ") - Bias(", CombinedResiduals$FunctionName, ")", sep = "")
#colnames(CombinedResiduals)[7] = "Residuals"
#pdf("../Results/ResidualPlots/Supplementary2.pdf")
#for (var in unique(CombinedResiduals$Categorisation)) {
#TempGraph = ggplot(CombinedResiduals[CombinedResiduals$Categorisation==var,], aes(x = Residuals, colour = ImputationApproach)) + 
#    geom_density(alpha = 0.07) + 
#    scale_colour_manual(values = PaletteOne) + 
#    scale_fill_manual(values = PaletteTwo) + 
#    theme_classic() + 
#    theme(
#      legend.position = "none") +
#    labs(title = var)
#print(TempGraph)
#}
#dev.off()

#**************Produce ridge plots *11 for Imputation error and *2 for slope error
source(file = "Script7_Expanded/Script7_Expanded_RidegPlotsImputationError_V1.3.R")
source(file = "Script7_Expanded/Script7_Expanded_RidegPlotsSlopeErrorPositive_V1.3.R")
source(file = "Script7_Expanded/Script7_Expanded_RidegPlotsSlopeErrorNoTrend_V1.3.R")


#**************Model which method performs best under each bias type *3
MixedModelImputationErrors = lmer(log10(RMSE) ~ - 1 + FunctionName + ImputationApproach + Missing + CorrelationLevel + ResponsePresent + Trend + ImputationApproach:FunctionName + ImputationApproach:Missing + ResponsePresent:ImputationApproach:Trend  + (1|Seed), data = SummarisedImputationErrors)

png("../Results/ModelOutputs/ResidualPlots/ImputationError.png", width = 10, height = 5, units = "in", res = 100)
residuals_plots(MixedModelImputationErrors, x = "log10(Imputation error)", title = "Model 1 - Imputation error assumption plots", y = NA)
dev.off()

MixedModel2Excel_FactorsPresent(MixedModelImputationErrors, "_ImputationError")

CombinedRegressionErrors_Positive = CombinedRegressionErrors[which(CombinedRegressionErrors$Trend == "PositiveTrend"),]
MixedModelRegressionErrors_PositiveTrend = lmer(sqrt(AbsDiff) ~ - 1 + FunctionName + ImputationApproach + Missing + CorrelationLevel + ImputationApproach:FunctionName + ImputationApproach:Missing + (1|Seed), data = CombinedRegressionErrors_Positive)

png("../Results/ModelOutputs/ResidualPlots/RegressionErrorPositive.png", width = 10, height = 5, units = "in", res = 100)
residuals_plots(MixedModelRegressionErrors_PositiveTrend, x = "sqrt(Slope error)", title = "Model 2 - Slope error (positive) assumption plots", y = NA)
dev.off()

MixedModel2Excel_FactorsPresent(MixedModelRegressionErrors_PositiveTrend, "_RegressionErrorPositive")

CombinedRegressionErrors_Null = CombinedRegressionErrors[which(CombinedRegressionErrors$Trend == "NoTrend"),]
MixedModelRegressionErrors_Null = lmer(sqrt(AbsDiff) ~ - 1 + FunctionName + ImputationApproach + Missing + CorrelationLevel + ImputationApproach:FunctionName + ImputationApproach:Missing + (1|Seed), data = CombinedRegressionErrors_Null)

png("../Results/ModelOutputs/ResidualPlots/RegressionErrorNull.png", width = 10, height = 5, units = "in", res = 100)
residuals_plots(MixedModelRegressionErrors_Null, x = "sqrt(Slope error)", title = "Model 3 - Slope error (no trend) assumption plots", y = NA)
dev.off()

MixedModel2Excel_FactorsPresent(MixedModelRegressionErrors_Null, "_RegressionErrorNull")

#**************Produce plots that show which method performs best under MCAR, and each severe bias *3

#Imputation errors


#bootstrap <- bootMer(MixedModelImputationErrors, FUN=function(x)predict(x, SummarisedImputationErrors, re.form=NA),nsim=500)
#saveRDS(bootstrap, "../Data/TemporaryData/ImputationErrors_BootstrapRaw.rds")
#bootstrap = readRDS("../Data/TemporaryData/ImputationErrors_BootstrapRaw.rds")
#lci <- apply(bootstrap$t, 2, quantile, 0.025)   
#uci <- apply(bootstrap$t, 2, quantile, 0.975)   
#pred <- predict(MixedModelImputationErrors,SummarisedImputationErrors,re.form=NA)
#PredSummarisedImputationErrors = cbind(SummarisedImputationErrors, pred, uci, lci)
#saveRDS(PredSummarisedImputationErrors, "../Data/TemporaryData/ImputationErrors_Bootstrap.rds")
PredSummarisedImputationErrors = readRDS("../Data/TemporaryData/ImputationErrors_Bootstrap.rds")
CIPredSummarisedImputationErrors = PredSummarisedImputationErrors %>%
  dplyr::group_by(ImputationApproach, FunctionBiasCombine, FunctionName, FunctionReName, Missing, ResponsePresent, Trend, CorrelationLevel) %>%
  dplyr::summarise(Min = mean(lci), Max = mean(uci),pred = mean(pred))

#bootstrap <- bootMer(MixedModelRegressionErrors_PositiveTrend, FUN=function(x)predict(x, CombinedRegressionErrors_Positive, re.form=NA),nsim=500)
#saveRDS(bootstrap, "../Data/TemporaryData/PositiveRegressionErrors_BootstrapRaw.rds")
#bootstrap = readRDS("../Data/TemporaryData/PositiveRegressionErrors_BootstrapRaw.rds")
#lci <- apply(bootstrap$t, 2, quantile, 0.025)   
#uci <- apply(bootstrap$t, 2, quantile, 0.975)   
#pred <- predict(MixedModelRegressionErrors_PositiveTrend,CombinedRegressionErrors_Positive,re.form=NA)
#PredCombinedRegressionErrors_Positive = cbind(CombinedRegressionErrors_Positive, pred, uci, lci)
#saveRDS(PredCombinedRegressionErrors_Positive, "../Data/TemporaryData/PositiveRegressionErrors_Bootstrap.rds")
PredCombinedRegressionErrors_Positive = readRDS("../Data/TemporaryData/PositiveRegressionErrors_Bootstrap.rds")
CIPredCombinedRegressionErrors_Positive = PredCombinedRegressionErrors_Positive %>%
  dplyr::group_by(ImputationApproach, FunctionReName, FunctionName, Missing, ResponsePresent, Trend, CorrelationLevel) %>%
  dplyr::summarise(Min = min(lci), Max = max(uci), pred = mean(pred))

#bootstrap <- bootMer(MixedModelRegressionErrors_Null, FUN=function(x)predict(x, CombinedRegressionErrors_Null, re.form=NA),nsim=500)
#saveRDS(bootstrap, "../Data/TemporaryData/NullRegressionErrors_BootstrapRaw.rds")
#bootstrap = readRDS("../Data/TemporaryData/NullRegressionErrors_BootstrapRaw.rds")
#lci <- apply(bootstrap$t, 2, quantile, 0.025)   
#uci <- apply(bootstrap$t, 2, quantile, 0.975)   
#pred <- predict(MixedModelRegressionErrors_Null,CombinedRegressionErrors_Null,re.form=NA)
#PredCombinedRegressionErrors_Null = cbind(CombinedRegressionErrors_Null, pred, uci, lci)
#saveRDS(PredCombinedRegressionErrors_Null, "../Data/TemporaryData/NullRegressionErrors_Bootstrap.rds")
PredCombinedRegressionErrors_Null = readRDS("../Data/TemporaryData/NullRegressionErrors_Bootstrap.rds")
CIPredCombinedRegressionErrors_Null = PredCombinedRegressionErrors_Null %>%
  dplyr::group_by(ImputationApproach, FunctionReName, FunctionName, Missing, ResponsePresent, Trend, CorrelationLevel) %>%
  dplyr::summarise(Min = min(lci), Max = max(uci),pred = mean(pred))


#Plot effect of phylogeny
source(file = "Script7_Expanded/Script7_Expanded_EffectOfPhylogeny_V1.0.R")

#Full plot
source(file = "Script7_Expanded/Script7_Expanded_ImputationErrorVsMissingVsBias_V1.2.R")

source(file = "Script7_Expanded/Script7_Expanded_RegressionErrorPositiveVsMissingVsBias_V1.2.R")

source(file = "Script7_Expanded/Script7_Expanded_RegressionErrorNullVsMissingVsBias_V1.2.R")



#Full plot when no phylogeny
source(file = "Script7_Expanded/Script7_Expanded_ImputationErrorVsMissingVsBias_NoPhylo_V1.2.R")

source(file = "Script7_Expanded/Script7_Expanded_RegressionErrorPositiveVsMissingVsBias_NoPhylo_V1.2.R")

source(file = "Script7_Expanded/Script7_Expanded_RegressionErrorNullVsMissingVsBias_NoPhylo_V1.2.R")


#Plot for ms
source(file = "Script7_Expanded/Script7_Expanded_ImputationErrorVsMissingVsBiasRefined_V1.2.R")

source(file = "Script7_Expanded/Script7_Expanded_RegressionErrorVsMissingVsBiasRefined_V1.3.R")

#Show imputation error doenst differ substantially between the slopes
source(file = "Script7_Expanded/Script7_Expanded_ImputationErrorVsMissingVsBiasRefined_V1.3.R")






#**************Produce models that show how including the response alters performance *2 (RMSE already modelled)
#Exclude complete-case then add response present
CombinedRegressionErrors_Positive_CCExclude = CombinedRegressionErrors[which(CombinedRegressionErrors$Trend == "PositiveTrend" & CombinedRegressionErrors$ImputationApproach != "Complete-case"),]
MixedModelRegressionErrors_PositiveTrend_ResponseQ = lmer(sqrt(AbsDiff) ~ - 1 + FunctionName + ImputationApproach + Missing + CorrelationLevel + ResponsePresent + ImputationApproach:FunctionName + ImputationApproach:Missing + ResponsePresent:ImputationApproach + (1|Seed), data = CombinedRegressionErrors_Positive_CCExclude)

png("../Results/ModelOutputs/ResidualPlots/RegressionErrorPositive_ResponseQ.png", width = 10, height = 5, units = "in", res = 100)
residuals_plots(MixedModelRegressionErrors_PositiveTrend_ResponseQ, x = "sqrt(Slope error)", title = "Model 4 - Slope error (positive) assumption plots \n
[response included in imputation]",
                y = sqrt(CombinedRegressionErrors_Positive_CCExclude$AbsDiff))
dev.off()


MixedModel2Excel_FactorsPresent(MixedModelRegressionErrors_PositiveTrend_ResponseQ, "_RegressionErrorPositive_ResponseQ")

CombinedRegressionErrors_Null_CCExclude = CombinedRegressionErrors[which(CombinedRegressionErrors$Trend == "NoTrend" & CombinedRegressionErrors$ImputationApproach != "Complete-case"),]
MixedModelRegressionErrors_NullTrend_ResponseQ = lmer(sqrt(AbsDiff) ~ - 1 + FunctionName + ImputationApproach + Missing + CorrelationLevel + ResponsePresent + ImputationApproach:FunctionName + ImputationApproach:Missing + ResponsePresent:ImputationApproach + (1|Seed), data = CombinedRegressionErrors_Null_CCExclude)

png("../Results/ModelOutputs/ResidualPlots/RegressionErrorNull_ResponseQ.png", width = 10, height = 5, units = "in", res = 100)
residuals_plots(MixedModelRegressionErrors_NullTrend_ResponseQ, x = "sqrt(Slope error)", title = "Model 5 - Slope error (no trend) assumption plots \n
[response included in imputation]",
                y = sqrt(CombinedRegressionErrors_Null_CCExclude$AbsDiff))
dev.off()


MixedModel2Excel_FactorsPresent(MixedModelRegressionErrors_NullTrend_ResponseQ, "_RegressionErrorNull_ResponseQ")


#**************Produce plots that summarise role of response and between-trait correlation

source(file = "Script7_Expanded/Script7_Expanded_ImputationErrorResponseVsImpApproach_V1.1.R")

bootstrap <- bootMer(MixedModelRegressionErrors_PositiveTrend_ResponseQ, FUN=function(x)predict(x, CombinedRegressionErrors_Positive_CCExclude, re.form=NA),nsim=500)
saveRDS(bootstrap, "../Data/TemporaryData/PositiveRegressionErrors_ResponseQ_BootstrapRaw.rds")
bootstrap = readRDS("../Data/TemporaryData/PositiveRegressionErrors_ResponseQ_BootstrapRaw.rds")
lci <- apply(bootstrap$t, 2, quantile, 0.025)   
uci <- apply(bootstrap$t, 2, quantile, 0.975)   
pred <- predict(MixedModelRegressionErrors_PositiveTrend_ResponseQ,CombinedRegressionErrors_Positive_CCExclude,re.form=NA)
PredCombinedRegressionErrors_Positive_ResponseQ = cbind(CombinedRegressionErrors_Positive_CCExclude, pred, uci, lci)

bootstrap <- bootMer(MixedModelRegressionErrors_NullTrend_ResponseQ, FUN=function(x)predict(x, CombinedRegressionErrors_Null_CCExclude, re.form=NA),nsim=500)
saveRDS(bootstrap, "../Data/TemporaryData/NullRegressionErrors_ResponseQ_BootstrapRaw.rds")
bootstrap = readRDS("../Data/TemporaryData/NullRegressionErrors_ResponseQ_BootstrapRaw.rds")
lci <- apply(bootstrap$t, 2, quantile, 0.025)   
uci <- apply(bootstrap$t, 2, quantile, 0.975)   
pred <- predict(MixedModelRegressionErrors_NullTrend_ResponseQ,CombinedRegressionErrors_Null_CCExclude,re.form=NA)
PredCombinedRegressionErrors_Null_ResponseQ = cbind(CombinedRegressionErrors_Null_CCExclude, pred, uci, lci)


source(file = "Script7_Expanded/Script7_Expanded_RegressionErrorResponseVsImpApproach_V1.1.R")
source(file = "Script7_Expanded/Script7_Expanded_RegressionErrorPositiveVsMissingVsBias_ResponseYes_V1.2.R")
source(file = "Script7_Expanded/Script7_Expanded_RegressionErrorPositiveVsMissingVsBias_ResponseNo_V1.2.R")
source(file = "Script7_Expanded/Script7_Expanded_RegressionErrorNullVsMissingVsBias_ResponseYes_V1.2.R")
source(file = "Script7_Expanded/Script7_Expanded_RegressionErrorNullVsMissingVsBias_ResponseNo_V1.2.R")

#**************Summarise elapsed time
CombineEigenWithImputationReport = left_join(CombinedImputationReport, CombinedEigenReport, by = c("Seed", "CorrelationLevel", "ResponsePresent", "Trend", "FunctionName", "Missing"))
CombineEigenWithImputationReport$ImputationApproach = plyr::revalue(CombineEigenWithImputationReport$ImputationApproach, c("BHPMFObv"="BHPMF", "BHPMFNode"="BHPMF + phylogeny"))

CombineEigenWithImputationReport = subset(CombineEigenWithImputationReport, (CombineEigenWithImputationReport$ImputationApproach == "Mice: mean matching + phylogeny"| CombineEigenWithImputationReport$ImputationApproach == "Mice: random forest + phylogeny"| CombineEigenWithImputationReport$ImputationApproach == "Mice: regression + phylogeny"| CombineEigenWithImputationReport$ImputationApproach == "BHPMF + phylogeny"|CombineEigenWithImputationReport$ImputationApproach == "Rphylopars"))

CombineEigenWithImputationReport$MinutesElapsed.x = Factor2Num(CombineEigenWithImputationReport$MinutesElapsed.x)
CombineEigenWithImputationReport$MinutesElapsed.y = Factor2Num(CombineEigenWithImputationReport$MinutesElapsed.y)

CombineEigenWithImputationReport$TotalElapsedTime = CombineEigenWithImputationReport$MinutesElapsed.x +
                                                        CombineEigenWithImputationReport$MinutesElapsed.y

CombineEigenWithImputationReport$TotalElapsedTime = ifelse(CombineEigenWithImputationReport$ImputationApproach == "Rphylopars", CombineEigenWithImputationReport$MinutesElapsed.x, CombineEigenWithImputationReport$TotalElapsedTime)
CombineEigenWithImputationReport$ImputationApproach  = droplevels(CombineEigenWithImputationReport$ImputationApproach )
CombineEigenWithImputationReport$ImputationApproach = factor(CombineEigenWithImputationReport$ImputationApproach, levels = c("Rphylopars", "Mice: mean matching + phylogeny", "Mice: regression + phylogeny", "Mice: random forest + phylogeny", "BHPMF + phylogeny"))

SummariseTimeElapsed = CombineEigenWithImputationReport %>%
  dplyr::group_by(ImputationApproach) %>%
  summarise(Mean = mean(TotalElapsedTime, na.rm = T), SD = sd(TotalElapsedTime, na.rm = T), N = n())

write.csv(SummariseTimeElapsed,"../Results/SummaryPlots/TimeElapsed(minutes).csv")


#**************Model likelihood of having poor imputation *3*4
CC = subset(CombinedRegressionErrors, ImputationApproach == "Complete-case", select = c("Seed", "CorrelationLevel", "ResponsePresent", "Trend", "FunctionName", "Missing", "TraitParameterEstimate", "SE"))
Imp = subset(CombinedRegressionErrors, ImputationApproach != "Complete-case")
CCvsImp = left_join(Imp, CC, by = c("Seed", "CorrelationLevel", "ResponsePresent", "Trend", "FunctionName", "Missing"))
CCvsImp$AbsSlopeDiffCCImp = abs(Factor2Num(CCvsImp$TraitParameterEstimate.x) - Factor2Num(CCvsImp$TraitParameterEstimate.y))
CCvsImp$SigDiffImpvsTrue_t = abs(Factor2Num(CCvsImp$Estimate) - Factor2Num(CCvsImp$TraitParameterEstimate.x))/(sqrt((Factor2Num(CCvsImp$SE.x))^2 + (Factor2Num(CCvsImp$TraitSE))^2))
CCvsImp$SigDiffImpvsTrue_p = abs(qt(c(0.025),(500 + 500 - 4)))
CCvsImp$SigDiffImpvsTrue_Sig = ifelse(CCvsImp$SigDiffImpvsTrue_t > CCvsImp$SigDiffImpvsTrue_p, 1, 0)

PredictErrorsDF = left_join(CCvsImp, SummarisedImputationErrors, by = c("Seed", "CorrelationLevel", "ResponsePresent", "Trend", "FunctionName", "Missing", "ImputationApproach"))
colnames(CombinedDescriptors) = c("Seed", 
                                  "CorrelationLevel", 
                                  "ResponsePresent", 
                                  "Trend", 
                                  "FunctionName", 
                                  "Missing", 
                                  "ImputationApproach",
                                  "PhyloClustering",
                                  "MeanImp",
                                  "MeanCC",
                                  "RangeImp",
                                  "RangeCC")
                    
PredictErrorsDF = left_join(PredictErrorsDF, CombinedDescriptors)
PredictErrorsDF$PhyloClustering = Factor2Num(PredictErrorsDF$PhyloClustering)
PredictErrorsDF$MeanImp = Factor2Num(PredictErrorsDF$MeanImp)
PredictErrorsDF$MeanCC = Factor2Num(PredictErrorsDF$MeanCC)
PredictErrorsDF$RangeImp = Factor2Num(PredictErrorsDF$RangeImp)
PredictErrorsDF$RangeCC = Factor2Num(PredictErrorsDF$RangeCC)
PredictErrorsDF$MeanDiff = abs(((PredictErrorsDF$MeanImp/PredictErrorsDF$MeanCC)*100) - 100)
PredictErrorsDF$RangeDiff = (((PredictErrorsDF$RangeImp/PredictErrorsDF$RangeCC)*100)) - 100 # Removed from models as the range values had both high and low imputation errors at zero, suggesting range is not important

PredictErrorsDF = subset(PredictErrorsDF, ImputationApproach == "Rphylopars")


MixedModelImputationErrors_Predict = lmer(log10(RMSE.y) ~ Missing + PhyloClustering + log10(MeanDiff) + (1|Seed), data = PredictErrorsDF)

vif.mer(MixedModelImputationErrors_Predict)
png("../Results/ModelOutputs/ResidualPlots/ImputationErrors_Predict.png", width = 10, height = 5, units = "in", res = 100)
residuals_plots(MixedModelImputationErrors_Predict, x = "log10(Imputation error)", title = "Model 6 - Imputation error assumption plots \n
[exclusively Rphylopars]",  y = NA)
dev.off()

MixedModel2Excel(MixedModelImputationErrors_Predict, "_ImputationError_Predict")

MixedModelRegressionErrors_Predict = lmer(sqrt(AbsDiff) ~ Missing + PhyloClustering + log10(MeanDiff) + sqrt(AbsSlopeDiffCCImp) + (1|Seed), data = PredictErrorsDF)

vif.mer(MixedModelRegressionErrors_Predict)
png("../Results/ModelOutputs/ResidualPlots/RegressionErrors_Predict.png", width = 10, height = 5, units = "in", res = 100)
residuals_plots(MixedModelRegressionErrors_Predict, x = "log10(Slope error)", title = "Model 7 - Slope error assumption plots \n
[exclusively Rphylopars]", y =  sqrt(PredictErrorsDF$AbsDiff))
dev.off()

MixedModel2Excel(MixedModelRegressionErrors_Predict, "_RegressionError_Predict")


MixedModelRegressionSlopeSig_Predict = glmer(SigDiffImpvsTrue_Sig ~ Missing + PhyloClustering + log10(MeanDiff) + sqrt(AbsSlopeDiffCCImp) + (1|Seed), data = PredictErrorsDF, family = binomial)

vif.mer(MixedModelRegressionSlopeSig_Predict)
png("../Results/ModelOutputs/ResidualPlots/RegressionSlopeSig_Predict.png", width = 10, height = 5, units = "in", res = 100)
plot(MixedModelRegressionSlopeSig_Predict, xlab = "Fitted values", ylab = "Residuals", main = "Model 8 - Slope significantly different assumption plots \n
[exclusively Rphylopars]")
dev.off()

MixedModel2Excel(MixedModelRegressionSlopeSig_Predict, "_RegressionSigSlope_Predict")



ggplot()+
  geom_point(aes(x = predict(MixedModelRegressionSlopeSig_Predict),y = residuals(MixedModelRegressionSlopeSig_Predict))) +
  xlim(c(-5, 2.5)) +
  geom_smooth(aes(x = predict(MixedModelRegressionSlopeSig_Predict),y = residuals(MixedModelRegressionSlopeSig_Predict)), method = "loess") +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "Predicted values", y = "Residuals", title = "Model 8 - Slope significantly different assumption plots \n
[exclusively Rphylopars]") + 
  theme_classic()
ggsave(plot = last_plot(), "../Results/ModelOutputs/ResidualPlots/RegressionSlopeSig_Predict2.png", width = 10, height = 5, units = "in", dpi = 100)

#**************Produce plots that predict likelihood of having poor imputation *3*4

#Bootstrap model 1
bootstrap <- bootMer(MixedModelImputationErrors_Predict, FUN=function(x)predict(x, PredictErrorsDF, re.form=NA),nsim=500)
saveRDS(bootstrap, "../Data/TemporaryData/ImputationErrors_Predict_BootstrapRaw.rds")
bootstrap = readRDS("../Data/TemporaryData/ImputationErrors_Predict_BootstrapRaw.rds")
lci_M1 <- apply(bootstrap$t, 2, quantile, 0.025)   
uci_M1 <- apply(bootstrap$t, 2, quantile, 0.975)   
pred_M1 <- predict(MixedModelImputationErrors_Predict,PredictErrorsDF,re.form=NA)
PredictErrorsDF = cbind(PredictErrorsDF, pred_M1, uci_M1, lci_M1)

#Bootstrap model 2
bootstrap <- bootMer(MixedModelRegressionErrors_Predict, FUN=function(x)predict(x, PredictErrorsDF, re.form=NA),nsim=500)
saveRDS(bootstrap, "../Data/TemporaryData/RegressionErrors_Predict_BootstrapRaw.rds")
bootstrap = readRDS("../Data/TemporaryData/RegressionErrors_Predict_BootstrapRaw.rds")
lci_M2 <- apply(bootstrap$t, 2, quantile, 0.025)   
uci_M2 <- apply(bootstrap$t, 2, quantile, 0.975)   
pred_M2 <- predict(MixedModelRegressionErrors_Predict,PredictErrorsDF,re.form=NA)
PredictErrorsDF = cbind(PredictErrorsDF, pred_M2, uci_M2, lci_M2)

#Bootstrap model 3
bootstrap <- bootMer(MixedModelRegressionSlopeSig_Predict, FUN=function(x)predict(x, PredictErrorsDF, re.form=NA, type = "response"),nsim=500)
saveRDS(bootstrap, "../Data/TemporaryData/RegressionErrorsSig_Predict_BootstrapRaw.rds")
bootstrap = readRDS("../Data/TemporaryData/RegressionErrorsSig_Predict_BootstrapRaw.rds")
lci_M3 <- apply(bootstrap$t, 2, quantile, 0.025)   
uci_M3 <- apply(bootstrap$t, 2, quantile, 0.975)   
pred_M3 <- predict(MixedModelRegressionSlopeSig_Predict,PredictErrorsDF,re.form=NA, type = "response")
PredictErrorsDF = cbind(PredictErrorsDF, pred_M3, uci_M3, lci_M3)

#Plot the predictions
source(file = "Script7_Expanded/Script7_Expanded_Predict_Plots_V1.1.R")

#Plot the CI and point estimate for slope coefficients
source(file = "Script7_Expanded/Script7_Expanded_RegressionErrorSigCIPlots_V1.0.R")

#Plot the T statistic
source(file = "Script7_Expanded/Script7_Expanded_SlopeTStat_V1.1.R")

#Calculate and plot R2
source(file = "Script7_Expanded/Script7_Expanded_R2_V1.0.R")

#Plot mean absolute error
source(file = "Script7_Expanded/Script7_Expanded_MeanAE_V1.0.R")

#Plot median absolute error
source(file = "Script7_Expanded/Script7_Expanded_MedianAE_V1.0.R")

#Where are BHPMF errors occuring
source(file = "Script7_Expanded/Script7_Expanded_CheckBHPMFPerformance_V1.0.R")

#Join model residual plots into unified supplementary
library(magick)


fl = c(c(
  "../Results/ModelOutputs/ResidualPlots/ImputationError.png",
  "../Results/ModelOutputs/ResidualPlots/RegressionErrorPositive.png",
  "../Results/ModelOutputs/ResidualPlots/RegressionErrorNull.png",
  "../Results/ModelOutputs/ResidualPlots/RegressionErrorPositive_ResponseQ.png",
  "../Results/ModelOutputs/ResidualPlots/RegressionErrorNull_ResponseQ.png",
  "../Results/ModelOutputs/ResidualPlots/ImputationErrors_Predict.png",
  "../Results/ModelOutputs/ResidualPlots/RegressionErrors_Predict.png",
  "../Results/ModelOutputs/ResidualPlots/RegressionSlopeSig_Predict.png",
  "../Results/ModelOutputs/ResidualPlots/RegressionSlopeSig_Predict2.png"
))
# magick
img = image_read(fl) # read from vector of paths
img2 = image_append(img, stack = TRUE) # places pics above one another

pdf("../Results/ModelOutputs/ResidualPlots/Supplementary3.pdf", width = 10, height = 45)
plot(img2)
dev.off()

