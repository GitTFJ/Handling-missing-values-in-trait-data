CC = subset(CombinedRegressionErrors, ImputationApproach == "Complete-case", select = c("Seed", "CorrelationLevel", "ResponsePresent", "Trend", "FunctionName", "Missing", "TraitParameterEstimate", "SE"))
Imp = subset(CombinedRegressionErrors, ImputationApproach != "Complete-case")
CCvsImp = left_join(Imp, CC, by = c("Seed", "CorrelationLevel", "ResponsePresent", "Trend", "FunctionName", "Missing"))
CCvsImp$AbsSlopeDiffCCImp = abs(Factor2Num(CCvsImp$TraitParameterEstimate.x) - Factor2Num(CCvsImp$TraitParameterEstimate.y))
CCvsImp$SigDiffImpvsTrue_t = abs(Factor2Num(CCvsImp$Estimate) - Factor2Num(CCvsImp$TraitParameterEstimate.x))/(sqrt((Factor2Num(CCvsImp$SE.x))^2 + (Factor2Num(CCvsImp$TraitSE))^2))
CCvsImp$SigDiffImpvsTrue_p = abs(qt(c(0.025),(500 + 500 - 4)))
CCvsImp$SigDiffImpvsTrue_Sig = ifelse(CCvsImp$SigDiffImpvsTrue_t > CCvsImp$SigDiffImpvsTrue_p, 1, 0)


WhatExplainsMiceSigDif = subset(SummarisedImputationErrors, grepl("Mic", ImputationApproach))
WhatExplainsMiceSigDif2 = subset(CCvsImp, grepl("Mic", ImputationApproach))

WhatExplainsMiceSigDif = left_join(WhatExplainsMiceSigDif, WhatExplainsMiceSigDif2, by = c("Seed", "CorrelationLevel", "ResponsePresent", "Trend", "FunctionName", "Missing", "ImputationApproach"))

WhatExplainsMiceSigDif$TraitSE = as.numeric(as.character(WhatExplainsMiceSigDif$TraitSE))

m3 = glmer(SigDiffImpvsTrue_Sig ~ log10(scale(RMSE.x)) + log10(scale(MeanVarOfObv.x)) + log10(scale(TraitSE)) + (1|ResponsePresent:TraitSE) + (1|ResponsePresent:RMSE.x) + (1|ResponsePresent:MeanVarOfObv.x) + (1|Seed), data = WhatExplainsMiceSigDif, family = binomial)
summary(m3)

confint(m3)
PE = summary(m3)
CI = confint(m3)  

DF = data.frame(Var = c("RMSE", "Imputed values variance", "Imputed slope standard error"), 
                Lower = CI[6:8,1],
                PE = PE$coefficients[2:4,1], 
                Upper = CI[6:8,2])



ggplot(DF) +
  geom_pointrange(aes(x = Var, 
                      ymin = exp(Lower),
                      y = exp(PE),
                      ymax = exp(Upper)), size = 1.3) +
  geom_hline(aes(yintercept = 1)) +
  coord_flip() +
  scale_y_log10(breaks = c(0.5, 1, 2),
                labels = c(0.5, 1, 2)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  labs(y = "Odds ratio", x = "")
ggsave("../Results/SummaryPlots/DriversOfSigErrorMice.png" ,plot = last_plot(), width = 20, height = 8, units = "cm")
