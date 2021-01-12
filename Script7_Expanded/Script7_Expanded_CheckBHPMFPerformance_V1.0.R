BHPMFReport = subset(CombinedImputationReport, grepl("BHPMF", ImputationApproach) & BHPMFError == "Time limit exceeded" & ImputationApproach == "BHPMFNode")

SumBHPMFReport = BHPMFReport %>%
  group_by(FunctionName, Missing) %>%
  summarise(Counter = n())

SumBHPMFReport$PercentObs = (SumBHPMFReport$Counter/80)*100

ggplot(SumBHPMFReport) +
  geom_col(aes(x = Missing*100, y = PercentObs)) +
  facet_wrap(FunctionName~.) +
  theme_bw() +
  labs(x = "Missing (%)", y = "Percent of imputations that failed to converge (%)")
ggsave("../Results/SummaryPlots/BHPMF_TimeExceeded.png" ,plot = last_plot(), width = 15, height = 15, units = "cm")


BHPMF_SummarisedImputationErrors = subset(SummarisedImputationErrors, grepl("BHPMF", ImputationApproach))
BHPMFReport = subset(CombinedImputationReport, grepl("BHPMF", ImputationApproach), select = c(
  "Seed",
  "CorrelationLevel",
  "ResponsePresent",
  "Trend",
  "FunctionName",
  "Missing",
  "ImputationApproach",
  "BHPMFError"
))
BHPMFReport$ImputationApproach = plyr::revalue(BHPMFReport$ImputationApproach, c("BHPMFObv"="BHPMF", "BHPMFNode"="BHPMF + phylogeny"))

BHPMF_SummarisedImputationErrors = left_join(BHPMF_SummarisedImputationErrors, BHPMFReport)
BHPMF_SummarisedImputationErrors$BHPMFError = droplevels(BHPMF_SummarisedImputationErrors$BHPMFError)
BHPMF_SummarisedImputationErrors$BHPMFError = as.character(BHPMF_SummarisedImputationErrors$BHPMFError)
BHPMF_SummarisedImputationErrors$BHPMFError[is.na(BHPMF_SummarisedImputationErrors$BHPMFError)] <- "Complete"

ggplot(BHPMF_SummarisedImputationErrors[which(BHPMF_SummarisedImputationErrors$ResponsePresent == "No"),]) +
  geom_smooth(aes(x = Missing*100, y = RMSE, colour = BHPMFError), method = "lm", alpha = 0.5) +
  facet_grid(BiasLevel~.) + 
  scale_colour_manual(values = c("black", "red")) +
  theme_bw() +
  labs(x = "Missing (%)", y = "RMSE", colour = " ")
ggsave("../Results/SummaryPlots/BHPMF_WhenForced.png" ,plot = last_plot(), width = 15, height = 15, units = "cm")
