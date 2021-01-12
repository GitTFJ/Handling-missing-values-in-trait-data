CIPredSummarisedImputationErrors2 = CIPredSummarisedImputationErrors
CIPredSummarisedImputationErrors2$BiasLevel = CIPredSummarisedImputationErrors2$FunctionName
levels(CIPredSummarisedImputationErrors2$BiasLevel) = list('No Bias' = c("MCAR"), 'Controlled Bias' =  c("MOPP", "MOPT"), 'Weak Bias' = c("WBP", "WBTP", "WBTPR", "WBTR"), 'Severe Bias' = c("SBP", "SBTPR", "SBTP", "SBTR"))

Ribbon = CIPredSummarisedImputationErrors2 %>%
  group_by(ImputationApproach, BiasLevel) %>%
  summarise(pred = mean(pred), Max = mean(Max), Min = mean(Min))
Ribbon = subset(Ribbon, ImputationApproach != "Rphylopars")
Ribbon$Phylogeny = ifelse(
  grepl("phylogeny", Ribbon$ImputationApproach), 
  "Present", 
  "Absent")
Ribbon$ImputationApproach = gsub(" + phylogeny", "", Ribbon$ImputationApproach, fixed = T)


ggplot(Ribbon) +
  geom_point(aes(x = ImputationApproach, y = 10^pred, colour = Phylogeny), size = 3) +
  geom_linerange(aes(x = ImputationApproach, ymin = 10^Min, ymax = 10^Max, colour = Phylogeny), size = 1.2) +
  coord_flip() +
  facet_grid(.~BiasLevel, scales = "free_x") +
  scale_colour_manual(values = c("grey", "black"))+
  scale_y_continuous(breaks = c(1,1.2,1.4,1.6,1.8,2.0,2.2,2.4,2.6, 2.8)) +
  labs(x = "", y = "Imputation error (when true slope is positive)") +
  theme_classic() +
  theme(panel.background = element_rect(fill = NA, color = "black"))
ggsave("../Results/SummaryPlots/ImputationError_VsImputationApproach.png" ,plot = last_plot(), width = 25, height = 4, units = "cm")

CIPredCombinedRegressionErrors_Positive2 = CIPredCombinedRegressionErrors_Positive
CIPredCombinedRegressionErrors_Positive2$BiasLevel = CIPredCombinedRegressionErrors_Positive2$FunctionName
levels(CIPredCombinedRegressionErrors_Positive2$BiasLevel) = list('No Bias' = c("MCAR"), 'Controlled Bias' =  c("MOPP", "MOPT"), 'Weak Bias' = c("WBP", "WBTP", "WBTPR", "WBTR"), 'Severe Bias' = c("SBP", "SBTPR", "SBTP", "SBTR"))

Ribbon = CIPredCombinedRegressionErrors_Positive2 %>%
  group_by(ImputationApproach, BiasLevel) %>%
  summarise(pred = mean(pred), Max = mean(Max), Min = mean(Min))
Ribbon = subset(Ribbon, ImputationApproach != "Rphylopars" & ImputationApproach != "Complete-case")
Ribbon$Phylogeny = ifelse(
  grepl("phylogeny", Ribbon$ImputationApproach), 
  "Present", 
  "Absent")
Ribbon$ImputationApproach = gsub(" + phylogeny", "", Ribbon$ImputationApproach, fixed = T)


ggplot(Ribbon) +
  geom_point(aes(x = ImputationApproach, y = pred^2, colour = Phylogeny), size = 3) +
  geom_linerange(aes(x = ImputationApproach, ymin = Min^2, ymax = Max^2, colour = Phylogeny), size = 1.2) +
  coord_flip() +
  facet_grid(.~BiasLevel, scales = "free_x") +
  scale_colour_manual(values = c("grey", "black"))+
  scale_y_continuous(breaks = c(0.025, 0.1, 0.15, 0.25)) +
  labs(x = "", y = "Slope error (when true slope is positive)") +
  theme_classic()+
  theme(panel.background = element_rect(fill = NA, color = "black"))
ggsave("../Results/SummaryPlots/RegressionError_PositiveVsImputationApproach.png" ,plot = last_plot(), width = 25, height = 4, units = "cm")


CIPredCombinedRegressionErrors_Null2 = CIPredCombinedRegressionErrors_Null
CIPredCombinedRegressionErrors_Null2$BiasLevel = CIPredCombinedRegressionErrors_Null2$FunctionName
levels(CIPredCombinedRegressionErrors_Null2$BiasLevel) = list('No Bias' = c("MCAR"), 'Controlled Bias' =  c("MOPP", "MOPT"), 'Weak Bias' = c("WBP", "WBTP", "WBTPR", "WBTR"), 'Severe Bias' = c("SBP", "SBTPR", "SBTP", "SBTR"))

Ribbon = CIPredCombinedRegressionErrors_Null2 %>%
  group_by(ImputationApproach, BiasLevel) %>%
  summarise(pred = mean(pred), Max = mean(Max), Min = mean(Min))
Ribbon = subset(Ribbon, ImputationApproach != "Rphylopars" & ImputationApproach != "Complete-case")
Ribbon$Phylogeny = ifelse(
  grepl("phylogeny", Ribbon$ImputationApproach), 
  "Present", 
  "Absent")
Ribbon$ImputationApproach = gsub(" + phylogeny", "", Ribbon$ImputationApproach, fixed = T)


ggplot(Ribbon) +
  geom_point(aes(x = ImputationApproach, y = pred^2, colour = Phylogeny), size = 3) +
  geom_linerange(aes(x = ImputationApproach, ymin = Min^2, ymax = Max^2, colour = Phylogeny), size = 1.2) +
  coord_flip() +
  facet_grid(.~BiasLevel, scales = "free_x") +
  scale_colour_manual(values = c("grey", "black"))+
  scale_y_continuous(breaks = c(0.01, 0.03, 0.05, 0.125)) +
  labs(x = "", y = "Slope error (when true slope exhibits no trend)") +
  theme_classic()+
  theme(panel.background = element_rect(fill = NA, color = "black"))
ggsave("../Results/SummaryPlots/RegressionError_NullVsImputationApproach.png" ,plot = last_plot(), width = 25, height = 4, units = "cm")
