pdf("../Results/SummaryPlots/Predict_RegressionErrorSig_Missingness.pdf", width = 20, height = 20)
p = ggplot(PredictErrorsDF) + 
  geom_smooth(aes(x = (Missing*100), y = pred_M3), method = "glm", method.args = list(family = "binomial"), colour = "black", se = F) +
  geom_smooth(aes(x = (Missing*100), y = uci_M3), method = "glm", method.args = list(family = "binomial"), linetype = "dotted", colour = "red", se = F) +
  geom_smooth(aes(x = (Missing*100), y = lci_M3), method = "glm", method.args = list(family = "binomial"), linetype = "dotted", colour = "red", se = F) +
  ylim(0,1) +
  theme_classic() +
  theme(
    text = element_text(size=35)
  ) +
  labs(x = "Missing data (%)", y = "Probability of difference between imputed and true trend  ", "\u00B1"," 95% confidence intervals", sep = "")
print(p)
dev.off()



pdf("../Results/SummaryPlots/Predict_RegressionErrorSig_PhyloClustering.pdf", width = 20, height = 20)
p = ggplot(PredictErrorsDF) + 
  geom_smooth(aes(x = PhyloClustering, y = pred_M3), method = "glm", method.args = list(family = "binomial"), colour = "black", se = F) +
  geom_smooth(aes(x = PhyloClustering, y = uci_M3), method = "glm", method.args = list(family = "binomial"), linetype = "dotted", colour = "red", se = F) +
  geom_smooth(aes(x = PhyloClustering, y = lci_M3), method = "glm", method.args = list(family = "binomial"), linetype = "dotted", colour = "red", se = F) +
  ylim(0,1) +
  scale_x_reverse() +
  theme_classic() +
  theme(
    text = element_text(size=35)
  ) +
  labs(x = "Phylogenetic clustering (evenly distibuted - clustered)", y = "Probability of difference between imputed and true trend  ", "\u00B1"," 95% confidence intervals", sep = "")
print(p)
dev.off()


pdf("../Results/SummaryPlots/Predict_RegressionErrorSig_MeanDiff.pdf", width = 20, height = 20)
p = ggplot(PredictErrorsDF) + 
  geom_smooth(aes(x = MeanDiff, y = pred_M3), method = "glm", method.args = list(family = "binomial"), colour = "black", se = F) +
  geom_smooth(aes(x = MeanDiff, y = uci_M3), method = "glm", method.args = list(family = "binomial"), linetype = "dotted", colour = "red", se = F) +
  geom_smooth(aes(x = MeanDiff, y = lci_M3), method = "glm", method.args = list(family = "binomial"), linetype = "dotted", colour = "red", se = F) +
  theme_classic() +
  theme(
    text = element_text(size=35)
  ) +
  labs(x = "Change in mean before and after imputation (%)", y = "Probability of difference between imputed and true trend  ", "\u00B1"," 95% confidence intervals", sep = "")
print(p)
dev.off()




pdf("../Results/SummaryPlots/Predict_RegressionErrorSig_SlopeDiff.pdf", width = 20, height = 20)
p = ggplot(PredictErrorsDF) + 
  geom_smooth(aes(x = AbsSlopeDiffCCImp, y = pred_M3), method = "glm", method.args = list(family = "binomial"), colour = "black", se = F) +
  geom_smooth(aes(x = AbsSlopeDiffCCImp, y = uci_M3), method = "glm", method.args = list(family = "binomial"), linetype = "dotted", colour = "red", se = F) +
  geom_smooth(aes(x = AbsSlopeDiffCCImp, y = lci_M3), method = "glm", method.args = list(family = "binomial"), linetype = "dotted", colour = "red", se = F) +
  coord_cartesian(xlim = c(0,1))+
  theme_classic() +
  theme(
    text = element_text(size=35)
  ) +
  labs(x = "Absolute difference between imputed and complete-case slope", y = "Probability of difference between imputed and true trend  ", "\u00B1"," 95% confidence intervals", sep = "")
print(p)
dev.off()


