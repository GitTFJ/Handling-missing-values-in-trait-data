pdf("../Results/SummaryPlots/Predict_RegressionError_Missingness.pdf", width = 20, height = 20)
p = ggplot(PredictErrorsDF) +
  geom_point(aes(x = Missing*100, y = (pred_M2)^2), alpha = 0.05) +
  geom_smooth(aes(x = Missing*100, y = (pred_M2)^2), linetype = "solid", colour = "black", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  geom_smooth(aes(x = Missing*100, y = (uci_M2)^2), linetype = "dotted", colour = "red", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  geom_smooth(aes(x = Missing*100, y = (lci_M2)^2), linetype = "dotted", colour = "red", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  coord_cartesian(ylim = c(0,0.5)) +
  theme_classic() +
  theme(
    text = element_text(size=35)
  ) +
  labs(x = "Missing data (%)", y = paste("Slope error " , "\u00B1"," 95% confidence intervals", sep = ""))
print(p)
dev.off()

pdf("../Results/SummaryPlots/Predict_RegressionError_PhyloClustering.pdf", width = 20, height = 20)
p = ggplot(PredictErrorsDF) +
  geom_point(aes(x = PhyloClustering, y = (pred_M2)^2), alpha = 0.05) +
  geom_smooth(aes(x = PhyloClustering, y = (pred_M2)^2), linetype = "solid", colour = "black", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  geom_smooth(aes(x = PhyloClustering, y = (uci_M2)^2), linetype = "dotted", colour = "red", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  geom_smooth(aes(x = PhyloClustering, y = (lci_M2)^2), linetype = "dotted", colour = "red", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  scale_x_reverse() +
  coord_cartesian(ylim = c(0,0.5)) +
  theme_classic() +
  theme(
    text = element_text(size=35)
  ) +
  labs(x = "Phylogenetic clustering (evenly distibuted - clustered)", y = paste("Slope error " , "\u00B1"," 95% confidence intervals", sep = ""))
print(p)
dev.off()


pdf("../Results/SummaryPlots/Predict_RegressionError_MeanDiff.pdf", width = 20, height = 20)
p = ggplot(PredictErrorsDF) +
  geom_point(aes(x = MeanDiff, y = (pred_M2)^2), alpha = 0.05) +
  geom_smooth(aes(x = MeanDiff, y = (pred_M2)^2), linetype = "solid", colour = "black", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  geom_smooth(aes(x = MeanDiff, y = (uci_M2)^2), linetype = "dotted", colour = "red", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  geom_smooth(aes(x = MeanDiff, y = (lci_M2)^2), linetype = "dotted", colour = "red", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  scale_x_log10(breaks = c(0.01, 0.1, 1, 10, 100), label = c(0.01, 0.1, 1, 10, 100)) +
  coord_cartesian(xlim = c(0.01, 0.1, 1, 100), ylim = c(0,0.5)) +
  theme_classic() +
  theme(
    text = element_text(size=35)
  ) +
  labs(x = "Change in mean before and after imputation (%)", y = paste("Slope error " , "\u00B1"," 95% confidence intervals", sep = ""))
print(p)
dev.off()


pdf("../Results/SummaryPlots/Predict_RegressionError_SlopeDiff.pdf", width = 20, height = 20)
p = ggplot(PredictErrorsDF) +
  geom_point(aes(x = AbsSlopeDiffCCImp, y = (pred_M2)^2), alpha = 0.05) +
  geom_smooth(aes(x = AbsSlopeDiffCCImp, y = (pred_M2)^2), linetype = "solid", colour = "black", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  geom_smooth(aes(x = AbsSlopeDiffCCImp, y = (uci_M2)^2), linetype = "dotted", colour = "red", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  geom_smooth(aes(x = AbsSlopeDiffCCImp, y = (lci_M2)^2), linetype = "dotted", colour = "red", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  scale_x_sqrt(breaks = c(0, 0.25, 0.5, 1), labels = c(0, 0.25, 0.5, 1)) +
  coord_cartesian(ylim = c(0,0.5), xlim = c(0,2)) +
  theme_classic() +
  theme(
    text = element_text(size=35)
  ) +
  labs(x = "Absolute difference between imputed and complete-case slope", y = paste("Slope error " , "\u00B1"," 95% confidence intervals", sep = ""))
print(p)
dev.off()


