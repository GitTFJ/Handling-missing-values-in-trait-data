PredictErrorsDF$pred_M1 = 10^(PredictErrorsDF$pred_M1)
PredictErrorsDF$uci_M1 = 10^(PredictErrorsDF$uci_M1)
PredictErrorsDF$lci_M1 = 10^(PredictErrorsDF$lci_M1)


pdf("../Results/SummaryPlots/Predict_ImputationError_Missingness.pdf", width = 20, height = 20)
p = ggplot(PredictErrorsDF) +
  geom_point(aes(x = Missing*100, y = (pred_M1)), alpha = 0.05) +
  geom_smooth(aes(x = Missing*100, y = (pred_M1)), linetype = "solid", colour = "black", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  geom_smooth(aes(x = Missing*100, y = (uci_M1)), linetype = "dotted", colour = "red", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  geom_smooth(aes(x = Missing*100, y = (lci_M1)), linetype = "dotted", colour = "red", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  coord_cartesian(ylim = c(0,4)) +
  theme_classic() +
  theme(
    text = element_text(size=35)
  ) +
  labs(x = "Missing data (%)", y = paste("Imputation error " , "\u00B1"," 95% confidence intervals", sep = ""))
print(p)
dev.off()


pdf("../Results/SummaryPlots/Predict_ImputationError_PhyloClustering.pdf", width = 20, height = 20)
p = ggplot(PredictErrorsDF) +
  geom_point(aes(x = PhyloClustering, y = (pred_M1)), alpha = 0.05) +
  geom_smooth(aes(x = PhyloClustering, y = (pred_M1)), linetype = "solid", colour = "black", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  geom_smooth(aes(x = PhyloClustering, y = (uci_M1)), linetype = "dotted", colour = "red", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  geom_smooth(aes(x = PhyloClustering, y = (lci_M1)), linetype = "dotted", colour = "red", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  scale_x_reverse() +
  coord_cartesian(ylim = c(0,4)) +
  theme_classic() +
  theme(
    text = element_text(size=35)
  ) +
  labs(x = "Phylogenetic clustering (evenly distibuted - clustered)", y = paste("Imputation error " , "\u00B1"," 95% confidence intervals", sep = ""))
print(p)
dev.off()


pdf("../Results/SummaryPlots/Predict_ImputationError_MeanDiff.pdf", width = 20, height = 20)
p = ggplot(PredictErrorsDF) +
  geom_point(aes(x = MeanDiff, y = (pred_M1)), alpha = 0.05) +
  geom_smooth(aes(x = MeanDiff, y = (pred_M1)), linetype = "solid", colour = "black", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  geom_smooth(aes(x = MeanDiff, y = (uci_M1)), linetype = "dotted", colour = "red", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  geom_smooth(aes(x = MeanDiff, y = (lci_M1)), linetype = "dotted", colour = "red", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  scale_x_log10(breaks = c(0.01, 0.1, 1, 10, 100), label = c(0.01, 0.1, 1, 10, 100)) +
  coord_cartesian(xlim = c(0.01, 100), ylim = c(0,4)) +
  theme_classic() +
  theme(
    text = element_text(size=35)
  ) +
  labs(x = "Change in mean before and after imputation (%)", y = paste("Imputation error " , "\u00B1"," 95% confidence intervals", sep = ""))
print(p)
dev.off()