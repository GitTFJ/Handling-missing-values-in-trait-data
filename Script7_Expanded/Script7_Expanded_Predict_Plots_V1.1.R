PredictErrorsDF$pred_M1 = 10^(PredictErrorsDF$pred_M1)
PredictErrorsDF$uci_M1 = 10^(PredictErrorsDF$uci_M1)
PredictErrorsDF$lci_M1 = 10^(PredictErrorsDF$lci_M1)


fnsz = 17
p1 = ggplot(PredictErrorsDF) +
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
    text = element_text(size=fnsz)
  ) +
  labs(title = 
         "a)",x = " ", y = paste(
"Imputation error " , "\u00B1"," 95%
confidence intervals", sep = ""))



p2 = ggplot(PredictErrorsDF) +
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
    text = element_text(size=fnsz)
  ) +
  labs(title = 
         "b)",x = " ", y = " ")



p3 = ggplot(PredictErrorsDF) +
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
    text = element_text(size=fnsz)
  ) +
  labs(title = 
         "c)",x = " ", y = " ")



p4 = ggplot(PredictErrorsDF) +
  geom_point(aes(x = MeanDiff, y = (pred_M2)^2), alpha = 0.05) +
  geom_smooth(aes(x = MeanDiff, y = (pred_M2)^2), linetype = "solid", colour = "black", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  geom_smooth(aes(x = MeanDiff, y = (uci_M2)^2), linetype = "dotted", colour = "red", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  geom_smooth(aes(x = MeanDiff, y = (lci_M2)^2), linetype = "dotted", colour = "red", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  scale_x_log10(breaks = c(0.01, 0.1, 1, 10, 100), label = c(0.01, 0.1, 1, 10, 100)) +
  coord_cartesian(xlim = c(0.01, 100), ylim = c(0,0.5)) +
  theme_classic() +
  theme(
    text = element_text(size=fnsz)
  ) +
  labs(title = 
         "d)",x = " ", y = paste(
"Slope error " , "\u00B1"," 95% 
confidence intervals", sep = ""))





p5 = ggplot(PredictErrorsDF) +
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
    text = element_text(size=fnsz)
  ) +
  labs(title = 
         "e)",x = " ", y = " ")



p6 = ggplot(PredictErrorsDF) +
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
    text = element_text(size=fnsz)
  ) +
  labs(title = 
         "f)",x = " ", y = " ")




p7 = ggplot(PredictErrorsDF) +
  geom_point(aes(x = AbsSlopeDiffCCImp, y = (pred_M2)^2), alpha = 0.05) +
  geom_smooth(aes(x = AbsSlopeDiffCCImp, y = (pred_M2)^2), linetype = "solid", colour = "black", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  geom_smooth(aes(x = AbsSlopeDiffCCImp, y = (uci_M2)^2), linetype = "dotted", colour = "red", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  geom_smooth(aes(x = AbsSlopeDiffCCImp, y = (lci_M2)^2), linetype = "dotted", colour = "red", se = F, method = "glm", formula = y~x,
              method.args = list(family = gaussian(link = 'log'))) +
  scale_x_sqrt(breaks = c(0, 0.1, 0.25, 0.5, 1), labels = c(0, 0.1, 0.25, 0.5, 1), limits = c(0,1)) +
  coord_cartesian(ylim = c(0,0.5), xlim = c(0,1)) +
  theme_classic() +
  theme(
    text = element_text(size=fnsz)
  ) +
  labs(title = 
         "g)",x = " ", y = " ")



p8 = ggplot(PredictErrorsDF) + 
  geom_smooth(aes(x = MeanDiff, y = pred_M3), method = "glm", method.args = list(family = "binomial"), colour = "black", se = F) +
  geom_smooth(aes(x = MeanDiff, y = uci_M3), method = "glm", method.args = list(family = "binomial"), linetype = "dotted", colour = "red", se = F) +
  geom_smooth(aes(x = MeanDiff, y = lci_M3), method = "glm", method.args = list(family = "binomial"), linetype = "dotted", colour = "red", se = F) +
  scale_x_log10(breaks = c(0.01, 0.1, 1, 10, 100), label = c(0.01, 0.1, 1, 10, 100), limits = c(0.01, 100)) +
  ylim(0,1) +
  theme_classic() +
  theme(
    text = element_text(size=fnsz)
  ) +
  labs(title = 
         "h)", x = 
"Change in mean before 
and after imputation (%)", y = paste(
"Probability of difference between imputed and true
trend  ", "\u00B1"," 95% confidence intervals", sep = ""))


p9 = ggplot(PredictErrorsDF) + 
  geom_smooth(aes(x = (Missing*100), y = pred_M3), method = "glm", method.args = list(family = "binomial"), colour = "black", se = F) +
  geom_smooth(aes(x = (Missing*100), y = uci_M3), method = "glm", method.args = list(family = "binomial"), linetype = "dotted", colour = "red", se = F) +
  geom_smooth(aes(x = (Missing*100), y = lci_M3), method = "glm", method.args = list(family = "binomial"), linetype = "dotted", colour = "red", se = F) +
  ylim(0,1) +
  theme_classic() +
  theme(
    text = element_text(size=fnsz)
  ) +
  labs(title = 
         "i)", x = "Missing data (%)", y = " ")



p10 = ggplot(PredictErrorsDF) + 
  geom_smooth(aes(x = PhyloClustering, y = pred_M3), method = "glm", method.args = list(family = "binomial"), colour = "black", se = F) +
  geom_smooth(aes(x = PhyloClustering, y = uci_M3), method = "glm", method.args = list(family = "binomial"), linetype = "dotted", colour = "red", se = F) +
  geom_smooth(aes(x = PhyloClustering, y = lci_M3), method = "glm", method.args = list(family = "binomial"), linetype = "dotted", colour = "red", se = F) +
  ylim(0,1) +
  scale_x_reverse() +
  theme_classic() +
  theme(
    text = element_text(size=fnsz)
  ) +
  labs(title = 
         "j)", x = 
"Phylogenetic clustering 
(evenly distibuted - clustered)", y = " ")





p11 = ggplot(PredictErrorsDF) + 
  geom_smooth(aes(x = AbsSlopeDiffCCImp, y = pred_M3), method = "glm", method.args = list(family = "binomial"), colour = "black", se = F) +
  geom_smooth(aes(x = AbsSlopeDiffCCImp, y = uci_M3), method = "glm", method.args = list(family = "binomial"), linetype = "dotted", colour = "red", se = F) +
  geom_smooth(aes(x = AbsSlopeDiffCCImp, y = lci_M3), method = "glm", method.args = list(family = "binomial"), linetype = "dotted", colour = "red", se = F) +
  coord_cartesian(xlim = c(0,1))+
  scale_x_sqrt(breaks = c(0, 0.1, 0.25, 0.5, 1), labels = c(0, 0.1, 0.25, 0.5, 1), limits = c(0,1)) +
  theme_classic() +
  theme(
    text = element_text(size=fnsz)
  ) +
  labs(title = 
         "k)", x = 
"Absolute difference between 
imputed and complete-case slope", y = " ")

p = ggarrange(p1, p2, p3, NA, p4, p5, p6, p7, p8, p9, p10, p11, ncol = 4, nrow = 3)
ggsave(plot = p, 
       "../Results/SummaryPlots/Predict.png",
       width = 20,
       height = 15,
       units = "in")
