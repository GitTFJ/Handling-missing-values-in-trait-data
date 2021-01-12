CIPredCombinedRegressionErrors_ResponseQ = rbind(PredCombinedRegressionErrors_Positive_ResponseQ, PredCombinedRegressionErrors_Null_ResponseQ) %>%
  dplyr::group_by(ImputationApproach, Trend, ResponsePresent) %>%
  dplyr::summarise(Mean = mean(pred), Min = mean(lci), Max = mean(uci))
CIPredCombinedRegressionErrors_ResponseQ$Sig = ifelse(((CIPredCombinedRegressionErrors_ResponseQ$Trend == "PositiveTrend" & CIPredCombinedRegressionErrors_ResponseQ$ImputationApproach == "Rphylopars") | (CIPredCombinedRegressionErrors_ResponseQ$Trend == "PositiveTrend" & CIPredCombinedRegressionErrors_ResponseQ$ImputationApproach == "BHPMF + phylogeny")), "Significantly increased error", "Significantly decreased error")


ggplot(CIPredCombinedRegressionErrors_ResponseQ[which((CIPredCombinedRegressionErrors_ResponseQ$ImputationApproach == "Rphylopars" | CIPredCombinedRegressionErrors_ResponseQ$ImputationApproach == "Mice: mean matching + phylogeny" | CIPredCombinedRegressionErrors_ResponseQ$ImputationApproach == "Mice: regression + phylogeny" | CIPredCombinedRegressionErrors_ResponseQ$ImputationApproach == "Mice: random forest + phylogeny" | CIPredCombinedRegressionErrors_ResponseQ$ImputationApproach == "BHPMF + phylogeny")),]) +
  geom_point(aes(x = ImputationApproach, y = (Mean)^2, colour = ResponsePresent)) +
  geom_errorbar(aes(x = ImputationApproach, ymin = (Min)^2, ymax = (Max)^2))+
  facet_grid(Trend~.) +
  coord_flip()

ggplot(data = CIPredCombinedRegressionErrors_ResponseQ[which((CIPredCombinedRegressionErrors_ResponseQ$ImputationApproach == "Rphylopars" | CIPredCombinedRegressionErrors_ResponseQ$ImputationApproach == "Mice: mean matching + phylogeny" | CIPredCombinedRegressionErrors_ResponseQ$ImputationApproach == "Mice: regression + phylogeny" | CIPredCombinedRegressionErrors_ResponseQ$ImputationApproach == "Mice: random forest + phylogeny" | CIPredCombinedRegressionErrors_ResponseQ$ImputationApproach == "BHPMF + phylogeny")),]) +
  geom_path(aes(x= ImputationApproach, y = (Mean)^2, colour = Sig), arrow=arrow(length=unit(0.4,"cm")), size=1.5) +
  coord_flip() +
  labs(x = "Imputation approach", y = "Slope error") +
  facet_grid(Trend~.) +
  scale_colour_manual(values = c("red2","black"), name = " ") +
  theme(
    text = element_text(size=20),
    panel.grid.major = element_blank(),
    panel.grid.minor.x  = element_blank(),
    panel.background = element_rect(fill = NA, color = "black"),
    legend.position="top"
  )

ggsave("../Results/SummaryPlots/RegressionErrorResponseVsImpApproach.png" ,plot = last_plot(), width = 40, height = 15, units = "cm")

