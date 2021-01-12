CIPredSummarisedImputationErrors_ResponseQ = PredSummarisedImputationErrors %>%
  dplyr::group_by(ImputationApproach, Trend, ResponsePresent) %>%
  dplyr::summarise(Mean = mean(pred), Min = mean(lci), Max = mean(uci))

ggplot(CIPredSummarisedImputationErrors_ResponseQ) +
  geom_point(aes(x = ImputationApproach, y = 10^(Mean), colour = ResponsePresent)) +
  geom_errorbar(aes(x = ImputationApproach, ymin = 10^(Min), ymax = 10^(Max)))+
  facet_grid(Trend~.) +
  coord_flip()


CIPredSummarisedImputationErrors_ResponseQ$Sig = (ifelse((CIPredSummarisedImputationErrors_ResponseQ$Trend == "NoTrend" & CIPredSummarisedImputationErrors_ResponseQ$ImputationApproach == "Mice: regression + phylogeny"), "Not-significant", "Significant effect"))


ggplot(CIPredSummarisedImputationErrors_ResponseQ[which((CIPredSummarisedImputationErrors_ResponseQ$ImputationApproach == "Rphylopars" | CIPredSummarisedImputationErrors_ResponseQ$ImputationApproach == "Mice: mean matching + phylogeny" | CIPredSummarisedImputationErrors_ResponseQ$ImputationApproach == "Mice: regression + phylogeny" | CIPredSummarisedImputationErrors_ResponseQ$ImputationApproach == "Mice: random forest + phylogeny" | CIPredSummarisedImputationErrors_ResponseQ$ImputationApproach == "BHPMF + phylogeny")),]) +
  geom_path(aes(x= ImputationApproach, y = 10^(Mean), colour = paste(Trend, Sig)), arrow=arrow(length=unit(0.4,"cm")), size=1.5) +
  coord_flip() +
  labs(x = "Imputation approach", y = "Imputation error") +
  facet_grid(Trend~.) +
  scale_colour_manual(values = c( "dark grey", "black", "red2"), name = " ", labels = c("Increased error (not significant)", "Significantly increased error", "Significantly decreased error")) +
  theme(
    text = element_text(size=20),
    panel.grid.major = element_blank(),
    panel.grid.minor.x  = element_blank(),
    panel.background = element_rect(fill = NA, color = "black"),
    legend.position="top"
  )

ggsave("../Results/SummaryPlots/ImputationErrorResponseVsImpApproach.png" ,plot = last_plot(), width = 40, height = 15, units = "cm")

