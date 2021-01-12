CombinedRegressionErrors$Diff_t = abs(Factor2Num(CombinedRegressionErrors$Estimate) - Factor2Num(CombinedRegressionErrors$TraitParameterEstimate))/(sqrt((Factor2Num(CombinedRegressionErrors$SE))^2 + (Factor2Num(CombinedRegressionErrors$TraitSE))^2))

tmp4 = CombinedRegressionErrors

tmp4$ImputationApproach = factor(tmp4$ImputationApproach, levels = c("Complete-case", "Rphylopars", "Mice: regression", "Mice: regression + phylogeny", "Mice: mean matching", "Mice: mean matching + phylogeny", "Mice: random forest", "Mice: random forest + phylogeny", "BHPMF", "BHPMF + phylogeny"))

levels(tmp4$ImputationApproach)[levels(tmp4$ImputationApproach)=="Mice: regression + phylogeny"] <- "Mice: regression + 
phylogeny"
levels(tmp4$ImputationApproach)[levels(tmp4$ImputationApproach)=="Mice: mean matching + phylogeny"] <- "Mice: mean matching + 
phylogeny"
levels(tmp4$ImputationApproach)[levels(tmp4$ImputationApproach)=="Mice: random forest + phylogeny"] <- "Mice: random forest + 
phylogeny"
levels(tmp4$ImputationApproach)[levels(tmp4$ImputationApproach)=="BHPMF + phylogeny"] <- "BHPMF + 
phylogeny"



ggplot(tmp4[which(tmp4$Trend == "PositiveTrend"),]) +
  geom_point(aes(x = Missing*100, y = Diff_t), alpha = 0.01) +
  facet_grid(BiasLevel ~ ImputationApproach) +
  geom_hline(aes(yintercept = 1.96), colour = "red") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size=14),
        panel.grid.minor =   element_blank(),
        panel.grid.major =   element_blank()) +
  labs(x = "Missing (%)", y = "Absolute slope t-statistic (true vs. imputed)")
ggsave(plot = last_plot(), 
       "../Results/SummaryPlots/TStat_MissingVsBiasVsImp_Positive.png",
       width = 18,
       height = 9,
       units = "in")

ggplot(tmp4[which(tmp4$Trend == "NoTrend"),]) +
  geom_point(aes(x = Missing*100, y = Diff_t), alpha = 0.01) +
  facet_grid(BiasLevel ~ ImputationApproach) +
  geom_hline(aes(yintercept = 1.96), colour = "red") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size=14),
        panel.grid.minor =   element_blank(),
        panel.grid.major =   element_blank()) +
  labs(x = "Missing data (%)", y = "Absolute slope t-statistic (true vs. imputed)")
ggsave(plot = last_plot(), 
       "../Results/SummaryPlots/TStat_MissingVsBiasVsImp_NoTrend.png",
       width = 18,
       height = 9,
       units = "in")

