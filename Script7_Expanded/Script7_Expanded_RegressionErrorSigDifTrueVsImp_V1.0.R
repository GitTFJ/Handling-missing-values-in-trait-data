Temp = CombinedRegressionErrors
Temp$AbsDiff2 = abs(Factor2Num(Temp$TraitParameterEstimate) - Factor2Num(Temp$Estimate))
Temp$SigDiffImpvsTrue_t = abs(Factor2Num(Temp$Estimate) - Factor2Num(Temp$TraitParameterEstimate))/(sqrt((Factor2Num(Temp$SE))^2 + (Factor2Num(Temp$TraitSE))^2))
Temp$SigDiffImpvsTrue_p = abs(qt(c(0.025),(500 + 500 - 4)))
Temp$SigDiffImpvsTrue_Sig = ifelse(Temp$SigDiffImpvsTrue_t > Temp$SigDiffImpvsTrue_p, 1, 0)

ggplot(data = Temp[which(Temp$Trend == "NoTrend" & (
  Temp$ImputationApproach == "Complete-case"|
    Temp$ImputationApproach == "Rphylopars"|
    Temp$ImputationApproach == "Mice: regression + phylogeny"|
    Temp$ImputationApproach == "Mice: mean matching + phylogeny"|
    Temp$ImputationApproach == "Mice: random forest + phylogeny"
)),]) +
  geom_point(aes(
    x = Missing * 100, 
    y = SigDiffImpvsTrue_t),
    alpha = 0.02) +
  geom_hline(aes(
    yintercept = 1.96),
    colour = "dark red",
    size = 1.5,
    alpha = 0.4) +
  facet_grid(BiasLevel ~ 
               ImputationApproach) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.title = element_text(face="bold")) +
  labs(x = "Missing (%)",
       y = "True vs. Imputed slope t-statistic")

ggsave("../Results/SummaryPlots/RegressionErrorNoTrendTrueVsImpT.jpg" ,plot = last_plot(), width = 50, height = 30, units = "cm", dpi = 400)


ggplot(data = Temp[which(Temp$Trend == "PositiveTrend" & (
  Temp$ImputationApproach == "Complete-case"|
    Temp$ImputationApproach == "Rphylopars"|
    Temp$ImputationApproach == "Mice: regression + phylogeny"|
    Temp$ImputationApproach == "Mice: mean matching + phylogeny"|
    Temp$ImputationApproach == "Mice: random forest + phylogeny"
)),]) +
  geom_point(aes(
    x = Missing * 100, 
    y = SigDiffImpvsTrue_t),
    alpha = 0.02) +
  geom_hline(aes(
    yintercept = 1.96),
    colour = "dark red",
    size = 1.5,
    alpha = 0.4) +
  facet_grid(BiasLevel ~ 
               ImputationApproach) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.title = element_text(face="bold")) +
  labs(x = "Missing (%)",
       y = "True vs. Imputed slope t-statistic")


ggsave("../Results/SummaryPlots/RegressionErrorPositiveTrueVsImpT.jpg" ,plot = last_plot(), width = 50, height = 30, units = "cm", dpi = 400)
