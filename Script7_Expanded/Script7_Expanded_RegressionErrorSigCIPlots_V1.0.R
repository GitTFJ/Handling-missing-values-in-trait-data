tmp = CombinedStartingTrend
tmp$Estimate = Factor2Num(tmp$Estimate)
tmp$EstimateLower = Factor2Num(tmp$Estimate) - 1.648*Factor2Num(tmp$SE)
tmp$EstimateUpper = Factor2Num(tmp$Estimate) + 1.648*Factor2Num(tmp$SE)
tmp$Missing = 0


tmp2 = CombinedRegressionErrors
tmp2$TraitLowerCI = Factor2Num(tmp2$TraitLowerCI)
tmp2$TraitUpperCI = Factor2Num(tmp2$TraitUpperCI)

tmp3 = tmp %>%
  group_by(Trend, Missing) %>%
  dplyr::summarise(MeanCoef = mean(Estimate, na.rm = T), 
                   MeanLower = mean(EstimateLower, na.rm = T), 
                   MeanUpper = mean(EstimateUpper, na.rm = T))

tmp4 = tmp2 %>%
  group_by(Trend, Missing, BiasLevel, FunctionName, ImputationApproach) %>%
  dplyr::summarise(MeanCoef = mean(TraitParameterEstimate, na.rm = T), 
                   MeanLower = mean(TraitLowerCI, na.rm = T), 
                   MeanUpper = mean(TraitUpperCI, na.rm = T))
tmp4 = subset(tmp4, Trend == "PositiveTrend")
tmp4$P= ifelse(((tmp4$MeanLower >  tmp3$MeanUpper[2] & 
                  tmp4$MeanUpper >  tmp3$MeanUpper[2]) | 
                    (tmp4$MeanLower <  tmp3$MeanLower[2] & 
                       tmp4$MeanUpper <  tmp3$MeanLower[2])),
                "Overlap",
                "No-overlap")

tmp4$ImputationApproach = factor(tmp4$ImputationApproach, levels = c("Complete-case", "Rphylopars", "Mice: regression", "Mice: regression + phylogeny", "Mice: mean matching", "Mice: mean matching + phylogeny", "Mice: random forest", "Mice: random forest + phylogeny", "BHPMF", "BHPMF + phylogeny"))

levels(tmp4$ImputationApproach)[levels(tmp4$ImputationApproach)=="Mice: regression + phylogeny"] <- "Mice: regression + 
phylogeny"
levels(tmp4$ImputationApproach)[levels(tmp4$ImputationApproach)=="Mice: mean matching + phylogeny"] <- "Mice: mean matching + 
phylogeny"
levels(tmp4$ImputationApproach)[levels(tmp4$ImputationApproach)=="Mice: random forest + phylogeny"] <- "Mice: random forest + 
phylogeny"
levels(tmp4$ImputationApproach)[levels(tmp4$ImputationApproach)=="BHPMF + phylogeny"] <- "BHPMF + 
phylogeny"

ggplot() +
  geom_point(data = tmp4, 
             aes(x = Missing*100, y = MeanCoef, colour = P), alpha = 0.35) +
  geom_linerange(data = tmp4, 
           aes(x = Missing*100, ymin = MeanLower, ymax = MeanUpper, colour = P), alpha = 0.35) +
  geom_rect(data = tmp3, aes(xmin = 0, xmax = 80, ymin = 0.63, ymax = 0.72), alpha = 0.1) +
  geom_hline(data = tmp3, aes(yintercept = 0), linetype = "dotted") +
  scale_color_manual(values = c("black", "red")) +
  coord_cartesian(ylim = c(-0.6, 1.2)) +
  scale_y_continuous(breaks = c(-0.3, 0, 0.3, 0.6, 0.9)) +
  scale_x_continuous(breaks = c(20, 40, 60, 80)) +
  facet_grid(BiasLevel~ImputationApproach) +
  labs(x = "Missing data (%)", y = "Slope coefficient") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size=14),
        panel.grid.minor =   element_blank(),
        panel.grid.major =   element_blank())
ggsave(plot = last_plot(), "../Results/SummaryPlots/ImputationApproahcVsBias_CI_SlopePositive.png", width = 18, height = 9, units = "in", dpi = 200)



tmp4 = tmp2 %>%
  group_by(Trend, Missing, BiasLevel, FunctionName, ImputationApproach) %>%
  dplyr::summarise(MeanCoef = mean(TraitParameterEstimate, na.rm = T), 
                   MeanLower = mean(TraitLowerCI, na.rm = T), 
                   MeanUpper = mean(TraitUpperCI, na.rm = T))
tmp4 = subset(tmp4, Trend == "NoTrend")
tmp4$P= ifelse(((tmp4$MeanLower >  tmp3$MeanUpper[1] & 
                   tmp4$MeanUpper >  tmp3$MeanUpper[1]) | 
                  (tmp4$MeanLower <  tmp3$MeanLower[1] & 
                     tmp4$MeanUpper <  tmp3$MeanLower[1])),
               "Overlap",
               "No-overlap")

tmp4$ImputationApproach = factor(tmp4$ImputationApproach, levels = c("Complete-case", "Rphylopars", "Mice: regression", "Mice: regression + phylogeny", "Mice: mean matching", "Mice: mean matching + phylogeny", "Mice: random forest", "Mice: random forest + phylogeny", "BHPMF", "BHPMF + phylogeny"))

levels(tmp4$ImputationApproach)[levels(tmp4$ImputationApproach)=="Mice: regression + phylogeny"] <- "Mice: regression + 
phylogeny"
levels(tmp4$ImputationApproach)[levels(tmp4$ImputationApproach)=="Mice: mean matching + phylogeny"] <- "Mice: mean matching + 
phylogeny"
levels(tmp4$ImputationApproach)[levels(tmp4$ImputationApproach)=="Mice: random forest + phylogeny"] <- "Mice: random forest + 
phylogeny"
levels(tmp4$ImputationApproach)[levels(tmp4$ImputationApproach)=="BHPMF + phylogeny"] <- "BHPMF + 
phylogeny"

ggplot() +
  geom_point(data = tmp4, 
             aes(x = Missing*100, y = MeanCoef, colour = P), alpha = 0.35) +
  geom_linerange(data = tmp4, 
                 aes(x = Missing*100, ymin = MeanLower, ymax = MeanUpper, colour = P), alpha = 0.35) +
  geom_rect(data = tmp3, aes(xmin = 0, xmax = 80, ymin = -0.07, ymax = 0.07), alpha = 0.1) +
  scale_color_manual(values = c("black", "red")) +
  scale_x_continuous(breaks = c(20, 40, 60, 80)) +
  facet_grid(BiasLevel~ImputationApproach) +
  labs(x = "Missing data (%)", y = "Slope coefficient") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size=14),
        panel.grid.minor =   element_blank(),
        panel.grid.major =   element_blank())
ggsave(plot = last_plot(), "../Results/SummaryPlots/ImputationApproahcVsBias_CI_SlopeNull.png", width = 18, height = 9, units = "in", dpi = 200)

