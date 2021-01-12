Ribbon = CIPredSummarisedImputationErrors %>%
  group_by(Missing, ImputationApproach, FunctionName, FunctionReName, Trend) %>%
  summarise(pred = mean(pred), Max = mean(Max), Min = mean(Min))

Ribbon = subset(Ribbon, (ImputationApproach == "Rphylopars" | ImputationApproach == "Mice: mean matching + phylogeny" | ImputationApproach == "Mice: regression + phylogeny" | ImputationApproach == "Mice: random forest + phylogeny" | ImputationApproach == "BHPMF + phylogeny") & Trend == "PositiveTrend")

Ribbon$Min = 10^(Ribbon$Min)
Ribbon$Max = 10^(Ribbon$Max)
Ribbon$pred = 10^(Ribbon$pred)
Ribbon$Missing = Ribbon$Missing*100



NoBias = ggplot() + 
  geom_ribbon(data = Ribbon[which(Ribbon$FunctionReName == "Random"),],
              aes(ymin = Min, ymax = Max, x = Missing, fill = ImputationApproach, linetype = ImputationApproach), alpha = 0.3) +
  geom_line(data = Ribbon[which(Ribbon$FunctionReName == "Random"),],
            aes(y = (pred), x = Missing, linetype = ImputationApproach, colour = ImputationApproach),size = 1) +
  facet_grid(.~FunctionReName, scale="free", space = "free_y") +
  scale_fill_manual(values = c("red2", "blue", "blue", "blue", "cyan3")) +
  scale_colour_manual(values = c("black","black","black","black", "black")) +
  scale_linetype_manual(values = c("solid", "solid", "dotted", "dashed", "solid")) +
  scale_x_continuous(breaks = c(0, 20, 40, 60)) +
  coord_cartesian(xlim = c(0,80), ylim = c(0,5)) +
  theme_bw() +
  guides(colour=FALSE) +
  theme(
    text = element_text(size=35),
    panel.grid.major = element_blank(),
    panel.grid.minor.x  = element_blank(),
    plot.title = element_text(face="bold.italic"),
    axis.title.y = element_text(face="bold"),
    legend.title=element_blank(),
    legend.spacing.x = unit(1, 'cm'),
    panel.background = element_rect(fill = NA, color = "black"),
    strip.background = element_rect(fill= "light grey", colour="black")) +
  labs(title = "No bias", x = " ", y = " ")

SevereBias = ggplot() + 
  geom_ribbon(data = Ribbon[which(Ribbon$FunctionReName == "Trait  " | Ribbon$FunctionReName == "Phylogeny  " | Ribbon$FunctionReName == "Response " | Ribbon$FunctionReName == "Trait*Response "),],
              aes(ymin = Min, ymax = Max, x = Missing, fill = ImputationApproach, linetype = ImputationApproach), alpha = 0.3) +
  geom_line(data = Ribbon[which(Ribbon$FunctionReName == "Trait  " | Ribbon$FunctionReName == "Phylogeny  " | Ribbon$FunctionReName == "Response " | Ribbon$FunctionReName == "Trait*Response "),],
            aes(y = pred, x = Missing, linetype = ImputationApproach, colour = ImputationApproach),size = 1) +
  facet_grid(.~FunctionReName, scale="free", space = "free_y") +
  scale_fill_manual(values = c("red2", "blue", "blue", "blue", "cyan3")) +
  scale_colour_manual(values = c("black","black","black","black", "black")) +
  scale_linetype_manual(values = c("solid", "solid", "dotted", "dashed", "solid")) +
  scale_x_continuous(breaks = c(0, 20, 40, 60)) +
  coord_cartesian(xlim = c(0,80), ylim = c(0,5)) +
  theme_bw() +
  guides(colour=FALSE) + 
  theme(
    text = element_text(size=35),
    panel.grid.major = element_blank(),
    panel.grid.minor.x  = element_blank(),
    plot.title = element_text(face="bold.italic"),
    axis.title.x = element_text(hjust = 0.2),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    legend.title=element_blank(),
    legend.spacing.x = unit(1, 'cm'),
    panel.background = element_rect(fill = NA, color = "black"),
    strip.background = element_rect(fill=NA, colour="black")) +
  labs(title = "Severe bias", x = "Missing data (%)")

  
Temp = subset(Ribbon, (FunctionName == "WBP" | FunctionName == "WBTP" | FunctionName == "WBTPR" | FunctionName == "WBTR"))
Temp = Temp %>%
  group_by(Missing, ImputationApproach) %>%
  summarise(pred = mean(pred), Max = mean(Max), Min = mean(Min))
Temp$TempFacet = "All weak bias types"

WeakBias = ggplot() + 
  geom_ribbon(data = Temp, aes(ymin = Min, ymax = Max, x = Missing, fill = ImputationApproach, linetype = ImputationApproach), alpha = 0.3) +
  geom_line(data = Temp,  aes(y = pred, x = Missing, linetype = ImputationApproach, colour = ImputationApproach),size = 1) +
  facet_grid(.~TempFacet, scale="free", space = "free_y") +
  scale_fill_manual(values = c("red2", "blue", "blue", "blue", "cyan3")) +
  scale_colour_manual(values = c("black","black","black","black", "black")) +
  scale_linetype_manual(values = c("solid", "solid", "dotted", "dashed", "solid")) +
  scale_x_continuous(breaks = c(0, 20, 40, 60)) +
  coord_cartesian(xlim = c(0,80), ylim = c(0,5)) +
  theme_bw() +
  guides(colour=FALSE) +
  theme(
    text = element_text(size=35),
    panel.grid.major = element_blank(),
    panel.grid.minor.x  = element_blank(),
    plot.title = element_text(face="bold.italic"),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    legend.title=element_blank(),
    legend.spacing.x = unit(1, 'cm'),
    panel.background = element_rect(fill = NA, color = "black"),
    strip.background = element_rect(fill= "light grey", colour="black")) +
  labs(title = "Weak bias", x = " ")

Ribbon = CIPredSummarisedImputationErrors %>%
  group_by(Missing, ImputationApproach, FunctionName, FunctionReName, Trend) %>%
  summarise(pred = mean(pred), Max = mean(Max), Min = mean(Min))

Ribbon = subset(Ribbon, (ImputationApproach == "Rphylopars" | ImputationApproach == "Mice: mean matching + phylogeny" | ImputationApproach == "Mice: regression + phylogeny" | ImputationApproach == "Mice: random forest + phylogeny" | ImputationApproach == "BHPMF + phylogeny") & Trend == "NoTrend")

Ribbon$Min = 10^(Ribbon$Min)
Ribbon$Max = 10^(Ribbon$Max)
Ribbon$pred = 10^(Ribbon$pred)
Ribbon$Missing = Ribbon$Missing*100



NoBias2 = ggplot() + 
  geom_ribbon(data = Ribbon[which(Ribbon$FunctionReName == "Random"),],
              aes(ymin = Min, ymax = Max, x = Missing, fill = ImputationApproach, linetype = ImputationApproach), alpha = 0.3) +
  geom_line(data = Ribbon[which(Ribbon$FunctionReName == "Random"),],
            aes(y = (pred), x = Missing, linetype = ImputationApproach, colour = ImputationApproach),size = 1) +
  facet_grid(.~FunctionReName, scale="free", space = "free_y") +
  scale_fill_manual(values = c("red2", "blue", "blue", "blue", "cyan3")) +
  scale_colour_manual(values = c("black","black","black","black", "black")) +
  scale_linetype_manual(values = c("solid", "solid", "dotted", "dashed", "solid")) +
  scale_x_continuous(breaks = c(0, 20, 40, 60)) +
  coord_cartesian(xlim = c(0,80), ylim = c(0,5)) +
  theme_bw() +
  guides(colour=FALSE) +
  theme(
    text = element_text(size=35),
    panel.grid.major = element_blank(),
    panel.grid.minor.x  = element_blank(),
    plot.title = element_text(face="bold.italic"),
    axis.title.y = element_text(face="bold"),
    legend.title=element_blank(),
    legend.spacing.x = unit(1, 'cm'),
    panel.background = element_rect(fill = NA, color = "black"),
    strip.background = element_rect(fill= "light grey", colour="black")) +
  labs(title = "No bias", x = " ", y = " ")

SevereBias2 = ggplot() + 
  geom_ribbon(data = Ribbon[which(Ribbon$FunctionReName == "Trait  " | Ribbon$FunctionReName == "Phylogeny  " | Ribbon$FunctionReName == "Response " | Ribbon$FunctionReName == "Trait*Response "),],
              aes(ymin = Min, ymax = Max, x = Missing, fill = ImputationApproach, linetype = ImputationApproach), alpha = 0.3) +
  geom_line(data = Ribbon[which(Ribbon$FunctionReName == "Trait  " | Ribbon$FunctionReName == "Phylogeny  " | Ribbon$FunctionReName == "Response " | Ribbon$FunctionReName == "Trait*Response "),],
            aes(y = pred, x = Missing, linetype = ImputationApproach, colour = ImputationApproach),size = 1) +
  facet_grid(.~FunctionReName, scale="free", space = "free_y") +
  scale_fill_manual(values = c("red2", "blue", "blue", "blue", "cyan3")) +
  scale_colour_manual(values = c("black","black","black","black", "black")) +
  scale_linetype_manual(values = c("solid", "solid", "dotted", "dashed", "solid")) +
  scale_x_continuous(breaks = c(0, 20, 40, 60)) +
  coord_cartesian(xlim = c(0,80), ylim = c(0,5)) +
  theme_bw() +
  guides(colour=FALSE) + 
  theme(
    text = element_text(size=35),
    panel.grid.major = element_blank(),
    panel.grid.minor.x  = element_blank(),
    plot.title = element_text(face="bold.italic"),
    axis.title.x = element_text(hjust = 0.2),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    legend.title=element_blank(),
    legend.spacing.x = unit(1, 'cm'),
    panel.background = element_rect(fill = NA, color = "black"),
    strip.background = element_rect(fill=NA, colour="black")) +
  labs(title = "Severe bias", x = "Missing data (%)")


Temp = subset(Ribbon, (FunctionName == "WBP" | FunctionName == "WBTP" | FunctionName == "WBTPR" | FunctionName == "WBTR"))
Temp = Temp %>%
  group_by(Missing, ImputationApproach) %>%
  summarise(pred = mean(pred), Max = mean(Max), Min = mean(Min))
Temp$TempFacet = "All weak bias types"

WeakBias2 = ggplot() + 
  geom_ribbon(data = Temp, aes(ymin = Min, ymax = Max, x = Missing, fill = ImputationApproach, linetype = ImputationApproach), alpha = 0.3) +
  geom_line(data = Temp,  aes(y = pred, x = Missing, linetype = ImputationApproach, colour = ImputationApproach),size = 1) +
  facet_grid(.~TempFacet, scale="free", space = "free_y") +
  scale_fill_manual(values = c("red2", "blue", "blue", "blue", "cyan3")) +
  scale_colour_manual(values = c("black","black","black","black", "black")) +
  scale_linetype_manual(values = c("solid", "solid", "dotted", "dashed", "solid")) +
  scale_x_continuous(breaks = c(0, 20, 40, 60)) +
  coord_cartesian(xlim = c(0,80), ylim = c(0,5)) +
  theme_bw() +
  guides(colour=FALSE) +
  theme(
    text = element_text(size=35),
    panel.grid.major = element_blank(),
    panel.grid.minor.x  = element_blank(),
    plot.title = element_text(face="bold.italic"),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    legend.title=element_blank(),
    legend.spacing.x = unit(1, 'cm'),
    panel.background = element_rect(fill = NA, color = "black"),
    strip.background = element_rect(fill= "light grey", colour="black")) +
  labs(title = "Weak bias", x = " ")



Plot = ggarrange(NoBias, WeakBias, SevereBias, NoBias2, WeakBias2, SevereBias2, ncol= 3, nrow = 2, common.legend = TRUE, legend="bottom", widths = c(1.2,1,4))
Plot = annotate_figure(Plot,
                left = text_grob(paste("Imputation error ", "\u00B1", " 95% confidence intervals", sep = ""), color = "black", rot = 90, size = 35))
ggsave("../Results/SummaryPlots/ImputationErrorVsMissingVsBiasRefined_SplitSlope.png" ,plot = last_plot(), width = 70, height = 40, units = "cm")
