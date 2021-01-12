library(broom)

R2 = CombinedImputationErrors%>%
  group_by(Seed, CorrelationLevel, ResponsePresent, Trend, FunctionName, Missing, ImputationApproach) %>%
  do(Fit = lm(True ~ Imputed, data = .))
R2 = glance(R2, Fit)

R2$FunctionReName = as.factor(R2$FunctionName)
levels(R2$FunctionReName) = list('Random' = "MCAR", 'Phylogeny' = "MOPP", 'Phylogeny ' = "WBP", 'Phylogeny  ' = "SBP", 'Trait' = "MOPT", 'Trait ' = "WBTP", 'Trait  ' = "SBTP", 'Trait*Response' = "WBTPR", 'Trait*Response ' = "SBTPR", 'Response' = "WBTR", 'Response ' = "SBTR")

R2$BiasLevel = as.factor(R2$FunctionName)
levels(R2$BiasLevel) = list('No Bias' = c("MCAR"), 'Controlled Bias' =  c("MOPP", "MOPT"), 'Weak Bias' = c("WBP", "WBTP", "WBTPR", "WBTR"), 'Severe Bias' = c("SBP", "SBTPR", "SBTP", "SBTR"))

R2 = subset(R2, (ImputationApproach == "Rphylopars" | ImputationApproach == "Mice: mean matching + phylogeny" | ImputationApproach == "Mice: regression + phylogeny" | ImputationApproach == "Mice: random forest + phylogeny" | ImputationApproach == "BHPMF + phylogeny"))
R2$Missing = R2$Missing*100

R2$ImputationApproach = factor(R2$ImputationApproach, levels = c("Rphylopars", "Mice: regression + phylogeny",  "Mice: mean matching + phylogeny","Mice: random forest + phylogeny", "BHPMF + phylogeny"))

Ribbon = R2
NoBias = ggplot() + 
  geom_smooth(data = Ribbon[which(Ribbon$FunctionReName == "Random"),],
              aes(y = r.squared, x = Missing, fill = ImputationApproach, linetype = ImputationApproach), alpha = 0.3, method = "lm", colour = "black") +
  facet_grid(.~FunctionReName, scale="free", space = "free_y") +
  scale_fill_manual(values = c("red2", "blue", "blue", "blue", "cyan3")) +
  #scale_colour_manual(values = c("black","black","black","black", "black")) +
  scale_linetype_manual(values = c("solid", "solid", "dotted", "dashed", "solid")) +
  scale_x_continuous(breaks = c(0, 20, 40, 60)) +
  coord_cartesian(xlim = c(0,80), ylim = c(0,1)) +
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
  labs(title = "No bias", x = " ", y = "R2")

ControlledBias = ggplot() + 
  geom_smooth(data = Ribbon[which(Ribbon$FunctionReName == "Trait" | Ribbon$FunctionReName == "Phylogeny"),],
              aes(y = r.squared, x = Missing, fill = ImputationApproach, linetype = ImputationApproach), alpha = 0.3, method = "lm", colour = "black") +
  facet_grid(.~FunctionReName, scale="free", space = "free_y") +
  scale_fill_manual(values = c("red2", "blue", "blue", "blue", "cyan3")) +
  #scale_colour_manual(values = c("black","black","black","black", "black")) +
  scale_linetype_manual(values = c("solid", "solid", "dotted", "dashed", "solid")) +
  scale_x_continuous(breaks = c(0, 20, 40, 60)) +
  coord_cartesian(xlim = c(0,80), ylim = c(0,1)) +
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
    strip.background = element_rect(fill=NA, colour="black")) +
  labs(title = "Controlled bias", x = " ")


WeakBias = ggplot() + 
  geom_smooth(data = Ribbon[which(Ribbon$FunctionReName == "Trait " | Ribbon$FunctionReName == "Phylogeny " | Ribbon$FunctionReName == "Response" | Ribbon$FunctionReName == "Trait*Response"),],
              aes(y = r.squared, x = Missing, fill = ImputationApproach, linetype = ImputationApproach), alpha = 0.3, method = "lm", colour = "black") +
  facet_grid(.~FunctionReName, scale="free", space = "free_y") +
  scale_fill_manual(values = c("red2", "blue", "blue", "blue", "cyan3")) +
  #scale_colour_manual(values = c("black","black","black","black", "black")) +
  scale_linetype_manual(values = c("solid", "solid", "dotted", "dashed", "solid")) +
  scale_x_continuous(breaks = c(0, 20, 40, 60)) +
  coord_cartesian(xlim = c(0,80), ylim = c(0,1)) +
  theme_bw() +
  guides(colour=FALSE) +
  theme(
    text = element_text(size=35),
    panel.grid.major = element_blank(),
    panel.grid.minor.x  = element_blank(),
    plot.title = element_text(face="bold.italic"),
    axis.title.x = element_text(face="bold"),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    legend.title=element_blank(),
    legend.spacing.x = unit(1, 'cm'),
    panel.background = element_rect(fill = NA, color = "black"),
    strip.background = element_rect(fill= "light grey", colour="black")) +
  labs(title = "Weak bias", x = "Missing data (%)")


SevereBias = ggplot() + 
  geom_smooth(data = Ribbon[which(Ribbon$FunctionReName == "Trait  " | Ribbon$FunctionReName == "Phylogeny  " | Ribbon$FunctionReName == "Response " | Ribbon$FunctionReName == "Trait*Response "),],
              aes(y = r.squared, x = Missing, fill = ImputationApproach, linetype = ImputationApproach), alpha = 0.3, method = "lm", colour = "black") +
  facet_grid(.~FunctionReName, scale="free", space = "free_y") +
  scale_fill_manual(values = c("red2", "blue", "blue", "blue", "cyan3")) +
  #scale_colour_manual(values = c("black","black","black","black", "black")) +
  scale_linetype_manual(values = c("solid", "solid", "dotted", "dashed", "solid")) +
  scale_x_continuous(breaks = c(0, 20, 40, 60)) +
  coord_cartesian(xlim = c(0,80), ylim = c(0,1)) +
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
    strip.background = element_rect(fill=NA, colour="black")) +
  labs(title = "Severe bias", x = " ")

Plot = ggarrange(NoBias, ControlledBias, WeakBias, SevereBias, ncol= 4, common.legend = TRUE, legend="bottom", widths = c(1.5,2,4,4))
Plot = annotate_figure(Plot,
                top = text_grob("", color = "black", size = 50, hjust = 22))
Plot
ggsave("../Results/SummaryPlots/R2VsMissingVsBias.png" ,plot = last_plot(), width = 90, height = 45, units = "cm")

