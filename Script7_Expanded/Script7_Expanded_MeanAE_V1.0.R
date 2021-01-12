SummarisedImputationErrors_MAE = SummarisedImputationErrors %>%
  group_by(ImputationApproach, FunctionReName, Missing) %>%
  summarise(MeanAE = mean(MeanAE, na.rm = T))


Ribbon =  SummarisedImputationErrors_MAE
Ribbon = subset(Ribbon, (ImputationApproach == "Rphylopars" | ImputationApproach == "Mice: mean matching + phylogeny" | ImputationApproach == "Mice: regression + phylogeny" | ImputationApproach == "Mice: random forest + phylogeny" | ImputationApproach == "BHPMF + phylogeny"))
Ribbon$Missing = Ribbon$Missing*100

Ribbon$ImputationApproach = factor(Ribbon$ImputationApproach, levels = c("Rphylopars", "Mice: regression + phylogeny",  "Mice: mean matching + phylogeny","Mice: random forest + phylogeny", "BHPMF + phylogeny"))

NoBias = ggplot() + 
  geom_smooth(data = Ribbon[which(Ribbon$FunctionReName == "Random"),],
              aes(y = MeanAE, x = Missing, fill = ImputationApproach, linetype = ImputationApproach), alpha = 0.3, method = "lm", colour = "black") +
  facet_grid(.~FunctionReName, scale="free", space = "free_y") +
  scale_fill_manual(values = c("red2", "blue", "blue", "blue", "cyan3")) +
  #scale_colour_manual(values = c("black","black","black","black", "black")) +
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
  labs(title = "No bias", x = " ", y = "Mean absolute error")

ControlledBias = ggplot() + 
  geom_smooth(data = Ribbon[which(Ribbon$FunctionReName == "Trait" | Ribbon$FunctionReName == "Phylogeny"),],
              aes(y = MeanAE, x = Missing, fill = ImputationApproach, linetype = ImputationApproach), alpha = 0.3, method = "lm", colour = "black") +
  facet_grid(.~FunctionReName, scale="free", space = "free_y") +
  scale_fill_manual(values = c("red2", "blue", "blue", "blue", "cyan3")) +
  #scale_colour_manual(values = c("black","black","black","black", "black")) +
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
    strip.background = element_rect(fill=NA, colour="black")) +
  labs(title = "Controlled bias", x = " ")


WeakBias = ggplot() + 
  geom_smooth(data = Ribbon[which(Ribbon$FunctionReName == "Trait " | Ribbon$FunctionReName == "Phylogeny " | Ribbon$FunctionReName == "Response" | Ribbon$FunctionReName == "Trait*Response"),],
              aes(y = MeanAE, x = Missing, fill = ImputationApproach, linetype = ImputationApproach), alpha = 0.3, method = "lm", colour = "black") +
  facet_grid(.~FunctionReName, scale="free", space = "free_y") +
  scale_fill_manual(values = c("red2", "blue", "blue", "blue", "cyan3")) +
  #scale_colour_manual(values = c("black","black","black","black", "black")) +
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
              aes(y = MeanAE, x = Missing, fill = ImputationApproach, linetype = ImputationApproach), alpha = 0.3, method = "lm", colour = "black") +
  facet_grid(.~FunctionReName, scale="free", space = "free_y") +
  scale_fill_manual(values = c("red2", "blue", "blue", "blue", "cyan3")) +
  #scale_colour_manual(values = c("black","black","black","black", "black")) +
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
    strip.background = element_rect(fill=NA, colour="black")) +
  labs(title = "Severe bias", x = " ")

Plot = ggarrange(NoBias, ControlledBias, WeakBias, SevereBias, ncol= 4, common.legend = TRUE, legend="bottom", widths = c(1.5,2,4,4))
Plot = annotate_figure(Plot,
                       top = text_grob("", color = "black", size = 50, hjust = 22))

ggsave("../Results/SummaryPlots/MAEVsMissingVsBias.png" ,plot = last_plot(), width = 90, height = 45, units = "cm")



