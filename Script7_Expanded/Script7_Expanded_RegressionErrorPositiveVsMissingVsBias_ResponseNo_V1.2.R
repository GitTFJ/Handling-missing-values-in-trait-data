CIPredCombinedRegressionErrors_Positive_Rsp = PredCombinedRegressionErrors_Positive_ResponseQ %>%
  dplyr::group_by(ImputationApproach, FunctionReName, FunctionName, Missing, ResponsePresent, Trend, CorrelationLevel) %>%
  dplyr::summarise(Min = min(lci), Max = max(uci), pred = mean(pred))

Temp = CIPredCombinedRegressionErrors_Positive_Rsp
Temp = subset(Temp, (ImputationApproach == "Rphylopars" | ImputationApproach == "Mice: mean matching + phylogeny" | ImputationApproach == "Mice: regression + phylogeny" | ImputationApproach == "Mice: random forest + phylogeny" | ImputationApproach == "Complete-case" | ImputationApproach == "BHPMF + phylogeny"))


Ribbon = Temp %>%
  group_by(Missing, ImputationApproach, FunctionName, FunctionReName, ResponsePresent) %>%
  summarise(pred = mean(pred), Max = mean(Max), Min = mean(Min))

cc = PredCombinedRegressionErrors_Positive
cc = subset(cc, ImputationApproach == "Complete-case")
cc = cc %>%
  dplyr::group_by(ImputationApproach, FunctionReName, FunctionName, Missing, ResponsePresent, Trend, CorrelationLevel) %>%
  dplyr::summarise(Min = min(lci), Max = max(uci), pred = mean(pred))

Ribbon_cc = cc %>%
  group_by(Missing, ImputationApproach, FunctionName, FunctionReName, ResponsePresent) %>%
  summarise(pred = mean(pred), Max = mean(Max), Min = mean(Min))

Ribbon = rbind(Ribbon, Ribbon_cc)

Ribbon$Min = (Ribbon$Min)^2
Ribbon$Max = (Ribbon$Max)^2
Ribbon$pred = (Ribbon$pred)^2
Ribbon$Missing = Ribbon$Missing*100


NoBias = ggplot() + 
  geom_ribbon(data = Ribbon[which((Ribbon$FunctionReName == "Random")& Ribbon$ResponsePresent == "No"),],
              aes(ymin = Min, ymax = Max, x = Missing, fill = ImputationApproach, linetype = ImputationApproach), alpha = 0.3) +
  geom_line(data = Ribbon[which((Ribbon$FunctionReName == "Random")& Ribbon$ResponsePresent == "No"),],
            aes(y = (pred), x = Missing, linetype = ImputationApproach, colour = ImputationApproach), size = 1) +
  facet_grid(.~FunctionReName, scale="free", space = "free_y") +
  scale_fill_manual(values = c("black", "red2", "blue", "blue", "blue", "cyan3")) +
  scale_colour_manual(values = c("black","black","black","black","black","black")) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "dotted", "dashed", "solid")) +
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
  labs(title = "No bias", x = " ", y = paste("Slope error " , "\u00B1"," 95% confidence intervals", sep = ""))

ControlledBias = ggplot() + 
  geom_ribbon(data = Ribbon[which((Ribbon$FunctionReName == "Trait" | Ribbon$FunctionReName == "Phylogeny")& Ribbon$ResponsePresent == "No"),],
              aes(ymin = Min, ymax = Max, x = Missing, fill = ImputationApproach, linetype = ImputationApproach), alpha = 0.3) +
  geom_line(data = Ribbon[which((Ribbon$FunctionReName == "Trait" | Ribbon$FunctionReName == "Phylogeny")& Ribbon$ResponsePresent == "No"),],
            aes(y = (pred), x = Missing, linetype = ImputationApproach, colour = ImputationApproach), size = 1) +
  facet_grid(.~FunctionReName, scale="free", space = "free_y") +
  scale_fill_manual(values = c("black", "red2", "blue", "blue", "blue", "cyan3")) +
  scale_colour_manual(values = c("black","black","black","black","black","black")) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "dotted", "dashed", "solid")) +
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
  geom_ribbon(data = Ribbon[which((Ribbon$FunctionReName == "Trait " | Ribbon$FunctionReName == "Phylogeny " | Ribbon$FunctionReName == "Response" | Ribbon$FunctionReName == "Trait*Response")& Ribbon$ResponsePresent == "No"),],
              aes(ymin = Min, ymax = Max, x = Missing, fill = ImputationApproach, linetype = ImputationApproach), alpha = 0.3) +
  geom_line(data = Ribbon[which((Ribbon$FunctionReName == "Trait " | Ribbon$FunctionReName == "Phylogeny " | Ribbon$FunctionReName == "Response" | Ribbon$FunctionReName == "Trait*Response")& Ribbon$ResponsePresent == "No"),],
            aes(y = (pred), x = Missing, linetype = ImputationApproach, colour = ImputationApproach), size = 1) +
  facet_grid(.~FunctionReName, scale="free", space = "free_y") +
  scale_fill_manual(values = c("black", "red2", "blue", "blue", "blue", "cyan3")) +
  scale_colour_manual(values = c("black","black","black","black","black","black")) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "dotted", "dashed", "solid")) +
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
  geom_ribbon(data = Ribbon[which((Ribbon$FunctionReName == "Trait  " | Ribbon$FunctionReName == "Phylogeny  " | Ribbon$FunctionReName == "Response " | Ribbon$FunctionReName == "Trait*Response ")& Ribbon$ResponsePresent == "No"),],
              aes(ymin = Min, ymax = Max, x = Missing, fill = ImputationApproach, linetype = ImputationApproach), alpha = 0.3) +
  geom_line(data = Ribbon[which((Ribbon$FunctionReName == "Trait  " | Ribbon$FunctionReName == "Phylogeny  " | Ribbon$FunctionReName == "Response " | Ribbon$FunctionReName == "Trait*Response ")& Ribbon$ResponsePresent == "No"),],
            aes(y = (pred), x = Missing, linetype = ImputationApproach, colour = ImputationApproach), size = 1) +
  facet_grid(.~FunctionReName, scale="free", space = "free_y") +
  scale_fill_manual(values = c("black", "red2", "blue", "blue", "blue", "cyan3")) +
  scale_colour_manual(values = c("black","black","black","black","black","black")) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "dotted", "dashed", "solid")) +
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

Plot = ggarrange(NoBias, ControlledBias, WeakBias, SevereBias, ncol= 4, common.legend = TRUE, legend="bottom", widths = c(1.2,2,4,4))
ggsave("../Results/SummaryPlots/RegressionErrorPositiveVsMissingVsBias_ResponseNo.jpeg" ,plot = Plot, width = 90, height = 45, units = "cm")