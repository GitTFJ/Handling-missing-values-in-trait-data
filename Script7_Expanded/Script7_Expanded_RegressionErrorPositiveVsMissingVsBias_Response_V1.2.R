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

plt_a = ggplot(Ribbon[which(Ribbon$ResponsePresent == "Yes"),]) +
  geom_ribbon(aes(x = Missing, ymin = Min, ymax = Max, fill= ImputationApproach), alpha = 0.3) +
  geom_line(aes(x = Missing, y = pred, colour = ImputationApproach, linetype = ImputationApproach)) +
  facet_grid(.~FunctionName) +
  scale_fill_manual(values = c("black", "red2", "blue", "blue", "blue", "cyan3")) +
  scale_colour_manual(values = c("black","black","black","black","black","black")) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "dotted", "dashed", "solid")) +
  scale_x_continuous(breaks = c(0, 20, 40, 60)) +
  coord_cartesian(xlim = c(0,80), ylim = c(0,1)) +
  guides(colour=FALSE)+
  labs(x = " ", y = " ") +
  theme_classic() +
  theme(
    text = element_text(size=35),
    panel.grid.major = element_blank(),
    panel.grid.minor.x  = element_blank(),
    plot.title = element_text(face="bold.italic"),
    axis.title.y = element_text(face="bold"),
    legend.title=element_blank(),
    legend.position = "bottom",
    panel.background = element_rect(fill = NA, color = "black"),
    strip.background = element_rect(fill= "light grey", colour="black")) 


plt_b = ggplot(Ribbon[which(Ribbon$ResponsePresent == "No"),]) +
  geom_ribbon(aes(x = Missing, ymin = Min, ymax = Max, fill= ImputationApproach), alpha = 0.3) +
  geom_line(aes(x = Missing, y = pred, colour = ImputationApproach, linetype = ImputationApproach)) +
  facet_grid(.~FunctionName) +
  scale_fill_manual(values = c("black", "red2", "blue", "blue", "blue", "cyan3")) +
  scale_colour_manual(values = c("black","black","black","black","black","black")) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "dotted", "dashed", "solid")) +
  scale_x_continuous(breaks = c(0, 20, 40, 60)) +
  coord_cartesian(xlim = c(0,80), ylim = c(0,1)) +
  guides(colour=FALSE) +
  labs(x = "Missing (%)", y = paste("Slope error " , "\u00B1"," 95% confidence intervals", sep = "")) +
  theme_classic() +
  theme(
    text = element_text(size=35),
    panel.grid.major = element_blank(),
    panel.grid.minor.x  = element_blank(),
    plot.title = element_text(face="bold.italic"),
    axis.title.y = element_text(face="bold", hjust = -0.5),
    legend.title=element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = NA, color = "black"),
    strip.background = element_rect(fill= "light grey", colour="black"))

Plot = ggarrange(plt_a, plt_b, ncol= 1, common.legend = TRUE, legend="bottom")
ggsave("../Results/SummaryPlots/RegressionErrorPositiveVsMissingVsBias_Response.jpeg" ,plot = Plot, width = 70, height = 35, units = "cm")

CIPredCombinedRegressionErrors_Null_Rsp = PredCombinedRegressionErrors_Null_ResponseQ %>%
  dplyr::group_by(ImputationApproach, FunctionReName, FunctionName, Missing, ResponsePresent, Trend, CorrelationLevel) %>%
  dplyr::summarise(Min = min(lci), Max = max(uci), pred = mean(pred))

Temp = CIPredCombinedRegressionErrors_Null_Rsp
Temp = subset(Temp, (ImputationApproach == "Rphylopars" | ImputationApproach == "Mice: mean matching + phylogeny" | ImputationApproach == "Mice: regression + phylogeny" | ImputationApproach == "Mice: random forest + phylogeny" | ImputationApproach == "Complete-case" | ImputationApproach == "BHPMF + phylogeny"))


Ribbon = Temp %>%
  group_by(Missing, ImputationApproach, FunctionName, FunctionReName, ResponsePresent) %>%
  summarise(pred = mean(pred), Max = mean(Max), Min = mean(Min))

cc = PredCombinedRegressionErrors_Null
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

ggplot(Ribbon[which(Ribbon$ResponsePresent == "Yes"),]) +
  geom_ribbon(aes(x = Missing, ymin = Min, ymax = Max, fill= ImputationApproach), alpha = 0.3) +
  geom_line(aes(x = Missing, y = pred, colour = ImputationApproach)) +
  facet_grid(.~FunctionName) +
  scale_fill_manual(values = c("black", "red2", "blue", "blue", "blue", "cyan3")) +
  scale_colour_manual(values = c("black","black","black","black","black","black")) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "dotted", "dashed", "solid")) +
  scale_x_continuous(breaks = c(0, 20, 40, 60)) +
  coord_cartesian(xlim = c(0,80), ylim = c(0,0.6)) +
  guides(colour=FALSE)+
  labs(x = "Missing (%)", y = paste("Slope error " , "\u00B1"," 95% confidence intervals", sep = "")) +
  theme_classic() +
  theme(
    text = element_text(size=13),
    panel.grid.major = element_blank(),
    panel.grid.minor.x  = element_blank(),
    plot.title = element_text(face="bold.italic"),
    axis.title.y = element_text(face="bold"),
    legend.title=element_blank(),
    legend.position = "bottom",
    panel.background = element_rect(fill = NA, color = "black"),
    strip.background = element_rect(fill= "light grey", colour="black")) 

ggplot(Ribbon[which(Ribbon$ResponsePresent == "No"),]) +
  geom_ribbon(aes(x = Missing, ymin = Min, ymax = Max, fill= ImputationApproach), alpha = 0.3) +
  geom_line(aes(x = Missing, y = pred, colour = ImputationApproach)) +
  facet_grid(.~FunctionName) +
  scale_fill_manual(values = c("black", "red2", "blue", "blue", "blue", "cyan3")) +
  scale_colour_manual(values = c("black","black","black","black","black","black")) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "dotted", "dashed", "solid")) +
  scale_x_continuous(breaks = c(0, 20, 40, 60)) +
  coord_cartesian(xlim = c(0,80), ylim = c(0,0.6)) +
  guides(colour=FALSE)+
  labs(x = "Missing (%)", y = paste("Slope error " , "\u00B1"," 95% confidence intervals", sep = "")) +
  theme_classic() +
  theme(
    text = element_text(size=13),
    panel.grid.major = element_blank(),
    panel.grid.minor.x  = element_blank(),
    plot.title = element_text(face="bold.italic"),
    axis.title.y = element_text(face="bold"),
    legend.title=element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = NA, color = "black"),
    strip.background = element_rect(fill= "light grey", colour="black")) 

