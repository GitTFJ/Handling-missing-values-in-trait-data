#Produce ridge plots
#Output: Save Ridge plots to folder


CombinedImputationErrors$Diff = CombinedImputationErrors$Imputed - CombinedImputationErrors$True
Comb = CombinedImputationErrors

Comb$FunctionName = factor(Comb$FunctionName, levels = c("MCAR", "MOPP", "MOPT", "WBP", "WBTP", "WBTR", "WBTPR", "SBP", "SBTP", "SBTR", "SBTPR"))
Comb$BiasLevel = Comb$FunctionName
levels(Comb$BiasLevel) = list('No Bias' = c("MCAR"), 'Controlled Bias' =  c("MOPP", "MOPT"), 'Weak Bias' = c("WBP", "WBTP", "WBTPR", "WBTR"), 'Severe Bias' = c("SBP", "SBTPR", "SBTP", "SBTR"))
Comb$FunctionReName = Comb$FunctionName
levels(Comb$FunctionReName) = list('Random' = "MCAR", 'Controlled - Phylogeny' = "MOPP", 'Controlled - Trait' = "MOPT", 'Weak bias - Phylogeny' = "WBP", 'Weak bias - Trait' = "WBTP", 'Weak bias - Trait & Response' = "WBTPR", 'Weak bias - Response' = "WBTR", 'Severe bias - Phylogeny' = "SBP",   'Severe bias - Trait' = "SBTP",  'Severe bias - Trait & Response' = "SBTPR",  'Severe bias - Response' = "SBTR")
Comb$MissingCat = as.factor(Comb$Missing*100)
Comb$ImputationApproachAdjust = Comb$ImputationApproach
Comb$ImputationApproachAdjust = factor(Comb$ImputationApproach, levels = c("Mice: mean matching", "Mice: mean matching + phylogeny", "Mice: regression", "Mice: regression + phylogeny", "Mice: random forest", "Mice: random forest + phylogeny", "BHPMF", "BHPMF + phylogeny", "Rphylopars"))
levels(Comb$ImputationApproachAdjust) = c("Mice: mean matching
                                          ", "Mice: mean matching 
+ phylogeny", "Mice: regression
                                          ", "Mice: regression 
+ phylogeny", "Mice: random forest
                                          ", "Mice: random forest 
+ phylogeny", "BHPMF
                                          ", "BHPMF
+ phylogeny", "Rphylopars
                                          ")

Comb$Colour = paste(Comb$ResponsePresent, Comb$Trend, sep = "_")
Comb$Colour = as.factor(Comb$Colour)
Comb = subset(Comb, Diff < 10 & Diff > -10)

ScalingfactorRphy = c(2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2, 2, 2, 1.5)
#Check seeds are performing comporably. If purple = good

for(Z in seq(1,11,1)){
  png(paste0("../Results/RidgePlots/ImputationError",Z,".png"), width = 5900, height = 3000, res = 150)
  X = levels(Comb$FunctionReName)[Z]
  Title = ifelse(X == "Random",paste("Values \n", "Missing: Completely at random", sep = ""), paste("Values \n", "Missing: ", X, sep = ""))
  MiceMeanMatching = ggplot(Comb[which(Comb$FunctionReName == X & (Comb$ImputationApproach == "Mice: mean matching" | Comb$ImputationApproach == "Mice: mean matching + phylogeny")),], aes(x = Diff, y = MissingCat, colour = ResponsePresent, fill = ResponsePresent, linetype = Trend)) +
    geom_density_ridges(alpha = 0.1, scale = 1.5) + 
    scale_color_manual(values = c("grey", "grey")) +
    scale_fill_manual(values = c("grey", "grey")) +
    scale_linetype_manual(values = c("solid", "solid")) +
    scale_size_manual(values=c(1.5, 0.5))+
    geom_vline(aes(xintercept = 0)) +
    facet_grid(.~ImputationApproachAdjust) +
    coord_cartesian(xlim = c(-10,10)) +
    scale_x_continuous(breaks = c(-5,0,5)) +
    theme(
      text = element_text(size=30),
      plot.title = element_text(face="bold.italic", size = 25),
      axis.title.x = element_text(face="bold"),
      axis.title.y = element_text(face="bold"),
      panel.background = element_rect(fill = NA, color = "grey"),
      strip.background = element_rect(fill="light grey", colour="grey"),
      legend.position = "none") +
    labs(title = Title, x = " ", y = "Missing data (%)")
  MiceRegression = ggplot(Comb[which(Comb$FunctionReName == X & (Comb$ImputationApproach == "Mice: regression" | Comb$ImputationApproach == "Mice: regression + phylogeny")),], aes(x = Diff, y = MissingCat, colour = ResponsePresent, fill = ResponsePresent, linetype = Trend)) +
    geom_density_ridges(alpha = 0.1, scale = 1.5) + 
    scale_color_manual(values = c("grey", "grey")) +
    scale_fill_manual(values = c("grey", "grey")) +
    scale_linetype_manual(values = c("solid", "solid")) +
    geom_vline(aes(xintercept = 0)) +
    facet_grid(.~ImputationApproachAdjust) +
    coord_cartesian(xlim = c(-10,10)) +
    scale_x_continuous(breaks = c(-5,0,5)) +
    theme(
      text = element_text(size=30),
      plot.title = element_text(face="bold.italic", size = 25),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      panel.background = element_rect(fill = NA, color = "black"),
      strip.background = element_rect(fill=NA, colour="black"),
      legend.position = "none") +
    labs(title = " \n ", x = " ")
  MiceRandomForest = ggplot(Comb[which(Comb$FunctionReName == X & (Comb$ImputationApproach == "Mice: random forest" | Comb$ImputationApproach == "Mice: random forest + phylogeny")),], aes(x = Diff, y = MissingCat, colour = ResponsePresent, fill = ResponsePresent, linetype = Trend)) +
    geom_density_ridges(alpha = 0.1, scale = 1.5) + 
    scale_color_manual(values = c("grey", "grey")) +
    scale_fill_manual(values = c("grey", "grey")) +
    scale_linetype_manual(values = c("solid", "solid")) +
    scale_size_manual(values=c(1.5, 0.5))+
    geom_vline(aes(xintercept = 0)) +
    facet_grid(.~ImputationApproachAdjust) +
    coord_cartesian(xlim = c(-10,10)) +
    scale_x_continuous(breaks = c(-5,0,5)) +
    theme(
      text = element_text(size=30),
      plot.title = element_text(face="bold.italic", size = 25),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      panel.background = element_rect(fill = NA, color = "grey"),
      strip.background = element_rect(fill="light grey", colour="grey"),
      legend.position = "none") +
    labs(title = " \n  ", x = "Imputed values - True values")
  BHPMF = ggplot(Comb[which(Comb$FunctionReName == X & (Comb$ImputationApproach == "BHPMF" | Comb$ImputationApproach == "BHPMF + phylogeny")),], aes(x = Diff, y = MissingCat, colour = ResponsePresent, fill = ResponsePresent, linetype = Trend)) +
    geom_density_ridges(alpha = 0.1, scale = 1.5) + 
    scale_color_manual(values = c("grey", "grey")) +
    scale_fill_manual(values = c("grey", "grey")) +
    scale_linetype_manual(values = c("solid", "solid")) +
    scale_size_manual(values=c(1.5, 0.5))+
    geom_vline(aes(xintercept = 0)) +
    facet_grid(.~ImputationApproachAdjust) +
    coord_cartesian(xlim = c(-10,10)) +
    scale_x_continuous(breaks = c(-5,0,5)) +
    theme(
      text = element_text(size=30),
      plot.title = element_text(face="bold.italic", size = 25),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      panel.background = element_rect(fill = NA, color = "grey"),
      strip.background = element_rect(fill= NA, colour="grey"),
      legend.position = "none") +
    labs(title = " \n  ", x = " ")
  RPhylopars = ggplot(Comb[which(Comb$FunctionReName == X & Comb$ImputationApproach == "Rphylopars"),], aes(x = Diff, y = MissingCat, colour = ResponsePresent, fill = ResponsePresent, linetype = Trend)) +
    geom_density_ridges(alpha = 0.1, scale = ScalingfactorRphy[Z]) + 
    scale_color_manual(values = c("grey", "grey")) +
    scale_fill_manual(values = c("grey", "grey")) +
    scale_linetype_manual(values = c("solid", "solid")) +
    scale_size_manual(values=c(1.5, 0.5))+
    geom_vline(aes(xintercept = 0)) +
    facet_grid(.~ImputationApproachAdjust) +
    coord_cartesian(xlim = c(-10,10)) +
    scale_x_continuous(breaks = c(-5,0,5)) +
    theme(
      text = element_text(size=30),
      plot.title = element_text(face="bold.italic", size = 25),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      panel.background = element_rect(fill = NA, color = "black"),
      strip.background = element_rect(fill="light grey", colour="black"),
      legend.position = "none") +
    labs(title = " \n  ", x = " ")

  
  
  
  Plt = ggarrange(MiceMeanMatching, MiceRegression, MiceRandomForest, BHPMF, RPhylopars, ncol= 5, widths = c(1,1,1,1,0.5))
  print(Plt)
  dev.off()
}

