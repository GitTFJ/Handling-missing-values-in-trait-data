x = subset(CombinedEigenReport, ErrorPresent == "No")
x$EigenvectorN = as.numeric(as.character(x$EigenvectorN))


ggplot(x) +
  geom_violin(aes(y = EigenvectorN, x = as.character(Missing)), alpha = 0.2, fill = "blue") +
  scale_x_discrete(labels = c("0", "", "", "", 
                              "20", "", "", "", 
                              "40", "", "", "", 
                              "60", "", "", "", 
                              "80")) +
  labs(x = "Missing (%)", y = "Number of eigenvectors") +
  coord_flip() +
  theme_classic()
ggsave("../Results/SummaryPlots/Eigen_missing.png" ,plot = last_plot(), width = 12, height = 12, units = "cm")

x$FunctionName = as.character(x$FunctionName)
x$FunctionName = factor(x$FunctionName, levels = c("BaseData","MCAR", "MOPP", "MOPT", "WBP", "WBTP", "WBTR", "WBTPR", "SBP", "SBTP", "SBTR", "SBTPR"))


ggplot(x) +
  geom_violin(aes(y = EigenvectorN, x = FunctionName), alpha = 0.2, fill = "blue") +
  coord_flip() +
  labs(x = " ", y = "Number of eigenvectors") +
  theme_classic() 
ggsave("../Results/SummaryPlots/Eigen_bias.png" ,plot = last_plot(), width = 12, height = 12, units = "cm")

y = SummarisedImputationErrors
y$ImputationError = ifelse(y$NumberOfImputationAttempts > 1, "Yes", y$ImputationError)
y$ImputationError = ifelse(is.na(y$ImputationError), "No", y$ImputationError)
table(y$ImputationError)
y = subset(y, RMSE < 10)

tmp = y %>%
  group_by(FunctionName, ImputationError, ImputationApproach) %>%
  dplyr::summarise(
    med_er = median(RMSE, na.rm = T),
    l_er = quantile(RMSE, na.rm = T, probs = 0.025),
    u_er = quantile(RMSE, na.rm = T, probs = 0.975),
    N = n()
    )

tmp2 = data.frame(
  ImputationApproach = c(
    rep("Mice: mean matching + phylogeny", 4),
    rep("Mice: regression + phylogeny", 4),
    rep("Mice: random forest + phylogeny", 4)
  ),
  ImputationApproachTrim = c(
    rep("Mean matching", 4),
    rep("Regression", 4),
    rep("Random forest", 4)
  ),
  FunctionName = rep(
    c("SBP",
      "SBTP",
      "SBTR",
      "SBTPR"), 3),
  Yes = NA,
  Yes_med = NA,
  Yes_l = NA,
  Yes_u = NA,
  No = NA,
  No_med = NA,
  No_l = NA,
  No_u = NA
  )

for(a in 1:nrow(tmp2)){
  ia = as.character(tmp2$ImputationApproach[a])
  fn = as.character(tmp2$FunctionName[a])
  y = tmp[which(tmp$ImputationApproach == ia & tmp$FunctionName == fn & tmp$ImputationError == "Yes"),]
  n = tmp[which(tmp$ImputationApproach == ia & tmp$FunctionName == fn & tmp$ImputationError == "No"),]
  tmp2$Yes[a] = y$N
  tmp2$Yes_med[a] = y$med_er
  tmp2$Yes_l[a] = y$l_er
  tmp2$Yes_u[a] = y$u_er
  tmp2$No[a] = n$N
  tmp2$No_med[a] = n$med_er
  tmp2$No_l[a] = n$l_er
  tmp2$No_u[a] = n$u_er
}
tmp2$perc = tmp2$Yes/(tmp2$Yes + tmp2$No)*100

ggplot(tmp2) +
  geom_col(aes(x = ImputationApproachTrim, y = perc), position = "dodge") +
  coord_flip() +
  labs(x = " ", y = 
"Observations where number of 
eigenvectors were trimmed (%)") +
  theme_classic() +
  facet_grid(FunctionName~.)
ggsave("../Results/SummaryPlots/Eigen_Removed.png" ,plot = last_plot(), width = 12, height = 12, units = "cm")

ggplot(tmp2) +
  geom_linerange(aes(x = ImputationApproachTrim, ymin = Yes_l, ymax = Yes_u), colour = "red", size = 1, alpha = 0.3, position = position_nudge(x = -0.1)) +
  geom_point(aes(x = ImputationApproachTrim, y = Yes_med), colour = "red", size = 2, alpha = 0.5, position = position_nudge(x = -0.1)) +
  geom_linerange(aes(x = ImputationApproachTrim, ymin = No_l, ymax = No_u), colour = "grey", size = 1, alpha = 0.6, position = position_nudge(x = 0.1)) +
  geom_point(aes(x = ImputationApproachTrim, y = No_med), colour = "grey", size = 2, alpha = 0.9, position = position_nudge(x = 0.1)) +
  coord_flip() +
  labs(x = " ", y = "Imputation error (RMSE)
       ") +
  facet_grid(FunctionName~.) +
  theme_classic()
ggsave("../Results/SummaryPlots/Eigen_ImpError.png" ,plot = last_plot(), width = 12, height = 12, units = "cm")
