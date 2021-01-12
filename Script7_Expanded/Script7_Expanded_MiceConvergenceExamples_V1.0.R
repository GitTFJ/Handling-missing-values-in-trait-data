DeletedData = readRDS("../Data/TemporaryData/DeletedData_Seed1_CorHigh_RespYes_Pos.rds")

imp_mcar0.2 = mice(DeletedData$DataPositiveTrend0.2MCAR[,3:7], m = 8, method = "pmm", maxit = 10)
plot(imp_mcar0.2)[,1]

imp_mcar0.6 = mice(DeletedData$DataPositiveTrend0.6MCAR[,3:7], m = 16, method = "pmm", maxit = 10)
plot(imp_mcar0.6)[,1]

imp_sbtp0.2 = mice(DeletedData$DataPositiveTrend0.2SBTP[,3:7], m = 8, method = "pmm", maxit = 10)
plot(imp_sbtp0.2)[,1]

imp_sbtp0.6 = mice(DeletedData$DataPositiveTrend0.6SBTP[,3:7], m = 16, method = "pmm", maxit = 10)
plot(imp_sbtp0.6)[,1]



imp_mcar0.2 = mice(DeletedData$DataPositiveTrend0.2MCAR[,3:7], m = 8, method = "norm", maxit = 10)
plot(imp_mcar0.2)[,1]

imp_mcar0.6 = mice(DeletedData$DataPositiveTrend0.6MCAR[,3:7], m = 16, method = "norm", maxit = 10)
plot(imp_mcar0.6)[,1]

imp_sbtp0.2 = mice(DeletedData$DataPositiveTrend0.2SBTP[,3:7], m = 8, method = "norm", maxit = 10)
plot(imp_sbtp0.2)[,1]

imp_sbtp0.6 = mice(DeletedData$DataPositiveTrend0.6SBTP[,3:7], m = 16, method = "norm", maxit = 10)
plot(imp_sbtp0.6)[,1]



imp_mcar0.2 = mice(DeletedData$DataPositiveTrend0.2MCAR[,3:7], m = 8, method = "rf", maxit = 10)
plot(imp_mcar0.2)[,1]

imp_mcar0.6 = mice(DeletedData$DataPositiveTrend0.6MCAR[,3:7], m = 16, method = "rf", maxit = 10)
plot(imp_mcar0.6)[,1]

imp_sbtp0.2 = mice(DeletedData$DataPositiveTrend0.2SBTP[,3:7], m = 8, method = "rf", maxit = 10)
plot(imp_sbtp0.2)[,1]

imp_sbtp0.6 = mice(DeletedData$DataPositiveTrend0.6SBTP[,3:7], m = 16, method = "rf", maxit = 10)
plot(imp_sbtp0.6)[,1]
