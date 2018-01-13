rm(list = ls())

Test1 = read.csv("Test1.csv", stringsAsFactors = FALSE)
Test2 = read.csv("Test2.csv", stringsAsFactors = FALSE)

predictors = Test1[,-c(1:4,8:13)]
if(!require(missForest)) {
  install.packages("missForest");
  require(missForest)
}
imputed_forest1 = missForest(predictors, maxiter = 10, ntree = 100, verbose = T)$ximp
write.csv(imputed_forest1, "imputed_Test1.csv", row.names = F)


predictors = Test2[,-c(1:4,8:13)]
if(!require(missForest)) {
  install.packages("missForest");
  require(missForest)
}
imputed_forest2 = missForest(predictors, maxiter = 10, ntree = 100, verbose = T)$ximp
write.csv(imputed_forest2, "imputed_Test2.csv", row.names = F)


imputed_forest1 = read.csv("imputed_Test1.csv", stringsAsFactors = FALSE)
imputed_forest2 = read.csv("imputed_Test2.csv", stringsAsFactors = FALSE)

Test1$X5 = imputed_forest1$X5
Test2$X5 = imputed_forest2$X5

write.csv(Test1, "Test1.csv", row.names = FALSE)
write.csv(Test2, "Test2.csv", row.names = FALSE)
