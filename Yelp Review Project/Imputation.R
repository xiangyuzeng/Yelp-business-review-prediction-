rm(list = ls())

Data = read.csv("Data.csv", stringsAsFactors = FALSE)
n = dim(Data)[1] # The number of rows of the data.frame

### ID is the 43th column in the dataset

# The code below generates a spread sheet with 101 predictors (X1 - X101)
# and the word corresponding to the names of predictors
iters = c(5:7, 13:42, 44:dim(Data)[2])
k = integer(1)
predictor.table = data.frame(predictor = character(101), idno = 1:101, stringsAsFactors = FALSE)
for (i in iters) {
    k = k + 1
    predictor.table$predictor[k] = names(Data)[i]
    names(Data)[i] = paste("X", k, sep = "")
}
write.csv(predictor.table, "predictor_table.csv", row.names = FALSE)

############################################################

#Check the randomness of NA rows
data_na = Data[is.na(Data$X5),]

boxplot(data_na$stars)
hist(data_na$stars)

freqs = table(data_na$stars)
null.probs = table(Data$stars)

barplot(freqs)
barplot(null.probs)

chisq.test(freqs, null.probs) #Goodness of fit test

predictors = Data[,-c(1:4,8:12,43)]

if(!require(mice)) {
    install.packages("mice"); 
    require(mice)
}
imputed_mice_ = mice(predictors, m = 3, maxit = 100)
imputed_mice1 = complete(imputed_mice_,1)
imputed_mice2 = complete(imputed_mice_,2)
imputed_mice3 = complete(imputed_mice_,3)
sen_score=rowMeans(cbind(imputed_mice1$X5,imputed_mice2$X5,imputed_mice3$X5))
imputed_mice1$X5 = sen_score
imputed_mice = imputed_mice1
write.csv(imputed_mice, "imputed_mice.csv", row.names = F)

if(!require(missForest)) {
    install.packages("missForest");
    require(missForest)
}
imputed_forest = missForest(predictors,maxiter = 10, ntree = 100, verbose = T)$ximp
write.csv(imputed_forest, "imputed_missForest.csv", row.names = F)

if(!require(mi)) {
    install.packages("mi"); 
    require(mi)
}
imputed_mi = mi(predictors, verbose = T)
write.csv(imputed_mi, "imputed_mi.csv", row.names = F)
