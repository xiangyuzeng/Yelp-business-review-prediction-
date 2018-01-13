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

imputed_missForest = read.csv("imputed_missForest.csv", stringsAsFactors = FALSE)

if(!require(leaps)) {
    install.packages("leaps"); require(leaps)}
stars = Data$stars
data_missForest = cbind(stars, imputed_missForest) 
# data_mice = cbind(stars,imputed_mice)
# data_mi=cbind(stars,imputed_mi)
#cross validation 
subsets_reg = regsubsets(stars~.+ X38*X94*X98 + X71:X75 + X94*X52*X36*X75 + X94*X33*X77*X95 + 
                             X3*X2*X33*X72*X71*X55, 
                         data=data_missForest, weights=NULL, nbest=1, nvmax=100, method = "seqrep", really.big = T)

parameters = subsets_reg$xnames
s=summary(subsets_reg)
s$which
# names(data_missForest)[s$which[1*1,]]
# 
# names(data_missForest)[s$which[30,]]
# 
# parameters[s$which[30,]]

order_cp = order(s$cp)
order_bic = order(s$bic)
order_adjr2 = order(s$adjr2, decreasing = T)

if(!require(boot)) {
    install.packages("boot"); require(boot)}

parameters[s$which[order_adjr2[1]]]
parameters[s$which[order_cp[1]]]
parameters[s$which[order_bic[1]]]
