str.generate = function(nums) {
    str = character(0)
    for (i in 1:length(nums)) {
        if (i == 1) {
            str = paste(nums[i], sep = "")
        } else {
            str = paste(str, ",", nums[i], sep = "")
        }
    }
    print(str)
}

model.generate = function(str) {
    str = unlist(strsplit(str, ","))
    model.str = predictor.str1 = predictor.str2 = character(0)
    for (i in 1:length(str)) {
        if (i == 1) {
            model.str = paste("X", str[i], sep = "")
            predictor.str1 = paste("X", str[i], " = Test1$X", str[i], sep = "")
            predictor.str2 = paste("X", str[i], " = Test2$X", str[i], sep = "")
        } else {
            model.str = paste(model.str, " + X", str[i], sep = "")
            predictor.str1 = paste(predictor.str1, ", X", str[i], " = Test1$X", str[i], sep = "")
            predictor.str2 = paste(predictor.str2, ", X", str[i], " = Test2$X", str[i], sep = "")
        }
    }
    print(model.str)
    print(predictor.str1)
    print(predictor.str2)
}


rm(list = ls())

Data = read.csv("Data.csv", stringsAsFactors = FALSE)
n = dim(Data)[1] # The number of rows of the data.frame

### ID is the 43th column in the dataset

# The code below generates a spread sheet with 101 predictors (X1 - X101)
# and the word corresponding to the names of predictors
iters = c(5:7, 13:42, 44:dim(Data)[2])
k = integer(1)
for (i in iters) {
  k = k + 1
  names(Data)[i] = paste("X", k, sep = "")
}

############################################################
imputed_missForest = read.csv("imputed_missForest.csv", stringsAsFactors = FALSE)
Data$X5 = imputed_missForest$X5

model = lm(stars ~ X1 + X2 + X3 + X4 + X5 + X15 + X18 + X23 + X24 + X25 + X26 + X27 + X29 + X30 + X33 + X36 + X38 + 
             X52 + X54 + X55 + X61 + X63 + X66 + X68 + X71 + X75 + X77 + X86 + X94 + X71:X75, data = Data)

############################################################

Test1 = read.csv("Test1.csv", stringsAsFactors = FALSE)
Test2 = read.csv("Test2.csv", stringsAsFactors = FALSE)
n1 = dim(Test1)[1]
n2 = dim(Test2)[1]


iters = c(5:7, 13:dim(Test1)[2])
k = integer(1)
for (i in iters) {
  k = k + 1
  names(Test1)[i] = paste("X", k, sep = "")
}

predictors1 = data.frame(X1 = Test1$X1, X2 = Test1$X2, X3 = Test1$X3, X4 = Test1$X4, X5 = Test1$X5, X15 = Test1$X15, 
                         X18 = Test1$X18, X23 = Test1$X23, X24 = Test1$X24, X25 = Test1$X25, X26 = Test1$X26, 
                         X27 = Test1$X27, X29 = Test1$X29, X30 = Test1$X30, X33 = Test1$X33, X36 = Test1$X36,
                         X38 = Test1$X38, X52 = Test1$X52, X54 = Test1$X54, X55 = Test1$X55, X61 = Test1$X61,
                         X63 = Test1$X63, X66 = Test1$X66, X68 = Test1$X68, X71 = Test1$X71, X75 = Test1$X75,
                         X77 = Test1$X77, X86 = Test1$X86, X94 = Test1$X94)

result1 = predict(model, predictors1)
result.frame1 = data.frame(Id = 1:dim(Test1)[1], Prediction = result1)


iters = c(5:7, 13:dim(Test2)[2])
k = integer(1)
for (i in iters) {
  k = k + 1
  names(Test2)[i] = paste("X", k, sep = "")
}

predictors2 = data.frame(X1 = Test2$X1, X2 = Test2$X2, X3 = Test2$X3, X4 = Test2$X4, X5 = Test2$X5, X15 = Test2$X15, 
                         X18 = Test2$X18, X23 = Test2$X23, X24 = Test2$X24, X25 = Test2$X25, X26 = Test2$X26, 
                         X27 = Test2$X27, X29 = Test2$X29, X30 = Test2$X30, X33 = Test2$X33, X36 = Test2$X36,
                         X38 = Test2$X38, X52 = Test2$X52, X54 = Test2$X54, X55 = Test2$X55, X61 = Test2$X61,
                         X63 = Test2$X63, X66 = Test2$X66, X68 = Test2$X68, X71 = Test2$X71, X75 = Test2$X75,
                         X77 = Test2$X77, X86 = Test2$X86, X94 = Test2$X94)

result2 = predict(model, predictors2)
result.frame2 = data.frame(Id = 1:dim(Test2)[1], Prediction = result2)


id = 1:(n1 + n2)
k = integer(1)
m = integer(1)
result.frame = data.frame(Id = id, Prediction = numeric(length(id)))
for (i in id) {
  k = k + 1
  if (k <= n1) {
    result.frame$Prediction[i] = result.frame1$Prediction[k]
  } else {
    m = m + 1
    result.frame$Prediction[i] = result.frame2$Prediction[m]
  }
}
write.csv(result.frame, "group1.csv", row.names = FALSE)