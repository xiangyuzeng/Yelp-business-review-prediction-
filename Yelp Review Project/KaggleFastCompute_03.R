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

model = lm(stars ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + 
               X16 + X17 + X18 + X19 + X20 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + 
               X30 + X31 + X32 + X33 + X34 + X35 + X36 + X37 + X38 + X39 + X40 + X41 + X42 + X43 + 
               X44 + X45 + X46 + X47 + X48 + X49 + X50 + X51 + X52 + X53 + X54 + X55 + X56 + X57 + 
               X58 + X59 + X60 + X61 + X62 + X63 + X64 + X65 + X66 + X67 + X68 + X69 + X70 + X71 + 
               X72 + X73 + X74 + X75 + X76 + X77 + X78 + X79 + X80 + X81 + X82 + X83 + X84 + X85 + 
               X86 + X87 + X88 + X89 + X90 + X91 + X92 + X93 + X94 + X95 + X96 + X97 + X98 + X99 + 
               X100 + X101 + X38:X94 + X38:X98 + X94:X98 + X71:X75 + X52:X94 + X36:X94 + X36:X52 + 
               X75:X94 + X52:X75 + X36:X75 + X33:X94 + X77:X94 + X33:X77 + X94:X95 + X33:X95 + X77:X95 + 
               X2:X3 + X3:X33 + X2:X33 + X3:X72 + X2:X72 + X33:X72 + X3:X71 + X2:X71 + X33:X71 + X71:X72 
           + X3:X55 + X2:X55 + X33:X55 + X55:X72 + X55:X71 + X38:X94:X98 + X36:X52:X94 + X52:X75:X94 + 
               X36:X75:X94 + X36:X52:X75 + X33:X77:X94 + X33:X94:X95 + X77:X94:X95 + X33:X77:X95 + 
               X2:X3:X33 + X2:X3:X72 + X3:X33:X72 + X2:X33:X72 + X2:X3:X71 + X3:X33:X71 + X2:X33:X71
           + X3:X71:X72 + X2:X71:X72 + X33:X71:X72 + X2:X3:X55 + X3:X33:X55 + X2:X33:X55 + X3:X55:X72
           + X2:X55:X72 + X33:X55:X72 + X3:X55:X71 + X2:X55:X71 + X33:X55:X71 + X55:X71:X72 + 
               X36:X52:X75:X94 + X33:X77:X94:X95 + X2:X3:X33:X72 + X2:X3:X33:X71 + X2:X3:X71:X72 +
               X3:X33:X71:X72 + X2:X33:X71:X72 + X2:X3:X33:X55 + X2:X3:X55:X72 + X3:X33:X55:X72 +
               X2:X33:X55:X72 + X2:X3:X55:X71 + X3:X33:X55:X71 + X2:X33:X55:X71 + X3:X55:X71:X72 +
               X2:X55:X71:X72 + X33:X55:X71:X72 + X2:X3:X33:X71:X72 + X2:X3:X33:X55:X72 + 
               X2:X3:X33:X55:X71 + X2:X3:X55:X71:X72 + X3:X33:X55:X71:X72 + X2:X33:X55:X71:X72 + 
               X2:X3:X33:X55:X71:X72, data = Data)

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

predictors1 = data.frame(X1 = Test1$X1, X2 = Test1$X2, X3 = Test1$X3, X4 = Test1$X4, X5 = Test1$X5, 
                         X6 = Test1$X6, X7 = Test1$X7, X8 = Test1$X8, X9 = Test1$X9, X10 = Test1$X10, 
                         X11 = Test1$X11, X12 = Test1$X12, X13 = Test1$X13, X14 = Test1$X14, X15 = Test1$X15, 
                         X16 = Test1$X16, X17 = Test1$X17, X18 = Test1$X18, X19 = Test1$X19, X20 = Test1$X20, 
                         X21 = Test1$X21, X22 = Test1$X22, X23 = Test1$X23, X24 = Test1$X24, X25 = Test1$X25,
                         X26 = Test1$X26, X27 = Test1$X27, X28 = Test1$X28, X29 = Test1$X29, X30 = Test1$X30, 
                         X31 = Test1$X31, X32 = Test1$X32, X33 = Test1$X33, X34 = Test1$X34, X35 = Test1$X35,
                         X36 = Test1$X36, X37 = Test1$X37, X38 = Test1$X38, X39 = Test1$X39, X40 = Test1$X40,
                         X41 = Test1$X41, X42 = Test1$X42, X43 = Test1$X43, X44 = Test1$X44, X45 = Test1$X45, 
                         X46 = Test1$X46, X47 = Test1$X47, X48 = Test1$X48, X49 = Test1$X49, X50 = Test1$X50, 
                         X51 = Test1$X51, X52 = Test1$X52, X53 = Test1$X53, X54 = Test1$X54, X55 = Test1$X55, 
                         X56 = Test1$X56, X57 = Test1$X57, X58 = Test1$X58, X59 = Test1$X59, X60 = Test1$X60, 
                         X61 = Test1$X61, X62 = Test1$X62, X63 = Test1$X63, X64 = Test1$X64, X65 = Test1$X65, 
                         X66 = Test1$X66, X67 = Test1$X67, X68 = Test1$X68, X69 = Test1$X69, X70 = Test1$X70, 
                         X71 = Test1$X71, X72 = Test1$X72, X73 = Test1$X73, X74 = Test1$X74, X75 = Test1$X75, 
                         X76 = Test1$X76, X77 = Test1$X77, X78 = Test1$X78, X79 = Test1$X79, X80 = Test1$X80,
                         X81 = Test1$X81, X82 = Test1$X82, X83 = Test1$X83, X84 = Test1$X84, X85 = Test1$X85, 
                         X86 = Test1$X86, X87 = Test1$X87, X88 = Test1$X88, X89 = Test1$X89, X90 = Test1$X90, 
                         X91 = Test1$X91, X92 = Test1$X92, X93 = Test1$X93, X94 = Test1$X94, X95 = Test1$X95, 
                         X96 = Test1$X96, X97 = Test1$X97, X98 = Test1$X98, X99 = Test1$X99, X100 = Test1$X100, 
                         X101 = Test1$X101)

result1 = predict(model, predictors1)
result.frame1 = data.frame(Id = 1:dim(Test1)[1], Prediction = result1)


iters = c(5:7, 13:dim(Test2)[2])
k = integer(1)
for (i in iters) {
    k = k + 1
    names(Test2)[i] = paste("X", k, sep = "")
}

predictors2 = data.frame(X1 = Test2$X1, X2 = Test2$X2, X3 = Test2$X3, 
                         X4 = Test2$X4, X5 = Test2$X5, X6 = Test2$X6, 
                         X7 = Test2$X7, X8 = Test2$X8, X9 = Test2$X9, 
                         X10 = Test2$X10, X11 = Test2$X11, X12 = Test2$X12, 
                         X13 = Test2$X13, X14 = Test2$X14, X15 = Test2$X15, X16 = Test2$X16, 
                         X17 = Test2$X17, X18 = Test2$X18, X19 = Test2$X19, X20 = Test2$X20, 
                         X21 = Test2$X21, X22 = Test2$X22, X23 = Test2$X23, X24 = Test2$X24, 
                         X25 = Test2$X25, X26 = Test2$X26, X27 = Test2$X27, X28 = Test2$X28, 
                         X29 = Test2$X29, X30 = Test2$X30, X31 = Test2$X31, X32 = Test2$X32, 
                         X33 = Test2$X33, X34 = Test2$X34, X35 = Test2$X35, X36 = Test2$X36, 
                         X37 = Test2$X37, X38 = Test2$X38, X39 = Test2$X39, X40 = Test2$X40, 
                         X41 = Test2$X41, X42 = Test2$X42, X43 = Test2$X43, X44 = Test2$X44, 
                         X45 = Test2$X45, X46 = Test2$X46, X47 = Test2$X47, X48 = Test2$X48, 
                         X49 = Test2$X49, X50 = Test2$X50, X51 = Test2$X51, X52 = Test2$X52, 
                         X53 = Test2$X53, X54 = Test2$X54, X55 = Test2$X55, X56 = Test2$X56, 
                         X57 = Test2$X57, X58 = Test2$X58, X59 = Test2$X59, X60 = Test2$X60, 
                         X61 = Test2$X61, X62 = Test2$X62, X63 = Test2$X63, X64 = Test2$X64, 
                         X65 = Test2$X65, X66 = Test2$X66, X67 = Test2$X67, X68 = Test2$X68, 
                         X69 = Test2$X69, X70 = Test2$X70, X71 = Test2$X71, X72 = Test2$X72, 
                         X73 = Test2$X73, X74 = Test2$X74, X75 = Test2$X75, X76 = Test2$X76, 
                         X77 = Test2$X77, X78 = Test2$X78, X79 = Test2$X79, X80 = Test2$X80, 
                         X81 = Test2$X81, X82 = Test2$X82, X83 = Test2$X83, X84 = Test2$X84, 
                         X85 = Test2$X85, X86 = Test2$X86, X87 = Test2$X87, X88 = Test2$X88, 
                         X89 = Test2$X89, X90 = Test2$X90, X91 = Test2$X91, X92 = Test2$X92, 
                         X93 = Test2$X93, X94 = Test2$X94, X95 = Test2$X95, X96 = Test2$X96, 
                         X97 = Test2$X97, X98 = Test2$X98, X99 = Test2$X99, X100 = Test2$X100, 
                         X101 = Test2$X101)

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