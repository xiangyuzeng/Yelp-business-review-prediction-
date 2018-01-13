rm(list = ls())

Data = read.csv("Yelp_train.csv", stringsAsFactors = FALSE)

n = dim(Data)[1] # The number of rows of the data.frame

# Assign each observation a ID No.
Data$id = integer(n)
for (i in seq_len(n)) {
    Data$id[i] = i
}

# This function detects whether a character is using standard ASCII value.
# Returns true on ASCII, false on non-ASCII
isASCII = function(txt) {
    return( all(charToRaw(txt) <= as.raw(127)) )
}

# This function counts the number of non-ASCII characters in a String of characters.
# Returns the count
ASCIICount = function(chars) {
    count = 0
    for (i in seq_len(length(chars))) {
        if (!isASCII(chars[i])) {
            count = count + 1
        }
    }
    return(count)
}

# Detect whether there are non-English comments in the file
# Record observation number of each non-English comment
num.lines = numeric(0)
for (i in 1:length(Data$text)) {
    line = Data$text[i];
    chars = strsplit(line, "")[[1]]
    if (ASCIICount(chars) > 7) {
        num.lines = append(num.lines, i)
    }
}

num.lines = num.lines[-c(1, 2, 4)]
# Display all non-English comments
print(Data$text[num.lines])
cat("non-English review IDs:", Data$id[num.lines])


# Remove all non-English comments
Data = Data[-num.lines, ]

# Declare list of all phrases from commnets
# and list of all words from comments
phrases = list()
words = list()

# Generate a list of phrases and words for all comments
for (i in seq_len(n)) {
    phrases[[i]] = unlist(strsplit(Data$text[i], ",|\\.|\\?|!|\""))
    words[[i]] = unlist(strsplit(Data$text[i], " +"))
}

# This function is used to create a new predictor
# First it counts the number of occurances the word is in the text
# Then it updates on the specific cell in the data.frame
# Finally it prints out how many observations contain at least one occurance of this word
# option 1: look into words
# option 2: look into phrases
new.var.count = function(Data, word, option = 1) {
    n = length(Data$text)
    Data[[word]] = integer(n)
    
    if (option == 1) {
        # option 1: words
        for (i in seq_len(n)) {
            Data[[word]][i] = length(grep(word, words[[i]], ignore.case = TRUE))
        }
    } else if (option == 2) {
        # option 2: phrases
        for (i in seq_len(n)) {
            Data[[word]][i] = length(grep(word, phrases[[i]], ignore.case = TRUE))
        }
    }
    return(Data)
}

# Adding new predictors...
Data = new.var.count(Data, "appealing")
Data = new.var.count(Data, "\\<appeti")
Data = new.var.count(Data, "(\\<delicious)|(delectable)|(tasty)|(yum)|(\\<tasteful)")
Data = new.var.count(Data, "choice")
Data = new.var.count(Data, "excellent")
Data = new.var.count(Data, "(superior)|(superb)")
Data = new.var.count(Data, "(top( |-)notch)|(first( |-)rate)", option = 2)
Data = new.var.count(Data, "high( |-)quality", option = 2)
Data = new.var.count(Data, "(mouth( |-)watering)|(scrumptious)|(luscious)", option = 2)
Data = new.var.count(Data, "(\\<enjoy)|(\\<palatable)|(\\<delight)|(\\<pleasing)|(\\<satisf)|(\\<pleasant)")
Data = new.var.count(Data, "nausea")
Data = new.var.count(Data, "\\<flav(o|ou)r[^l]")
Data = new.var.count(Data, "(distasteful)|(repulsive)|(sickening)|(unappetizing)|(unsavory)")
Data = new.var.count(Data, "(rancid)|(stale)|(rotten)")
Data = new.var.count(Data, "bad")
Data = new.var.count(Data, "not bad", option = 2)
Data$bad = Data$bad - Data$`not bad`
Data = new.var.count(Data, "(holy)|(wow)|(god)")
Data = new.var.count(Data, "(nice)|(happy)|(easy)")
Data = new.var.count(Data, "(wonderful)|(beautiful)|(best)")
Data = new.var.count(Data, "lovely")
Data = new.var.count(Data, "better")
Data = new.var.count(Data, "(like)|(love\\>)|(great)")
Data = new.var.count(Data, "(look.* forward)|(worth)", option = 2)
Data = new.var.count(Data, "(many)|(plenty of)", option = 2)
Data = new.var.count(Data, "well")
Data = new.var.count(Data, "fine")
Data = new.var.count(Data, "cheap")
Data = new.var.count(Data, "(friendly)|(welcome)|(attentive)|(passion)")
Data = new.var.count(Data, "(unique)|(creative)")
Data = new.var.count(Data, "(fresh)|(authentic)|(healthy)")
Data = new.var.count(Data, "disappoint")
Data = new.var.count(Data, "return")
Data = new.var.count(Data, "not good", option = 2)
Data = new.var.count(Data, "good")
Data$good = Data$good - Data$`not good`
Data = new.var.count(Data, "(noisy)|(dirty)|(nothing)")
Data = new.var.count(Data, "(quiet)|(comfortable)")
Data = new.var.count(Data, "(never)|(forever)")
Data = new.var.count(Data, "(but)|(however)")
Data = new.var.count(Data, "(terrible)|(trouble)|(weird)")
Data = new.var.count(Data, "reserv")
Data = new.var.count(Data, "crowded")
Data = new.var.count(Data, "(wait)|(even)|(slow)")
Data = new.var.count(Data, "have to", option = 2)
Data = new.var.count(Data, "(worse)|(awful)")
Data = new.var.count(Data, "expensive")
Data = new.var.count(Data, "serious")
Data = new.var.count(Data, "awesome")
Data = new.var.count(Data, "average")
Data = new.var.count(Data, "(clean)|(tidy)|(fast)")
Data = new.var.count(Data, "(center)|(convenient)")
Data = new.var.count(Data, "close")
Data = new.var.count(Data, "pretty")
Data = new.var.count(Data, "(definitely)|(truly)|(especially)")
Data = new.var.count(Data, "(though)|(while)")
Data = new.var.count(Data, "wrong")
Data = new.var.count(Data, "small")
Data = new.var.count(Data, "(avoid)|(skip)")
Data = new.var.count(Data, "insane")
Data = new.var.count(Data, "back")
Data = new.var.count(Data, "come")
Data = new.var.count(Data, "!")
Data = new.var.count(Data, "\\?")
Data = new.var.count(Data, "(outstanding)|(extraordinary)|(best)")
Data = new.var.count(Data, "interesting")
Data = new.var.count(Data, "recommend")
Data = new.var.count(Data, "slip")
Data = new.var.count(Data, "limit")
Data = new.var.count(Data, "so( |-)so", option = 2)

# The code below renames the names of predictors
# ranging from X1 to X101
### ID is the 43th column in the dataset, so DONT include IDs
iters = c(5:7, 13:42, 44:dim(Data)[2])
k = integer(1)
for (i in iters) {
    k = k + 1
    names(Data)[i] = paste("X", k, sep = "")
}

##### N words #####
# Divide every word predictor by number of words(X4)
for (i in c(15:42, 44:length(Data))) {
    Data[[i]] = as.numeric(Data[[i]])
    Data[[i]] = Data[[i]] / Data$X4
}
##### End of N Words #####

# Starting imputation...
# to compute all NA values in sentiment_score
predictors = Data[,-c(1:4,8:13,43)]
# We decide to use the missForest package
if(!require(missForest)) {
    install.packages("missForest");
    require(missForest)
}
imputed_forest = missForest(predictors,maxiter = 10, ntree = 100, verbose = T)$ximp

# Starting Model Selection...
if(!require(leaps)) {
    install.packages("leaps"); 
    require(leaps)
}
data_missForest = cbind(Data$stars, imputed_missForest)
subsets_reg = regsubsets(stars ~ . + X38*X94*X98 + X71:X75 + X94*X52*X36*X75 + X94*X33*X77*X95, 
                         data = data_missForest, weights = NULL, nbest = 3, nvmax = 100, method = "seqrep", really.big = TRUE)

parameters = subsets_reg$xnames
# s is the model selection list generated by regsubsets()
s=summary(subsets_reg)

order_cp = order(s$cp)
order_bic = order(s$bic)
order_adjr2 = order(s$adjr2, decreasing = T)

# starting cross validation...
if(!require(boot)) {
    install.packages("boot");
    require(boot)
}

data_inter = data_missForest
data_inter$X38X94 = data_inter$X38*data_inter$X94
data_inter$X38X98 = data_inter$X38*data_inter$X98
data_inter$X94X98 = data_inter$X94*data_inter$X98
data_inter$X71X75 = data_inter$X71*data_inter$X75
data_inter$X52X94 = data_inter$X52*data_inter$X94
data_inter$X36X94 = data_inter$X36*data_inter$X94
data_inter$X36X52 = data_inter$X36*data_inter$X52
data_inter$X75X94 = data_inter$X75*data_inter$X94
data_inter$X52X75 = data_inter$X52*data_inter$X75
data_inter$X36X75 = data_inter$X36*data_inter$X75
data_inter$X33X94 = data_inter$X33*data_inter$X94
data_inter$X77X94 = data_inter$X77*data_inter$X94
data_inter$X33X77 = data_inter$X33*data_inter$X77
data_inter$X94X95 = data_inter$X94*data_inter$X95
data_inter$X33X95 = data_inter$X33*data_inter$X95
data_inter$X77X95 = data_inter$X77*data_inter$X95
data_inter$X38X94X98 = data_inter$X38*data_inter$X94*data_inter$X98
data_inter$X36X52X94 = data_inter$X36*data_inter$X52*data_inter$X94
data_inter$X52X75X94 = data_inter$X52*data_inter$X75*data_inter$X94
data_inter$X36X75X94 = data_inter$X36*data_inter$X75*data_inter$X94
data_inter$X36X52X75 = data_inter$X36*data_inter$X52*data_inter$X75
data_inter$X33X77X94 = data_inter$X33*data_inter$X77*data_inter$X94
data_inter$X33X94X95 = data_inter$X33*data_inter$X94*data_inter$X95
data_inter$X77X94X95 = data_inter$X77*data_inter$X94*data_inter$X95
data_inter$X33X77X95 = data_inter$X33*data_inter$X77*data_inter$X95
data_inter$X36X52X75X94= data_inter$X36*data_inter$X52*data_inter$X75*data_inter$X94
data_inter$X33X77X94X95= data_inter$X33*data_inter$X77*data_inter$X94*data_inter$X95

for (i in 1:nrow(s$which)) {
    model = s$which[i,]
    model[1]=T
    subset = c(1:ncol(data_inter))[model]
    if (length(subset) > 1) {
        Xw = data_inter[,subset]
        wlm = glm(stars ~ ., data = Xw)
    }
    else {
        Xw = data_inter[,subset[2]]
        wlm = glm(stars ~ ., data = Xw)
    }
    s$CV[i] = cv.glm(model.frame(wlm), wlm, K=5)$delta[1]
}

order_cv = order(s$CV)

# Display results of model selection
print("Best adjr2 models: ")
print(parameters[s$which[order_adjr2[1]]])
print("Best cp models: ")
print(parameters[s$which[order_cp[1]]])
print("Best bic models: ")
print(parameters[s$which[order_bic[1]]])
print("Best cv models: ")
print(parameters[s$which[order_cv[1]]])


criteria = as.data.frame(1:length(s$adjr2))
names(criteria)="id"
criteria$adjr2 = s$adjr2
criteria$adjr_rank = rank(-s$adjr2)
criteria$cp = s$cp
criteria$cp_rank = rank(s$cp)
criteria$bic = s$bic
criteria$bic_rank = rank(s$bic)
criteria$cv = s$CV
criteria$cv_rank = rank(s$CV)

# the criteria to choose the model
print(head(criteria))

parameters[s$which[288,]]
# Display the predictors we are going to use
cat(parameters[s$which[288,]], sep = " + ")

Data$X5 = imputed_missForest$X5

model = lm(stars ~ X1 + X2 + X3 + X5 + X7 + X9 + X10 + X11 + X12 + X14 + X15 + X16 + X17 + X18 + 
               X19 + X20 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X30 + X31 + X32 + 
               X33 + X35 + X36 + X37 + X38 + X39 + X40 + X41 + X42 + X43 + X44 + X46 + X47 + X49 + 
               X50 + X52 + X54 + X55 + X56 + X57 + X58 + X59 + X61 + X62 + X63 + X65 + X66 + X67 + 
               X68 + X69 + X71 + X72 + X73 + X75 + X77 + X78 + X80 + X81 + X82 + X84 + X85 + X86 + 
               X87 + X88 + X90 + X91 + X92 + X93 + X94 + X95 + X96 + X98 + X99 + X100 + X101 + 
               X38:X94 + X94:X98 + X71:X75 + X52:X94 + X36:X94 + X36:X52 + X75:X94 + X52:X75 + 
               X36:X75 + X77:X95 + X36:X52:X94 + X52:X75:X94 + X36:X75:X94 + X33:X94:X95 + 
               X33:X77:X95 + X33:X77:X94:X95, data = Data)

############################################################

# 1st diagnostics
par(mfrow = c(2, 2))
for (i in c(1, 2, 4, 5)) {
    plot(model, which = i)
}

# Remove ID 16804 due to its high value in Cook's Distance (only one word "best" is included in the comment)
# Remove ID 34396 due to wording (disgusting * 4, which leads to super negative score)
# Remove ID 26541 due to "best", "good", and "great". But customer is implicitly expressing a dispointing idea.
Data = Data[-c(16804, 26539, 34391), ]


# 2nd diagnostics
model = lm(stars ~ X1 + X2 + X3 + X5 + X7 + X9 + X10 + X11 + X12 + X14 + X15 + X16 + X17 + X18 + 
               X19 + X20 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X30 + X31 + X32 + 
               X33 + X35 + X36 + X37 + X38 + X39 + X40 + X41 + X42 + X43 + X44 + X46 + X47 + X49 + 
               X50 + X52 + X54 + X55 + X56 + X57 + X58 + X59 + X61 + X62 + X63 + X65 + X66 + X67 + 
               X68 + X69 + X71 + X72 + X73 + X75 + X77 + X78 + X80 + X81 + X82 + X84 + X85 + X86 + 
               X87 + X88 + X90 + X91 + X92 + X93 + X94 + X95 + X96 + X98 + X99 + X100 + X101 + 
               X38:X94 + X94:X98 + X71:X75 + X52:X94 + X36:X94 + X36:X52 + X75:X94 + X52:X75 + 
               X36:X75 + X77:X95 + X36:X52:X94 + X52:X75:X94 + X36:X75:X94 + X33:X94:X95 + 
               X33:X77:X95 + X33:X77:X94:X95, data = Data)
par(mfrow = c(2, 2))
for (i in c(1, 2, 4, 5)) {
    plot(model, which = i)
}

par(mfrow = c(1, 1))
# Residual Plot
plot(fitted(model), resid(model), pch = 23, bg = "red", cex = 1.4, 
     main = "Residual Plot", 
     xlab = "Fitted values", 
     ylab = "Residuals")
lines(c(1, 1), c(-5, 5), col = "blue", cex = 5)
lines(c(5, 5), c(-5, 5), col = "blue", cex = 5)

# Normal-QQ Plot
qqnorm(rstudent(model), pch = 23, bg = "red", cex = 1.4,
       main = "QQ plot with Standardized Residuals")
abline(a = 0, b = 1, col = "blue", lwd = 2.5)

summary(model)