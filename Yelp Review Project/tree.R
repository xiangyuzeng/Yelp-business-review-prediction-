library(tree)
OnlyData = Data[, -c(2:4,8:12,43)]
OnlyData$X5 = OnlyData$X94 = OnlyData$X52 = OnlyData$X36 = OnlyData$X77 = NULL
model = tree(stars ~., data = OnlyData)
plot(model)
text(model)
