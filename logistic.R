# Clear memory and load packages
rm(list=ls())
library(stats)
library(glmnet)

# Reading the data
data.diff <- dget("arranged-data/data-diff-2011.R")
data.diff <- data.diff[,-c(1,2)]
data.tourney <- dget("arranged-data/data-tourney-diff-2011.R")
data.tourney <- data.tourney[,-c(1,2)]
# Cleaning the data
data.diff[,2] <- as.factor(data.diff[,2])
data.diff[,4] <- as.factor(data.diff[,4])
data.diff[,6] <- as.factor(data.diff[,6])
data.tourney[,2] <- as.factor(data.tourney[,2])
data.tourney[,4] <- as.factor(data.tourney[,4])
data.tourney[,6] <- as.factor(data.tourney[,6])
# Cleaning the response
y <- data.diff[,1]
y <- sapply(y, function(y){
  if(y>0) return("WIN")
  else return("LOSE")
})
y <- as.factor(y)
data.tr <- data.diff
data.tr[,1] <- y

# -------------------- Fitting The Model -------------------- #

# Arrange the data
x <- data.matrix(data.tr[,-1])
y <- factor(data.tr[,1])

# Fit lasso logistic model, do cross validation
model.log = cv.glmnet(x, y, family = "binomial", , alpha=1, nfolds=50, type.measure = "class")
plot(model.log)
coef(model.log, s = "lambda.min")
var <- c(1,3,4,8,9,11,12,14:21)
x <- x[,var]

# Fit ridge logistic model, do cross validation
model.log = cv.glmnet(x, y, family = "binomial", , alpha=1, nfolds=50, type.measure = "class")
# Check the accuracy
my.pred <- predict(model.log, s = "lambda.min", newx=x, type = "class")
sum(my.pred==y)/length(y)

# -------------------- Testing The Model -------------------- #

x.new <- data.matrix(data.tourney[,-1])
x.new <- x.new[,var]
y.new <- data.tourney[,1]
y.new <- sapply(y.new, function(y){
  if(y>0) return("WIN")
  else return("LOSE")
})
y.new <- as.factor(y.new)

my.pred <- predict(model.log, s = "lambda.1se", newx=x.new, type = "class")
sum(y.new == my.pred)/length(y.new)