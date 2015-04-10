# install.packages('e1071',dependencies=TRUE)

# Clearing memory, loading packages
rm(list=ls())
library(e1071)
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
myY <- data.diff[,1]
myY <- sapply(myY, function(y){
  if(y>0) return("WIN")
  else return("LOSE")
})
myY <- as.factor(myY)
data.tr <- data.diff
data.tr[,1] <- myY
# Cleaning the response
myY <- data.tourney[,1]
myY <- sapply(myY, function(y){
  if(y>0) return("WIN")
  else return("LOSE")
})
myY <- as.factor(myY)
data.test <- data.tourney
data.test[,1] <- myY

# -------------------- Fitting The Model -------------------- #

# Fitting the model
model.svm  <- svm(score.diff~., data = data.tr)
summary(model.svm)
# Checking its accuracy
pred <- predict(model.svm, data.tr)
tab <- table(pred=pred, true=data.tr[,1])
classAgreement(tab)
tuned <- tune.svm(score.diff~., data = data.tr)
# Error estimation of svm using 10-fold cross validation: 0.2394209

# -------------------- Testing The Model -------------------- #

data.tr[1:67,] <- data.test
pred <- predict(model.svm, data.tr[1:67,])
tab <- table(pred=pred, true=data.test[,1])
classAgreement(tab)