# install.packages('rpart',dependencies=TRUE)
# install.packages('randomForest',dependencies=TRUE)
# install.packages('party')

# Clearing memory, loading packages
rm(list=ls())
library(e1071)
library(party)
library(randomForest)
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

# Random Forest prediction of Kyphosis data
model.tree <- cforest(score.diff~., data = data.tr, controls=cforest_unbiased(ntree=300, mtry=3))
pred <- predict(model.tree)
tab <- table(pred=pred, true=data.tr[,1])
classAgreement(tab)

# -------------------- Testing The Model -------------------- #

data.tr[1:67,] <- data.test
pred <- predict(model.tree, newdata=data.tr[1:67,])
tab <- table(pred=pred, true=data.test[,1])
classAgreement(tab)