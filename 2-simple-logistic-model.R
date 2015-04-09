rm(list=ls())           # Clear Memory
# Read Data
data.diff <- dget("arranged-data/data-diff-2011.R")
data.diff <- data.diff[,-c(1,2)]
data.tourney <- dget("arranged-data/data-tourney-diff-2011.R")
data.tourney <- data.tourney[,-c(1,2)]

# -------------------- Fitting Linear Model -------------------- #

myModel <- lm(score.diff ~. , data=data.diff)
summary(myModel)
plot(data.diff[,1], predict(myModel))

newPred <- predict(myModel, newdata = data.tourney[,-1], type="response")
plot(data.tourney[,1], newPred)

accuracy <- as.logical(data.tourney[,1] * newPred > 0)
sum(accuracy) / length(accuracy)

# -------------------- Fitting Logistic Model -------------------- #

myY <- data.diff[,1]
myY <- sapply(myY, function(y){return(y>0)})
myY <- as.numeric(myY)
data.diff[,1] <- myY
data.diff[,1] <- factor(data.diff[,1])

myModel <- glm(score.diff ~. , data=data.diff, family=binomial)
summary(myModel)
myPred <- predict(myModel)
myPred <- sapply(myPred, function(y){return(y>0.5)})
myPred <- as.numeric(myPred)
sum(myY==myPred) / length(myY)

newY <- data.tourney[,1]
newY <- sapply(newY, function(y){return(y>0)})
newY <- as.numeric(newY)
newPred <- predict(myModel, newdata = data.tourney, type="response")
newPred <- sapply(newPred, function(y){return(y>0.5)})
newPred <- as.numeric(newPred)
sum(newY==newPred) / length(newY)





newPred <- predict(myModel, type="response")
logloss <- 0
for(i in 1:length(myY)) {
  if(myY[i]==1) logloss <- logloss + log(newPred[i])
  else logloss <- logloss + log(1-newPred[i])
}

logloss <- -logloss/length(myY)

