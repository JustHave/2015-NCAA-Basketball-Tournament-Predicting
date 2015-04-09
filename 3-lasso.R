# Clear Memory, loading package
rm(list=ls())
library(glmnet)

# Read data
data.diff <- dget("arranged-data/data-diff-2013.R")
data.diff <- data.diff[,-c(1,2)]
data.tourney <- dget("arranged-data/data-tourney-diff-2013.R")
data.tourney <- data.tourney[,-c(1,2)]

# Process data
data.diff[,2] <- as.factor(data.diff[,2])
data.diff[,4] <- as.factor(data.diff[,4])
data.diff[,6] <- as.factor(data.diff[,6])
data.tourney[,2] <- as.factor(data.tourney[,2])
data.tourney[,4] <- as.factor(data.tourney[,4])
data.tourney[,6] <- as.factor(data.tourney[,6])
myY <- data.diff[,1]
myX <- data.matrix(data.diff[,-1])
newY <- data.tourney[,1]
newX <- data.matrix(data.tourney[,-1])

# -------------------- Fitting Linear Model -------------------- #

fit = glmnet(x=myX, y=myY, alpha = 1)
par(mfrow=c(1,1))
plot(fit, xvar = "lambda", label = TRUE)
plot(fit, xvar = "dev", label = TRUE)

cvfit = cv.glmnet(x=myX, y=myY, type.measure = "mse", nfolds = 20)
plot(cvfit, xvar = "lambda", label = TRUE)

newPred <- predict(cvfit, newx = newX, s = "lambda.min")
plot(newY, newPred)
accuracy <- newY * newPred > 0
sum(accuracy) / length(accuracy)

# -------------------- Fitting Logistic Model -------------------- #

myY <- sapply(myY, function(y){return(y>0)})
myY <- as.factor(myY)
newY <- sapply(newY, function(y){return(y>0)})
newY <- as.factor(newY)

fit = glmnet(x=myX, y=myY, family = "binomial")
plot(fit, xvar = "dev", label = TRUE)

cvfit = cv.glmnet(x=myX, y=myY, type.measure = "class", nfolds = 20， family = "binomial")
plot(cvfit, xvar = "lambda", label = TRUE)
cvfit$lambda.min

newPred <- predict(cvfit, newx = newX, type = "class", s = "lambda.min")
sum(newY == newPred) / length(newY)


newPred <- predict(cvfit, newx = newX, s = "lambda.min", type="response")
logloss <- 0
for(i in 1:length(newY)) {
  if(newY[i]=="TRUE") logloss <- logloss + log(newPred[i])
  else logloss <- logloss + log(1-newPred[i])
}

logloss <- -logloss/length(newY)