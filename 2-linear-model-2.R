rm(list=ls())
library(aod)
library(ggplot2)

year <- 2003

regular <- read.csv("regData.csv")
tour <- read.csv("tourData.csv")
regular <- regular[,-1]
tour <- tour[,-1]
myData <- regular[regular[1]==year,]
myData <- myData[,4:17]
row.names(myData) <- NULL

ggplot(myData, aes(x=dif.fgm, y=dif.score)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm)

# -------------------- 1 -------------------- #

myY <- sapply(myData[,1], function(d) {
  if(d>0) {return(1)}
  else {return(0)}
})
myData[,1] <- myY
myData$dif.score <- factor(myData$dif.score)

myModel <- glm(dif.score~. , data=myData, family = "binomial")
summary(myModel)
# confint(myModel)
# wald.test(b = coef(myModel), Sigma = vcov(myModel), Terms = 4)
exp(coef(myModel))

myYpred <- predict(myModel)
myYpred <- sapply(myYpred, function(d) {
  if(d>0.5) {return(1)}
  else {return(0)}
})
sum(myY == myYpred) / length(myY)

# -------------------- 2 -------------------- #

newData <- tour[tour[1]==year,]
newData <- newData[,4:17]
row.names(newData) <- NULL
myY <- sapply(newData[,1], function(d) {
  if(d>0) {return("1")}
  else {return("0")}
})
newData[,1] <- myY
newData$dif.score <- factor(newData$dif.score)

myYpred <- predict(myModel, newdata = newData, type = "response")
plot(myY,myYpred)
myYpred <- sapply(myYpred, function(d) {
  if(d>0.5) {return(1)}
  else {return(0)}
})
sum(myY == myYpred) / length(myY)