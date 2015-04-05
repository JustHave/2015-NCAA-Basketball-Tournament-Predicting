rm(list=ls())

tour <- read.csv("tourData.csv")
tour <- tour[,-1]
myData <- tour[tour[1]==2006,]
myData <- myData[,4:17]
row.names(myData) <- NULL

myModel <- lm(dif.score~. , data=myData)
plot(myData[,1],predict(myModel))
summary(myModel)

myY <- sapply(myData[,1], function(d) {
  if(d>0) {return(1)}
  else {return(0)}
})
myYpred <- sapply(predict(myModel), function(d) {
  if(d>0) {return(1)}
  else {return(0)}
})
sum(myY == myYpred) / length(myY)

# -------------------- 1 -------------------- #

myData[,1] <- myY
myModel <- glm(dif.score~. , data=myData, family = "binomial")
summary(myModel)

myYpred <- sapply(predict(myModel), function(d) {
  if(d>0) {return(1)}
  else {return(0)}
})
sum(myY == myYpred) / length(myY)

# -------------------- 2 -------------------- #

newData <- tour[tour[1]==2010,]
newData <- newData[,4:17]
row.names(newData) <- NULL

myPred <- predict(myModel,newData[,2:14])

myY <- sapply(myData[,2], function(d) {
  if(d>0) {return(1)}
  else {return(0)}
})
myYpred <- sapply(myPred, function(d) {
  if(d>0) {return(1)}
  else {return(0)}
})
sum(myY == myYpred) / length(myY)