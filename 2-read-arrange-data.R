# Clearing memory and reading data
rm(list=ls())
data.regular <- rbind(
  read.csv("original-data/regular_season_detailed_results_2015.csv"), 
  read.csv("original-data/regular_season_detailed_results.csv"))
data.regular <- data.regular[data.regular$wloc!="N",]
data.team <- read.csv("original-data/teams.csv")
data.team <- data.team$team_id
data.year <- sort(unique(data.regular$season), decreasing=T)

# Initializing paired data
data.pair <- matrix(NA, nrow(data.regular), ncol(data.regular))
data.pair <- as.data.frame(data.pair)
names(data.pair) <- names(data.regular)
data.pair$wloc <- data.regular$wloc

# Row index
k <- 0
# Start iteration
for(y in data.year) {
  data.temp <- data.regular[data.regular$season==y,]
  for(i in 1:(length(data.team)-1)) {
    for(j in i:length(data.team)) {
      t1 <- data.team[i]
      t2 <- data.team[j]
      index.row <- c(intersect(which(data.temp$wteam==t1), 
                               which(data.temp$lteam==t2)), 
                     intersect(which(data.temp$lteam==t1), 
                               which(data.temp$wteam==t2)))
      if(length(index.row)==2) {
        data.pair[0:(length(index.row)-1)+k+1,] <- data.temp[index.row,]
        k <- k + length(index.row)
      }
    }
  }
  print(y)
}
# Cleaning and storing data
data.pair <- data.pair[complete.cases(data.pair),]
write.csv(data.pair, file = "arranged-data/data-pair.csv")
data.pair <- read.csv("arranged-data/data-pair.csv")
data.pair <- data.pair[,-1]

# Initializing paired data
data.diff <- matrix(NA, nrow(data.pair), 4)
data.diff <- as.data.frame(data.diff)
names(data.diff) <- c("season", "team.home", "team.alien", "diff.score")

# Compute different in score
for(i in 1:nrow(data.pair)) {
  data.diff[i,1] <- data.pair[i,1]
  if(data.pair[i,7]=="H") {
    data.diff[i,2] <- data.pair[i,3]
    data.diff[i,3] <- data.pair[i,5]
    data.diff[i,4] <- data.pair[i,4] - data.pair[i,6]
  } else {
    data.diff[i,2] <- data.pair[i,5]
    data.diff[i,3] <- data.pair[i,3]
    data.diff[i,4] <- data.pair[i,6] - data.pair[i,4]
  }
}
# Storing data
write.csv(data.diff, file = "arranged-data/data-diff.csv")
rm(list=ls())
data.diff <- read.csv("arranged-data/data-diff.csv")
data.diff <- data.diff[,-1]

hist(data.diff[,4], nclass=20)
summary(data.diff[,4])

# Initializing score data
diff.score <- max(data.diff[,4]):min(data.diff[,4])
data.score <- matrix(NA, length(diff.score), 3)
data.score <- as.data.frame(data.score)
data.score[,1] <- diff.score
names(data.score) <- c("diff.score", "num.win", "num.played")

for(s in 1:nrow(data.score)) {
  index.row <- which(data.diff[,4]==data.score[s,1])
  data.score[s,3] <- length(index.row)
  num.wined <- 0
  if(length(index.row)!=0) {for(i in index.row) {
    if(i%%2==0) {
      if(data.diff[i-1,4]<0) num.wined <- num.wined+1
    } else {
      if(data.diff[i+1,4]<0) num.wined <- num.wined+1
    }
  }}
  data.score[s,2] <- num.wined
}

p <- data.score[,2] / data.score[,3]
plot(data.score[,1],p)

write.csv(data.score, file = "arranged-data/data-score.csv")


# Initializing wining data
data.win <- matrix(NA, nrow(data.diff), 2)
data.win <- as.data.frame(data.win)
names(data.win) <- c("result.alien", "diff.score.at.home")

for(i in 1:nrow(data.diff)) {
  data.win[i,2] <- data.diff[i,4]
  if(i%%2==0) {
    if(data.diff[i-1,4]<0) data.win[i,1] <- 1
    else data.win[i,1] <- 0
  } else {
    if(data.diff[i+1,4]<0) data.win[i,1] <- 1
    else data.win[i,1] <- 0
  }
}
write.csv(data.win, file = "arranged-data/data-result.csv")

data.win[,2] <- data.matrix(data.win[,2])

model.log <- glm(result.alien~., data.win, family="binomial")
pred <- sapply(predict(model.log,type="response"), function(y) {
  if(y>0.5) return(1)
  else return(0)
})
sum(pred == data.win[,1]) / length(pred)
tab <- table(pred=pred, true=data.win[,1])
