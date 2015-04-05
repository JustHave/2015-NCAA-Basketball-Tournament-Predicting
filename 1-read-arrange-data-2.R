rm(list=ls())

teams <- read.csv("teams.csv")
teamAverage <- read.csv("team_average.csv")
tDetailed <- read.csv("tourney_detailed_results.csv")

TEAM <- teams$team_id
YEAR <- sort(unique(tDetailed$season), decreasing=T)

logic.win <- sapply(names(tDetailed),function(varname){
  if(substring(varname, 1, 1) == "w"){
    return(T)
  } else {
    return(F)
  }
})
logic.win[3] <- F
logic.win[7] <- F
NAME <- sapply(names(tDetailed[logic.win]), function(varname) {
  return(paste("dif.", substring(varname, 2), sep=""))
})
names(NAME) <- NULL
NAME <- NAME[-1]

# -------------------- 1 -------------------- #

tempData <- list()
for(i in 1:nrow(tDetailed)) {
  if(tDetailed[i,3] < tDetailed[i,5]) {
    sea <- tDetailed[i,1]
    t1 <- tDetailed[i,3]
    t2 <- tDetailed[i,5]
    ds <- tDetailed[i,4] - tDetailed[i,6]
  } else {
    sea <- tDetailed[i,1]
    t1 <- tDetailed[i,5]
    t2 <- tDetailed[i,3]
    ds <- tDetailed[i,6] - tDetailed[i,4]
  }
  tempData[[i]] <- c(sea,t1,t2,ds)
}
tempData <- sapply(tempData, function(t) {return(t)})
tempData <- as.data.frame(t(tempData))

# -------------------- 2 -------------------- #

tourData <- matrix(0, nrow(tempData), ncol(tempData)+length(NAME))
tourData <- as.data.frame(tourData)
names(tourData) <- c("season", "team1", "team2", "dif.score", NAME)
tourData[,1:4] <- tempData

for(i in 1:nrow(tourData)) {
  y <- tourData[i,1]
  t1 <- tourData[i,2]
  t2 <- tourData[i,3]
  y <- ((2014-y)*14+3) : ((2015-y)*14+1)
  t1 <- which(teamAverage[,1]==t1)
  t2 <- which(teamAverage[,1]==t2)
  tourData[i,5:17] <- teamAverage[t1,y] - teamAverage[t2,y]
}

write.csv(tourData,"tourData.csv")