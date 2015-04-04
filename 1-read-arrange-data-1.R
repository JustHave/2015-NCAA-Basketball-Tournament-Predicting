rm(list=ls())

teams <- read.csv("teams.csv")
regResult <- read.csv("regular_season_detailed_results.csv")

# -------------------- 1 -------------------- #

logic.win <- sapply(names(regResult),function(varname){
  if(substring(varname, 1, 1) == "w"){
    return(T)
  } else {
    return(F)
  }
})
logic.win[3] <- F
logic.win[7] <- F

logic.lose <- sapply(names(regResult),function(varname){
  if(substring(varname, 1, 1) == "l"){
    return(T)
  } else {
    return(F)
  }
})
logic.lose[5] <- F

# -------------------- 2 -------------------- #

TEAM <- teams$team_id
YEAR <- sort(unique(regResult$season), decreasing=T)
NAME <- sapply(names(regResult)[logic.win], function(varname) {
  return(substring(varname, 2))
})
names(NAME) <- NULL

YEAR <- sapply(YEAR, function(y) {
  return(rep(y, length(NAME)))
})
YEAR <- as.vector(YEAR)
NAME <- interaction(NAME,YEAR)

aveData <- matrix(0, nrow(teams), length(NAME))
aveData <- as.data.frame(aveData)
rownames(aveData) <- TEAM
colnames(aveData) <- NAME

TEAM <- teams$team_id
YEAR <- sort(unique(regResult$season), decreasing=T)
NAME <- sapply(names(regResult)[logic.win], function(varname) {
  return(substring(varname, 2))
})
names(NAME) <- NULL

# -------------------- 3 -------------------- #

for(y in YEAR) {
  for(t in TEAM) {
    logic.y <- regResult$season == y
    logic.t <- regResult$wteam == t
    tempData.1 <- regResult[logic.y & logic.t, ]
    tempData.1 <- tempData.1[logic.win]
    names(tempData.1) <- NAME
    logic.t <- regResult$lteam == t
    tempData.2 <- regResult[logic.y & logic.t, ]
    tempData.2 <- tempData.2[logic.lose]
    names(tempData.2) <- NAME
    tempData <- rbind(tempData.1, tempData.2)
    tempData <- colMeans(tempData)
    
    k <- which(YEAR == y)
    n <- which(TEAM == t)
    aveData[n, ((k-1)*length(NAME)+1):(k*length(NAME))] <- tempData
  }
}

write.csv(aveData, "team_average.csv")