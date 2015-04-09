rm(list=ls())           # Clear Memory
# Read Data
data.teams <- read.csv("original-data/teams.csv")
data.seeds <- read.csv("original-data/tourney_seeds.csv")
data.regular <- read.csv("original-data/regular_season_detailed_results.csv")
data.tourney <- read.csv("original-data/tourney_detailed_results.csv")

year <- 2011
data.seeds <- data.seeds[data.seeds[,1]==year,]
data.regular <- data.regular[data.regular[,1]==year,]
data.tourney <- data.tourney[data.tourney[,1]==year,]
row.names(data.seeds) <- row.names(data.regular) <- NULL
row.names(data.tourney) <- NULL

complement <- 200

# -------------------- Compute average team data -------------------- #

# Team ID
id.team <- data.teams$team_id
# The row index of data for win and lose team
row.win <- c(4,9:21)
row.lose <- c(6,22:34)

# Rename rows of the data
name.data <- names(data.regular)[row.win]
name.data <- sapply(name.data, function(varname) {
  return(paste("ave",substring(varname, 2),sep="."))
})
name.data <- c("team", name.data, "num.win", "num.lose")
names(name.data) <- NULL

# Initialize the team average data frame
data.average <- matrix(NA, length(id.team), length(name.data))
data.average <- as.data.frame(data.average)
names(data.average) <- name.data

for(i in id.team) {
  # Subset data when i is the winning team
  col.team <- which(data.regular[,3]==i)
  data.temp.w <- as.matrix(data.regular[col.team,row.win])
  # Subset data when i is the losing team
  col.team <- which(data.regular[,5]==i)
  data.temp.l <- as.matrix(data.regular[col.team,row.win])
  # In case that R may match names
  names(data.temp.w) <- names(data.temp.l) <- NULL
  # Bind and get the average
  data.temp <- rbind(data.temp.w, data.temp.l)
  data.temp <- colMeans(data.temp)
  
  # Find row index
  col.team <- which(data.teams$team_id==i)
  # Plug in the data we got
  data.average[col.team, 1] <- i
  data.average[col.team, 2:15] <- data.temp
  data.average[col.team, 16] <- nrow(data.temp.w)
  data.average[col.team, 17] <- nrow(data.temp.l)
}

# -------------------- Different the average data -------------------- #

name.data <- name.data[-1]
name.data <- sapply(name.data, function(varname){
  return(paste("diff", varname, sep="."))
})
name.data <- c("team.1", "team.2", "score.diff","team.1.loc", 
               "team.1.seed.rank", "team.1.area", 
               "team.2.seed.rank", "team.2.area", 
               name.data)
names(name.data) <- NULL

# Initialize the arranged data
data.diff <- matrix(NA, nrow(data.regular),length(name.data))
data.diff <- as.data.frame(data.diff)
names(data.diff) <- name.data

# Plug in the first few columns
data.diff[,1] <- data.regular[,3]
data.diff[,2] <- data.regular[,5]
data.diff[,3] <- data.regular[,4] - data.regular[,6]
data.diff[,4] <- data.regular[,7]

# Arrange the order of two teams
for(i in 1:nrow(data.regular)) {
  if(data.diff[i,1] > data.diff[i,2]) {
    # Arrange the order of two teams
    col.team <- data.diff[i,1]
    data.diff[i,1] <- data.diff[i,2]
    data.diff[i,2] <- col.team
    # Adjust the other parameters
    data.diff[i,3] <- -data.diff[i,3]
    if(data.diff[i,4]=="H") {data.diff[i,4]="A"}
    else if(data.diff[i,4]=="A") {data.diff[i,4]="H"}
  }
}

# Plug in the next few columns
for(i in 1:nrow(data.regular)) {
  # Filling data for team 1
  t1 <- data.diff[i,1]
  col.team <- which(data.seeds[,3]==t1)
  if(length(col.team)!=0) {
    data.temp <- data.seeds[col.team,2]
    data.diff[i,5] <- as.numeric(substring(data.temp, 2, 3))
    data.diff[i,6] <- substring(data.temp, 1, 1)
  } else {
    data.diff[i,5] <- complement
    data.diff[i,6] <- "not.seed"
  }
  # Filling data for team 2
  t2 <- data.diff[i,2]
  col.team <- which(data.seeds[,3]==t2)
  if(length(col.team)!=0) {
    data.temp <- data.seeds[col.team,2]
    data.diff[i,7] <- as.numeric(substring(data.temp, 2, 3))
    data.diff[i,8] <- substring(data.temp, 1, 1)
  } else {
    data.diff[i,7] <- complement
    data.diff[i,8] <- "not.seed"
  }
}

# Plug in the differential data
for(i in 1:nrow(data.regular)) {
  t1 <- data.diff[i,1]
  t2 <- data.diff[i,2]
  data.temp.w <- which(data.average[,1]==t1)
  data.temp.l <- which(data.average[,1]==t2)
  data.temp <- data.average[data.temp.w,-1] - data.average[data.temp.l,-1]
  data.diff[i,9:24] <- data.temp
}

dput(data.average, file = "arranged-data/data-average-2011.R")
dput(data.diff, file = "arranged-data/data-diff-2011.R")

# -------------------- Arrange the tourneyment data -------------------- #

# Initialize the arranged data
data.diff <- matrix(NA, nrow(data.tourney),length(name.data))
data.diff <- as.data.frame(data.diff)
names(data.diff) <- name.data

# Plug in the first few columns
data.diff[,1] <- data.tourney[,3]
data.diff[,2] <- data.tourney[,5]
data.diff[,3] <- data.tourney[,4] - data.tourney[,6]
data.diff[,4] <- data.tourney[,7]

# Arrange the order of two teams
for(i in 1:nrow(data.tourney)) {
  if(data.diff[i,1] > data.diff[i,2]) {
    # Arrange the order of two teams
    col.team <- data.diff[i,1]
    data.diff[i,1] <- data.diff[i,2]
    data.diff[i,2] <- col.team
    # Adjust the other parameters
    data.diff[i,3] <- -data.diff[i,3]
    if(data.diff[i,4]=="H") {data.diff[i,4]="A"}
    else if(data.diff[i,4]=="A") {data.diff[i,4]="H"}
  }
}

# Plug in the next few columns
for(i in 1:nrow(data.tourney)) {
  # Filling data for team 1
  t1 <- data.diff[i,1]
  col.team <- which(data.seeds[,3]==t1)
  if(length(col.team)!=0) {
    data.temp <- data.seeds[col.team,2]
    data.diff[i,5] <- as.numeric(substring(data.temp, 2, 3))
    data.diff[i,6] <- substring(data.temp, 1, 1)
  } else {
    data.diff[i,5] <- complement
    data.diff[i,6] <- "not.seed"
  }
  # Filling data for team 2
  t2 <- data.diff[i,2]
  col.team <- which(data.seeds[,3]==t2)
  if(length(col.team)!=0) {
    data.temp <- data.seeds[col.team,2]
    data.diff[i,7] <- as.numeric(substring(data.temp, 2, 3))
    data.diff[i,8] <- substring(data.temp, 1, 1)
  } else {
    data.diff[i,7] <- complement
    data.diff[i,8] <- "not.seed"
  }
}

# Plug in the differential data
for(i in 1:nrow(data.tourney)) {
  t1 <- data.diff[i,1]
  t2 <- data.diff[i,2]
  data.temp.w <- which(data.average[,1]==t1)
  data.temp.l <- which(data.average[,1]==t2)
  data.temp <- data.average[data.temp.w,-1] - data.average[data.temp.l,-1]
  data.diff[i,9:24] <- data.temp
}

dput(data.diff, file = "arranged-data/data-tourney-diff-2011.R")









