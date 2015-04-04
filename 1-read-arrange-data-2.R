rm(list=ls())

teams <- read.csv("teams.csv")
teamAverage <- read.csv("team_average.csv")
tDetailed <- read.csv("tourney_detailed_results.csv")

TEAM <- teams$team_id
YEAR <- sort(unique(tDetailed$season), decreasing=T)