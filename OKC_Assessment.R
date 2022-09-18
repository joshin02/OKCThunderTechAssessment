## Set working directory
setwd("~/Desktop/OKC_Internship/")
## libraries 
library(dplyr)

## Read shot data csv file
shotData <- read.csv("shots_data.csv")

## Teams 
teamA <- shotData %>% 
  filter(team == "Team A")

teamB <- shotData %>% 
  filter(team == "Team B")

## Helper Functions
### function used to obtain number of non-corner 3 shots attempted 
NC3Shots <- function(team) {
  team %>% 
    filter(abs(x) >= abs(23.75) & abs(y) > abs(7.8)) %>% 
    count()
}
### function used to obtain number of corner 3 shots attempted
C3Shots <- function(team) {
  team %>% 
    filter(abs(x) >= abs(22) & abs(y) <= abs(7.8)) %>% 
    count()
}
### function used to obtain number of 2 pt shots attempted
twoPtShots <- function(team) {
  team %>% 
    filter(!(abs(x) >= abs(22) & abs(y) <= abs(7.8)) 
           & !(abs(x) >= abs(23.75) & abs(y) > abs(7.8))) %>% 
    count()
}
### function used to calculate shot distribution percentage for shot zone
shotZonePercentage <- function(shotType, team) {
  return((shotType / count(team)) * 100)
}
### function to get number of three point shots made for a given team
threesMade <- function(team) {
  return (team %>% 
            filter ((abs(x) >= abs(22) & abs(y) <= abs(7.8) 
                    | abs(x) >= abs(23.75) & abs(y) > abs(7.8))
                    & fgmade == 1) %>% 
            count())
}
### function to get corner 3 shots made for a given team
C3shotsMade <- function(team) {
  return(team %>% 
           filter(abs(x) >= abs(22) & abs(y) <= abs(7.8)
                  & fgmade == 1) %>% 
           count())
}
### function to get non-corner 3 shots made for a given team
NC3shotsMade <- function(team) {
  return(team %>% 
           filter(abs(x) >= abs(23.75) & abs(y) > abs(7.8)
                  & fgmade == 1) %>% 
           count())
}
### function to get 2 pt shots made for a given team
twoPtShotsMade <- function(team) {
  return(team %>% 
           filter(!(abs(x) >= abs(22) & abs(y) <= abs(7.8)) 
                  & !(abs(x) >= abs(23.75) & abs(y) > abs(7.8))
                  & fgmade == 1) %>% 
           count())
}
### function to get eFG percentage for given zone
eFGPercentage <- function(shotType, threePtMade, shotsAttempted) {
  return(((shotType + (.5 * threePtMade)) / shotsAttempted) * 100)
}

### Shot Distribution for Team A
### C3 Shot Distribution 
shotZonePercentage(C3Shots(teamA), teamA)
### NC3 Shot Distribution
shotZonePercentage(NC3Shots(teamA), teamA)
### 2PT Shot Distribution
shotZonePercentage(twoPtShots(teamA), teamA)

### Shot Distribution for Team B
### C3 Shot Distribution 
shotZonePercentage(C3Shots(teamB), teamB)
### NC3 Shot Distribution
shotZonePercentage(NC3Shots(teamB), teamB)
### 2PT Shot Distribution
shotZonePercentage(twoPtShots(teamB), teamB)


### for this calculation of eFG, shots attempted was assumed to be number of shots 
### attempted in a specfiic shot zone

### eFG for Team A
### C3 eFG
eFGPercentage(C3shotsMade(teamA), threesMade(teamA), C3Shots(teamA))
### NC3 eFG
eFGPercentage(NC3shotsMade(teamA), threesMade(teamA), NC3Shots(teamA))
### 2PT eFG
eFGPercentage(twoPtShotsMade(teamA), threesMade(teamA), twoPtShots(teamA))

### eFG for Team B
### C3 eFG
eFGPercentage(C3shotsMade(teamB), threesMade(teamB), C3Shots(teamB))
### NC3 eFG
eFGPercentage(NC3shotsMade(teamB), threesMade(teamB), NC3Shots(teamB))
### 2PT eFG
eFGPercentage(twoPtShotsMade(teamB), threesMade(teamB), twoPtShots(teamB))

### for this calculation of eFG, shots attempted was assumed to be total number of 
### shots attempted by a team

### eFG for Team A
### C3 eFG
eFGPercentage(C3shotsMade(teamA), threesMade(teamA), count(teamA))
### NC3 eFG
eFGPercentage(NC3shotsMade(teamA), threesMade(teamA), count(teamA))
### 2PT eFG
eFGPercentage(twoPtShotsMade(teamA), threesMade(teamA), count(teamA))

### eFG for Team B
### C3 eFG
eFGPercentage(C3shotsMade(teamB), threesMade(teamB), count(teamB))
### NC3 eFG
eFGPercentage(NC3shotsMade(teamB), threesMade(teamB), count(teamB))
### 2PT eFG
eFGPercentage(twoPtShotsMade(teamB), threesMade(teamB), count(teamB))