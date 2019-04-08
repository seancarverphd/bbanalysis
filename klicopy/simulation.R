## simulation.R
## Script written by Rebeca Berger, Class of 2017,
## American University
##
##  This script uses data and code from Marchi & Albert.
##  See: https://github.com/maxtoki/baseball_R
##   Citation: Max Marchi and Jim Albert (2013), 
##             Analyzing Baseball With R, CRC Press.
##
## From Chapter 9 of Marchi & Albert. 
## Simulating an inning using the transition matrix 

data2011 <- read.csv("data/all2011.csv", header=FALSE)
fields <- read.csv("data/fields.csv")
names(data2011) <- fields[, "Header"]

data2011$HALF.INNING <- with(data2011, 
                             paste(GAME_ID, INN_CT, BAT_HOME_ID))
data2011$RUNS.SCORED <- with(data2011, (BAT_DEST_ID > 3) +
                               (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))

get.state <- function(runner1, runner2, runner3, outs){
  runners <- paste(runner1, runner2, runner3, sep="")
  paste(runners, outs)                      
}

RUNNER1 <- ifelse(as.character(data2011[,"BASE1_RUN_ID"])=="", 0, 1)
RUNNER2 <- ifelse(as.character(data2011[,"BASE2_RUN_ID"])=="", 0, 1)
RUNNER3 <- ifelse(as.character(data2011[,"BASE3_RUN_ID"])=="", 0, 1)
data2011$STATE <- get.state(RUNNER1, RUNNER2, RUNNER3, data2011$OUTS_CT)

NRUNNER1 <- with(data2011, as.numeric(RUN1_DEST_ID==1 | BAT_DEST_ID==1))
NRUNNER2 <- with(data2011, as.numeric(RUN1_DEST_ID==2 | 
                                        RUN2_DEST_ID==2 | BAT_DEST_ID==2))
NRUNNER3 <- with(data2011, as.numeric(RUN1_DEST_ID==3 | RUN2_DEST_ID==3 |
                                        RUN3_DEST_ID==3 | BAT_DEST_ID==3))

NOUTS <- with(data2011, OUTS_CT + EVENT_OUTS_CT)
## The following line gives all the states of the games in 2011 in transition form
data2011$NEW.STATE <- get.state(NRUNNER1, NRUNNER2, NRUNNER3, NOUTS)

data2011 <- subset(data2011, (STATE!=NEW.STATE) | (RUNS.SCORED>0))

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(plyr)
data.outs <- ddply(data2011, .(HALF.INNING), summarize,
                   Outs.Inning = sum(EVENT_OUTS_CT))
data2011 <- merge(data2011, data.outs)
data2011C <- subset(data2011, Outs.Inning == 3)
## The following line is in place so that only batting events are counted, i.e. no stolen bases, wild pitches, passed balls, etc 
data2011C <- subset(data2011, BAT_EVENT_FL == TRUE)

library(car)
data2011C$NEW.STATE <- recode(data2011C$NEW.STATE,
                              "c('000 3', '100 3', '010 3', '001 3',
                              '110 3', '101 3', '011 3', '111 3') = '3'")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 9.2.3 Computing the transition probabilities - 
# IMPORTANT 

T.matrix <- with(data2011C, table(STATE, NEW.STATE))


P.matrix <- prop.table(T.matrix, 1)

# The following is the probability matrix from state to state (all 25) using 2011 data 
P.matrix <- rbind(P.matrix, c(rep(0, 24), 1))

# This is all of the transitional differences staring with zero runners on and no outs (the beginning of an inning)
P1 <- round(P.matrix["000 0", ], 3)
# and this makes it look a little neater
data.frame(Prob = P1[P1 > 0])

P2 <- round(P.matrix["010 2", ], 3)
data.frame(Prob = P2[P2 > 0])

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 9.2.4 Simulating the Markov chain

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# JUNE 23, 2017
hometeam <- substr(data2011C$GAME_ID,1,3)
substr(data2011C$GAME_ID,1,3)=="NYA"
sum(substr(data2011C$GAME_ID,1,3)=="NYA")
NYAplays <- data2011C[substr(data2011C$GAME_ID,1,3)=="NYA",]
# View(NYAplays)

NYAtrans <- with(NYAplays, table(STATE, NEW.STATE))
NYAtransM <- prop.table(NYAtrans, 1)

#Check: 
colSums(NYAtransM)
rowSums(NYAtransM)

# Function that returns a specified team's transition matrix for their home games
# IMPORTANT ****** # 
FindCountMat <- function(AllTeamData, HomeTeam='ALL') {
  if (identical(HomeTeam,'ALL')) {
    TeamPlays <- AllTeamData
  } else {
    TeamPlays <- AllTeamData[substr(AllTeamData$GAME_ID,1,3)==HomeTeam, ]  
  }
  teamtransCount <- with(TeamPlays, table(STATE, NEW.STATE))
  return(teamtransCount)
}

FindCountStates <- function(AllTeamData, HomeTeam='ALL') {
  if (identical(HomeTeam,'ALL')) {
    TeamPlays <- AllTeamData
  } else {
    TeamPlays <- AllTeamData[substr(AllTeamData$GAME_ID,1,3)==HomeTeam, ]
  }
  teamstateCount <- with(TeamPlays, table(STATE))
  teamnewstateCount <- with(TeamPlays, table(NEW.STATE))  # for 3 outs
  teamstateCount <- c(teamstateCount,teamnewstateCount['3']) # Append 3 outs count
  return(teamstateCount)
}

FindTransMat <- function(AllTeamData, HomeTeam='ALL') {
  teamtransCount <- FindCountMat(AllTeamData, HomeTeam=HomeTeam)
  teamtransProp <- prop.table(teamtransCount, 1)
  return(teamtransProp)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

