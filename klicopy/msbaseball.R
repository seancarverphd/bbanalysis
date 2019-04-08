# msbaseball.R:
# Script written by Sean Carver, Professorial Lecturer
# American University.
#
# This script Calls FindTransMat() function 
#   coded in script "simulation.R," written by Rebeca Berger, 
#   class of 2017, American University.
#   simulation.R uses data and code from Marchi & Albert.
#   See: https://github.com/maxtoki/baseball_R
#   Citation: Max Marchi and Jim Albert (2013), 
#             Analyzing Baseball With R, CRC Press.
#

# More Readable State Names Than Used in Marchi & Albert
# Code without final 3-out state XXX, needed for rows:
rowcodes <- function() {
  return(c("0:","0X:","0XX:",
           "3:","3X:","3XX:",
           "2:","2X:","2XX:",
           "23:","23X:","23XX:",
           "1:","1X:","1XX:",
           "13:","13X:","13XX:",
           "12:","12X:","12XX:",
           "123:","123X:","123XX:"))
}

# Code with final 3-out state, needed for columns:
colcodes <- function() {
  return(c(rowcodes(), "XXX"))
}

# Get a list of the abbreviations, used for all teams in a data set. 
# (This list may differ from data set to data set, or year to year.)
FindTeams <- function(AllTeamData) {
  TeamAbbr <- c()
  for (k in 1:length(AllTeamData)) { }
    TeamAbbr <- c(TeamAbbr, substr(AllTeamData$GAME_ID,1,3))  
  return(levels(factor(TeamAbbr)))
}

# Get a list of all transition matrices, used for all team in a data set.
# (This list may differ from data set to data set, or year to year.)
TransMatList <- function(AllTeamData) {
  Teams <- FindTeams(AllTeamData)
  TMList <- list()
  k <- 1
  for (team in Teams) {
    TM <- FindTransMat(AllTeamData,team)
    row.names(TM)<-rowcodes()
    colnames(TM)<-colcodes()
    TMList[[k]] <- TM
    k <- k + 1
  }
  names(TMList) <- Teams
  return(TMList)
}

CountMat <- function(AllTeamData, HomeTeam='ALL') {
  CM <- FindCountMat(AllTeamData, HomeTeam)
  row.names(CM) <- rowcodes()
  colnames(CM) <- colcodes()
  return(CM)
}

# Get a matrix concatenating pairs of strings for states.
# These pairs correspond to labels of possible or impossible transitions.
# For easy reading the trailing, ':' has been removed, where applicable.
TransitionNameMatrix <- function() {
  ROWS <- rowcodes()
  COLS <- colcodes()
  nROWS <- length(ROWS) 
  nCOLS <- length(COLS)
  v = rep(NA,nROWS)
  M <- matrix(rep(v,nCOLS), ncol=nCOLS)
  for (i in 1:nROWS) {
    for (j in 1:nCOLS) {
      trans1 <- paste(ROWS[i],COLS[j],sep="")
      M[i,j] <- sub(':$','',trans1)
    }
  }
  return(M)
}

# The matrix of transition labels from TransitionNameMatrix() concatentated into a 600-dim vector.
TransitionNameVector <- function() {
  return (c(TransitionNameMatrix()))
}

StateNameVector <- function() {
  return (gsub(":","",colcodes()))
}

TransitionCountVector <- function(AllTeamData, hometeam='ALL') {
  CM <- CountMat(AllTeamData, hometeam)
  counts <- c(CM)
  names(counts) <- TransitionNameVector()
  return(counts)
}

StateCountVector <- function(AllTeamData, hometeam='ALL') {
  counts <- FindCountStates(AllTeamData, hometeam)
  names(counts) <- gsub(":","",colcodes())
  return(counts)
}

# A 600 by (number of teams) matrix of transition probabilities across teams.
Cloud <- function(AllTeamData) {
  ML <- TransMatList(AllTeamData)
  A <- c()
  for (M in ML) {
    A <- cbind(A,c(M))
  }
  colnames(A) <- names(ML)
  row.names(A) <- TransitionNameVector()
  return(A)
}

Normalized.Cloud <- function(AllTeamData) {
  C <- Cloud(AllTeamData)
  meanC <- rowMeans(C)
  return(C-meanC)
}

# Transition Matrices, specific teams 2011 MLB:
WAS <- FindTransMat(data2011C,"WAS") # Washington Nationals, 2011.
BAL <- FindTransMat(data2011C,"BAL") # Baltimore Orioles, 2011.
NYA <- FindTransMat(data2011C,"NYA") # New York Yankees, 2011.
ALL <- FindTransMat(data2011C)  # ALL MLB teams averaged together, 2011.

# List of Transition Matrices, one for each team.
TM <- TransMatList(data2011C)  # List for each team 2011 MLB team.

# Assign new codes to transition matrices:
row.names(WAS)<-rowcodes()
colnames(WAS)<-colcodes()
row.names(BAL)<-rowcodes()
colnames(BAL)<-colcodes()
row.names(NYA)<-rowcodes()
colnames(NYA)<-colcodes()
row.names(ALL)<-rowcodes()
colnames(ALL)<-colcodes()

# Baseball simulator
sim.baseball <- function(n, transition.matrix, seed=FALSE) {
  if (!identical(seed, FALSE)) {
    set.seed(seed)
  }
  half.innings <- vector(mode="character",length=n)
  for (k in 1:n) {
    state <- "0:"
    one.half.inning <- state
    while (state != "XXX") {
      probabilities <- transition.matrix[state,]
      new.state <- sample(names(probabilities),1,prob=probabilities)
      one.half.inning <- paste(one.half.inning, new.state, sep="")
      state <- new.state
    }
    half.innings[k] <- one.half.inning
  }
  return (half.innings)
}

# Get the states that a half inning passes through
getStates <- function(one.half.inning) {
  states <- strsplit(one.half.inning,":")
  for (k in 1:length(states[[1]])-1) {
    states[[1]][k] <- paste(states[[1]][k],":",sep="")
  }
  return (states)
}

# Get the transitions that a half inning undergoes
getTransitions <- function(one.half.inning) {
  states <- getStates(one.half.inning)
  transitions <- list()  # Initialize transitions as an empty list
  number.of.transitions <- length(states[[1]])-1
  for (k in 1:number.of.transitions) {
    from.state <- states[[1]][k]
    to.state <- states[[1]][k+1]
    transitions[[k]] <- c(from.state, to.state)
  }
  return (transitions)
}

# Get the probabilities for the transitions of a half inning
getProbabilities <- function(one.half.inning, transition.matrix) {
  trans <- getTransitions(one.half.inning)
  probabilities <- c()
  for (k in 1:length(trans)) {
    from.state <- trans[[k]][1]
    to.state <- trans[[k]][2]
    this.probability <- transition.matrix[from.state, to.state]
    probabilities <- c(probabilities, this.probability)
  }
  return (probabilities)
}

# Get the log-likelihood for one inning
getLogLikelihood <- function(one.half.inning, transition.matrix) {
  return (sum(log(getProbabilities(one.half.inning,
                                   transition.matrix))))
}

# Get the log-likelihood for a vector of innings
likes.baseball <- function(innings, transition.matrix) {
  likes <- rep(NA,length(innings)) # initialize vector of likes
  k <- 1 # Initialize index into innings
  for (one.inning in innings) {
    likes[k] <- getLogLikelihood(one.inning, transition.matrix)
    k <- k + 1
  }
  return (likes)
}