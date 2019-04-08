# NEXT 3 LINES LOAD LIBRARIES OF FUNCTIONS
library(tm)
library(wordcloud)
library(RColorBrewer)
library(stringr)

innings <- unique(sim.baseball(10000, ALL, seed=1))
freq.unrounded <- exp(likes.baseball(innings, ALL))*100
freq <- round(exp(likes.baseball(innings, ALL))*100)

#wordcloud(innings, freq, min.freq=1)

#innings <- unique(sim.baseball(2500, BAL, seed=1))
#freq <- round(exp(likes.baseball(innings, BAL))*2500)
#wordcloud(innings,freq,random.order=TRUE,colors='black')

#innings <- unique(sim.baseball(2600, WAS, seed=1))
#freq <- round(exp(likes.baseball(innings,WAS))*2600)
#wordcloud(innings,freq,random.order=TRUE,colors=brewer.pal(8,'RdBu'))

transitions.from <- function(from.string) {
  batter <- TRUE
  runner1 <- grepl('1',from.string)
  runner2 <- grepl('2',from.string)
  runner3 <- grepl('3',from.string)
  outs <- str_count(from.string,'X')
  trans.list <- list.transitions(batter, runner1, runner2, runner3, outs)
  whole.transitions <- c()
  for (transition in trans.list) {
    whole.transitions <- c(whole.transitions, paste(from.string, transition, sep=''))
  }
  return(whole.transitions)
}

all.transitions <- function() {
  from.states <- rowcodes()
  transitions <- c()
  for (state in from.states) {
    transitions <- c(transitions, transitions.from(state))
  }
  return(transitions)
}

num.equal <- function(x, y) {
  return(is.numeric(x) & x==y)
}

num.geqorzero <- function(x, y) {
  return ((is.numeric(x) & (x >= y)) | x==0)
}

list.transitions <- function(batter, runner1, runner2, runner3, outs) {
  trans.list <- c()
  if (isTRUE(batter)) {
    for (new.batter in 0:4) {
      more.transitions <- list.transitions(new.batter, runner1, runner2, runner3, outs)
      trans.list <- c(trans.list, more.transitions)
      #print(more.transitions)
    }
    return(unique(trans.list))
  }
  if (isTRUE(runner1)) {
    for (new.runner1 in 0:4) {
      #print(new.runner1)
      more.transitions <- list.transitions(batter, new.runner1, runner2, runner3, outs)
      trans.list <- c(trans.list, more.transitions)
      #print(more.transitions)
    }
    return(unique(trans.list))
  }
  if (isTRUE(runner2)) {
    do.return <- TRUE
    for (new.runner2 in 0:4) {
      trans.list <- c(trans.list, list.transitions(batter, runner1, new.runner2, runner3, outs))
    }
    return(unique(trans.list))
  }
  if (isTRUE(runner3)) {
    for (new.runner3 in 0:4) {
      trans.list <- c(trans.list, list.transitions(batter, runner1, runner2, new.runner3, outs))
    }
    return(unique(trans.list))
  }
  batter.out <- num.equal(batter, 0)
  runner1.out <- num.equal(runner1, 0)
  runner2.out <- num.equal(runner2, 0)
  runner3.out <- num.equal(runner3, 0)
  new.outs <- outs + batter.out + runner1.out + runner2.out + runner3.out
  if (new.outs >= 3) {
    return('XXX')
  } else if (new.outs == 2) {
    out.string <- 'XX'
  } else if (new.outs == 1) {
    out.string <- 'X'
  } else {
    out.string <- ''
  }
  #print(c(batter, runner1, runner2, runner3))
  runner.on.first <- num.equal(batter,1) | num.equal(runner1, 1) | num.equal(runner2, 1) | num.equal(runner3, 1)
  if (runner.on.first) {
    one.string <- '1'
  } else {
    one.string <- ''
  }
  runner.on.second <- num.equal(batter,2) | num.equal(runner1, 2) | num.equal(runner2, 2) | num.equal(runner3, 2)
  if (runner.on.second) {
    two.string <- '2'
  } else {
    two.string <- ''
  }
  runner.on.third <- num.equal(batter,3) | num.equal(runner1, 3) | num.equal(runner2, 3) | num.equal(runner3, 3)
  if (runner.on.third) {
    three.string <- '3'
  } else {
    three.string <- ''
  }
  if (!runner.on.first & !runner.on.second & !runner.on.third) {
    zero.string <- '0'
  } else {
    zero.string <- ''
  }
  legit <- ((batter < runner1) | (num.equal(batter, 4) & num.equal(runner1, 4)) | num.equal(runner1, 0) | identical(runner1, FALSE)) &
           ((batter < runner2) | (num.equal(batter, 4) & num.equal(runner2, 4)) | num.equal(runner2, 0) | identical(runner2, FALSE)) &
           ((batter < runner3) | (num.equal(batter, 4) & num.equal(runner3, 4)) | num.equal(runner3, 0) | identical(runner3, FALSE)) &
           ((runner1 < runner2) | (num.equal(runner1, 4) & num.equal(runner2, 4)) | num.equal(runner2, 0) | identical(runner1, FALSE) | identical(runner2, FALSE)) &
           ((runner1 < runner3) | (num.equal(runner1, 4) & num.equal(runner3, 4)) | num.equal(runner3, 0) | identical(runner1, FALSE) | identical(runner3, FALSE)) &
           ((runner2 < runner3) | (num.equal(runner2, 4) & num.equal(runner3, 4)) | num.equal(runner3, 0) | identical(runner2, FALSE) | identical(runner3, FALSE)) &
           num.geqorzero(runner2,2) & num.geqorzero(runner3,3)

  if (legit) {
    return.string <- paste(zero.string,one.string,two.string,three.string,out.string,sep='')
    return(unique(return.string))
  }
}