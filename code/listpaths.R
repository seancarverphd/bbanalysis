library(stringr)
source('~/Code/bbanalysis/code/utransfigure.R')

ttable <- query("SELECT * FROM transitions")
row.names(ttable) <- ttable$transition
t0table <- data.frame(transition="3XX:23XX", count_transition=0, u_conditional=Inf, old_state="3XX", count_old_state=183472, u_old_state=1.814434)
row.names(t0table) <- t0table$transition
t0 = rbind(ttable,t0table)

# return the count of the possible half innings
count.half.innings <- function(n) {
  return(length(list.half.innings(n)))
}

list.half.innings <- function(n) {
  paths <- list.paths(n)
  return(paths[grepl('XXX', paths)])
}

played.half.innings <- function(n) {
  q <- paste('SELECT DISTINCT(sequence) FROM innings WHERE LENGTH(sequence)-LENGTH(REPLACE(sequence,":","")) =',n,'AND LENGTH(sequence)=LENGTH(REPLACE(sequence,".","")) AND finalxxx=TRUE;')
  return(query(q)$sequence)
}

unplayed.half.innings <- function(n) {
  listed.n <- list.half.innings(n)
  played.n <- played.half.innings(n)
  stopifnot(length(setdiff(played.n,listed.n))==0)
  return(setdiff(listed.n,played.n))
}

get.batter.transitions <- function(inning) {
  states <- strsplit(inning,'[:.]')
  old_states <- states[[1]][-length(states[[1]])]
  new_states <- states[[1]][-1]
  punct <- punctuations(inning)
  n <- length(old_states)
  transitions <- rep('',str_count(inning,':'))
  index = 0
  for (i in 1:n) {
    if (identical(punct[i],":")) {
      index = index + 1
      transitions[index] <- paste(old_states[i],punct[i],new_states[i],sep='')
    }
  }
  return(transitions)
}

punctuations <- function(inning) {
  p <- strsplit(inning,'[0123X]')  # creates a list p[[1]] is something like '','','',':','','','.' 
  return (p[[1]][!p[[1]] %in% c('')])  # removes the '' from p[[1]]
}

get.transitions <- function(inning) {
  states <- strsplit(inning,'[:.]')
  old_states <- states[[1]][-length(states[[1]])]
  new_states <- states[[1]][-1]
  punct <- punctuations(inning)
  n <- length(old_states)
  transitions <- rep('',n)
  for (i in 1:n) {
    transitions[i] <- paste(old_states[i],punct[i],new_states[i],sep='')
  }
  return(transitions)
}

get.u.inning <- function(inning) {
  transitions <- get.batter.transitions(inning)
  nontransitions <- setdiff(transitions,all.transitions())
  if (length(nontransitions>0)) {
    print(transitions)
    #print(nontransitions)
  }
  stopifnot(length(nontransitions)==0)  # Will throw error if any non-transitions
  u <- 0
  for (transition in transitions) {
    u <- u + t0[transition,]$u_conditional
  }
  return(u)
}

sequence.df <- function(n) {
  listed.n <- list.half.innings(n)
  played.n <- played.half.innings(n)
  stopifnot(length(setdiff(played.n,listed.n))==0)
  unlikeliness <- unlist(lapply(listed.n,get.u.inning))
  played <- is.element(listed.n, played.n)
  return(data.frame(sequence=listed.n, played=played, u_sequence=unlikeliness, n=rep(n,length(listed.n)), stringsAsFactors = FALSE))
}

sequence.df.range <- function(r) {
  df <- data.frame(sequence=character(),played=logical(),u_sequence=double(), n=integer(), stringsAsFactors = FALSE)
  for (n in r) df <- rbind(df, sequence.df(n))
  return(df)
}

innings.plot <- function(df) {
  ggplot(data=df, mapping=aes(x=u_sequence, y=n, color=played)) + geom_jitter()
}

bcard.plot <- function(df) {
  ggplot(data=df, mapping=aes(x=u_sequence, y=n, color=played)) + geom_jitter() + theme_bw() + theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank() )
}

list.paths <- function(n) {
  # recursive function to add n states onto n-1 states
  n <- as.integer(n) # coerce to integer
  stopifnot(n >= 0)
  stopifnot(n < 50) # will get too large too fast
  if (n == 0) {
    return ('0')  # base of recursion
  }
  else {
    paths <- list.paths(n-1)
    more.paths <- c()
    for (p in paths) {
      next.paths <- continue.inning(p)
      more.paths <- c(more.paths, next.paths)
    }
  }
  return(more.paths)
}

continue.inning <- function(p) {
  # Takes the first k states of an inning and adds one.
  # If last states is XXX drops it and returns c() empty vector.
  if (grepl(':',p)) {
    last.state <- str_replace(p, "^.+:", "")
  } else last.state <- p
  continuations <- next.states(last.state)
  full.paths <- c()
  for (cont in continuations) {
    full.paths <- c(full.paths, paste(p, cont, sep=':'))
  }
  return (full.paths)
}

next.states <- function(from.string) {
  if (identical(from.string,'XXX')) return (c())
  batter <- TRUE
  runner1 <- grepl('1',from.string)
  runner2 <- grepl('2',from.string)
  runner3 <- grepl('3',from.string)
  outs <- str_count(from.string,'X')
  trans.list <- list.transitions(batter, runner1, runner2, runner3, outs)
  return (trans.list)
}

transitions.from <- function(from.string) {
  trans.list <- next.states(from.string)
  whole.transitions <- c()
  for (transition in trans.list) {
    whole.transitions <- c(whole.transitions, paste(from.string, transition, sep=':'))
  }
  return(whole.transitions)
}

transient.states <- function() {
  return(c("0","0X","0XX",
           "3","3X","3XX",
           "2","2X","2XX",
           "23","23X","23XX",
           "1","1X","1XX",
           "13","13X","13XX",
           "12","12X","12XX",
           "123","123X","123XX"))
}

all.transitions <- function() {
  from.states <- transient.states()
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
