library(RMySQL)

con <- dbConnect(MySQL(), user='baseball', password='BabeRuth60!', dbname='retrotrans', 'host'='localhost')

close.db <- function() {
  dbDisconnect(con)
}

close.all <- function() {
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) 
    dbDisconnect(con) 
}

query <- function(...) {
  dbGetQuery(con, ...)
}

ttable <- query("SELECT * FROM transitions")
row.names(ttable) <- ttable$transition
t0table <- data.frame(transition="3XX:23XX", count_transition=0, u_conditional=Inf, old_state="3XX", count_old_state=183472, u_old_state=1.814434)
row.names(t0table) <- t0table$transition
t0 = rbind(ttable,t0table)

get.transitions <- function(inning) {
  states <- strsplit(inning,':')
  old_states <- states[[1]][-length(states[[1]])]
  new_states <- states[[1]][-1]
  n <- length(old_states)
  transitions <- rep('',n)
  for (i in 1:n) {
    transitions[i] <- paste(old_states[i],':',new_states[i],sep='')
  }
  return(transitions)
}

get.u.inning <- function(inning) {
  transitions <- get.transitions(inning)
  nontransitions <- setdiff(transitions,all.transitions())
  if (length(nontransitions>0)) print(nontransitions)
  stopifnot(length(nontransitions)==0)  # Will throw error if any non-transitions
  u <- 0
  for (transition in transitions) {
    u <- u + t0[transition,]$u_conditional
  }
  return(u)
}

same.half.inning <- function(inning, batting, event) {
  return (inning==event$inning & batting==event$batting_team)
}

check.year.innings <- function(years) {
  for (y in years) {
    print(y)
    game.list <- query(paste("SELECT game_id FROM event_games WHERE year = ", y))
    for(i in 1:nrow(game.list)) {
      game <- game.list[i,]
      event.list <- query(paste("SELECT game_id, event_id, inning, batting_team, old_state, new_state, transition FROM plays WHERE game_id = '",game,"' ORDER BY event_id",sep=""))
      if (event.list[1,]$game_id == 'SEA200709261') next
      inning <- 1
      batting <- 0
      stopifnot(same.half.inning(inning, batting, event.list[1,]))
      for (j in 1:nrow(event.list)) {
        stopifnot(j==event.list[j,]$event_id)
        if (!same.half.inning(inning, batting, event.list[j,]))
          if (batting==0) batting <- 1
          else {
            stopifnot(batting==1)
            batting <- 0
            inning <- inning + 1
          }
          stopifnot(same.half.inning(inning, batting, event.list[j,]))
      }
    }
  }
}
