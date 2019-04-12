library(RMySQL)

con <- dbConnect(MySQL(), user='baseball', password='BabeRuth60!', dbname='retrosheet', 'host'='localhost')

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

same.half.inning <- function(inning, batting, event) {
  return (inning==event$inning & batting==event$batting_team)
}

check.year.games <- function(years) {
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

check.year.innings <- function(years) {
  for (y in years) {
    print(y)
    game.list <- query(paste("SELECT * FROM event_games WHERE year = ", y))
    for(i in 1:nrow(game.list)) {
      game <- game.list[i,]
      event.list <- query(paste("SELECT game_id, event_id, inning, batting_team, old_state, new_state, transition FROM plays WHERE game_id = '",game,"' ORDER BY event_id",sep=""))
      if (event.list[1,]$game_id == 'SEA200709261') next
      sequence <- '0'
      inning <- 1
      batting <- 0
      overlap <- '0'
      for (j in 1:nrow(event.list)) {
        event <- event.list[j,]
        if (same.half.inning(inning, batting, event)) {
          old_state <- event$old_state
          old_strip <- substr(old_state,1,nchar(old_state)-1)
          stopifnot(identical(old_strip, overlap))
          sequence <- paste(sequence, event$new_state)
          new_state <- event$new_state
          overlap <- substr(new_state,2,nchar(new_state))
        }
      }
    }
  }
}
paste.values <- function(...) {
  arguments <- list(...)
  first <- TRUE
  for (a in arguments)
    if (first) {
      whole <- a
      first <- FALSE
    }
    else whole <- paste(whole,',',a,sep='')
  return (whole)
}

q <- function(inner) {
  return (paste("'",inner,"'",sep=''))
}

insert.half.inning <- function(game, inning, batting_team, overlap, sequence) {
  game_id <- game$game_id
  # inning <- inning
  # batting_team <- batting_team
  home_team <- game$home_team
  visiting_team <- game$visiting_team
  if (batting_team == 0) {
    at_bat <- visiting_team
    at_field <- home_team
  }
  else {
    at_bat <- home_team
    at_field <- visiting_team
  }
  year <- game$year
  date <- game$date
  double_header_game <- game$double_header_game
  finalxxx <- identical(overlap, 'XXX')
  all_batter <- !grepl('[.]',sequence)
  # sequence <- sequence
  u_sequence <- get.u.inning(sequence)
  list.cols <- "game_id, inning, batting_team, home_team, visiting_team, at_bat, at_field, year, date, double_header_game, finalxxx, all_batter, sequence, u_sequence"
  list.vals <- paste.values(q(game_id), inning, batting_team, q(home_team), q(visiting_team), q(at_bat), q(at_field), year, q(date), double_header_game, finalxxx, all_batter, q(sequence), u_sequence)
  query(paste("INSERT INTO innings1930 (", list.cols, ") VALUES (", list.vals, ");",sep='')) 
}
  
make.year.innings <- function(years) {
  for (y in years) {
    print(y)
    game.list <- query(paste("SELECT * FROM event_games WHERE year = ", y))
    for(i in 1:nrow(game.list)) {
      game <- game.list[i,]
      event.list <- query(paste("SELECT game_id, event_id, inning, batting_team, old_state, new_state, transition FROM plays WHERE game_id = '",game,"' ORDER BY event_id",sep=""))
      if (event.list[1,]$game_id == 'SEA200709261') next
      sequence <- '0'
      inning <- 1
      batting <- 0
      overlap <- '0'
      for (j in 1:nrow(event.list)) {  # loop over events in game
        event <- event.list[j,]
        if (same.half.inning(inning, batting, event)) {  # runs if event continues half.inning
          old_state <- event$old_state
          old_strip <- substr(old_state,1,nchar(old_state)-1)
          #if (!identical(old_strip, overlap)) {
          #  print(old_strip)
          #  print(overlap)
          # }
          stopifnot(identical(old_strip, overlap))
          new_state <- event$new_state
          sequence <- paste(sequence, new_state, sep='')
          overlap <- substr(new_state,2,nchar(new_state))
        } 
        else {  # runs if event starts new half.inning old_state 0, new_state ??
          # insert completed inning into database
          insert.half.inning(game, inning, batting, overlap, sequence)
          new_state <- event$new_state
          sequence <- paste('0',new_state,sep='')
          overlap <- substr(new_state,2,nchar(new_state))
          # Now update inning counters
          if (batting==0) batting <- 1
          else {
            stopifnot(batting==1)
            batting <- 0
            inning <- inning + 1
          }
        }
      }
      # insert completed last half.inning (in game) into database
      insert.half.inning(game, inning, batting, overlap, sequence)
    }
  }
}

make.year.innings(1930)
