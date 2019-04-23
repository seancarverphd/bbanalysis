library(RMySQL)
library(rlist)
library(stringr)
source("~/Code/bbanalysis/code/listpaths.R")

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

check.year.games <- function(years) {
  for (y in years) {
    print(y)
    year.game.list <- query(paste("SELECT game_id FROM event_games WHERE year = ", y))
    for(i in 1:nrow(year.game.list)) {
      game <- year.game.list[i,]
      game.event.list <- query(paste("SELECT game_id, event_id, inning, batting_team, old_state, new_state, transition FROM plays WHERE game_id = '",game,"' ORDER BY event_id",sep=""))
      if (game.event.list[1,]$game_id == 'SEA200709261') next
      inning <- 1
      batting <- 0
      stopifnot(same.half.inning(inning, batting, game.event.list[1,]))
      for (j in 1:nrow(game.event.list)) {
        stopifnot(j==game.event.list[j,]$event_id)
        if (!same.half.inning(inning, batting, game.event.list[j,]))
          if (batting==0) batting <- 1
          else {
            stopifnot(batting==1)
            batting <- 0
            inning <- inning + 1
          }
          stopifnot(same.half.inning(inning, batting, game.event.list[j,]))
      }
    }
  }
}

check.year.innings <- function(years) {
  for (y in years) {
    print(y)
    year.game.list <- query(paste("SELECT * FROM event_games WHERE year = ", y))
    for(i in 1:nrow(year.game.list)) {
      game <- year.game.list[i,]
      game.event.list <- query(paste("SELECT game_id, event_id, inning, batting_team, old_state, new_state, transition FROM plays WHERE game_id = '",game,"' ORDER BY event_id",sep=""))
      if (game.event.list[1,]$game_id == 'SEA200709261') next
      sequence <- '0'
      inning <- 1
      batting <- 0
      overlap <- '0'
      for (j in 1:nrow(game.event.list)) {
        event <- game.event.list[j,]
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
      whole <- paste('(',a,sep='')
      first <- FALSE
    }
    else whole <- paste(whole,',',a,sep='')
  return (paste(whole,')',sep=''))
}

q <- function(inner) {
  return (paste("'",inner,"'",sep=''))
}

prepare.insert <- function(table, game, inning, batting_team, overlap, sequence) {
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
  new.row <- paste.values(q(game_id), inning, batting_team, q(home_team), q(visiting_team), q(at_bat), q(at_field), year, q(date), double_header_game, finalxxx, all_batter, q(sequence), u_sequence)
  # insert.row(new.row, table)
  return(new.row)
}

insert.row <- function(new.row, table) {
  list.cols <- " (game_id, inning, batting_team, home_team, visiting_team, at_bat, at_field, year, date, double_header_game, finalxxx, all_batter, sequence, u_sequence)"
  #print(paste("INSERT INTO ", table, list.cols, " VALUES ",new.row))
  query(paste("INSERT INTO ", table, list.cols, " VALUES ",new.row))
}

insert.batch <- function(list.batch, table) {
  list.cols <- " (game_id, inning, batting_team, home_team, visiting_team, at_bat, at_field, year, date, double_header_game, finalxxx, all_batter, sequence, u_sequence)"
  # print(paste("INSERT INTO ", table, list.cols, " VALUES ",paste(list.batch,collapse=',')))
  query(paste("INSERT INTO ", table, list.cols, " VALUES ",paste(list.batch,collapse=',')))
}

init.inning.counter <- function(game) {
  # For the game SEA200709261 the home team batted first.  For no other game did this happen.
  if (identical(game$game_id,'SEA200709261')) counter <- list(1,1)
  else counter <- list(1,0)
  names(counter) <- c("inning", "batting")
  return (counter)
}

increment.inning.counter <- function(game, counter) {
  # For the game SEA200709261 the home team batted first.  For no other game did this happen.
  if (identical(game$game_id,'SEA200709261')) {
    if (counter$batting==1) counter$batting <- 0
    else {
      stopifnot(counter$batting==0)
      counter$batting <- 1
      counter$inning <- counter$inning + 1
    }
  }
  else {
    if (counter$batting==0) counter$batting <- 1  # Now update inning counters
    else {
      stopifnot(counter$batting==1)
      counter$batting <- 0
      counter$inning <- counter$inning + 1
    }
  }
  return (counter)
}

same.half.inning <- function(counter, event) {
  return (counter$inning==event$inning & counter$batting==event$batting_team)
}

batch.update <- function(list.batch, k, batch.size, table, n, y, t0, force=FALSE) {
  if (k%%batch.size==0 | force) {
    b <- as.integer(k/batch.size)
    # trim list.batch if not complete (last batch)
    list.batch <- list.batch[1:batch.index(k,batch.size)]
    insert.batch(list.batch, table)
    list.batch <- vector("list", batch.size)
    print(paste("year", y, "batch", b, "size", batch.size, "of", n))
    print(Sys.time() - t0)
  }
  return (list.batch)
}

batch.index <- function(k, batch.size) {
  return (((k-1)%%batch.size)+1)
}

make.year.innings <- function(years, table, batch.size) {
  for (y in years) {
    print(y)
    # Get n number of half.innings in year
    n <- query(paste("SELECT COUNT(DISTINCT(CONCAT(plays.game_id, inning, batting_team))) FROM plays INNER JOIN event_games ON event_games.game_id=plays.game_id WHERE year=",y,";",sep=""))
    n <- n[1,1]
    # Get all events for year
    year.event.list <- query(paste("SELECT plays.game_id, plays.event_id, plays.inning, plays.batting_team, old_state, new_state, transition FROM plays INNER JOIN event_games ON event_games.game_id = plays.game_id WHERE year=",y,sep=""))
    # Get all games for year
    game.list <- query(paste("SELECT * FROM event_games WHERE year = ", y))
    # Done pulling data print number of innings and games
    print(paste(n,"innings and", nrow(game.list), "games for year", y))
    t0 <- Sys.time()
    k <- 0
    list.batch <- vector("list",batch.size)
    for(i in 1:nrow(game.list)) {
      game <- game.list[i,]
      # Pull out events for the game
      game.event.list <- subset(year.event.list, game_id==game[1,1])
      # Should already be ordered but just in case
      game.event.list <- game.event.list[order(game.event.list$event_id),]
      # The following assertion tests (imperfectly) that events are in a 1:n sequence
      stopifnot(game.event.list[nrow(game.event.list), 2]==nrow(game.event.list))
      counter <- init.inning.counter(game)  # different only for SEA200709261
      sequence <- '0'
      overlap <- '0'
      for (j in 1:nrow(game.event.list)) {  # loop over events in game
        event <- game.event.list[j,]
        if (same.half.inning(counter, event)) {  # runs if event continues half.inning
          old_state <- event$old_state
          old_strip <- substr(old_state,1,nchar(old_state)-1)
          if (!identical(old_strip, overlap)) print(paste("game", game[1,1], "old_strip", old_strip, "overlap", overlap))
          stopifnot(identical(old_strip, overlap))
          new_state <- event$new_state
          sequence <- paste(sequence, new_state, sep='')
          overlap <- substr(new_state,2,nchar(new_state))
        } 
        else {  # runs if event starts new half.inning old_state 0, new_state ??
          k <- k+1  # insert previous completed inning into database
          list.batch[[batch.index(k,batch.size)]] <- prepare.insert(table, game, counter$inning, counter$batting, overlap, sequence)
          # print(list.batch[[batch.index(k,batch.size)]])
          list.batch <- batch.update(list.batch, k, batch.size, table, n, y, t0, force=FALSE)
          if (is.null(list.batch[[1]])) t0 <- Sys.time()  # if just updated, reset time counter
          new_state <- event$new_state
          sequence <- paste('0',new_state,sep='')
          overlap <- substr(new_state,2,nchar(new_state))
          counter <- increment.inning.counter(game, counter)
        }
      }
      k <- k+1  # insert completed last half.inning (in game) into database
      list.batch[[batch.index(k,batch.size)]] <- prepare.insert(table, game, counter$inning, counter$batting, overlap, sequence)
      # print(list.batch[[batch.index(k,batch.size)]])
      list.batch <- batch.update(list.batch, k, batch.size, table, n, y, t0, force=FALSE)
      if (is.null(list.batch[[1]])) t0 <- Sys.time()  # if just updated, reset time counter
    }
    stopifnot(k==n)
    if (!is.null(list.batch[[1]])) batch.update(list.batch, k, batch.size, table, n, y, t0, force=TRUE)  # if not just updated, update last partial batch
  }
}

redo.innings <- function() {
  make.year.innings(2013:2018,"innings",1000)
}
