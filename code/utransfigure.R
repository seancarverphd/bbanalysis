library(RMySQL)
library(stringr)
library(ggplot2)

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

trans <- query("SELECT * FROM transitions;")
ostates <- query("SELECT * FROM old_states")

# p <- ggplot(data=trans, mapping=aes(x=u_old_state, y=u_conditional, group=transition)) + geom_point()
# p <- ggplotly(p)
# p

q <- ggplot(data=trans, mapping=aes(x=as.numeric(factor(rank(u_old_state))), y=u_conditional, label=transition)) + geom_text(size=2)
q
