library(RMySQL)

con <- dbConnect(MySQL(), user='baseball', password='BabeRuth60!', dbname='retrosheet', 'host'='localhost')

close.db <- function() {
  dbDisconnect(con)
}

query <- function(qtext) {
  if (length(dbListResults(con)) > 0) {
    dbClearResult(dbListResults(con)[[1]])
  }
  q <- dbSendQuery(con, qtext)
  return(q)
}

add.year.innings <- function(years) {
  for (y in years) {
    print(y)
  }
}

