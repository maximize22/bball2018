
library(jsonlite)

### REFERENCE: https://www.reddit.com/r/Sabermetrics/comments/99yvdc/statsapi_working_documentation/


game.dt <- '2019-03-21'

get_all_game_ids_one_dt <- function(game.dt) {
  tgt.url <- paste0('http://statsapi.mlb.com/api/v1/schedule?sportId=1&date=',game.dt)
  j.data <- fromJSON(tgt.url)
  game.info <- as.data.frame(j.data$dates$games)
  game.info <- subset(game.info, game.info$gameType %in% c('R','P'))
  return(game.info)
}

###for 2019

#brute force names/dates
d.list <- as.data.frame(seq(from=as.Date("2019-03-20"),to=as.Date("2019-03-21"),by="days"))  #2016-07-10 is the last day before all-star break
e.list <- as.data.frame(seq(from=as.Date("2019-03-28"),to=as.Date("2019-07-07"),by="days"))
f.list <- as.data.frame(seq(from=as.Date("2019-07-11"),to=as.Date("2019-09-29"),by="days"))  #2016-07-10 is the last day before all-star break

names(d.list) <- c("t.dates")
names(e.list) <- c("t.dates")
names(f.list) <- c("t.dates")

d.list <- rbind(d.list, e.list)
d.list <- rbind(d.list, f.list)

all.game.data <- ddply(d.list, c("t.dates"), .fun = function(x) {
  g1 <- get_all_game_ids_one_dt(x$t.dates)
  return(g1)
}
)  
