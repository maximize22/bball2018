library(XML)
library(lubridate)
library(RCurl)
library(pitchRx)
library(plyr)
source(paste0(getwd(),"/devl/baseball/ref_fns.R"))

base.url <- "http://gd2.mlb.com/components/game/mlb/year_2016/"

#### work on power rankings to start
br.url <- 'http://www.baseball-reference.com/leagues/MLB/2016-standings.shtml'
br.extr <- (readHTMLTable(br.url, header = TRUE, as.data.frame=TRUE))
br.table <- as.data.frame(br.table$expanded_standings_overall)


tgt.date <- Sys.Date()

today.url <- set.dir.url(tgt.date)
tomorrow.url <- set.dir.url(tgt.date + 1)

### get list of today's guids
filenames = getURL(today.url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
gid.list <- gregexpr("gid.+>", filenames, perl=TRUE)
gids <- regmatches(filenames, gid.list)
g2 <- lapply(gids, function(x) substr(x,1,30))
g3 <- as.data.frame(g2, stringsAsFactors = FALSE)
colnames(g3) <- "today.gid"
g3$away.mlb.nm <- substr(g3$today.gid,16,18)
g3$home.mlb.nm <- substr(g3$today.gid,23,25)
g4 <- merge(x=g3, y=team.nm, by.x='away.mlb.nm', by.y='mlb.nm', all.y=FALSE)
g4 <- rename(g4, c("bbr.nm" = "away.bbr.nm"))
g4 <- merge(x=g4, y=team.nm, by.x='home.mlb.nm', by.y='mlb.nm', all.y=FALSE)
g4 <- rename(g4, c("bbr.nm" = "home.bbr.nm"))
### g4 has list of all games TODAY ###

away.table <- br.table
home.table <- br.table
colnames(away.table) <- paste("aw", colnames(away.table), sep =".")
colnames(home.table) <- paste("hm", colnames(home.table), sep =".")

g5 <- merge(x=g4, y=away.table, by.x = 'away.bbr.nm', by.y='aw.Tm', all.y=FALSE)
g5 <- merge(x=g5, y=home.table, by.x = 'home.bbr.nm', by.y='hm.Tm', all.y=FALSE)

i <- nrow(g5)

scoreboard.url <- paste0(tomorrow.url,"scoreboard.xml")
xmlfile <- xmlTreeParse(scoreboard.url)
xmltop <- xmlRoot(xmlfile)
tgt.game <- xmlToList(xmltop)
for(j in 1:i) {
  k <- j*5
  x <- c(tgt.game[[k-4]][1], tgt.game[[k-1]][1],tgt.game[[k-1]]$.attrs[2:4],tgt.game[[k]][1],tgt.game[[k]]$.attrs[2:4])
  if(j==1) {
    p.list <- as.data.frame(x, stringsAsFactors=FALSE)
  } else {
    p.list <- rbind(p.list, x)
  }
}
p.list$mlb.away.tm <- substr(p.list$id,12,14)
p.list$mlb.home.tm <- substr(p.list$id,19,21)
colnames(p.list) <- c('id','hm.pitcher','hm.pt.wins','hm.pt.loss','hm.pt.era','aw.pitcher','aw.pt.wins','aw.pt.loss','aw.pt.era','away.mlb.nm','home.mlb.nm')
g6 <- merge(x=g5, y=p.list, by.x='away.mlb.nm',by.y='away.mlb.nm',all.y=FALSE)

###should match on id rather than team; can do 1-2 days ahead despite some pitchers not fitting in
###we now have rankings and pitchers
###apply logic and subset factors to make choice
###get Vegas odds

###get pitcher and hitter rankings and apply
###recent bullpen usage



#with each row of g3
ln <- "linescore.xml"
ln.url <- paste0(today.url,g3[2,1],"/",ln)
#xmlToDataFrame(ln.url)
xmlfile <- xmlTreeParse(ln.url)
x <-xmlElementsByTagName(xmlRoot(xmlfile)[[1]], "game")

xmltop <- xmlRoot(xmlfile)
tgt.game <- xmlToList(xmltop)
tgt.attr <- as.data.frame(tgt.game$.attrs)
tgt.attr <- as.data.frame(tgt.game)




# Finally, get the data in a data-frame and have a look at the first rows and columns

plantcat_df <- data.frame(t(plantcat),row.names=NULL)
plantcat_df[1:5,1:4]




data <- readHTMLTable(paste0(ln.url, stringsAsFactors = FALSE))

dat <- scrape(start = "2013-06-01", end = "2013-06-01")
d1 <- scrape(start = '2016-04-03', end = '2016-04-03', suffix = 'game.xml')
data(gids, package = "pitchRx")
gids[grep("2016_04_.+", gids)]


http://www.vegasinsider.com/mlb/matchups/matchups.cfm/date/05-01-16
http://www.baseball-reference.com/leagues/MLB/2016-pitching-leaders.shtml

