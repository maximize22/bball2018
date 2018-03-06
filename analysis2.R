library(reshape2)
library(dplyr)

a1 <- merge(x=bb.data,y=date.list, by='t.dates')

### what do we want to find
### 1. team records home/away by day of the week

aa1 <- a1[,c('home.bbr.nm','home.win','dow')]
aa2 <- as.data.frame(count(aa1, c("home.bbr.nm","dow","home.win")))
aa3 <- dcast(aa2, home.bbr.nm+dow~home.win,sum)
aa3$home.w.pct <- aa3$W / (aa3$W + aa3$L)

bb1 <- a1[,c('away.bbr.nm','away.win','dow')]
bb2 <- as.data.frame(count(bb1, c("away.bbr.nm","dow","away.win")))
bb3 <- dcast(bb2, away.bbr.nm+dow~away.win,sum)
bb3$away.w.pct <- bb3$W / (bb3$W + bb3$L)





### 2. team records by favorite/underdog
### 3. team records at rounded betting lines
### 4. pitcher records, home and away
### 5. over/under results
### 6. team record vs R/L
### 7. NEED DATA record in time of day (getaway day games, west coast vs all other TZ grouped)
### 8. winner when line goes up or down

### other data
### bullpen - innings, pitches, last 2 games
### starters - total # of pitches last 3 starts
### starter proj innings pitched
### historic pitcher vs batters * number of expected ABs