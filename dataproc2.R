library(plyr)
library(dplyr)
library(magrittr)

### clean up the basic bb.data file
### currently has all of 2016 data in it
### clean and save with new name
### use as template, and bias the model with whole year data???


###shouldn't need this, loaded to vegas.R file
### JUST THE CODE AT THE BOTTOM

load(file=paste0(getwd(),"/devl/baseball/vegas2016.Rdata")) # b16
load(file=paste0(getwd(),"/devl/baseball/vegas2017.Rdata")) # bb.data17

b16 <- bb.data17

b16$home.pitcher.hand <- ifelse(grepl('(L)',b16$home.pitcher),'L','R')
b16$away.pitcher.hand <- ifelse(grepl('(L)',b16$away.pitcher),'L','R')
b16$home.pitcher <- sub('\\(L\\)\\s','',b16$home.pitcher)
b16$home.pitcher <- sub('\\(R\\)\\s','',b16$home.pitcher)
b16$away.pitcher <- sub('\\(L\\)\\s','',b16$away.pitcher)
b16$away.pitcher <- sub('\\(R\\)\\s','',b16$away.pitcher)
b16$home.W <- as.numeric(substr(b16$home.WL, 2, regexpr('-',b16$home.WL)-1))
b16$home.L <- as.numeric(substr(b16$home.WL, regexpr('-',b16$home.WL)+1,regexpr(')',b16$home.WL)-1))
b16$away.W <- as.numeric(substr(b16$away.WL, 2, regexpr('-',b16$away.WL)-1))
b16$away.L <- as.numeric(substr(b16$away.WL, regexpr('-',b16$away.WL)+1,regexpr(')',b16$away.WL)-1))
###brute force
b16$home.WL <- substr(b16$home.WL,regexpr(')',b16$home.WL)+3, nchar(b16$home.WL))
b16$away.WL <- substr(b16$away.WL,regexpr(')',b16$away.WL)+3, nchar(b16$away.WL))
b16$home.homeW <- as.numeric(substr(b16$home.WL, 1, regexpr('-',b16$home.WL)-1))
b16$home.homeL <- as.numeric(substr(b16$home.WL, regexpr('-',b16$home.WL)+1,regexpr(' ',b16$home.WL)-1))
b16$away.awayW <- as.numeric(substr(b16$away.WL, 1, regexpr('-',b16$away.WL)-1))
b16$away.awayL <- as.numeric(substr(b16$away.WL, regexpr('-',b16$away.WL)+1,regexpr(' ',b16$away.WL)-1))
b16$OUresult <- substr(b16$away.ATS,1,regexpr(':',b16$away.ATS)-1)
b16$OUscored <- as.numeric(substr(b16$away.ATS,regexpr(':',b16$away.ATS)+2,nchar(b16$away.ATS)))
b16$OUscored <- ifelse(is.na(b16$OUscored),1,b16$OUscored)
b16$OUscored <- ifelse(b16$OUscored==0,1,b16$OUscored)
###clean up columns
b16 <- b16[, !(colnames(b16) %in% c("home.WL","home.OU","home.CurOU","home.ATS","away.WL","away.MLTrend","away.OUTrend","away.ATS"))]

b16.1 <- merge(x=b16, y=team.nm[,c(1,5)], by.x = 'away.bbr.nm', by.y = 'bbr.nm', all.y=FALSE)
b16.1 <- plyr::rename(b16.1, c("time.zone" = "away.time.zone"))
b16.1 <- merge(x=b16.1, y=team.nm[,c(1,5)], by.x = 'home.bbr.nm', by.y = 'bbr.nm', all.y=FALSE)
b16.1 <- plyr::rename(b16.1, c('time.zone' = 'home.time.zone'))
b16.1$away.tz.diff <- b16.1$home.time.zone - b16.1$away.time.zone
b16.1$away.win.diff <- b16.1$home.W - b16.1$away.W
b16.1$away.loss.diff <- b16.1$home.L - b16.1$away.L
b16 <- b16.1
save(b16, file=paste0(getwd(),"/devl/baseball/vegas2017.Rdata")) ### this changes the data, should be added to the input script




###just fooling
summ.df <- b16 %>%
        group_by(home.bbr.nm, home.win,home.pitcher.hand,away.pitcher.hand) %>%
        summarize(
            Observations= n()
        )

summ.df2 <- b16 %>%
  group_by(away.bbr.nm, away.win,home.pitcher.hand,away.pitcher.hand) %>%
  summarize(
    Observations= n()
  )
names(summ.df) <- c('team.nm','result','team.pitcher.hand','opp.pitcher.hand','home.count')
names(summ.df2) <- c('team.nm','result','opp.pitcher.hand','team.pitcher.hand','away.count')
simple.rec <- merge(x=summ.df,y=summ.df2,  all.x = TRUE, all.y = TRUE)
write.csv(simple.rec, file=paste0(getwd(),"/devl/baseball/hands.txt"))

# not sure where I was going with this
#sr <- melt(simple.rec, id.vars = c('team.nm', 'opp.pitcher.hand', 'team.pitcher.hand'), measure.vars = c('home.count', 'away.count'))

sr.w <- subset(simple.rec, result == 'W', select= -c(result))
sr.l <- subset(simple.rec, result == 'L', select= -c(result))
names(sr.w) <- c('team.nm','tm.pit.h','opp.pit.h','home.wins','away.wins')
names(sr.l) <- c('team.nm','tm.pit.h','opp.pit.h','home.loss','away.loss')
sr.c <- merge(x=sr.w, y=sr.l, all.x=TRUE,all.y=TRUE)
sr.c$hwp <- sr.c$home.wins/(sr.c$home.loss+sr.c$home.wins)
sr.c$awp <- sr.c$away.wins/(sr.c$away.loss+sr.c$away.wins)
write.csv(sr.c, file=paste0(getwd(),"/devl/baseball/handspct16.txt"))

###now we have the vegas data.  can be cleaned and summarized more
### want to get more in season data
### scores of games
### game time
### added Day of the week elsewhere already
### box score and or bullpen innnings?

### structure - aggregate some season data

### figure out how to join to prior game (if any - null value if not)
### figure out how to join to 2+ prior days
