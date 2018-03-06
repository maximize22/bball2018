library(XML)
library(lubridate)
library(RCurl)
library(pitchRx)
library(plyr)
library(httr)
library(stringr)
library(reshape2)
library(dplyr)
library(magrittr)

###get vegas info
###current date or next day
###historical

source(paste0(getwd(),"/devl/baseball/ref_fns.R"))

tgt.date <- Sys.Date() + 1
save.date <- Sys.Date() - 2

###future date
vegas.url <- set.vegas.url('http://www.vegasinsider.com/mlb/matchups/matchups.cfm/date/',(tgt.date))
vegas.raw <- GET(vegas.url)
vegas.content <- content(vegas.raw, as="text")
doc = htmlParse(vegas.content, asText=TRUE)
tables = readHTMLTable(doc)
t1 <- tables[[8]]
t.row <- nrow(t1)
pitching.data <- as.data.frame(t1[52:t.row,],stringsAsFactors = FALSE)
names(pitching.data) <- c('teams','pitchers', 'WL','Streak','ML','OU','CurML','CurOU','CurRun','MLTrend','OUTrend')
pd2 <- subset(pitching.data, pitching.data$pitchers != 'NA')
pd2 <- subset(pd2, pd2$pitchers != 'Pitchers')
pd2 <- subset(pd2, pd2$pitchers != 'Record')
pd2 <- subset(pd2, pd2$Streak != 'NA')
###sure this could be done quicker with plyr
pd2$teams <- gsub("Â"," ", pd2$teams)
pd2$pitchers <- gsub("Â"," ", pd2$pitchers)
pd2$WL <- gsub("Â"," ", pd2$WL)
pd2$Streak <- gsub("Â"," ", pd2$Streak)
pd2$ML <- gsub("Â"," ", pd2$ML)
pd2$OU <- gsub("Â"," ", pd2$OU)
pd2$CurML <- gsub("Â"," ", pd2$CurML)
pd2$CurOU <- gsub("Â"," ", pd2$CurOU)
pd2$CurRun <- gsub("Â"," ", pd2$CurRun)
pd2$MLTrend <- gsub("Â"," ", pd2$MLTrend)
pd2$OUTrend <- gsub("Â"," ", pd2$OUTrend)
pd2$teams <- substr(pd2$teams,5,nchar(pd2$teams)-2)
pd2$pitchers <- gsub("[[:digit:]]","",pd2$pitchers)
pd2$pitchers <- gsub("\\(-\\)","",pd2$pitchers)
### can't get this to work, undefined cols selected
###pd3 <- join(x=pd2, y=team.nm, by= c("teams"="insider.nm"))
home.info <- pd2[ !c(TRUE,FALSE), ]
away.info <- pd2[ c(TRUE,FALSE), ]
names(home.info) <- c('home.team.long','home.pitcher', 'home.WL','home.Streak','home.ML','home.OU','home.CurML','home.CurOU','home.CurRun','home.MLTrend','home.OUTrend')
names(away.info) <- c('away.team.long','away.pitcher', 'away.WL','away.Streak','away.ML','away.OU','away.CurML','away.CurOU','away.CurRun','away.MLTrend','away.OUTrend')
pd3 <- cbind(home.info, away.info)
pd3 <- merge(x=pd3, y=team.nm[,c(1,3,4)], by.x='home.team.long', by.y='insider.nm', all.y=FALSE)
pd3 <- rename(pd3, home.bbr.nm = bbr.nm, home.league.nm = league.nm)
pd3 <- merge(x=pd3, y=team.nm[,c(1,3,4)], by.x='away.team.long', by.y='insider.nm', all.y=FALSE)
pd3 <- rename(pd3, away.bbr.nm = bbr.nm, away.league.nm = league.nm)
pd3$t.dates <- tgt.date




###historical

# 2016 early d.list <- as.data.frame(seq(from=as.Date("2016-04-03"),to=as.Date("2016-07-10"),by="days"))  #2016-07-10 is the last day before all-star break
d.list <- as.data.frame(seq(from=as.Date("2017-04-02"),to=as.Date("2017-07-09"),by="days"))  #2016-07-10 is the last day before all-star break
d.list <- as.data.frame(seq(from=as.Date("2017-07-14"),to=as.Date("2017-10-01"),by="days"))  #2016-07-10 is the last day before all-star break


names(d.list) <- c("t.dates")

all.data <- ddply(d.list, c("t.dates"), .fun = function(x) {

  vegas.url <- set.vegas.url('http://www.vegasinsider.com/mlb/matchups/matchups.cfm/date/',(x$t.dates))
  vegas.raw <- GET(vegas.url)
  vegas.content <- content(vegas.raw, as="text")
  doc = htmlParse(vegas.content, asText=TRUE)
  tables = readHTMLTable(doc)
  t1 <- tables[[7]]  #was 8 in 2016
  
  print(x$t.dates)
  t.row <- nrow(t1)
  pitching.data <- as.data.frame(t1[52:t.row,],stringsAsFactors = FALSE)
  names(pitching.data) <- c('teams','pitchers', 'WL','Streak','ML','OU','CurML','CurOU','CurRun','MLTrend','OUTrend','ATS')
  pd2 <- subset(pitching.data, pitching.data$pitchers != 'NA')
  pd2 <- subset(pd2, pd2$pitchers != 'Pitchers')
  pd2 <- subset(pd2, pd2$pitchers != 'Record')
  pd2 <- subset(pd2, pd2$Streak != 'NA')
  # for historical take the winning character and add it
  pd2$win <- ifelse(grepl("Ã\u0082Â", pd2$teams),"W","L")
  pd2$teams <- gsub("Ã\u0082Â«"," ", pd2$teams, fixed=TRUE)
  ###sure this could be done quicker with plyr
  pd2$teams <- gsub("Â"," ", pd2$teams)
  pd2$pitchers <- gsub("Â"," ", pd2$pitchers)
  pd2$WL <- gsub("Â"," ", pd2$WL)
  pd2$Streak <- gsub("Â"," ", pd2$Streak)
  pd2$ML <- gsub("Â"," ", pd2$ML)
  pd2$OU <- gsub("Â"," ", pd2$OU)
  pd2$CurML <- gsub("Â"," ", pd2$CurML)
  pd2$CurOU <- gsub("Â"," ", pd2$CurOU)
  pd2$CurRun <- gsub("Â"," ", pd2$CurRun)
  pd2$MLTrend <- gsub("Â"," ", pd2$MLTrend)
  pd2$OUTrend <- gsub("Â"," ", pd2$OUTrend)
  pd2$ATS <- gsub("Â"," ", pd2$ATS)
  pd2$teams <- substr(pd2$teams,5,nchar(pd2$teams)-2)
  pd2$teams <- str_trim(pd2$teams, "right")
  if(x$t.dates == as.Date("2016-04-03")) {
    pd2 <- pd2[c(7,8),]
  }
  ### can't get this to work, undefined cols selected
  ###pd3 <- join(x=pd2, y=team.nm, by= c("teams"="insider.nm"))
  home.info <- pd2[ !c(TRUE,FALSE), ]
  away.info <- pd2[ c(TRUE,FALSE), ]
  names(home.info) <- c('home.team.long','home.pitcher', 'home.WL','home.Streak','home.ML','home.OU','home.CurML','home.CurOU','home.CurRun','home.MLTrend','home.OUTrend','home.ATS','home.win')
  names(away.info) <- c('away.team.long','away.pitcher', 'away.WL','away.Streak','away.ML','away.OU','away.CurML','away.CurOU','away.CurRun','away.MLTrend','away.OUTrend','away.ATS','away.win')
  pd3 <- cbind(home.info, away.info)
  pd3 <- merge(x=pd3, y=team.nm[,c(1,3,4)], by.x='home.team.long', by.y='insider.nm', all.y=FALSE)
  pd3 <- rename(pd3, home.bbr.nm = bbr.nm, home.league.nm = league.nm)
  pd3 <- merge(x=pd3, y=team.nm[,c(1,3,4)], by.x='away.team.long', by.y='insider.nm', all.y=FALSE)
  pd3 <- rename(pd3, away.bbr.nm = bbr.nm, away.league.nm = league.nm)
  pd3$home.ATS <- ifelse(grepl("Cover",pd3$home.ATS),pd3$away.ATS,pd3$home.ATS)
  pd3$away.ATS <- ifelse(grepl("Cover",pd3$away.ATS),pd3$home.ATS,pd3$away.ATS)
  pd3 <- subset(pd3, pd3$home.ATS != 'NA')   ### remove rainouts

  return(pd3)
  }
)

bb.data <- all.data
save(bb.data, file=paste0(getwd(),"/devl/baseball/vegas.Rdata"))


load(file=paste0(getwd(),"/devl/baseball/vegas.Rdata"))
max.dt <- max(bb.data$t.dates) +1

###redo 7/17, wasn't complete... maybe remove the last day each time and redo?
###just get through yesterday, then add on today
#d.list <- as.data.frame(seq(from=max.dt,to=Sys.Date()-1,by="days"))  #2016-07-10 is the last day before all-star break
d.list <- as.data.frame(seq(from=max.dt,to=as.Date('2016-10-02'),by="days"))  #2016-07-10 is the last day before all-star break


names(d.list) <- c("t.dates")

all.data <- ddply(d.list, c("t.dates"), .fun = function(x) {
  
  vegas.url <- set.vegas.url('http://www.vegasinsider.com/mlb/matchups/matchups.cfm/date/',(x$t.dates))
  vegas.raw <- GET(vegas.url)
  vegas.content <- content(vegas.raw, as="text")
  doc = htmlParse(vegas.content, asText=TRUE)
  tables = readHTMLTable(doc)
  t1 <- tables[[7]]
  print(x$t.dates)
  t.row <- nrow(t1)
  pitching.data <- as.data.frame(t1[52:t.row,],stringsAsFactors = FALSE)
  names(pitching.data) <- c('teams','pitchers', 'WL','Streak','ML','OU','CurML','CurOU','CurRun','MLTrend','OUTrend','ATS')
  pd2 <- subset(pitching.data, pitching.data$pitchers != 'NA')
  pd2 <- subset(pd2, pd2$pitchers != 'Pitchers')
  pd2 <- subset(pd2, pd2$pitchers != 'Record')
  pd2 <- subset(pd2, pd2$Streak != 'NA')
  # for historical take the winning character and add it
  pd2$win <- ifelse(grepl("Ã\u0082Â", pd2$teams),"W","L")
  pd2$teams <- gsub("Ã\u0082Â«"," ", pd2$teams, fixed=TRUE)
  ###sure this could be done quicker with plyr
  pd2$teams <- gsub("Â"," ", pd2$teams)
  pd2$pitchers <- gsub("Â"," ", pd2$pitchers)
  pd2$WL <- gsub("Â"," ", pd2$WL)
  pd2$Streak <- gsub("Â"," ", pd2$Streak)
  pd2$ML <- gsub("Â"," ", pd2$ML)
  pd2$OU <- gsub("Â"," ", pd2$OU)
  pd2$CurML <- gsub("Â"," ", pd2$CurML)
  pd2$CurOU <- gsub("Â"," ", pd2$CurOU)
  pd2$CurRun <- gsub("Â"," ", pd2$CurRun)
  pd2$MLTrend <- gsub("Â"," ", pd2$MLTrend)
  pd2$OUTrend <- gsub("Â"," ", pd2$OUTrend)
  pd2$ATS <- gsub("Â"," ", pd2$ATS)
  pd2$teams <- substr(pd2$teams,5,nchar(pd2$teams)-2)
  pd2$teams <- str_trim(pd2$teams, "right")
  if(x$t.dates == as.Date("2016-04-03")) {
    pd2 <- pd2[c(7,8),]
  }
  ### can't get this to work, undefined cols selected
  ###pd3 <- join(x=pd2, y=team.nm, by= c("teams"="insider.nm"))
  home.info <- pd2[ !c(TRUE,FALSE), ]
  away.info <- pd2[ c(TRUE,FALSE), ]
  names(home.info) <- c('home.team.long','home.pitcher', 'home.WL','home.Streak','home.ML','home.OU','home.CurML','home.CurOU','home.CurRun','home.MLTrend','home.OUTrend','home.ATS','home.win')
  names(away.info) <- c('away.team.long','away.pitcher', 'away.WL','away.Streak','away.ML','away.OU','away.CurML','away.CurOU','away.CurRun','away.MLTrend','away.OUTrend','away.ATS','away.win')
  pd3 <- cbind(home.info, away.info)
  pd3 <- merge(x=pd3, y=team.nm[,c(1,3,4)], by.x='home.team.long', by.y='insider.nm', all.y=FALSE)
  pd3 <- rename(pd3, home.bbr.nm = bbr.nm, home.league.nm = league.nm)
  pd3 <- merge(x=pd3, y=team.nm[,c(1,3,4)], by.x='away.team.long', by.y='insider.nm', all.y=FALSE)
  pd3 <- rename(pd3, away.bbr.nm = bbr.nm, away.league.nm = league.nm)
  
  pd3$home.ATS <- ifelse(grepl("Cover",pd3$home.ATS),pd3$away.ATS,pd3$home.ATS)
  pd3$away.ATS <- ifelse(grepl("Cover",pd3$away.ATS),pd3$home.ATS,pd3$away.ATS)
  pd3 <- subset(pd3, pd3$home.ATS != 'NA')   ### remove rainouts
  
  return(pd3)
}
)

bb.data <- rbind(bb.data, all.data)  # add new each day

bb.data17 <- bb.data # rename for 2017

save(bb.data17, file=paste0(getwd(),"/devl/baseball/vegas2017.Rdata"))


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
save(b16, file=paste0(getwd(),"/devl/baseball/vegas.Rdata")) ### this changes the data, should be added to the input script

###combine old files
load(file=paste0(getwd(),"/devl/baseball/vegas2016.Rdata")) # b16
b2016 <- b16
b2016$away.win.diff <- b2016$home.W - b2016$away.W
b2016$away.loss.diff <- b2016$home.L - b2016$away.L
b2016$yr <- 2016

load(file=paste0(getwd(),"/devl/baseball/vegas2017.Rdata")) # bb.data17
b2017 <- b16
b2017$yr <- 2017

b.comb <- rbind(b2016,b2017)
