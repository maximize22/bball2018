

pad.zero <- function(in.txt) {
  if(in.txt < 10) {
    out.txt <- paste0("0",as.character(in.txt))
  } else { 
    out.txt <- as.character(in.txt)
  }
  return(out.txt)
}

set.dir.url <-function(tgt.date) {
  #get year and month, build URL
  month.part <- month(as.POSIXlt(tgt.date, format="%Y-%m-%d"))
  day.part <- day(as.POSIXlt(tgt.date, format="%Y-%m-%d"))
  
  mp <- pad.zero(month.part)
  dp <- pad.zero(day.part)
  
  dir.url <- paste0(base.url,"month_",mp,"/day_",dp,"/")
  return(dir.url)
}

set.vegas.url <-function(in.url, tgt.date) {
  #get year and month, build URL
  month.part <- month(as.POSIXlt(tgt.date, format="%Y-%m-%d"))
  day.part <- day(as.POSIXlt(tgt.date, format="%Y-%m-%d"))
  year.part <- year(as.POSIXlt(tgt.date, format="%Y-%m-%d"))
  
  mp <- pad.zero(month.part)
  dp <- pad.zero(day.part)
  yp <- substr(year.part, 3,4)
  
  dir.url <- paste0(in.url,mp,"-",dp,"-",yp)
  return(dir.url)
}

bbr.nm = c('LAA','TEX','ATL','CHC','CHW','BAL','CIN','PIT','CLE','PHI','COL','ARI','DET','MIN','HOU','OAK','KCR','SEA','MIA','MIL'
           ,'NYY','BOS','SDP','LAD','SFG','NYM','TOR','TBR','WSN','STL')
mlb.nm = c('ana','tex','atl','chn','cha','bal','cin','pit','cle','phi','col','ari','det','min','hou','oak','kca','sea','mia','mil'
           ,'nya','bos','sdn','lan','sfn','nyn','tor','tba','was','sln')
insider.nm = c('L.A. Angels','Texas','Atlanta','Chi. Cubs','Chi. White Sox','Baltimore','Cincinnati','Pittsburgh','Cleveland','Philadelphia',
          'Colorado','Arizona','Detroit','Minnesota','Houston','Oakland','Kansas City','Seattle','Miami','Milwaukee',
          'N.Y. Yankees','Boston','San Diego','L.A. Dodgers','San Francisco','N.Y. Mets','Toronto','Tampa Bay','Washington','St. Louis')
league.nm = c('AL','AL','NL','NL','AL','AL','NL','NL','AL','NL',
              'NL','NL','AL','AL','AL','AL','AL','AL','NL','NL',
              'AL','AL','NL','NL','NL','NL','AL','AL','NL','NL')
### time zone 1 = EST, 4 = PST
time.zone = c(4,2,1,2,2,1,1,1,1,1,3,3,1,2,2,4,2,4,1,2
              ,1,1,4,4,4,1,1,1,1,2)

team.nm = data.frame(bbr.nm,mlb.nm,insider.nm,league.nm,time.zone)

date.list <- as.data.frame(seq(from=as.Date("2016-04-03"),to=as.Date("2016-10-10"),by="days"))
names(date.list) <- c('t.dates')
date.list$dow <- weekdays(date.list$t.dates)
date.list$month.id <- month(as.POSIXlt(date.list$t.dates, format="%Y-%m-%d"))
