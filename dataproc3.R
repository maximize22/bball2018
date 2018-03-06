library(dplyr)
library(tidyr)

game.log <- read.csv(file=paste0(getwd(),"/devl/baseball/GL2016.TXT"),header=FALSE,stringsAsFactors = FALSE)

names(game.log) <- c("date.id","game.format","day.of.week","away.team.nm","away.team.lg","away.game.nb","home.team.nm","home.team.lg","home.game.nb","away.score","home.score","total.outs",
                    "day.night","postpone.info","forfeit.info","protest.info","park.id","attendance","time.in.mins","away.line.score","home.line.score",
                    "aw.at.bats","aw.hits","aw.dbls","aw.trpl","aw.hrs","aw.rbi","aw.sac.hit","aw.sac.fly","aw.hbp","aw.bb","aw.ibb","aw.ks","aw.sb","aw.cs","aw.gidp","aw.catcher.int","aw.lob",
                    "aw.pitchers.used","aw.indiv.er","aw.team.er","aw.wild.ptch","aw.balk",
                    "aw.putout","aw.assist","aw.err","aw.pass.ball","aw.dbl.ply","aw.trpl.ply",
                    "hm.at.bats","hm.hits","hm.dbls","hm.trpl","hm.hrs","hm.rbi","hm.sac.hit","hm.sac.fly","hm.hbp","hm.bb","hm.ibb","hm.ks","hm.sb","hm.cs","hm.gidp","hm.catcher.int","hm.lob",
                    "hm.pitchers.used","hm.indiv.er","hm.team.er","hm.wild.ptch","hm.balk",
                    "hm.putout","hm.assist","hm.err","hm.pass.ball","hm.dbl.ply","hm.trpl.ply",
                    "hp.ump.id","hp.ump.name","1b.ump.id","1b.ump.name","2b.ump.id","2b.ump.name","3b.ump.id","3b.ump.name","lf.ump.id","lf.ump.nm","rf.ump.id","rf.ump.nm",
                    "away.mgr.id","away.mgr.name","home.mgr.id","home.mgr.name","win.pitch.id","win.pitch.name","los.pitch.id","los.pitch.name","sav.pitch.id","sav.pitch.name",
                    "gw.rbi.bat.id","gw.rbi.bat.name","away.start.pitch.id","away.start.pitch.name","home.start.pitch.id","home.start.pitch.name",
                    "aw.bat1.id","aw.bat1.nm","aw.bat1.pos",
                    "aw.bat2.id","aw.bat2.nm","aw.bat2.pos",
                    "aw.bat3.id","aw.bat3.nm","aw.bat3.pos",
                    "aw.bat4.id","aw.bat4.nm","aw.bat4.pos",
                    "aw.bat5.id","aw.bat5.nm","aw.bat5.pos",
                    "aw.bat6.id","aw.bat6.nm","aw.bat6.pos",
                    "aw.bat7.id","aw.bat7.nm","aw.bat7.pos",
                    "aw.bat8.id","aw.bat8.nm","aw.bat8.pos",
                    "aw.bat9.id","aw.bat9.nm","aw.bat9.pos",
                    "hm.bat1.id","hm.bat1.nm","hm.bat1.pos",
                    "hm.bat2.id","hm.bat2.nm","hm.bat2.pos",
                    "hm.bat3.id","hm.bat3.nm","hm.bat3.pos",
                    "hm.bat4.id","hm.bat4.nm","hm.bat4.pos",
                    "hm.bat5.id","hm.bat5.nm","hm.bat5.pos",
                    "hm.bat6.id","hm.bat6.nm","hm.bat6.pos",
                    "hm.bat7.id","hm.bat7.nm","hm.bat7.pos",
                    "hm.bat8.id","hm.bat8.nm","hm.bat8.pos",
                    "hm.bat9.id","hm.bat9.nm","hm.bat9.pos",
                    "addl.info","info.complete.yn")


###clean up columns
game.log <- game.log[, !(colnames(game.log) %in% c("info.complete.yn","addl.info","forfeit.info"))]
game.log$away.team.nm <- tolower(game.log$away.team.nm)
game.log$home.team.nm <- tolower(game.log$home.team.nm)
game.log$t.date <- as.Date(as.character(game.log$date.id), "%Y%m%d")

### get inning scores, figure out how to mash the 10+ innings
inning.log <- game.log[, (colnames(game.log) %in% c("date.id","away.team.nm","home.team.nm","away.line.score","home.line.score"))]
###brute force
inning.log$aw.i1 <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,2,3),substr(inning.log$away.line.score,1,1))
inning.log$away.line.score <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,4,nchar(inning.log$away.line.score)),substr(inning.log$away.line.score,2,nchar(inning.log$away.line.score)))
inning.log$aw.i2 <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,2,3),substr(inning.log$away.line.score,1,1))
inning.log$away.line.score <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,4,nchar(inning.log$away.line.score)),substr(inning.log$away.line.score,2,nchar(inning.log$away.line.score)))                                    
inning.log$aw.i3 <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,2,3),substr(inning.log$away.line.score,1,1))
inning.log$away.line.score <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,4,nchar(inning.log$away.line.score)),substr(inning.log$away.line.score,2,nchar(inning.log$away.line.score)))                                    
inning.log$aw.i4 <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,2,3),substr(inning.log$away.line.score,1,1))
inning.log$away.line.score <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,4,nchar(inning.log$away.line.score)),substr(inning.log$away.line.score,2,nchar(inning.log$away.line.score)))                                    
inning.log$aw.i5 <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,2,3),substr(inning.log$away.line.score,1,1))
inning.log$away.line.score <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,4,nchar(inning.log$away.line.score)),substr(inning.log$away.line.score,2,nchar(inning.log$away.line.score)))                                    
inning.log$aw.i6 <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,2,3),substr(inning.log$away.line.score,1,1))
inning.log$away.line.score <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,4,nchar(inning.log$away.line.score)),substr(inning.log$away.line.score,2,nchar(inning.log$away.line.score)))                                    
inning.log$aw.i7 <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,2,3),substr(inning.log$away.line.score,1,1))
inning.log$away.line.score <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,4,nchar(inning.log$away.line.score)),substr(inning.log$away.line.score,2,nchar(inning.log$away.line.score)))                                    
inning.log$aw.i8 <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,2,3),substr(inning.log$away.line.score,1,1))
inning.log$away.line.score <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,4,nchar(inning.log$away.line.score)),substr(inning.log$away.line.score,2,nchar(inning.log$away.line.score)))                                    
inning.log$aw.i9 <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,2,3),substr(inning.log$away.line.score,1,1))
inning.log$away.line.score <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,4,nchar(inning.log$away.line.score)),substr(inning.log$away.line.score,2,nchar(inning.log$away.line.score)))                                    
inning.log$aw.i10 <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,2,3),substr(inning.log$away.line.score,1,1))
inning.log$away.line.score <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,4,nchar(inning.log$away.line.score)),substr(inning.log$away.line.score,2,nchar(inning.log$away.line.score)))                                    
inning.log$aw.i11 <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,2,3),substr(inning.log$away.line.score,1,1))
inning.log$away.line.score <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,4,nchar(inning.log$away.line.score)),substr(inning.log$away.line.score,2,nchar(inning.log$away.line.score)))                                    
inning.log$aw.i12 <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,2,3),substr(inning.log$away.line.score,1,1))
inning.log$away.line.score <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,4,nchar(inning.log$away.line.score)),substr(inning.log$away.line.score,2,nchar(inning.log$away.line.score)))                                    
inning.log$aw.i13 <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,2,3),substr(inning.log$away.line.score,1,1))
inning.log$away.line.score <- ifelse(substr(inning.log$away.line.score,1,1)=='(',substr(inning.log$away.line.score,4,nchar(inning.log$away.line.score)),substr(inning.log$away.line.score,2,nchar(inning.log$away.line.score)))                                    
inning.log$aw.i14 <- as.numeric(inning.log$away.line.score)


inning.log$hm.i1 <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,2,3),substr(inning.log$home.line.score,1,1))
inning.log$home.line.score <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,4,nchar(inning.log$home.line.score)),substr(inning.log$home.line.score,2,nchar(inning.log$home.line.score)))
inning.log$hm.i2 <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,2,3),substr(inning.log$home.line.score,1,1))
inning.log$home.line.score <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,4,nchar(inning.log$home.line.score)),substr(inning.log$home.line.score,2,nchar(inning.log$home.line.score)))
inning.log$hm.i3 <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,2,3),substr(inning.log$home.line.score,1,1))
inning.log$home.line.score <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,4,nchar(inning.log$home.line.score)),substr(inning.log$home.line.score,2,nchar(inning.log$home.line.score)))
inning.log$hm.i4 <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,2,3),substr(inning.log$home.line.score,1,1))
inning.log$home.line.score <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,4,nchar(inning.log$home.line.score)),substr(inning.log$home.line.score,2,nchar(inning.log$home.line.score)))
inning.log$hm.i5 <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,2,3),substr(inning.log$home.line.score,1,1))
inning.log$home.line.score <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,4,nchar(inning.log$home.line.score)),substr(inning.log$home.line.score,2,nchar(inning.log$home.line.score)))
inning.log$hm.i6 <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,2,3),substr(inning.log$home.line.score,1,1))
inning.log$home.line.score <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,4,nchar(inning.log$home.line.score)),substr(inning.log$home.line.score,2,nchar(inning.log$home.line.score)))
inning.log$hm.i7 <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,2,3),substr(inning.log$home.line.score,1,1))
inning.log$home.line.score <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,4,nchar(inning.log$home.line.score)),substr(inning.log$home.line.score,2,nchar(inning.log$home.line.score)))
inning.log$hm.i8 <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,2,3),substr(inning.log$home.line.score,1,1))
inning.log$home.line.score <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,4,nchar(inning.log$home.line.score)),substr(inning.log$home.line.score,2,nchar(inning.log$home.line.score)))
inning.log$hm.i9 <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,2,3),substr(inning.log$home.line.score,1,1))
inning.log$home.line.score <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,4,nchar(inning.log$home.line.score)),substr(inning.log$home.line.score,2,nchar(inning.log$home.line.score)))
inning.log$hm.i10 <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,2,3),substr(inning.log$home.line.score,1,1))
inning.log$home.line.score <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,4,nchar(inning.log$home.line.score)),substr(inning.log$home.line.score,2,nchar(inning.log$home.line.score)))
inning.log$hm.i11 <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,2,3),substr(inning.log$home.line.score,1,1))
inning.log$home.line.score <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,4,nchar(inning.log$home.line.score)),substr(inning.log$home.line.score,2,nchar(inning.log$home.line.score)))
inning.log$hm.i12 <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,2,3),substr(inning.log$home.line.score,1,1))
inning.log$home.line.score <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,4,nchar(inning.log$home.line.score)),substr(inning.log$home.line.score,2,nchar(inning.log$home.line.score)))
inning.log$hm.i13 <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,2,3),substr(inning.log$home.line.score,1,1))
inning.log$home.line.score <- ifelse(substr(inning.log$home.line.score,1,1)=='(',substr(inning.log$home.line.score,4,nchar(inning.log$home.line.score)),substr(inning.log$home.line.score,2,nchar(inning.log$home.line.score)))
inning.log$hm.i14 <- as.numeric(inning.log$home.line.score)

                                     
                                     