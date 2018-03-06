
away1 <- as.data.frame.matrix(table(bb.data$away.ML,bb.data$away.win))
home1 <- as.data.frame.matrix(table(bb.data$home.ML,bb.data$home.win))

away1$ML <- row.names(away1)
home1$ML <- row.names(home1)
away1 <- away1[,c(3,2,1)]
home1 <- home1[,c(3,2,1)]

away1$fav <- ifelse(grepl("\\+", away1$ML),"DOG","FAV")
away2 <- away1[order(away1$fav,away1$ML),]

home1$fav <- ifelse(grepl("\\+", home1$ML),"DOG","FAV")
home2 <- home1[order(home1$fav,home1$ML),]

home2$pct <- home2$W/ (home2$W + home2$L)

home2$MLbin <- as.integer(home2$ML) %/% 10
home.pct <- aggregate(home2[,c(2,3)], by = list(home2$fav,home2$MLbin), FUN=sum)
names(home.pct) <- c('fav','odds','W','L')
home.pct$wp <- home.pct$W / (home.pct$W + home.pct$L)
home.pct$home.pay <- ifelse(home.pct$fav == 'FAV', (home.pct$W*100*(-10/home.pct$odds)-home.pct$L*100)/(home.pct$W + home.pct$L),(home.pct$W*10*(home.pct$odds)-home.pct$L*100)/(home.pct$W + home.pct$L))

away2$MLbin <- as.integer(away2$ML) %/% 10
away.pct <- aggregate(away2[,c(2,3)], by = list(away2$fav,away2$MLbin), FUN=sum)
names(away.pct) <- c('fav','odds','W','L')
away.pct$wp <- away.pct$W / (away.pct$W + away.pct$L)
away.pct$away.pay <- ifelse(away.pct$fav == 'FAV', (away.pct$W*100*(-10/away.pct$odds)-away.pct$L*100)/(away.pct$W + away.pct$L),(away.pct$W*10*(away.pct$odds)-away.pct$L*100)/(away.pct$W + away.pct$L))

pd3$home.MLbin <- as.integer(pd3$home.ML) %/% 10
pd3$away.MLbin <- as.integer(pd3$away.ML) %/% 10

pd4 <- merge(pd3, home.pct[,c(2,6)], by.x=c('home.MLbin'),by.y=c('odds'), all.x=TRUE, all.y=FALSE)
pd4 <- merge(pd4, away.pct[,c(2,6)], by.x=c('away.MLbin'),by.y=c('odds'), all.x=TRUE, all.y=FALSE)
pd5 <- pd4[order(pd4$home.pay),]
pd5 <- merge(x=pd5,y=date.list, by='t.dates')
###pd5 <- pd5[,-c(9,11,12,14,19,22,24)]
pd5 <- pd5[,-c(10,12,13,15,20,23,25,33)]
###melt to get pitcher + team, w/l stats for home, w/l for away, merge, then merge with pd3 to show team stats

aa1 <- a1[,c('home.bbr.nm','home.win','dow')]
aa2 <- as.data.frame(count_(aa1, c("home.bbr.nm","dow","home.win")))
aa3 <- dcast(aa2, home.bbr.nm+dow~home.win,sum)
aa3$home.w.pct <- aa3$W / (aa3$W + aa3$L)

bb1 <- a1[,c('away.bbr.nm','away.win','dow')]
bb2 <- as.data.frame(count_(bb1, c("away.bbr.nm","dow","away.win")))
bb3 <- dcast(bb2, away.bbr.nm+dow~away.win,sum)
bb3$away.w.pct <- bb3$W / (bb3$W + bb3$L)


pd6 <- merge(pd5, aa3[,c(1,2,5)], by=c('home.bbr.nm','dow'), all.x=TRUE, all.y=FALSE)
pd6 <- merge(pd6, bb3[,c(1,2,5)], by=c('away.bbr.nm','dow'), all.x=TRUE, all.y=FALSE)
pd6 <- pd6[,-c(2,4,5,6,7,8,13,14,19,20,22,23)]
### reorder columns with dplyr
pd6 <- pd6 %>% select(2,1, everything())
pd6 <- pd6[order(pd6$home.pay),]

save(pd6, file=paste0(getwd(),"/devl/baseball/pd6.Rdata"))

p.away <- as.data.frame.matrix(table(bb.data$away.pitcher,bb.data$away.bbr.name,bb.data$away.win))
p.home <- as.data.frame.matrix(table(bb.data$home.pitcher,bb.data$home.win))
