library(dplyr)
library(reshape2)
library(tidyr)
load(file=paste0(getwd(),"/vegas2018.Rdata")) # b16

bb <- bb.data18
bb$fav.out <- ifelse(bb$home.ML < 100 & bb$home.win == 'W', 'W', 'L')
bb$home.ML.grp <- round(as.integer(bb$home.ML) / 10)

bb1 <- bb %>%
  select(home.ML.grp,fav.out) %>%
  group_by(home.ML.grp,fav.out) %>%
  as.data.frame(tally())

bb1 <- dcast(bb1, home.ML.grp ~ fav.out)
write.csv(bb1,"wl18.csv")
