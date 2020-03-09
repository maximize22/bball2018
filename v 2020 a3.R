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

http://lookup-service-prod.mlb.com
## turns out this is not much use for game data, but has team and player level data
## could we construct game data from player data - not sure, prob not.

http://gd2.mlb.com/components/game/mlb/
  
bbr.url <- 'https://www.baseball-reference.com/leagues/MLB/2019-schedule.shtml'
bbr.raw <- GET(bbr.url)
bbr.content <- content(bbr.raw, as="text")
doc = htmlParse(bbr.content, asText=TRUE)
tables = readHTMLTable(doc)
doc1 <- htmlTreeParse(bbr.url, useInternal = TRUE, asText = TRUE)

doc.html.name.value <- xpathApply(doc1, '//h2|//p', function(x) { list(name=xmlName(x), content=xmlValue(x)); })
