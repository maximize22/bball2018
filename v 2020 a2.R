library("jsonlite")

### ref URL
###   https://appac.github.io/mlb-data-api-docs/#stats-data

json_team <- "http://lookup-service-prod.mlb.com/json/named.team_all_season.bam?sport_code=%27mlb%27&all_star_sw=%27N%27&sort_order=name_asc&season=%272018%27"
team_data <- fromJSON(json_team)

t1 <- as.data.frame(team_data$team_all_season$queryResults$row)
t2 <- t1[, c('name_display_full','mlb_org_id','time_zone_text','league_id','name_abbrev','league','division_abbrev'
              ,'team_id','team_code','division_id','season')]

json_games <- "http://lookup-service-prod.mlb.com/json/named.org_game_type_date_info.bam?current_sw=%27Y%27&sport_code=%27mlb%27game_type=%27R%27&season=%272018%27"
json_g_data <- fromJSON(file=json_games)


json_file <- "http://lookup-service-prod.mlb.com/json/named.player_info.bam?sport_code='mlb'&player_id='493316'"
json_data <- fromJSON(file=json_file)

#get 40 man roster,
json_roster <- "http://lookup-service-prod.mlb.com/json/named.roster_40.bam?team_id='144'"
json_data <- fromJSON(json_roster)

#player stats
http://lookup-service-prod.mlb.com/json/named.sport_hitting_tm.bam?league_list_id='mlb'&game_type='R'&season='2017'&player_id='493316'

#
