rm(list = ls())

library(dplyr)

load("~/Dropbox/Master_Project/code/Data/events data Prem Bund 22.RData")

events <- events %>% arrange(match_id, half, second, id, is.na(pos_x))

events <- distinct(events, match_id, player_id, team_id, action_id, opponent_id, half, second, pos_x, pos_y, pos_dest_x, pos_dest_y, .keep_all=TRUE)

events <- events %>% mutate(homeaway = ifelse(team_id == team1_id, "home", "away"))

## add XG using basic model

## Make sure player and team names have 1 to 1 correspondance with player and team ids

player_names <- events %>% select(player_id, player_name) %>% distinct(player_id, .keep_all = T) %>% na.omit()
team_names <- events %>% select(team_id, team_name) %>% distinct(team_id, .keep_all = T) %>% na.omit()

events <- events %>% select(-c(team_name, player_name, team1_name, team2_name, opponent_name, opponent_team_name))

events <- events %>% left_join(select(team_names, team_id, team_name), by = "team_id")
events <- events %>% left_join(select(team_names, team1_id = team_id, team1_name = team_name), by = "team1_id")
events <- events %>% left_join(select(team_names, team2_id = team_id, team2_name = team_name), by = "team2_id")
events <- events %>% left_join(select(team_names, opponent_team_id = team_id, opponent_team_name = team_name), by = "opponent_team_id")

events <- events %>% left_join(select(player_names, player_id, player_name), by = "player_id")
events <- events %>% left_join(select(player_names, opponent_id = player_id, opponent_name = player_name), by = "opponent_id")



matches <- events %>% 
  group_by(match_id, match_date, season_id, tournament_id, team1_name, team1_id, team1_score, team2_score, team2_id, team2_name, duration) %>% 
  summarise() %>% ungroup() %>% mutate(match_date = as.Date(match_date, format = "%Y-%m-%d"))



## Columns for current team1 and team2 scores
events <- events %>% mutate(home_goal = ifelse((action_id == 8010 & homeaway == "home") | (action_id == 8020 & homeaway == "away"), 1, 0),
                            away_goal = ifelse((action_id == 8010 & homeaway == "away") | (action_id == 8020 & homeaway == "home"), 1, 0))
events <- events %>% group_by(match_id) %>%
  mutate(current_team1_score = cumsum(lag(home_goal, default = 0)), current_team2_score = cumsum(lag(away_goal, default = 0))) %>%
  select(-c(home_goal, away_goal)) %>% ungroup()

## Columns for current team1 and team2 number of players
events <- events %>%
  mutate(home_red = ifelse(action_id %in% c(3030, 3100) & homeaway == "home", 1, 0), 
         away_red = ifelse(action_id %in% c(3030, 3100) & homeaway == "away", 1, 0))
events <- events %>% group_by(match_id) %>%
  mutate(current_team1_players = 11 - cumsum(lag(home_red, default = 0)), current_team2_players = 11 - cumsum(lag(away_red, default = 0))) %>%
  select(-c(home_red, away_red)) %>% ungroup()


## Columns for length, distance from goal, destination distance from goal, distance from own goal
events <- events %>% mutate(len = ifelse(is.na(len), sqrt((pos_x - pos_dest_x)^2 + (pos_y - pos_dest_y)^2), len),
                            dist_from_goal = sqrt((105 - pos_x)^2 + (34 - pos_y)^2),
                            dest_dfg = sqrt((105 - pos_dest_x)^2 + (34 - pos_dest_y)^2),
                            df_own_goal = sqrt((0 - pos_x)^2 + (34 - pos_y)^2))


events_2 <- events


## ignore actions: 28048, 28047, 28040, 28042, 13011, 13012, 26000, 22000, 33030


## Remove duplicated actions - specify groups of actions containing duplicates and order of importance

duplicate_actions <- list(c(26001, 28043, 28044, 28045, 1011), c(26002, 26012, 28046, 1012), c(1040, 1011), c(1070, 1061, 1011), c(1050, 1031, 1011), c(6020, 6030, 7000, 2060), c(2010, 2020), c(2052, 1032, 1062, 26002, 1012, 1022, 2040, 10000), c(2051, 21000), c(28080, 28090))
# accurate crossing from set piece; inaccurate set-piece cross; assist; extra attacking pass assist; key assist; ball recoveries; challenges; turnover and bad ball control; dribbling; successful play on offside line and opening


for(i in 1:length(duplicate_actions)){
  events_2 <- events_2 %>% mutate(duplicate = ifelse(action_id %in% duplicate_actions[[i]], 1, 0))
  
  cleaned <- events_2 %>% filter(duplicate == 1)
  
  cleaned$action_id <- factor(cleaned$action_id, levels = duplicate_actions[[i]])
  
  cleaned <- cleaned %>% arrange(match_id, half, second, action_id) %>%
    distinct(match_id, half, second, player_id, team_id, .keep_all = T) %>%
    mutate(keep = 1)
  
  events_2 <- events_2 %>% left_join(select(cleaned, match_id, team_id, player_id, half, second, id, keep)) %>%
    mutate(keep = ifelse(is.na(keep), 0, keep))
  
  events_2 <- filter(events_2, duplicate == 0 | keep == 1) %>% select(-c(duplicate, keep))
}




saves <- events_2 %>% filter(action_id == 13040 | lead(action_id) == 13040 | lead(action_id, 2) == 13040 | lead(action_id, 3) == 13040| lead(action_id, 4) == 13040 | lead(action_id, 5) == 13040) %>%
  filter(action_id %in% c(13040, 4010)) %>%
  arrange(match_id, half, second) %>% distinct() %>%
  group_by(match_id, half) %>% mutate(xg_saved = ifelse(action_id == 13040, lag(xg, default = NA), NA)) %>% ungroup()

events_2 <- events_2 %>% left_join(select(saves, match_id, id, half, second, xg_saved))




## Action scores

#goal
event_scores <- events_2 %>%
  mutate(att = ifelse(action_id == 8010, ifelse(!is.na(xg), 14 + 15*(0.8 - xg), 14 + 15*(dist_from_goal/40)), NA))

#playing in scoring attacks
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 23000, 3, att))

#shot on target
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 4010, ifelse(!is.na(xg), ifelse(xg < 0.25, 23*(0.25 - xg), 10*(0.25 - xg)), 0.3*(dist_from_goal - 15)), att))

#shot into the bar/post
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 4030, ifelse(!is.na(xg), ifelse(xg < 0.25, 1 + 30*(0.25 - xg), 1 + 18*(0.25 - xg)), 0.5*(dist_from_goal - 12)), att))

#wide shot
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 4020, ifelse(!is.na(xg), -2.2 - 9*xg, ifelse(dist_from_goal < 15, -3.7 + 3*(-1 + dist_from_goal/15), -2.7 + 1.5*(-1 + dist_from_goal/50))), att))

#shot blocked + shot blocked by field player
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id %in% c(4040, 4050), ifelse(!is.na(xg), -1.5 - 6*xg, ifelse(dist_from_goal < 15, -1.8 + 2*(-1 + dist_from_goal/19), -1.5 + (-1 + dist_from_goal/50))), att))


#assist
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 1040, 8.1 + 2.8*(len/60) + 3*(1-(dest_dfg/60)), att))

#extra attacking pass assist
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 1070, 8.6 + 3*(len/60) + 4*(1-(dest_dfg/60)), att))

#key assist
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 1050, 10.1 + 3*(len/60) + 4*(1-(dest_dfg/60)), att))

#accurate key pass
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 1031, 6.1 + (len/100) + 1.2*(1-(dest_dfg/60)), att))

#extra attacking pass accurate
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 1061, 3.4 + (len/100) + (1-(dest_dfg/70)), att))

#inaccurate key pass
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 1032, -1.9 + 0.5*(len/110) + 0.5*((dist_from_goal - dest_dfg)/80), att))

#inaccurate extra attacking pass
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 1062, -1.9 + 0.5*(len/110) + 0.5*((dist_from_goal - dest_dfg)/80), att))


#accurate crossing from set piece
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 28043, ifelse(dest_dfg < 10, 2.6 + (len/60) + (1-(dest_dfg/12)), 1.9 + (len/60) + (1-(dest_dfg/40))), att))

#accurate crossing from set piece with shot
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 28044, ifelse(dest_dfg < 10, 3.1 + (len/60) + (1-(dest_dfg/12)), 2.4 + (len/60) + (1-(dest_dfg/40))), att))

#accurate crossing from set piece with goal
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 28045, ifelse(dest_dfg < 10, 3.6 + (len/60) + (1-(dest_dfg/12)), 2.9 + (len/60) + (1-(dest_dfg/40))), att))

#inaccurate set piece cross
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 28046, -2.9 + (len/100) + (1-(dest_dfg/100)), att))

#cross accurate
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 26001, ifelse(dest_dfg < 12, 2.5 + 1.4*(len/60) + 1.2*(1-(dest_dfg/12)), 1.9 + (len/60) + 1.2*(1-(dest_dfg/40))), att))

#cross inaccurate
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 26002, -2.6 + (len/100) + (1-(dest_dfg/100)), att))

#inaccurate blocked cross
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 26012, -2.4 + (len/100) + (1-(dest_dfg/100)), att))


#attacking pass accurate
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 1011, 0.3*(len/110) + 0.3*(1-(dest_dfg/110)), att))

#attacking pass inaccurate
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 1012, -1.7 + 0.5*(len/110) + 0.5*(dist_from_goal - dest_dfg)/80, att))

#non attacking pass accurate
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 1021, 0.5*(len/110), att))

#non attacking pass inaccurate
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 1022, -1.9 + 0.5*(len/110) + 0.5*(dist_from_goal - dest_dfg)/60, att))


#successful dribbling
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 2051, ifelse(dist_from_goal < 20, 1.4 + 1.2*(1-(dist_from_goal/30)), 0.9 + 1.1*(1-(dist_from_goal/110))), att))

#unsuccessful dribbling
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 2052, -1.9 + (dist_from_goal/110), att))

#dribbling
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 21000, 0.4 + 0.5*(1-(dist_from_goal/110)), att))

#picking up
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 7000, 0.8*(1-(dist_from_goal/110)), att))

#opening
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 28090, 0.3 + (1-(dist_from_goal/90)), att))

#successful play on offside line
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 28080, 1.5, att))

#offside
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 3040, -1.6, att))

#bad ball control
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 10000, ifelse(dist_from_goal < 20, -1.8 + (dist_from_goal/34), -1.1 + 0.5*(dist_from_goal/110)), att))

#turnover
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 2040, ifelse(df_own_goal < 25, -0.7 - 1.1*(1-(df_own_goal/22)), -0.1 - 0.6*(1-(df_own_goal/110))), att))

#ball recoveries
event_scores <- event_scores %>%
  mutate(att = ifelse(action_id == 2060, ifelse(df_own_goal < 25, 0.5 + (1-(df_own_goal/48)), 0.3 + 0.88*(1-(df_own_goal/110))), att))


#tackle
event_scores <- event_scores %>%
  mutate(def = ifelse(action_id == 2030, ifelse(zone_id == 4, 4.4 + 1.3*(1-(df_own_goal/30)), 2.9 + 1.3*(1-(df_own_goal/110))), NA))

#challenge
event_scores <- event_scores %>%
  mutate(def = ifelse(action_id == 2010, ifelse(zone_id == 4, 1.3 + 0.6*(1-(df_own_goal/30)), 0.6 + (1-(df_own_goal/110))), def))

#air challenge
event_scores <- event_scores %>%
  mutate(def = ifelse(action_id == 2020, ifelse(zone_id == 4, 1.2 + 0.6*(1-(df_own_goal/30)), 0.5 + (1-(df_own_goal/110))), def))

#clearance
event_scores <- event_scores %>%
  mutate(def = ifelse(action_id == 9000, ifelse(df_own_goal < 20, 1.1 + (1-(df_own_goal/26)), 0.4 + 1.1*(1-(df_own_goal/110))), def))

#shots blocked (blocks)
event_scores <- event_scores %>%
  mutate(def = ifelse(action_id == 6010, ifelse(df_own_goal < 10, 1.2 + 4*(1-(df_own_goal/13)), 0.7 + 1.7*(1-(df_own_goal/55))), def))

#pass interception
event_scores <- event_scores %>%
  mutate(def = ifelse(action_id == 6020, ifelse(df_own_goal < 20, 1.1 + 1.2*(1-(df_own_goal/42)), 0.9 + (1-(df_own_goal/110))), def))

#cross interception
event_scores <- event_scores %>%
  mutate(def = ifelse(action_id == 6020, ifelse(df_own_goal < 20, 1.3 + 1.8*(1-(df_own_goal/42)), 1 + 1.5*(1-(df_own_goal/110))), def))

#players that created offside trap
event_scores <- event_scores %>%
  mutate(def = ifelse(action_id == 28050, 1.2, def))

#team pressing successful, names of participants
event_scores <- event_scores %>%
  mutate(def = ifelse(action_id == 33031, 2, def))

#save
event_scores <- event_scores %>%
  mutate(def = ifelse(action_id == 13040, 3.5 + 15*xg_saved, def))


#error
event_scores <- event_scores %>%
  mutate(def = ifelse(action_id == 19010, ifelse(df_own_goal < 25, -5.5 - 1.5*(1-(df_own_goal/17)), -3.6 - 1.5*(1-(df_own_goal/110))), def))

#error leading to goal
event_scores <- event_scores %>%
  mutate(def = ifelse(action_id == 19020, -10, def))

#unforced error
event_scores <- event_scores %>%
  mutate(def = ifelse(action_id == 19030, ifelse(df_own_goal < 25, -4.5 - 1.6*(1-(df_own_goal/28)), -3.5 - 1.5*(1-(df_own_goal/110))), def))

#own goal
event_scores <- event_scores %>%
  mutate(def = ifelse(action_id == 19020, -5.4 - 0.7*(df_own_goal/15), def))

#mistake while creating the offside trap
event_scores <- event_scores %>%
  mutate(def = ifelse(action_id == 19020, -3, def))


#red card
event_scores <- event_scores %>%
  mutate(def = ifelse(action_id == 3030, ifelse(half == 1, 0.5*(-13 - 4*(1-(second/3060))), 0.5*(-8 - 4*(1-(second/3300)))), def))

#RC for two YC
event_scores <- event_scores %>%
  mutate(def = ifelse(action_id == 3100, ifelse(half == 1, 0.5*(-12 - 4*(1-(second/3060))), 0.5*(-7 - 4*(1-(second/3300)))), def))

#yellow card
event_scores <- event_scores %>%
  mutate(def = ifelse(action_id == 3020, ifelse(half == 1, -3.5 - (1-(second/3060)), -2.5 - (1-(second/3300))), def))

#foul
event_scores <- event_scores %>%
  mutate(def = ifelse(action_id == 3010, ifelse(zone_id == 4, -10, ifelse(zone_id == 2, -2.8 - 2*(1-(df_own_goal/20)), -0.5 - 1.5*(1-(df_own_goal/110)))), def))



## Fill in missing values for att and def with action averages
action_averages <- event_scores %>% filter(!is.na(att) | !is.na(def)) %>% group_by(action_id) %>%
  summarise(avg_att = mean(att), avg_def = mean(def)) %>% ungroup()

event_scores <- event_scores %>% left_join(action_averages) %>%
  mutate(att = ifelse(is.na(att), avg_att, att), def = ifelse(is.na(def), avg_def, def))


# ggplot(filter(event_scores, !is.na(att))) + geom_point(aes(x = xg, y = att, colour = as.factor(action_name)))

sup.dat <- event_scores %>% select(match_id, team1_id, team2_id, match_date) %>% distinct()

sup.dat["points"] <- 0
sup.dat["goal_diff"] <- 0
sup.dat["league_pos"] <- 0
sup.dat["points_per_match"] <- 0

matchid.s <- sup.dat$match_id
# one can only start this with the eleventh game
for (matchid in matchid.s){
  tryCatch({
    
    date_matchid <- filter(event_scores, match_id == matchid, !is.na(match_date))$match_date %>% head(1) %>% as.Date()
    tour_matchid <- filter(event_scores, match_id == matchid, !is.na(tournament_id))$tournament_id %>% head(1)
    season_matchid <- filter(event_scores, match_id == matchid, !is.na(season_id))$season_id %>% head(1)
    
    team_matches <- event_scores %>% filter(as.Date(match_date) < date_matchid, tournament_id == tour_matchid, season_id == season_matchid) %>%
      group_by(match_id, match_date, season_id, tournament_id, team_name, team_id, homeaway, team1_name, team1_id, team1_score, team2_score, team2_id, team2_name, duration) %>% 
      summarise() %>% ungroup() %>% mutate(match_date = as.Date(match_date, format = "%Y-%m-%d")) %>% na.omit()
    
    team_matches <- team_matches %>% 
      mutate(points = ifelse(homeaway == "home", 
                             ifelse(team1_score > team2_score, 3, ifelse(team1_score == team2_score, 1, 0)),
                             ifelse(team2_score > team1_score, 3, ifelse(team1_score == team2_score, 1, 0))),
             goal_diff = ifelse(homeaway == "home",
                                team1_score - team2_score, team2_score - team1_score))
    
    league_table <- team_matches %>% group_by(season_id, tournament_id, team_name, team_id) %>%
      summarise(played = n_distinct(match_id), points = sum(points), goal_diff = sum(goal_diff)) %>% ungroup() %>%
      mutate(points_per_match = points / played) %>%
      arrange(season_id, tournament_id, desc(points), desc(goal_diff), points_per_match, team_name) %>%
      group_by(season_id, tournament_id) %>% mutate(league_pos = 1:n()) %>% ungroup() %>%
      mutate(match_id = matchid)
    
    teamid <- sup.dat %>% filter(match_id == matchid) %>% select(team1_id, team2_id)
    home.sup <- league_table %>% filter(team_id  == teamid[[1]]) %>% select(points, goal_diff, league_pos, points_per_match)
    away.sup <- league_table %>% filter(team_id  == teamid[[2]]) %>% select(points, goal_diff, league_pos, points_per_match)
    
    sup <- cbind(home.sup - away.sup, matchid)
    colnames(sup)[dim(sup)[2]] <- "match_id"
    sup.dat[sup.dat$match_id == matchid,5:8] <- sup[,1:4]
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

event_scores <- event_scores %>% left_join(sup.dat)

player_ratings <- event_scores %>% group_by(match_id, team_id, player_name) %>%
  summarise(att = sum(att, na.rm = T), def = sum(def, na.rm = T), app = n_distinct(match_id)) %>%
  ungroup() %>% filter(!is.na(team_id), !is.na(player_name))
colnames(player_ratings) <- c("match_id", "team_id", "player_name", "player_att", "player_def", "app")
event_scores <- event_scores %>% bind_rows(player_ratings) %>% arrange(match_id)

## Load data
event_scores <- event_scores %>%  group_by(match_id) %>%
  mutate(second.totalgame = case_when(
    half == 1 ~ second,
    TRUE ~ max(second, half==1, na.rm = TRUE) + second
  ))
event_scores <- event_scores %>% mutate(minute = second.totalgame/60)


print(dim(event_scores))
save(event_scores, file = "event_scores.RData")

