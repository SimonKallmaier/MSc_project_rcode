rm(list = ls())

if (!require("dplyr")) {
  install.packages("dplyr", repos="http://cran.rstudio.com/") 
  library("dplyr")
}

if (!require("tidyr")) {
  install.packages("tidyr", repos="http://cran.rstudio.com/") 
  library("tidyr")
}


path <- "/home/simon/Dropbox/Master_Project/code/Rcode/main"

load(paste(path, "/data/events_100.RData", sep=""))
load(paste(path, "/data/event_scores.RData", sep=""))

file <- "/data_Rfiles/data_prep_functions.R"
source(paste(path, file, sep=""), chdir = FALSE)



event_goals <- event_scores %>% select(match_id, match_date, minute, team_id, team1_id, team2_id, action_name, points, goal_diff, league_pos, points_per_match) %>% filter(action_name == "Goal")
event_goals["home_goals"] <- ifelse(event_goals$team_id == event_goals$team1_id, 1, 0)
event_goals["away_goals"] <- ifelse(event_goals$team_id == event_goals$team2_id, 1, 0)

final_scores <- event_goals %>% group_by(match_id) %>% summarise(home_score = sum(home_goals), away_score = sum(away_goals))
final_scores <- final_scores %>% left_join(event_goals) %>% select(-c(minute, team_id, team1_id, team2_id, action_name, home_goals, away_goals)) %>% distinct()


zero_goal_games <- unique(event_scores$match_id)[which(unique(event_scores$match_id) %in% unique(event_goals$match_id) == FALSE)]
zero_goals_dat <- event_scores %>% 
  filter(match_id %in% zero_goal_games) %>%
  select(match_id, match_date, points, goal_diff, league_pos, points_per_match) %>% distinct() %>% drop_na() %>%
  mutate("home_score" = 0) %>% mutate("away_score" = 0)
final_scores <- bind_rows(final_scores, zero_goals_dat)

final_scores <- final_scores %>% gather(team, goals, home_score:away_score) %>% arrange(match_id)
final_scores$points <- final_scores$points * ifelse(final_scores$team == "home_score", 1, -1)
final_scores$league_pos <- final_scores$league_pos * ifelse(final_scores$team == "home_score", 1, -1)
final_scores$goal_diff <- final_scores$goal_diff * ifelse(final_scores$team == "home_score", 1, -1)
final_scores$points_per_match <- final_scores$points_per_match * ifelse(final_scores$team == "home_score", 1, -1)

ratings.dat <- event_scores %>% group_by(match_id, team_id) %>% summarise(sum_att = sum(player_att, na.rm = TRUE), sum_def = sum(player_def, na.rm = TRUE)) %>%
    filter(!is.na(team_id))
rm.dat <- event_scores %>% group_by(match_id) %>% select(match_id, team1_id, team2_id) %>% distinct() %>% drop_na()
colnames(rm.dat) <- c("match_id", "home_score", "away_score")
rm.dat <- rm.dat %>% gather(team, team_id, home_score:away_score) %>% arrange(match_id)
ratings.dat <- ratings.dat %>% left_join(rm.dat)
final_scores <- final_scores %>% left_join(ratings.dat)
rm(rm.dat, ratings.dat)

event_goals <- event_goals %>% group_by(match_id, team_id) %>%
    mutate(diff = case_when(
        minute - lag(minute, default = first(minute)) != 0 ~ minute - lag(minute, default = first(minute)),
        TRUE ~ minute
    ))

event_goals_sup <- final_scores %>% select(match_id, team_id)
event_goals_sup <- event_goals_sup %>% left_join(event_goals %>% group_by(match_id, team_id) %>% summarise(rate = mean(diff)))
event_goals_sup[is.na(event_goals_sup$rate), "rate"] <- 90


event_goals <- event_goals %>% group_by(match_id) %>% mutate(id = row_number())
event_goals <- event_goals %>% group_by(match_id) %>% mutate(delta = case_when(id == max(id) ~ 0, TRUE ~ 1))
event_goals <- event_goals %>% group_by(match_id) %>% mutate(relative_goals = home_goals - away_goals)

rel_goals_all <- c()
matchids <- unique(event_goals$match_id)
for (mid in matchids){
    rel_goals <- event_goals %>% filter(match_id == mid) %>% pull(relative_goals)
    rel_goals_all <- c(rel_goals_all, sum_relative_goals(r = rel_goals))
}
event_goals["relative_goals_sum"] <- rel_goals_all
event_goals <- event_goals %>% mutate(relative_goals_sum_round = case_when(
    relative_goals_sum >= 3 ~ 2,
    relative_goals_sum <= -3 ~ -2,
    TRUE ~ relative_goals_sum
))

h_current_score <- c(); a_current_score <- c()
for (mid in matchids){
    hgoals <- event_goals %>% filter(match_id == mid) %>% pull(home_goals)
    agoals <- event_goals %>% filter(match_id == mid) %>% pull(away_goals)
    h_current_score <- c(h_current_score, sum_relative_goals(r = hgoals))
    a_current_score <- c(a_current_score, sum_relative_goals(r = agoals))
}
event_goals["h_current_score"] <- h_current_score
event_goals["a_current_score"] <- a_current_score
event_goals <- event_goals %>% mutate(score = paste(h_current_score, a_current_score))
event_goals <- event_goals %>% mutate(score_rates = case_when(
    relative_goals_sum >= 4 ~ paste(3, 0),
    relative_goals_sum <= -4 ~ paste(0, 3),
    TRUE ~ case_when(
        home_goals == 1 ~ paste(h_current_score-1, a_current_score),
        TRUE ~ paste(h_current_score, a_current_score-1))
    ))
event_goals <- event_goals %>% mutate(goal_dummy = case_when(
    home_goals == 1 ~ 1, TRUE ~ 0
))

dat <- final_scores %>% left_join(event_goals) %>% select(-c(team1_id, team2_id, minute, match_date, points, goal_diff, league_pos, points_per_match, goals, action_name))
dat[is.na(dat$diff), "diff"] <- 90

nb_games_per_id <- final_scores %>% arrange(team_id, match_date) %>% group_by(team_id) %>% summarise(n=n())
final_scores <- final_scores %>% arrange(team_id)
teamids <- final_scores %>% pull(team_id) %>% unique()
final_scores$team <- ifelse(final_scores$team == "home_score", 1, 0)

# for the data argument function in log likelihoods.R

final_scores <- final_scores %>% left_join(event_scores %>% group_by(match_id) %>% summarise(max = max(minute, na.rm = TRUE)), by ="match_id")

lag <- 3
att_momentum <- c()
def_momentum <- c()
for (teamid in teamids) {
    att_momentum <- c(att_momentum, get_mean_goals(teamid = teamid,n = nb_games_per_id %>% filter(team_id == teamid) %>% pull(n), lag = lag, scored = TRUE))
    def_momentum <- c(def_momentum, get_mean_goals(teamid = teamid,n = nb_games_per_id %>% filter(team_id == teamid) %>% pull(n), lag = lag, scored = FALSE))
}
final_scores["att_momentum"] <- att_momentum
final_scores["def_momentum"] <- def_momentum
final_scores <- final_scores %>% arrange(match_id)

matchids <- unique(final_scores$match_id)

new_def <- c()
for (mid in matchids){
    def_vec <- final_scores %>% filter(match_id == mid) %>% select(def_momentum) %>% pull()
    new_def <- c(new_def, c(def_vec[2], def_vec[1]))
}
final_scores["def_momentum_opponent"] <- new_def

new_att <- c()
for (mid in matchids){
    att_vec <- final_scores %>% filter(match_id == mid) %>% select(att_momentum) %>% pull()
    new_att <- c(new_att, c(att_vec[2], att_vec[1]))
}
final_scores["att_momentum_opponent"] <- new_att

save(final_scores, file = "data/scores.RData")
save(event_goals, file = "data/event_goals.RData")