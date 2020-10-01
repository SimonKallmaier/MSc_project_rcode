
library(dplyr)
library(tidyr)

#### poisson regression ####

sum_relative_goals <- function(r) {
    r_vec <- rep(NA, length(r))
    for (i in 1:length(r)) {
        r_vec[i] <- sum(r[1:i])
    }
    return(r_vec)
}

get_mean_goals <- function(teamid, n, lag, scored=TRUE){
    if (scored == TRUE) {
        team_dat <- final_scores %>% filter(team_id == teamid)
    }
    else{
        teamid_games <- final_scores %>% filter(team_id == teamid) %>% pull(match_id)
        team_dat <- final_scores %>% filter(match_id %in% teamid_games & team_id != teamid)
    }
    weighting <- 1:lag
    sum_mean <- rep(0, n)
    for (i in (lag+1):n){
        sum_mean[i] <- mean(pull(team_dat[(i-lag):(i-1),"goals"]) * weighting)
    }
    for (i in lag:1){
        if (i == 1){
            sum_mean[i] <- mean(sum_mean)
        }
        else {
            weighting <- (lag-i+2):lag
            sum_mean[i] <- mean(pull(team_dat[1:(i-1),"goals"]) * weighting)
        }
    }
    return(sum_mean)
}

get_mean_goals_noweights <- function(teamid, n, lag, scored=TRUE){
    if (scored == TRUE) {
        team_dat <- final_scores %>% filter(team_id == teamid)
    }
    else{
        teamid_games <- final_scores %>% filter(team_id == teamid) %>% pull(match_id)
        team_dat <- final_scores %>% filter(match_id %in% teamid_games & team_id != teamid)
    }
    sum_mean <- rep(0, n)
    for (i in (lag+1):n){
        sum_mean[i] <- mean(pull(team_dat[(i-lag):(i-1),"goals"]))
    }
    for (i in lag:1){
        if (i == 1){
            sum_mean[i] <- mean(sum_mean)
        }
        else {
            weighting <- (lag-i+2):lag
            sum_mean[i] <- mean(pull(team_dat[1:(i-1),"goals"]))
        }
    }
    return(sum_mean)
}

get_mean_goals_exp <- function(teamid, n, lag, scored=TRUE){
    if (scored == TRUE) {
        team_dat <- final_scores %>% filter(team_id == teamid)
    }
    else{
        teamid_games <- final_scores %>% filter(team_id == teamid) %>% pull(match_id)
        team_dat <- final_scores %>% filter(match_id %in% teamid_games & team_id != teamid)
    }
    sum_mean <- rep(0, n)
    for (i in (lag+1):n){
        sum_mean[i] <- sum(pull(team_dat[(i-lag):(i-1),"goals"]) * exp(c(-(1:lag))))
    }
    for (i in lag:1){
        if (i == 1){
            sum_mean[i] <- mean(sum_mean)
        }
        else {
            weighting <- exp(-(1:(i-1)))
             exp(c(-(1:lag)))
            sum_mean[i] <- sum(pull(team_dat[1:(i-1),"goals"]) * weighting)
        }
    }
    return(sum_mean)
}

lastXevents <- function(mid, location, lag){
  
  if (location == "home") {
    tid <- events %>% filter(match_id == mid) %>% pull(team1_id) %>% unique()
    tid.str <- "team1_id";M.str <- "H_measure"
  }
  else {
    tid <- events %>% filter(match_id == mid) %>% pull(team2_id) %>% unique()
    tid.str <- "team2_id";M.str <- "W_measure"
  }
  current.date <- events %>% filter(match_id == mid) %>% pull(match_date) %>% unique()
  
  prev.date <- events[events[,tid.str] == tid,] %>%
    filter(match_date < current.date) %>% summarise(max = max(match_date)) %>% pull()
  
  prev.game <- events[events[,tid.str] == tid,] %>% filter(match_date == prev.date)
  M <- pull(prev.game[,M.str])
  M.lag <- M[(length(M)-lag + 1):(length(M))]

  return(M.lag)
}

timelag <- function(mid, location, lag){
    if (location == "home") {goal.str <- "H_goals"; M.str <- "H_measure"}
    else {goal.str <- "W_goals"; M.str <- "W_measure"}
    
    M <- pull(events[events$match_id == mid, M.str])
    M.lag <- lastXevents(mid=mid, location=location, lag=lag)
    M.all <- c(M.lag, M)
    goals <- pull(events[events$match_id == mid, goal.str])

    start_indexes <- seq(1, length(goals))
    M_matrix <- matrix(nrow = length(start_indexes), ncol = lag + 1)

    for (m_index in 1:(length(start_indexes))){
        M_matrix[m_index,] <- M.all[start_indexes[m_index]:(start_indexes[m_index] + lag)]
    }
    
    return(list("M_matrix" = M_matrix,"y" = goals))
}

add_sup_variables <- function(mid, location, lag) {

    mylist <- timelag(mid=mid, location=location, lag=lag)

    M_matrix_mid <- as.data.frame(mylist$M_matrix)
    cnames <- c()
    for (c in 1:(lag+1)) {
        cnames <- c(cnames, paste("lag", (lag+1) -c, sep = "_"))
    }
    colnames(M_matrix_mid) <- cnames
    M_dat <- cbind(events[events$match_id == mid, ], M_matrix_mid)

    if (location == "home") {loc <- 1; diff <- 1}
    else {loc <- 0; diff <- -1}
    M_dat["home"] <- loc
    
    M_dat <- M_dat %>% left_join(dat %>% filter(team==loc)  %>% select(match_id, att_momentum, def_momentum, att_momentum_opponent, def_momentum_opponent), by="match_id")

    additional.cols <- c("home", "points", "goal_diff", "league_pos", "att_momentum", "def_momentum", "att_momentum_opponent", "def_momentum_opponent")
    M_dat_model <- M_dat %>% select(cnames, additional.cols)
    M_dat_model[,c("points", "goal_diff", "league_pos")] <- M_dat_model[,c("points", "goal_diff", "league_pos")] * diff
    
    M_dat_model["y"] <- mylist$y
    
    return(M_dat_model)
}

get_scores <- function(dat, location){
  ttt.home <- dat %>% select(y, time_remaining, home) %>% filter(home==location)
  count <- 0
  score <- rep(NA, 100)
  for (i in 1:100){
    if (ttt.home[i,"y"] == 0){
      score[i] <- count
    }
    else{
      count <- count + 1
      score[i] <- count
    }
  }
  return(score)
}
get_current_scores <- function(dat) {
  home.perspective <- get_scores(dat=dat, location = 1) - get_scores(dat=dat, location = 0)
  away.perspective <- get_scores(dat=dat, location = 0) - get_scores(dat=dat, location = 1)
  return(c(home.perspective, away.perspective))
}

homeaway_gather <- function(mid, lag) {
  M_home <- add_sup_variables(mid = mid, location = "home", lag = lag)
  M_away <- add_sup_variables(mid = mid, location = "away", lag = lag)
  
  DF <- bind_rows(M_home, M_away)
  drops <- c("lag_0")
  DF <- DF[ , !(names(DF) %in% drops)]
  DF["time_remaining"] <- rep(seq(100,1, by = -1), times = 2)
  DF["time_remaining"] <- DF["time_remaining"]/100
  DF["current_scores"] <- get_current_scores(DF)
  return(DF)
}

traintest_split <- function(ids, p){
  nb.games <- length(ids)
  train_index <- 1:(round(nb.games * p))
  test_index <- (round(nb.games * p) + 1):nb.games
  return(list("train" = ids[train_index], "test" = ids[test_index]))
}

'%!in%' <- function(x,y)!('%in%'(x,y))
