
rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(xtable)

#load("scores.RData")
#load("event_goals.RData")
#load("events_100.RData")

source("point_processes/log_likelihoods.R")
source("data_Rfiles/data_prep_functions.R")


features <- c("goal_diff", "points_per_match", "team", "goals", "att_momentum", "def_momentum", "att_momentum_opponent", "def_momentum_opponent")
nb_games_per_id <- final_scores %>% arrange(team_id, match_date) %>% group_by(team_id) %>% summarise(n=n())
dat <- final_scores
dat <- dat %>% arrange(match_id, team)

coefs_all_models <- read.csv("coefs_all_models.csv")[,-1]
coefs_all_models[2:11,c(1,4,5,6)] <- 1
feat <- c("goal_diff", "points_per_match", "S_home", "C_home", "S_away", "C_away", "Intercept")
baseline_params <- coefs_all_models %>% filter(parameter %in% feat)

get_rates <- function(mid, model) {
    ## prep ## 
    M <- events %>% filter(match_id==mid) %>% select(H_a_sum, H_d_sum, W_a_sum, W_d_sum) %>% as.matrix()
    Z.h <- M[,1]; Z.w <- M[,3]
    
    y_vec_home <- events %>% filter(match_id==mid) %>% select(H_goals) %>% pull()
    y_vec_away <- events %>% filter(match_id==mid) %>% select(W_goals) %>% pull()
    score = cumsum(y_vec_home) - cumsum(y_vec_away)
    score <- if_else(score > 2, 2, score)
    score <- if_else(score < -2, -2, score)
  
    score_params <- coefs_all_models[2:11,] %>% pull(model)
    score_params_lambda <- score_params[1:5]
    score_params_mu <- score_params[6:10]
    
    mult_home <- score_params_lambda[score+3]
    mult_away <- score_params_mu[score+3]
    
    ## basline ## 
    baseline <- exp(as.matrix(dat %>% filter(match_id == mid) %>% select(features[-c(3,4)]) %>% mutate(Intercept = 1)) %*%  as.matrix(baseline_params[model]))
    
    a_h <- coefs_all_models %>% filter(parameter == "alpha_home") %>% pull(model)
    a_w <- coefs_all_models %>% filter(parameter == "alpha_away") %>% pull(model)
    b_h <- coefs_all_models %>% filter(parameter == "beta_home") %>% pull(model)
    b_w <- coefs_all_models %>% filter(parameter == "beta_away") %>% pull(model)
    xi1 <- coefs_all_models %>% filter(parameter == "xi1") %>% pull(model)
    xi2 <- coefs_all_models %>% filter(parameter == "xi2") %>% pull(model)
    
    my_hpvec_home <- baseline[2]*mult_home + c(0,my_integrate_noreset(alpha=a_h, beta=b_h, Z=Z.h)*100) + 1:100 * xi1 / 100
    my_hpvec_away <- baseline[1]*mult_away + c(0,my_integrate_noreset(alpha=a_w, beta=b_w, Z=Z.w)*100) + 1:100 * xi2 / 100
    return(cbind(my_hpvec_home, my_hpvec_away))
}


mid <- 1171298
hp_rates_m1 <- get_rates(mid, "M1");hp_rates_m4 <- get_rates(mid, "M4");hp_rates_m5 <- get_rates(mid, "M5")
hp_rates_dat <- as.data.frame(cbind(hp_rates_m1[,1], hp_rates_m4[,1], hp_rates_m5[,1]))
colnames(hp_rates_dat) <- c("M1", "M4", "M5")
hp_rates_dat["Interval"] <- 1:100;hp_rates_dat["loc"] <- "home"
y_vec_home <- events %>% filter(match_id==mid) %>% select(H_goals) %>% pull()
hp_rates_dat["goals"] <- y_vec_home
hp_rates_dat_home <- hp_rates_dat

hp_rates_dat <- as.data.frame(cbind(hp_rates_m1[,2],  hp_rates_m4[,2], hp_rates_m5[,2]))
colnames(hp_rates_dat) <- c("M1", "M4", "M5")
hp_rates_dat["Interval"] <- 1:100;hp_rates_dat["loc"] <- "away"
y_vec_away <- events %>% filter(match_id==mid) %>% select(W_goals) %>% pull()
hp_rates_dat["goals"] <- y_vec_away

hp_rates_dat <- bind_rows(hp_rates_dat_home, hp_rates_dat)
hp_rates_dat_melt <-  reshape2::melt(hp_rates_dat, id.vars = c("Interval", "loc", "goals"))

p <- ggplot(hp_rates_dat_melt  %>% filter(loc=="home"), aes(x=Interval, y=value, col=variable)) + geom_line()
p <- p + geom_vline(aes(xintercept = Interval), data = hp_rates_dat_melt %>% filter(goals == 1))
p <- p + theme_bw() + scale_x_continuous(expand = c(0.01,0.01)) + scale_y_continuous(expand = c(0.01,0.01), limits = c(0, 4)) 
p <- p + theme(legend.position = "none")
p <- p + labs(title = "In-play data over-estimates score", y="Rate")
p2 <- p

mid <- 1244269
hp_rates_m1 <- get_rates(mid, "M1");hp_rates_m4 <- get_rates(mid, "M4");hp_rates_m5 <- get_rates(mid, "M5")
hp_rates_dat <- as.data.frame(cbind(hp_rates_m1[,1], hp_rates_m4[,1], hp_rates_m5[,1]))
colnames(hp_rates_dat) <- c("M1", "M4", "M5")
hp_rates_dat["Interval"] <- 1:100;hp_rates_dat["loc"] <- "home"
y_vec_home <- events %>% filter(match_id==mid) %>% select(H_goals) %>% pull()
hp_rates_dat["goals"] <- y_vec_home
hp_rates_dat_home <- hp_rates_dat

hp_rates_dat <- as.data.frame(cbind(hp_rates_m1[,2],  hp_rates_m4[,2], hp_rates_m5[,2]))
colnames(hp_rates_dat) <- c("M1", "M4", "M5")
hp_rates_dat["Interval"] <- 1:100;hp_rates_dat["loc"] <- "away"
y_vec_away <- events %>% filter(match_id==mid) %>% select(W_goals) %>% pull()
hp_rates_dat["goals"] <- y_vec_away

hp_rates_dat <- bind_rows(hp_rates_dat_home, hp_rates_dat)
hp_rates_dat_melt <-  reshape2::melt(hp_rates_dat, id.vars = c("Interval", "loc", "goals"))


p <- ggplot(hp_rates_dat_melt %>% filter(loc=="home"), aes(x=Interval, y=value, col=variable)) + geom_line()
p <- p + geom_vline(aes(xintercept = Interval), data = hp_rates_dat_melt %>% filter(goals == 1))
p <- p + theme_bw() + scale_x_continuous(expand = c(0.01,0.01)) + scale_y_continuous(expand = c(0.01,0.01), limits = c(0,3.5)) 
p <- p +theme(legend.position = c(.95, .05),legend.justification = c("right", "bottom"),
              legend.title = element_blank(),legend.box.just = "right")
p <- p + labs(title = "In-play data increases performace", y = "Rate")
p1 <- p


p <- grid.arrange(p1, p2, nrow=1)
p



sum_rates <- matrix(data = NA, nrow = length(unique(events$match_id)) * 2, ncol = 11)
i <- 1
for (mid in unique(events$match_id)){
    
    home_rate_sum <- c()
    away_rate_sum <- c()
    h_goals <- sum(events %>% filter(match_id==mid) %>% select(H_goals) %>% pull())
    w_goals <- sum(events %>% filter(match_id==mid) %>% select(W_goals) %>% pull())
    
    for (m in c("M1","M2","M3","M4","M5","M6","M7","M8")) {
        hp_rates <- get_rates(mid, m)
        home_rate_sum <- c(home_rate_sum, sum(hp_rates[,1]/100) )
        away_rate_sum <- c(away_rate_sum, sum(hp_rates[,2]/100) )
    }
    home_rate_sum <- c(home_rate_sum, mid, 1, h_goals)
    away_rate_sum <- c(away_rate_sum, mid, 0, w_goals)
    sum_rates[i, ] <- home_rate_sum
    sum_rates[i+1, ] <- away_rate_sum
    i <- i + 2
}

sum_rates_df <- as_tibble(sum_rates)
colnames(sum_rates_df) <- c("M1","M2","M3","M4","M5","M6","M7","M8", "mid", "loc", "true_goal")
sum_rates_df_melt <- sum_rates_df %>% reshape2::melt(id.vars = c("mid", "loc", "true_goal"))

sum_rates_df_melt <- sum_rates_df_melt %>% mutate(diff = true_goal - value)

get_rel_rates <- function(model1, model2) {
    i <- 1
    for (mid in unique(events$match_id)){
        
        y_vec_home <- events %>% filter(match_id==mid) %>% select(H_goals) %>% pull()
        y_vec_away <- events %>% filter(match_id==mid) %>% select(W_goals) %>% pull()
        hp_rates_m1 <- get_rates(mid, model1)
        hp_rates_m5 <- get_rates(mid, model2)
        
        rel_rates_home <- hp_rates_m5[,1]-hp_rates_m1[,1]
        rel_rates_away <- hp_rates_m5[,2]-hp_rates_m1[,2]
        
        if (i == 1){
            G <- cbind(rel_rates_home, rel_rates_away, y_vec_home, y_vec_away)
        }
        else{
            g <- cbind(rel_rates_home, rel_rates_away, y_vec_home, y_vec_away)
            G <- rbind(G,g)
        }
        i <- i + 1
    }
    rel_rates_dat <- as.data.frame(G)
    return(rel_rates_dat)
}

rel_rates_dat <- get_rel_rates("M1", "M5") # relative score: M5 - M1

compare_model_dat <- sum_rates_df %>% 
  mutate(error_m1 = abs(M1 - true_goal)) %>% 
  mutate(error_m2 = abs(M2 - true_goal)) %>%
  mutate(error_m3 = abs(M3 - true_goal)) %>% 
  mutate(error_m4 = abs(M4 - true_goal)) %>%
  mutate(error_m5 = abs(M5 - true_goal)) %>% 
  mutate(error_m6 = abs(M6 - true_goal)) %>%
  mutate(error_m7 = abs(M7 - true_goal)) %>% 
  mutate(error_m8 = abs(M8 - true_goal))


p1 <- ggplot(compare_model_dat, aes(x=error_m5, y=error_m4)) + geom_point(alpha = 0.3) + geom_abline(slope = 1) + theme_bw() +  scale_x_continuous(expand = c(0.0,0.0)) + scale_y_continuous(expand = c(0.01,0.01)) + 
  labs(x="Absolute error, M5", y="Absolute error, M4")

p <- ggplot(sum_rates_df_melt %>% filter(variable %in% c("M4", "M5")), aes(x = value, col=variable)) + geom_density(bw=0.2) + theme_bw()
p <- p +  theme(legend.position = c(.95, .75),legend.justification = c("right", "bottom"),legend.box.just = "right", legend.title = element_blank()) + labs(x="Mean rate parameter for one game")
p <- p + scale_x_continuous(expand = c(0.0,0.0)) + scale_y_continuous(expand = c(0.01,0.01))
p2 <- p

p <- grid.arrange(p1, p2, nrow=1)
p


rel_rates_dat <- get_rel_rates("M1", "M5") # relative score: M5 - M1
mod_h_m5m1 <- glm(data = rel_rates_dat, y_vec_home ~ rel_rates_home, family = binomial())
mod_w_m5m1 <- glm(data = rel_rates_dat, y_vec_away ~ rel_rates_away, family = binomial())

rel_rates_dat <- get_rel_rates("M4", "M5") 
mod_h_m5m4 <- glm(data = rel_rates_dat, y_vec_home ~ rel_rates_home, family = binomial())
mod_w_m5m4 <- glm(data = rel_rates_dat, y_vec_away ~ rel_rates_away, family = binomial())

res_rel_rates <- rbind(
  summary(mod_h_m5m1)$coefficients[2,c(1,2,4)],
  summary(mod_w_m5m1)$coefficients[2,c(1,2,4)],
  summary(mod_h_m5m4)$coefficients[2,c(1,2,4)],
  summary(mod_w_m5m4)$coefficients[2,c(1,2,4)],
)

res_rel_rates_dat <- as.data.frame(res_rel_rates)
res_rel_rates_dat["Description"] <- c("M5-M1, home", "M5-M1, away","M5-M4, home", "M5-M4, away","M4-M1, home", "M4-M1, away")
print(xtable(res_rel_rates_dat, digits = 4), include.rownames = FALSE)

