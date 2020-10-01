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

features <- c("goal_diff", "points_per_match", "team", "goals", "att_momentum", "def_momentum", "att_momentum_opponent", "def_momentum_opponent")
nb_games_per_id <- final_scores %>% arrange(team_id, match_date) %>% group_by(team_id) %>% summarise(n=n())

dat <- final_scores
dat <- dat %>% arrange(match_id, team)
split <- traintest_split(unique(dat$match_id), 0.9)

train_dat <- dat %>% filter(match_id %in% split$train)
data.passin.inplay <- create_data_optim(dat=train_dat, func=function(x) {x})

coefs_all_models <- read.csv("point_processes/coefs_all_models.csv")[,-1]
feat <- c("goal_diff", "points_per_match", "S_home", "C_home", "S_away", "C_away", "Intercept")
baseline_params <- coefs_all_models %>% filter(parameter %in% feat)

################## Hessian matrix and standard errors ##################

library(numDeriv)
library(Matrix)
library(HelpersMG)

m1_coefs <- coefs_all_models %>% filter(M1 != 0) %>% pull(M1)
m2_coefs <- coefs_all_models %>% filter(M2 != 0) %>% pull(M2)
m3_coefs <- coefs_all_models %>% filter(M3 != 0) %>% pull(M3)
m4_coefs <- coefs_all_models %>% filter(M4 != 0) %>% pull(M4)

hessian_m1 <- hessian(log.likelihood1, data=data.passin.inplay, x=m1_coefs, method = "Richardson")
hessian_m2 <- hessian(log.likelihood2.full, data=data.passin.inplay, x=m2_coefs, method = "Richardson")
hessian_m3 <- hessian(log.likelihood3.full, data=data.passin.inplay, x=m3_coefs, method = "Richardson")
hessian_m4 <- hessian(log.likelihood4.full, data=data.passin.inplay, x=m4_coefs, method = "Richardson")

m5_coefs <- coefs_all_models %>% filter(M5 != 0) %>% pull(M5)
hessian_m5 <- hessian(log.likelihood.HP.baseline, data=data.passin.inplay, x=m5_coefs, method = "Richardson")

m6_coefs <- coefs_all_models %>% filter(M6 != 0) %>% pull(M6)
hessian_m6 <- hessian(log.likelihood.HP_withtime, data=data.passin.inplay, x=m6_coefs, method = "Richardson")

m7_coefs <- coefs_all_models %>% filter(M7 != 0) %>% pull(M7)
hessian_m7 <- hessian(log.likelihood.HP, data=data.passin.inplay, x=m7_coefs, method = "Richardson")

m8_coefs <- coefs_all_models %>% filter(M8 != 0) %>% pull(M8)
hessian_m8 <- hessian(log.likelihood.HP_withtime_withscore, data=data.passin.inplay, x=m8_coefs, method = "Richardson")

get_standard_errors <- function(hes) {
  a <- solve(hes)
  n = dim(a)[1];
  root = matrix(0,n,n);
  for (i in 1:n){
    sum = 0;
    if (i>1){sum = sum(root[i,1:(i-1)]^2) }
    
    x = a[i,i] - sum;
    if (x<0){x = 0}
    
    root[i,i] = sqrt(x);
    if (i < n){
      for (j in (i+1):n){
        if (root[i,i] == 0){x=0}
        else{
          sum = 0;
          if (i>1) {
            sum = root[i,1:(i-1)] %*% t(t(root[j,1:(i-1)]))
          }
          x = (a[i,j] - sum)/root[i,i];
        }
        root[j,i] = x;
      }
    }
  }
  SE <-   sqrt(diag(root %*% t(root)))
  return(SE)
}

coef_names <- c(
  "gamma" , "lambda-2", "lambda-1", "lambda0","lambda1", "lambda2", "mu-2",  "mu-1",  "mu0", "mu1", "mu2",
  "goal_diff", "points_per_match", "S_home", "C_home", "S_away", "C_away",
  "Intercept", "alpha_home", "alpha_away", "beta_home", "beta_away", "xi1", "xi2", "log-likelihood"
)


M1_SE <- as_tibble(cbind(opt1$par, get_standard_errors(hessian_m1)))
colnames(M1_SE) <- c("Estimate, M1", "SE, M1")
M1_SE["parameter"] <- c("gamma", "goal_diff", "points_per_match", "S_home", "C_home", "S_away", "C_away", "Intercept")
M1_SE

M2_SE <- as_tibble(cbind(opt2$par, get_standard_errors(hessian_m2)))
colnames(M2_SE) <- c("Estimate M2", "SE, M2")
M2_SE["parameter"] <- c("gamma" , "lambda-2", "lambda-1", "lambda0","lambda1", "lambda2", "mu-2",  "mu-1",  "mu0", "mu1", "mu2", "goal_diff", "points_per_match", "S_home", "C_home", "S_away", "C_away","Intercept")

M3_SE <- as_tibble(cbind(opt3$par, get_standard_errors(hessian_m3)))
colnames(M3_SE) <- c("Estimate, M3", "SE, M3")
M3_SE["parameter"] <- c("gamma" , "lambda-2", "lambda-1", "lambda0","lambda1", "lambda2", "mu-2",  "mu-1",  "mu0", "mu1", "mu2", "goal_diff", "points_per_match", "S_home", "C_home", "S_away", "C_away","Intercept", "xi1", "xi2")

M4_SE <- as_tibble(cbind(opt4$par, get_standard_errors(hessian_m4)))
colnames(M4_SE) <- c("Estimate, M4", "SE, M4")
M4_SE["parameter"] <- c("gamma", "goal_diff", "points_per_match", "S_home", "C_home", "S_away", "C_away", "Intercept", "xi1", "xi2")

M5_SE <- as_tibble(cbind(round(m5_coefs, 4), round(get_standard_errors(hessian_m5),4)))
colnames(M5_SE) <- c("Estimate, M5", "SE, M5")
M5_SE["parameter"] <- c("gamma", "goal_diff", "points_per_match", "S_home", "C_home", "S_away", "C_away", "Intercept",  "alpha_home", "alpha_away", "beta_home", "beta_away")

M6_SE <- as_tibble(cbind(round(m6_coefs, 4), round(get_standard_errors(hessian_m6),4)))
colnames(M6_SE) <- c("Estimate, M6", "SE, M6")
M6_SE["parameter"] <- c("gamma", "goal_diff", "points_per_match", "S_home", "C_home", "S_away", "C_away", "Intercept",  "alpha_home", "alpha_away", "beta_home", "beta_away", "xi1", "xi2")

M7_SE <- as_tibble(cbind(round(m7_coefs, 4), round(get_standard_errors(hessian_m7),4)))
colnames(M7_SE) <- c("Estimate, M7", "SE, M7")
M7_SE["parameter"] <- c("gamma" , "lambda-2", "lambda-1", "lambda0","lambda1", "lambda2", "mu-2",  "mu-1",  "mu0", "mu1", "mu2",
  "goal_diff", "points_per_match", "S_home", "C_home", "S_away", "C_away","Intercept", "alpha_home", "alpha_away", "beta_home", "beta_away")

M8_SE <- as_tibble(cbind(round(m8_coefs, 4), round(get_standard_errors(hessian_m8),4)))
colnames(M8_SE) <- c("Estimate, M8", "SE, M8")
M8_SE["parameter"] <- c("gamma" , "lambda-2", "lambda-1", "lambda0","lambda1", "lambda2", "mu-2",  "mu-1",  "mu0", "mu1", "mu2",
  "goal_diff", "points_per_match", "S_home", "C_home", "S_away", "C_away","Intercept", "alpha_home", "alpha_away", "beta_home", "beta_away", "xi1", "xi2")

SE_dat <- list(M1_SE, M2_SE, M3_SE, M4_SE, M5_SE, M6_SE, M7_SE, M8_SE) %>%
    Reduce(function(dtf1,dtf2) full_join(dtf1,dtf2,by="parameter"), .)
SE_dat <- SE_dat %>% select(parameter, colnames(SE_dat))



SE_dat %>% filter(parameter %in% c("xi1", "xi2")) %>%
  select(`Estimate, M4`, `SE, M4`) %>% mutate(z = `Estimate, M4`/`SE, M4`) %>%
  mutate(p_val = pnorm(z, lower.tail=FALSE))


SE_dat %>% filter(parameter %in% c( "alpha_home", "alpha_away", "beta_home", "beta_away")) %>%
  select(`Estimate, M5`, `SE, M5`) %>% mutate(z = `Estimate, M5`/`SE, M5`) %>%
  mutate(p_val = pnorm(z, lower.tail=FALSE)) %>% pull(p_val) %>% round(digits = 4)

SE_dat %>% filter(parameter %in% c("xi1", "xi2", "alpha_home", "alpha_away", "beta_home", "beta_away")) %>%
  select(`Estimate, M4`, `SE, M4`, `Estimate, M5`, `SE, M5`, `Estimate, M6`, `SE, M6`)

SE_dat %>% filter(parameter %in% c("xi1", "xi2", "alpha_home", "alpha_away", "beta_home", "beta_away")) %>%
    select(`Estimate, M4`, `SE, M4`, `Estimate, M5`, `SE, M5`, `Estimate, M6`, `SE, M6`) %>% 
    mutate(z = `Estimate, M4`/`SE, M4`) %>%  mutate(p_val_m4 = pnorm(z, lower.tail=FALSE)) %>% select(-z) %>%
    mutate(z = `Estimate, M5`/`SE, M5`) %>%  mutate(p_val_m5 = pnorm(z, lower.tail=FALSE)) %>% select(-z) %>%
    mutate(z = `Estimate, M6`/`SE, M6`) %>%  mutate(p_val_m6 = pnorm(z, lower.tail=FALSE)) %>% select(-z) %>%
    select(`Estimate, M4`, `SE, M4`, p_val_m4, `Estimate, M5`, `SE, M5`, p_val_m5,`Estimate, M6`, `SE, M6`, p_val_m6) %>%
    xtable(digits = -3) %>% print.xtable(include.rownames = FALSE)
