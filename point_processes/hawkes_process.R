rm(list = ls())

print("run optim functions: ")

# if (!require("optimParallel")) {
#   install.packages("optimParallel", repos="http://cran.rstudio.com/") 
#   library("optimParallel")
# }


library(dplyr)
library(tidyr)


#load("scores.RData")
#load("event_goals.RData")
#load("events_100.RData")

source("point_processes/log_likelihoods.R")
source("data_Rfiles/data_prep_functions.R")

features <- c("goal_diff", "points_per_match", "team", "goals", "att_momentum", "def_momentum", "att_momentum_opponent", "def_momentum_opponent")

dat <- final_scores
dat <- dat %>% arrange(match_id, team)
split <- traintest_split(unique(dat$match_id), 0.9)
train_dat <- dat %>% filter(match_id %in% split$train)

#######################################################################################################################

data.passin.inplay <- create_data_optim(dat=train_dat, func=function(x) {x})
#data.passin.inplay <- create_data_optim(dat=train_dat, func=sigmoid::sigmoid)
#data.passin.inplay <- create_data_optim(dat=train_dat, func=function(x) {ifelse(x>0, x, 0)})

print("opt1: ")
nb_pars <- 8
lwr <- c(0, rep(-0.2, 7))
upr <- c(3, rep(0.2, 7))
opt1 <- optim(par = c(0.5,rep(0.1, nb_pars-1)), log.likelihood1, data=train_dat[,features], lower = lwr, upper = upr, method = "L-BFGS-B", hessian = TRUE)

print("opt2: ")
gamma <- opt1$par[1]; l.s <- rep(1,5); m.s <- rep(1,5); theta <- opt1$par[-1]
pars <- c(gamma, l.s, m.s, theta)
opt2 <- optim(par=pars, fn=log.likelihood2.full,data=data.passin.inplay, control = list(maxit=100000))

print("opt3: ")
pars <- c(opt2$par, 1, 1)
opt3 <- optim(par=pars, fn=log.likelihood3.full, data=data.passin.inplay, control = list(maxit=100000))

print("opt4: ")
pars <- c(opt1$par, 0, 0)
opt4 <- optim(par = pars, log.likelihood4.full, data=data.passin.inplay, control = list(maxit=10000), hessian = TRUE)

print("opt5: ")
pars <- c(0.1386, 0.0061,0.1375, -0.0154,  0.0114,  0.0052, -0.0131,  0.1338,   0.0010,  0.0069,  5,  5)
log.likelihood.HP.baseline(par=pars, data=data.passin.inplay)
opt5 <- optim(par = pars, log.likelihood.HP.baseline,data=data.passin.inplay, control = list(maxit=10000))

M5 <- c(opt5$par[1], rep(0, 10), opt5$par[-1], 0,0, opt5$value)
print("M5:")
print(M5)
print("-------------------")


print("opt6: ")
pars <- c(0.1386, 0.0061,0.1375, -0.0154,  0.0114,  0.0052, -0.0131,  0.1338,  0.001707, 0.001298, 5, 5, 0, 0)
log.likelihood.HP_withtime(par=pars, data=data.passin.inplay)
opt6 <- optim(par = pars, log.likelihood.HP_withtime,data=data.passin.inplay, control = list(maxit=100000))

M6 <- c(opt6$par[1], rep(0, 10), opt6$par[-1], opt6$value)
print("M6:")
print(M6)
print("-------------------")


print("opt7: ")
pars <- c(opt1$par[1], rep(1, 10), opt1$par[-1], 0.001707, 0.001298, 5, 5)
log.likelihood.HP(par=pars, data=data.passin.inplay)
opt7 <- optim(par = pars, log.likelihood.HP,data=data.passin.inplay, control = list(maxit=100000))

M7 <- c(opt7$par,0,0, opt7$value)
print("M7:")
print(M7)
print("-------------------")


print("opt8: ")
xi1 <- 0; xi2 <- 0
pars <- c(opt7$par, xi1, xi2)
log.likelihood.HP_withtime_withscore(par=pars, data=data.passin.inplay)
opt8 <- optim(par = pars, log.likelihood.HP_withtime_withscore,data=data.passin.inplay, control = list(maxit=100000))

M8 <- c(opt8$par, opt8$value)
print("M8:")
print(M8)
print("-------------------")
