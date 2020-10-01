
######## data preparation######## 

data.argument <- function(match.id, final_scores, event_goals){
  
  # fixed values 1171144
  X.H <- final_scores %>% filter(match_id == match.id) %>% select(features) %>% filter(team==1) %>%
    select(-c(team, goals)) %>% mutate(Intercept = 1)
  X.W <- final_scores %>% filter(match_id == match.id) %>% select(features) %>% filter(team==0) %>%
    select(-c(team, goals)) %>% mutate(Intercept = 1)
  X <- rbind(X.H, X.W)
  
  # interval scores
  max.minute <- final_scores %>% filter(match_id == match.id) %>% pull(max) %>% unique()
  
  match <- event_goals %>% filter(match_id == match.id)  %>%
    select(match_id, diff, minute, delta, relative_goals_sum, goal_dummy) %>%
    mutate(interval = ceiling(minute/max.minute * 100))
  
  if (dim(match)[1] != 0) {
    interval.scores <- rep(0, 100)
    for (i in 1:dim(match)[1]){
      if (i != dim(match)[1]) {
        interval.scores[pull(match, interval)[i] : pull(match, interval)[i+1]] <- pull(match, relative_goals_sum)[i]
      }
      else{
        interval.scores[pull(match, interval)[i] : 100] <- pull(match, relative_goals_sum)[i]
      }
    }
    # change 2 to 3 if one like to have additinoal parameters
    interval.scores <- if_else(interval.scores > 2, 2, interval.scores)
    interval.scores <- if_else(interval.scores < -2, -2, interval.scores)
  }
  else {interval.scores <- rep(0,100)}
  
  
  
  mylist <- list(
    "X" = X, "interval.scores" = interval.scores, "match" = match
  )
  return(mylist)
}

create_data_optim <- function(dat, func) {
  #set.seed(4321412)
  #train_games <- sample(pull(distinct(dat, match_id)), round(0.9 * dim(distinct(dat, match_id))[1]))
  
  train_games <- unique(dat$match_id)
  nb_games <- length(train_games)
  data.arg <- lapply(train_games, function(match.id) {
    data.argument(match.id, dat, event_goals)
  })
  X.arg <- sapply(1:nb_games, function(i){as.matrix(data.arg[[i]]$X)})
  interval.scores.arg <- sapply(1:nb_games, function(i){as.matrix(data.arg[[i]]$interval.scores)})
  match.arg <- sapply(1:nb_games, function(i){as.matrix(data.arg[[i]]$match)})
  
  create_lagged_game <- function(mid){
    M <- events %>% filter(match_id==mid) %>% select(H_a_sum, H_d_sum, W_a_sum, W_d_sum) %>% as.matrix()
    return(M)
  }
  
  Z <- lapply(train_games, function(mid){create_lagged_game(mid)})
  
  Z_home <- sapply(1:length(Z), function(x) {
    func(Z[[x]][,1] - Z[[x]][,4])
    #func(Z[[x]][,1])
  })

  Z_away <- sapply(1:length(Z), function(x) {
    func(Z[[x]][,3] - Z[[x]][,2])
    #func(Z[[x]][,3])
  })
  
  data.passin.inplay <- list(
    "X" = X.arg, "interval.scores" = interval.scores.arg,
    "match.arg" = match.arg, "Z_home"=Z_home, "Z_away"=Z_away
  )
  return(data.passin.inplay)
}

create_Z_Y_dat <- function(func){
  data.passin.inplay <- create_data_optim(dat=dat, func=func)
  Z_home <- data.passin.inplay$Z_home; Z_away <- data.passin.inplay$Z_away
  
  Y_home <- matrix(data = NA, nrow = 100, ncol = 617)
  Y_away <- matrix(data = NA, nrow = 100, ncol = 617)
  for (i in 1:617){
    y_support <- as_tibble(data.passin.inplay$match.arg[[i]][,c(6,7)])
    if (dim(y_support)[2] != 1) {
      y_home <- rep(0, 100); y_away <- rep(0, 100)
      y_home[y_support %>% filter(goal_dummy == 1) %>% pull(interval)] <- 1
      y_away[y_support %>% filter(goal_dummy == 0) %>% pull(interval)] <- 1
    }
    else {
      if (pull(y_support[1,]) == 1) {
        y_home[pull(y_support[2,])] <- 1
      }
      else {
        y_away[pull(y_support[2,])] <- 1
      }
    }
    Y_home[,i] <- y_home; Y_away[,i] <- y_away
  }
  
  Z_home_tibble <- gather(as_tibble(Z_home)); Z_home_tibble["loc"] <- "home"
  Z_away_tibble <- gather(as_tibble(Z_away)); Z_away_tibble["loc"] <- "away"
  Z_tibble <- bind_rows(Z_home_tibble, Z_away_tibble)
  colnames(Z_tibble) <- c("key", "Z", "loc")
  Y_home_tibble <- gather(as_tibble(Y_home)); Y_home_tibble["loc"] <- "home"
  Y_away_tibble <- gather(as_tibble(Y_away)); Y_away_tibble["loc"] <- "away"
  Y_tibble <- bind_rows(Y_home_tibble, Y_away_tibble)
  colnames(Y_tibble) <- c("key", "Y", "loc")
  
  Z_Y <- Z_tibble %>% bind_cols(Y_tibble) %>% select(-c(key1, loc1))
  Z_Y["Y"] <- as.factor(Z_Y$Y)
  return(Z_Y)
}

add_roll_func <- function(func, dat, lag) {
  my_roll_func <- function(my_key, my_loc) {
    z <- dat %>% filter(key==my_key & loc==my_loc) %>% pull(Z)
    rs <- c(rep(0, lag-1), zoo::rollapply(z, lag, func))
    for (i in (lag-1):1){
      rs[i] <- func(z[1:i])  
    }
    return(rs)
  }
  
  rs_home <- c()
  for (k in unique(dat$key)) {
    rs_home <- c(rs_home, my_roll_func(my_key=k, my_loc="home"))
  }
  rs_away <- c()
  for (k in unique(dat$key)) {
    rs_away <- c(rs_away, my_roll_func(my_key=k, my_loc="away"))
  }
  R <- c(rs_home, rs_away)
  return(R)
}


######## Poisson Process ########

## First step, Homogeneous Poisson process

log.likelihood1 <- function(par, data){
    H <- data %>% filter(team==1) %>% pull(goals)
    W <- data %>% filter(team==0) %>% pull(goals)
    X <- data %>% select(-goals)
    X["intercept"] <- 1
    X.H <- X %>% filter(team==1) %>% select(-team)
    X.W <- X %>% filter(team==0) %>% select(-team)
    gamma <- par[1];theta <- as.matrix(par[-1])
    ll <- sum( (as.matrix(X.H) %*% theta + gamma) * H + as.matrix(X.W) %*% theta* W - exp(as.matrix(X.H) %*% theta + gamma) - exp(as.matrix(X.W) %*% theta) -log(factorial(H)) -log(factorial(W)) )
    return(-ll)
}

## Second step, Inhomogenous Poisson process, add scores

log.likelihood2.full <- function(par, data){
    xh.seq <- seq(1,13, 2);xw.seq <- seq(2,14, 2)
    nb_games <- 617
    ll <- sapply(1:nb_games, function(i){
        
        # data
        X.H <- data$X[,i][xh.seq]
        X.W <- data$X[,i][xw.seq]
        interval.scores <- data$interval.scores[,i]
        match <- data$match.arg[[i]]
        
        # define parameter
        gamma <- par[1]
        lambda.s <- par[2:6]
        mu.s <- par[7:11]
        theta <- par[12:18]
        
        lambda.k <- as.double(matrix(X.H, nrow=1) %*% matrix(theta) + gamma)
        mu.k <- as.double(matrix(X.W, nrow=1) %*% matrix(theta))
        
        # piece wise constant function
        integral.lambda <- sum(sapply(-2:2, function(s){
            mean(interval.scores[1:100] == s) * (lambda.s[s+3])
        }))
        integral.mu <- sum(sapply(-2:2, function(s){
            mean(interval.scores[1:100] == s) * (mu.s[s+3])
        }))
        
        intgrls <- - exp(lambda.k) * integral.lambda - exp(mu.k) * integral.mu
        
        ##### get value of sum #####
        
        req.intervals <- ifelse(match[,"interval"] == 1, 2, match[,"interval"])
        J <- match[,"goal_dummy"]
        if (dim(match)[1] != 0) {
          sm <- sum(sapply(1: dim(match)[1], function(l){
            lambda.s.l <- lambda.s[(interval.scores[req.intervals - 1]+3)[l]]
            mu.s.l <- mu.s[(interval.scores[req.intervals - 1]+3)[l]]
            #m <- J[l] * (lambda.k + log(lambda.s.l)) + (1-J[l]) * (mu.k + log(mu.s.l))
            m <- J[l] * (log(exp(lambda.k)* lambda.s.l)) + (1-J[l]) * (log(exp(mu.k)* mu.s.l))
          }))
        }
        else {sm <- 0}
        
        
        ll.onegame <- intgrls + sm
    })
   return(-sum(ll))
}



## Third step, Inhomogenous Poisson process, add scores, add time

log.likelihood3.full <- function(par, data){
  xh.seq <- seq(1,13, 2);xw.seq <- seq(2,14, 2)
  nb_games <- 617
  ll <- sapply(1:nb_games, function(i){
    
    # data
    X.H <- data$X[,i][xh.seq]
    X.W <- data$X[,i][xw.seq]
    interval.scores <- data$interval.scores[,i]
    match <- data$match.arg[[i]]
    
    # define parameter
    gamma <- par[1]
    lambda.s <- par[2:6]
    mu.s <- par[7:11]
    theta <- par[12:18]
    xi1 <- par[19]; xi2 <- par[20]
    
    lambda.k <- as.double(matrix(X.H, nrow=1) %*% matrix(theta) + gamma)
    mu.k <- as.double(matrix(X.W, nrow=1) %*% matrix(theta))
    
    # piece wise constant function
    integral.lambda <- sum(sapply(-2:2, function(s){
      mean(interval.scores[1:100] == s) * lambda.s[s+3]
    }))
    integral.mu <- sum(sapply(-2:2, function(s){
      mean(interval.scores[1:100] == s) * mu.s[s+3]
    }))
    
    # add time
    lambda.time <- xi1 * 0.5
    mu.time <- xi2 * 0.5
    
    intgrls <- - exp(lambda.k) * integral.lambda - exp(mu.k) * integral.mu - lambda.time - mu.time
    
    ##### get value of sum #####
    
    req.intervals <- ifelse(match[,"interval"] == 1, 2, match[,"interval"])
    J <- match[,"goal_dummy"]
    
    if (dim(match)[1] != 0) {
      sm <- sum(sapply(1: dim(match)[1], function(l){
        lambda.s.l <- lambda.s[(interval.scores[req.intervals - 1]+3)[l]]
        mu.s.l <- mu.s[(interval.scores[req.intervals - 1]+3)[l]]
        interval <- match[[l, 7]]
        m <- J[l] * log(exp(lambda.k)* lambda.s.l + xi1 * interval/100) + (1-J[l]) * log(exp(mu.k)* mu.s.l + xi2 * interval/100)
      }))
    }
    else {sm <- 0}
    ll.onegame <- intgrls + sm
  })
  return(-sum(ll))
}


## Fourth step: time without score
log.likelihood4.full <- function(par, data){
    xh.seq <- seq(1,13, 2);xw.seq <- seq(2,14, 2)
    nb_games <- 617
    ll <- sapply(1:nb_games, function(i){
        
        # data
        X.H <- data$X[,i][xh.seq]
        X.W <- data$X[,i][xw.seq]
        match <- data$match.arg[[i]]
        
        # define parameter
        gamma <- par[1]
        theta <- par[2:8]
        xi1 <- par[9]; xi2 <- par[10]
        
        lambda.k <- as.double(matrix(X.H, nrow=1) %*% matrix(theta) + gamma)
        mu.k <- as.double(matrix(X.W, nrow=1) %*% matrix(theta))
        
        # add time
        lambda.time <- xi1 * 0.5
        mu.time <- xi2 * 0.5
        
        intgrls <- - exp(lambda.k)  - exp(mu.k) - lambda.time - mu.time
        
        ##### get value of sum #####
        
        J <- match[,"goal_dummy"]
        
        if (dim(match)[1] != 0) {
          sm <- sum(sapply(1: dim(match)[1], function(l){
              interval <- match[[l, 7]]
              m <- J[l] * log(exp(lambda.k) + xi1 * interval/100) + (1-J[l]) * log(exp(mu.k) + xi2 * interval/100)
          }))
        }
        else {sm <- 0}
        ll.onegame <- intgrls + sm
    })
   return(-sum(ll))
}


######## Hawkes Process ########

## Exponential decay function, different specifications

hp_classic <- function(y_vec, a, b) {
  exp.decay <- function(t){ 
    e.d <- sum(sapply(y_vec, function(t_i){
      ifelse(t_i < t, a * exp(-b * (t - t_i) ), 0 )
    }))
    return(e.d)
  }
  hp.vec <- sapply(1:100, function(t){
    exp.decay(t)
  })
  return(hp.vec)
}

my_integrate_noreset <- function(alpha, beta, Z_inplay){
  int <- sapply(2:100, function(tau){
    sum(sapply(1:(tau-1), function(tau_i){
      exp(- beta * (tau - tau_i)/100) * Z_inplay[tau_i]
    }))
  })*0.01
  return(alpha * int)
}

my_integrate_noreset_reformulated <- function(alpha, beta, Z_inplay){
  exp_betas <- exp(- (beta * 1:99/100))
  int <- sapply(1:99, function(x) {
    Z_inplay[x] * sum(exp_betas[1:(100-x)])
  })*0.01
  return(alpha * int)
}

hp_rate <- function(t, Z_inplay, alpha, beta) {
  hp <- sum(sapply(1:(t-1), function(tau){
    exp(-beta * ((t-tau)/100)) * Z_inplay[tau]
  }))
  return(alpha*hp)
}

## log likelihood

log.likelihood.HP.baseline <- function(par, data){
  xh.seq <- seq(1,13, 2);xw.seq <- seq(2,14, 2)
  nb_games <- 617
  #nb_games <- 5
  ll <- sapply(1:nb_games, function(i){
    
    # data
    X.H <- data$X[,i][xh.seq]
    X.W <- data$X[,i][xw.seq]
    match <- data$match.arg[[i]]
    
    # first attacking measure, then defensive measure from opponent.
    Z.H <- data$Z_home[,i]
    Z.W <- data$Z_away[,i]

    # define parameter
    gamma <- par[1];theta <- par[2:8]
    a.h <- par[9]; b.h <- par[11];
    a.w <- par[10]; b.w <- par[12]
    
    ##### integrals #####
    lambda.k <- as.double(matrix(X.H, nrow=1) %*% matrix(theta) + gamma)
    mu.k <- as.double(matrix(X.W, nrow=1) %*% matrix(theta))
    
    # add Hawkes process integral
    integral.HP.lambda.vec <- c(0, my_integrate_noreset_reformulated(alpha = a.h, beta = b.h, Z_inplay = Z.H))
    integral.HP.mu.vec <- c(0, my_integrate_noreset_reformulated(alpha = a.w, beta = b.w, Z_inplay = Z.W))
    
    intgrls <- - exp(lambda.k) - exp(mu.k) - sum(integral.HP.lambda.vec) - sum(integral.HP.mu.vec)
    
    ##### get value of sum #####
    req.intervals <- ifelse(match[,"interval"] == 1, 2, match[,"interval"])
    J <- match[,"goal_dummy"]
    
    if (dim(match)[1] != 0) {
      sm <- sum(sapply(1:dim(match)[1], function(l){
        interval <- req.intervals[l]
        
        lambda.hawkes <- hp_rate(t = interval, alpha = a.h, beta = b.h, Z_inplay = Z.H)
        mu.hawkes <- hp_rate(t = interval, alpha = a.w, beta = b.w, Z_inplay = Z.W)
        
        m_h <- J[l] * log(exp(lambda.k) + lambda.hawkes)
        m_w <- (1-J[l]) * log(exp(mu.k) + mu.hawkes)
        return(m_h + m_w)
      }))
    }
    else {sm <- 0}
    ll.onegame <- intgrls + sm
    #print(paste(ll.onegame, i))
    return(ll.onegame)
  })
  return(-sum(ll))
}


log.likelihood.HP_withtime <- function(par, data){
  xh.seq <- seq(1,13, 2);xw.seq <- seq(2,14, 2)
  nb_games <- 617
  ll <- sapply(1:nb_games, function(i){
    
    # data
    X.H <- data$X[,i][xh.seq]
    X.W <- data$X[,i][xw.seq]
    interval.scores <- data$interval.scores[,i]
    match <- data$match.arg[[i]]
    
    # first attacking measure, then defensive measure from opponent.
    Z.H <- data$Z_home[,i]
    Z.W <- data$Z_away[,i]

    # define parameter
    gamma <- par[1];theta <- par[2:8]
    a.h <- par[9]; b.h <- par[11];
    a.w <- par[10]; b.w <- par[12]
    xi1 <- par[13]; xi2 <- par[14]
    
    ##### integrals #####
    lambda.k <- as.double(matrix(X.H, nrow=1) %*% matrix(theta) + gamma)
    mu.k <- as.double(matrix(X.W, nrow=1) %*% matrix(theta))
    
    # time
    lambda.time <- xi1 * 0.5
    mu.time <- xi2 * 0.5
    
    # add Hawkes process integral
    integral.HP.lambda.vec <- c(0, my_integrate_noreset_reformulated(alpha = a.h, beta = b.h, Z_inplay = Z.H))
    integral.HP.mu.vec <- c(0, my_integrate_noreset_reformulated(alpha = a.w, beta = b.w, Z_inplay = Z.W))
    
    intgrls <- - exp(lambda.k) - exp(mu.k) - sum(integral.HP.lambda.vec) - sum(integral.HP.mu.vec) - lambda.time - mu.time
    
    ##### get value of sum #####
    req.intervals <- ifelse(match[,"interval"] == 1, 2, match[,"interval"])
    J <- match[,"goal_dummy"]
    
    if (dim(match)[1] != 0) {
      sm <- sum(sapply(1:dim(match)[1], function(l){
        interval <- req.intervals[l]
        
        lambda.hawkes <- hp_rate(t = interval, alpha = a.h, beta = b.h, Z_inplay = Z.H)
        mu.hawkes <- hp_rate(t = interval, alpha = a.w, beta = b.w, Z_inplay = Z.W)
        
        m_h <- J[l] * log(exp(lambda.k) + lambda.hawkes + xi1 * interval/100)
        m_w <- (1-J[l]) * log(exp(mu.k) + mu.hawkes + xi2 * interval/100)
        return(m_h + m_w)
      }))
    }
    else {sm <- 0}
    ll.onegame <- intgrls + sm
    #print(paste(l,m_h, m_w, i))
    return(ll.onegame)
  })
  return(-sum(ll))
}


log.likelihood.HP <- function(par, data){
  xh.seq <- seq(1,13, 2);xw.seq <- seq(2,14, 2)
  nb_games <- 617
  #nb_games <- 5
  ll <- sapply(1:nb_games, function(i){
    
    # data
    X.H <- data$X[,i][xh.seq]
    X.W <- data$X[,i][xw.seq]
    interval.scores <- data$interval.scores[,i]
    match <- data$match.arg[[i]]
    
    Z.H <- data$Z_home[,i]
    Z.W <- data$Z_away[,i]
    
    # define parameter
    gamma <- par[1];theta <- par[12:18]
    a.h <- par[19]; b.h <- par[21];
    a.w <- par[20]; b.w <- par[22]
    
    lambda.s <- par[2:6]
    mu.s <- par[7:11]

    ##### integrals #####
    lambda.k <- as.double(matrix(X.H, nrow=1) %*% matrix(theta) + gamma)
    mu.k <- as.double(matrix(X.W, nrow=1) %*% matrix(theta))
    
    # piece wise constant function
    integral.lambda <- sum(sapply(-2:2, function(s){
      mean(interval.scores[1:100] == s) * lambda.s[s+3]
    }))
    integral.mu <- sum(sapply(-2:2, function(s){
      mean(interval.scores[1:100] == s) * mu.s[s+3]
    }))
    
    # add Hawkes process integral
    integral.HP.lambda.vec <- c(0, my_integrate_noreset_reformulated(alpha = a.h, beta = b.h, Z_inplay = Z.H))
    integral.HP.mu.vec <- c(0, my_integrate_noreset_reformulated(alpha = a.w, beta = b.w, Z_inplay = Z.W))
    
    intgrls <- - exp(lambda.k) * integral.lambda - exp(mu.k) * integral.mu - sum(integral.HP.lambda.vec) - sum(integral.HP.mu.vec)
    
    ##### get value of sum #####
    req.intervals <- ifelse(match[,"interval"] == 1, 2, match[,"interval"])
    J <- match[,"goal_dummy"]
    
    if (dim(match)[1] != 0) {
      
      sm <- sum(sapply(1:dim(match)[1], function(l){
        lambda.s.l <- lambda.s[(interval.scores[req.intervals - 1]+3)[l]]
        mu.s.l <- mu.s[(interval.scores[req.intervals - 1]+3)[l]]
        interval <- req.intervals[l]
        
        lambda.hawkes <- hp_rate(t = interval, alpha = a.h, beta = b.h, Z_inplay = Z.H)
        mu.hawkes <- hp_rate(t = interval, alpha = a.w, beta = b.w, Z_inplay = Z.W)
        m_h <- J[l] * log(exp(lambda.k)* lambda.s.l + lambda.hawkes)
        m_w <- (1-J[l]) * log(exp(mu.k)* mu.s.l + mu.hawkes)
        return(m_h + m_w)
      }))
    }
    else {sm <- 0}
    ll.onegame <- intgrls + sm
    #print(paste(l,m_h, m_w, i))
    return(ll.onegame)
  })
  return(-sum(ll))
}


log.likelihood.HP_withtime_withscore <- function(par, data){
  xh.seq <- seq(1,13, 2);xw.seq <- seq(2,14, 2)
  nb_games <- 617
  ll <- sapply(1:nb_games, function(i){
    
    # data
    X.H <- data$X[,i][xh.seq]
    X.W <- data$X[,i][xw.seq]
    interval.scores <- data$interval.scores[,i]
    match <- data$match.arg[[i]]
    
    Z.H <- data$Z_home[,i]
    Z.W <- data$Z_away[,i]

    # define parameter  
    gamma <- par[1];lambda.s <- par[2:6]
    mu.s <- par[7:11];theta <- par[12:18]
    a.h <- par[19]; b.h <- par[21];a.w <- par[20]; b.w <- par[22]
    xi1 <- par[23]; xi2 <- par[24]

    ##### integrals #####
    lambda.k <- as.double(matrix(X.H, nrow=1) %*% matrix(theta) + gamma)
    mu.k <- as.double(matrix(X.W, nrow=1) %*% matrix(theta))
    
    # piece wise constant function
    integral.lambda <- sum(sapply(-2:2, function(s){
        mean(interval.scores[1:100] == s) * lambda.s[s+3]
    }))
    integral.mu <- sum(sapply(-2:2, function(s){
        mean(interval.scores[1:100] == s) * mu.s[s+3]
    }))

    # time
    lambda.time <- xi1 * 0.5
    mu.time <- xi2 * 0.5
    
    # add Hawkes process integral
    integral.HP.lambda.vec <- c(0, my_integrate_noreset_reformulated(alpha = a.h, beta = b.h, Z_inplay = Z.H))
    integral.HP.mu.vec <- c(0, my_integrate_noreset_reformulated(alpha = a.w, beta = b.w, Z_inplay = Z.W))
    
    intgrls <- - exp(lambda.k) * integral.lambda - exp(mu.k) * integral.mu - sum(integral.HP.lambda.vec) - sum(integral.HP.mu.vec) - lambda.time - mu.time
    
    ##### get value of sum #####
    req.intervals <- ifelse(match[,"interval"] == 1, 2, match[,"interval"])
    J <- match[,"goal_dummy"]
    
    if (dim(match)[1] != 0) {
      sm <- sum(sapply(1:dim(match)[1], function(l){
        lambda.s.l <- lambda.s[(interval.scores[req.intervals - 1]+3)[l]]
        mu.s.l <- mu.s[(interval.scores[req.intervals - 1]+3)[l]]
        interval <- req.intervals[l]
        
        lambda.hawkes <- hp_rate(t = interval, alpha = a.h, beta = b.h, Z_inplay = Z.H)
        mu.hawkes <- hp_rate(t = interval, alpha = a.w, beta = b.w, Z_inplay = Z.W)
        m_h <- J[l] * log(exp(lambda.k)* lambda.s.l + lambda.hawkes + xi1 * interval/100)
        m_w <- (1-J[l]) * log(exp(mu.k)* mu.s.l + mu.hawkes + xi2 * interval/100)
        return(m_h + m_w)
      }))
    }
    else {sm <- 0}
    ll.onegame <- intgrls + sm
    #print(paste(l,m_h, m_w, i))
    return(ll.onegame)
  })
  return(-sum(ll))
}



