rm(list = ls())
library(dplyr)
library(tidyr)

load("/home/simon/Dropbox/Master_Project/code/Rcode/main/data/event_scores.RData")

time.shape <- function(m.id, time.incr) {
    dat <- events.id[events.id$match_id == m.id,]
    time.increment <- max(dat$minute, na.rm = TRUE) / time.incr
    seq.1000 <- seq(0, max(dat$minute, na.rm = TRUE), by = time.increment)
    
    # Home team
    hometeam_name = as.character(dat$team1_name[1]) 
    H_a_sum <- c()
    H_d_sum <- c()
    # Away team
    awayteam_name = as.character(dat$team2_name[1]) 
    W_a_sum <- c()
    W_d_sum <- c()
    for (i in 2:length(seq.1000)) {
        sums.list <- dat %>%
            filter(minute >= seq.1000[i-1] & minute < seq.1000[i]) %>%
            filter(team_name == hometeam_name) %>%
            summarise(H_a_sum = sum(att, na.rm = TRUE), H_d_sum = sum(def, na.rm = TRUE)) %>%
            select(H_a_sum, H_d_sum) %>% as.list()
        H_a_sum <- c(H_a_sum, sums.list$H_a_sum[1])
        H_d_sum <- c(H_d_sum, sums.list$H_d_sum[1])
        
        sums.list <- dat %>%
            filter(minute >= seq.1000[i-1] & minute < seq.1000[i]) %>%
            filter(team_name == awayteam_name) %>%
            summarise(W_a_sum = sum(att, na.rm = TRUE), W_d_sum = sum(def, na.rm = TRUE)) %>%
            select(W_a_sum, W_d_sum) %>% as.list()
        W_a_sum <- c(W_a_sum, sums.list$W_a_sum[1])
        W_d_sum <- c(W_d_sum, sums.list$W_d_sum[1])
    }
    
    H_a_sum[is.na(H_a_sum)] <- 0
    H_d_sum[is.na(H_d_sum)] <- 0
    W_a_sum[is.na(W_a_sum)] <- 0
    W_d_sum[is.na(W_d_sum)] <- 0
    
    my_list <- list("H_a_sum" = H_a_sum, "H_d_sum" = H_d_sum,
                    "W_a_sum" = W_a_sum, "W_d_sum" = W_d_sum)
    return(my_list)
}

# ## Premier league: 39
# events.id <- event_scores %>% filter(tournament_id == 39) %>%
#     select(att, def, minute, half, match_id, team_name, action_name, team1_name, team2_name)
events.id <- event_scores %>% select(att, def, minute, half, match_id, team_name, action_name, team1_name, team2_name)


goal.mins <- event_scores %>%
    filter(action_name == "Goal") %>%
    select(match_id, minute, team_name, team1_name, team2_name)


vec.high.scores <- c("Goal", "Assist", "Key assist",
                     "Playing in scoring attacks", "Extra attacking pass Assist")
events.id <- events.id %>% 
    mutate(att = replace(att, action_name %in% vec.high.scores, 0))

time.incr <- 100
events.shaped.list <- sapply(as.character(unique(events.id$match_id)),
       FUN = function(i){
           print(i)
           Sys.sleep(1)
           time.shape(m.id = as.integer(i), time.incr = time.incr)
        },
       simplify = FALSE,USE.NAMES = TRUE
)

fill.events <- sapply(as.character(unique(events.id$match_id)), function(i) {
    rbind(matrix(unlist(events.shaped.list[i]), ncol = 4))
})

fill.events <- gather(as_tibble(fill.events))
fill.events <- fill.events %>% replace(., is.na(.), 0)

fill.events["dummy"] <- rep(c(rep("H_a_sum", time.incr), rep("H_d_sum", time.incr),
                              rep("W_a_sum", time.incr),rep("W_d_sum", time.incr)),
                            length(unique(events.id$match_id)))

fill.events.2 <- fill.events %>%
    group_by(key) %>%
    mutate(row = row_number()) %>%
    pivot_wider(names_from = dummy, values_from = value) %>%
    select(-row)

H_a_sum_filled <- drop_na(fill.events.2[,c(1,2)])
H_d_sum_filled <- drop_na(fill.events.2[,c(1,3)])
W_a_sum_filled <- drop_na(fill.events.2[,c(1,4)])
W_d_sum_filled <- drop_na(fill.events.2[,c(1,5)])

match.ids <- H_a_sum_filled %>% pull(key)
H_a_sum_filled <- H_a_sum_filled %>% pull(H_a_sum)
H_d_sum_filled <- H_d_sum_filled %>% pull(H_d_sum)
W_a_sum_filled <- W_a_sum_filled %>% pull(W_a_sum)
W_d_sum_filled <- W_d_sum_filled %>% pull(W_d_sum)

fill.events.final <- as_tibble(cbind(match.ids, H_a_sum_filled, H_d_sum_filled,
                           W_a_sum_filled, W_d_sum_filled))


fill.events.final$H_a_sum_filled <- as.numeric(fill.events.final$H_a_sum_filled)
fill.events.final$H_d_sum_filled <- as.numeric(fill.events.final$H_d_sum_filled)
fill.events.final$W_a_sum_filled <- as.numeric(fill.events.final$W_a_sum_filled)
fill.events.final$W_d_sum_filled <- as.numeric(fill.events.final$W_d_sum_filled)

time.incr.df <- events.id %>%
    group_by(match_id) %>%
    summarise(time.increment = max(minute, na.rm = TRUE) / time.incr,
              max = max(minute, na.rm = TRUE))

time.incr.vec <- c()
for (i in 1:dim(time.incr.df)[1]){
    time.incr.vec <- c(time.incr.vec, seq(0, pull(time.incr.df[i,3]), by = pull(time.incr.df[i,2]))[-1])
}
fill.events.final["time"] <- time.incr.vec

events <- fill.events.final 
rm(fill.events.final, fill.events.2, fill.events)
colnames(events) <- c("match_id", "H_a_sum", "H_d_sum", "W_a_sum", "W_d_sum", "time")

## Normalize data
normalize <- function(X) (X - mean(X))/sd(X)

events$H_a_sum[!is.na(events.id$H_a_sum)] <- normalize(events.id$H_a_sum[!is.na(events.id$H_a_sum)])
events$H_d_sum[!is.na(events.id$H_d_sum)] <- normalize(events.id$H_d_sum[!is.na(events.id$H_d_sum)])
events$W_a_sum[!is.na(events.id$W_a_sum)] <- normalize(events.id$W_a_sum[!is.na(events.id$W_a_sum)])
events$W_d_sum[!is.na(events.id$W_d_sum)] <- normalize(events.id$W_d_sum[!is.na(events.id$W_d_sum)])

goal.dummy <- c()
for (m.id in unique(events$match_id)) {
    ev.id <- events %>% filter(match_id == m.id)
    mins <- goal.mins %>% filter(match_id == m.id)
    goal.dummy.vec <- rep(0, time.incr)
    if (dim(mins)[1] == 0) {
        goal.dummy <- c(goal.dummy, goal.dummy.vec)
        next
    }
    for (j in 1:dim(mins)[1]){
        for (i in 1:dim(ev.id)[1]) {
            if (pull(ev.id[i, "time"]) > pull(mins[j, "minute"])){
                if (pull(mins[j, "team_name"]) == pull(mins[j, "team1_name"])) {
                    goal.dummy.vec[i] <- 1
                }
                else {
                    goal.dummy.vec[i] <- -1
                }
                break
            }
        }
    }
    goal.dummy <- c(goal.dummy, goal.dummy.vec)
}
events["goal_dummy"] <- goal.dummy


events$match_id <- as.integer(events$match_id)
sup.dat <- event_scores %>% select(match_id, team1_id, team2_id, match_date, points, goal_diff, league_pos, points_per_match) %>%
    distinct() %>% drop_na()

events <- events %>% left_join(sup.dat) %>%
    mutate(H_measure = H_a_sum - W_d_sum) %>%
    mutate(W_measure = W_a_sum - H_d_sum)

events <- events %>%
    mutate(H_goals = case_when(goal_dummy == 1 ~ 1,TRUE ~ 0)) %>%
    mutate(W_goals = case_when(goal_dummy == -1 ~ 1, TRUE ~ 0)) %>%
    arrange(team1_id, match_date, time)

save(events, file = "events_100.RData")
