require(dplyr)
require(magrittr)
require(ggplot2)

rm(list = ls())

set.seed(11091987)

# number of trials
n_trial <- 10000

# function to determine if shot is inside rim
shot_made <- function(shots, radius = 1, center_x = 0, center_y = 0){
  (shots[,1] - center_x)^2 + (shots[,2] - center_y)^2 < radius^2
}

# function to find random normal distribution sd equals 75% probability of making free throw
shot_prob <- function(sd_val, n_trial = 100){
  shots <- cbind(rnorm(n_trial, mean = 0, sd = sd_val),
                 rnorm(n_trial, mean = 0, sd = sd_val))
  sum(shot_made(shots)) / n_trial
}

# shot attempts - need to know what sd equals 75% probability of making free throw
sd_vals <- seq(0, 1, by = .001)

shot_probs <- lapply(sd_vals, shot_prob, n_trial = n_trial) %>%
  unlist %>%
  data_frame(sd = sd_vals,
             prob = .) %>%
  # sort so that the sd closest to 75% probability is first row
  arrange(abs(.75 - prob))

# given sd, calculate probability of successful shot if no error on x-axis
shots <- cbind(0, rnorm(n_trial, mean = 0, sd = shot_probs$sd[1]))
sum(shot_made(shots)) / n_trial

# improvement over NBA average
sum(shot_made(shots)) / n_trial - shot_probs$prob[1]



