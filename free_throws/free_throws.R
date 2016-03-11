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

# function to find probability of making free throw based on
# random normal distributions with mean = 0 and varying sd
shot_prob <- function(sd_val, n_trial = 100){
  shots <- cbind(rnorm(n_trial, mean = 0, sd = sd_val),
                 rnorm(n_trial, mean = 0, sd = sd_val))
  sum(shot_made(shots)) / n_trial
}

# function to draw circle in ggplot
circleFun <- function(center = c(0, 0), radius = 1, npoints = 100){
  tt <- seq(0, 2*pi, length.out = npoints)
  xx <- center[1] + radius * cos(tt)
  yy <- center[2] + radius * sin(tt)
  return(data_frame(x = xx, y = yy))
}

# function to generate shot plot
shot_plot <- function(sd = 1, n_trial = 100, granny = FALSE){
  if(granny == TRUE){
    dat <- data_frame(x = 0,
                      y = rnorm(n_trial, mean = 0, sd = sd),
                      made = shot_made(cbind(x, y)))
  } else {
    dat <- data_frame(x = rnorm(n_trial, mean = 0, sd = sd),
               y = rnorm(n_trial, mean = 0, sd = sd),
               made = shot_made(cbind(x, y)))
  }
  
  pct_made <- round(sum(dat$made) / n_trial * 100)
   
  ggplot(dat, aes(x, y, color = made)) +
    geom_point(alpha = .2) +
    geom_path(data = circleFun(), aes(x, y, color = NULL)) +
    coord_equal() +
    labs(title = paste0("Standard Deviation = ", round(sd, digits = 3), "\n",
                       pct_made, "% of Shots Made"),
         x = "X",
         y = "Y",
         color = "Shot Made") +
    theme_minimal()
}

# shot attempts - need to know what sd equals 75% probability of making free throw
## sd values to test
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

# plot NBA average
shot_plot(sd = shot_probs$sd[1], n_trial = n_trial)

# plot granny shots
shot_plot(sd = shot_probs$sd[1], n_trial = n_trial, granny = TRUE)





