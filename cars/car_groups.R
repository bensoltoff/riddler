require(dplyr)
require(snowfall)
require(ggplot2)

rm(list = ls())

set.seed(11091987)

car_groups <- function(n_car, n_trial = 100){
  # simulate speeds
  cars <- replicate(n_trial, sample(1:100, n_car, replace = TRUE))

    # get cumulative minimum speeds
  cars_min <- apply(cars, 2, cummin)
  
  # unique groups
  groups <- apply(cars_min, 2, FUN = function(x) length(unique(x)))
  
  # number of groups
  mean(groups)
}

# set constants
n_cars <- c(2:299, seq(300, 10000, by = 200))
system.time({
  sfInit(parallel = TRUE, cpus = 4)
  trials <- data_frame(n_car = n_cars,
           groups = sfLapply(n_cars, car_groups, n_trial = 10000) %>%
             unlist)
  sfStop()
})

ggplot(trials, aes(n_car, groups)) +
  geom_point() +
  geom_line() +
  geom_smooth(se = FALSE)


