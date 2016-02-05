require(dplyr)

rm(list = ls())

set.seed(11091987)

# set constants
n_car <- 1000
n_trial <- 1000

# simulate speeds
cars <- replicate(n_trial, sample(1:100, n_car, replace = TRUE))

# get cumulative minimum speeds
cars_min <- apply(cars, 2, cummin)

# unique groups
groups <- apply(cars_min, 2, FUN = function(x) length(unique(x)))

# number of groups
mean(groups)



