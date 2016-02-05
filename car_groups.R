require(dplyr)

rm(list = ls())

set.seed(11091987)

# set constants
n <- 100

# simulate speeds
cars <- sample(1:100, n, replace = TRUE)

# get cumulative minimum speeds
cars_min <- cummin(cars)

# unique groups
groups <- unique(cars_min)

# number of groups
length(groups)