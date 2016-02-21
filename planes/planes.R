rm(list = ls())

require(dplyr)
require(ggplot2)

set.seed(11091987)

simulate_seats <- function(seats = 100, planes = 100000) {
  m <- matrix(seq_len(seats), nrow = seats, ncol = planes)
  
  m[1, ] <- sample(seats, planes, replace = TRUE)
  m[cbind(m[1, ], seq_len(planes))] <- 1
  
  for (i in seq(2, seats - 1)) {
    taken <- which(m[i, ] != i)
    
    switch_with <- sample(seq(i, seats), length(taken), replace = TRUE)
    
    replacements <- m[cbind(switch_with, taken)]
    m[cbind(switch_with, taken)] <- m[i, taken]
    m[i, taken] <- replacements
  }
  m
}

# sim for flight of 100 seats
sim <- simulate_seats(seats = 100, planes = 100000)
mean(sim[100, ] == 100)

# sim for flights of between 10 and 500 seats
seats <- 3:853

system.time({
  sims <- lapply(seats, function(x) mean(simulate_seats(seats = x, planes = 10000)[x, ] == x))
})

sims <- data_frame(seats = seats,
                   prob = unlist(sims))

ggplot(sims, aes(seats, prob)) +
  geom_point(alpha = .2) +
  geom_smooth(se = FALSE) +
  ylim(0,1) +
  labs(x = "Number of Seats on Plane",
       y = "Probability Last Passenger Gets Correct Seat") +
  theme_bw()




