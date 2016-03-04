require(dplyr)
require(magrittr)
require(ggplot2)

rm(list = ls())

set.seed(11091987)

# simulate random number draws for both players
n_draw <- 10000

p1 <- cbind(runif(n_draw), runif(n_draw))
p2 <- cbind(runif(n_draw), runif(n_draw))

# strategy - always take first number
sum(p1[,1] > p2[,1])

# strategy - always take second number
sum(p1[,2] > p2[,2])

# strategy - if first number less than .5, take second number
sum(apply(p1, 1, function(x) ifelse(x[1] < .5, x[2], x[1])) >
      apply(p2, 1, function(x) ifelse(x[1] < .5, x[2], x[1])))

# strategy - test above strategy with cutpoint at each value between 0 and 1
# system.time({
#   results <- NULL
#   for(cutpoint in seq(0.01, .99, by = .01)){
#     results <- rbind(results,
#                      c(cutpoint, sum(apply(p1, 1, function(x) ifelse(x[1] < cutpoint, x[2], x[1])) >
#                                        apply(p2, 1, function(x) ifelse(x[1] < cutpoint, x[2], x[1])))))
#   }
# })

system.time({
  results <- lapply(seq(0, 1, by = .01),
                    function(cutpoint) c(cutpoint, sum(apply(p1, 1, function(x) ifelse(x[1] < cutpoint, x[2], x[1])) >
                                                         apply(p2, 1, function(x) ifelse(x[1] < cutpoint, x[2], x[1]))))) %>%
    unlist %>%
    matrix(ncol = 2, byrow = TRUE)
})

results %<>%
  as.data.frame %>%
  tbl_df %>%
  rename(cutpoint = V1, win = V2) %>%
  mutate(win_prop = win / n_draw)

ggplot(results, aes(cutpoint, win_prop)) +
  geom_line() +
  ylim(0,1)
