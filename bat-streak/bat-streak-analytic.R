library(tidyverse)
library(tictoc)
library(here)
library(rootSolve)

theme_set(theme_minimal())

set.seed(1234)

# https://math.stackexchange.com/questions/417762/probability-of-20-consecutive-success-in-100-runs
prob_hit_streak <- function(hit_streak = 57, bat_avg = .200, seasons = 20,
                            season_length = 160, plate_app = 4) {
  # set up parameters
  r <- hit_streak
  n <- seasons * season_length
  p <- pbinom(q = 0, size = plate_app, prob = bat_avg, lower.tail = FALSE)
  q <- 1 - p
  
  # calculate roots of x near 1
  root_x <- function(x, r = r, n = n, p = p, q = q) {
    1 - x + q * p^r * x^(r + 1)
  }
  
  x <- uniroot.all(root_x, interval = c(0, (1 / p)),
                   r = r,
                   n = n,
                   p = p,
                   q = q,
                   tol = 0.00000000001)
  # keep the root that is not 1 / p
  x <- x[which(x != 1 / p)]
  
  # escape if no other root is found near one
  # probability is essentially 1
  if (length(x) == 0) return(1)
  
  # calculate probability of no successful run of length hit_streak
  q_n <- ((1 - p * x) / ((r + 1 - r * x) * q)) * (1 / (x^(n + 1)))
  
  # return probability of a successful run of length hit_streak
  1 - q_n
}

# calculate probability of hit streaks of n=1,...,70 for all bat averages
hit_streak_prob <- expand.grid(
  hit_streak = seq(from = 1, to = 70),
  bat_avg = c(.200, .250, .300, .350, .400)
) %>%
  as_tibble() %>%
  mutate(prob = map2_dbl(hit_streak, bat_avg, prob_hit_streak)) %>%
  mutate(bat_avg = factor(bat_avg, labels = c(".200", ".250", ".300", ".350", ".400")))

# visualize probability curve for part 1
ggplot(hit_streak_prob, aes(x = hit_streak, y = prob, color = bat_avg)) +
  geom_line() +
  geom_vline(xintercept = 56, linetype = 2) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  labs(title = "Chances of accruing an N-game hit streak",
       subtitle = "20 seasons, 160 games per season",
       x = "Length of hit streak",
       y = "Chance of achieving hit streak in career",
       color = "Career\nbatting\naverage")

# performance enhancing scenario
expand.grid(
  hit_streak = seq(from = 1, to = 70),
  bat_avg = .500
) %>%
  as_tibble() %>%
  mutate(prob = map2_dbl(hit_streak, bat_avg, prob_hit_streak, seasons = 10)) %>%
  ggplot(aes(x = hit_streak, y = prob)) +
  geom_line() +
  geom_vline(xintercept = 56, linetype = 2) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Chances of accruing an N-game hit streak",
       subtitle = "10 seasons, 160 games per season, career .500 batter",
       x = "Length of hit streak",
       y = "Chance of achieving hit streak in career")

