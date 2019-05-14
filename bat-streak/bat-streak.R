library(tidyverse)
library(magrittr)
library(furrr)
library(tictoc)
library(here)

plan(multiprocess)  # for parallel processing
theme_set(theme_minimal())

set.seed(1234)

n_sim <- 1e04

# simulation method
sim_season <- function(bat_avg, seasons = 20, season_length = 160, plate_app = 4){
  # simulate at-bats for each game, check if at least one is a hit
  at_bats <- map_lgl(.x = 1:(seasons * season_length),
                     ~ rbinom(n = 1, size = plate_app, prob = bat_avg) > 0)
  
  # calculate spells of consecutive outcomes
  spells <- rle(at_bats)
  
  # calculate max hit streak
  max(spells$lengths[spells$values])
}

# simulate for all averages
tic()
sim_compare <- tibble(
  `.200` = future_map_dbl(1:n_sim, ~ sim_season(bat_avg = .200)),
  `.250` = future_map_dbl(1:n_sim, ~ sim_season(bat_avg = .250)),
  `.300` = future_map_dbl(1:n_sim, ~ sim_season(bat_avg = .300)),
  `.350` = future_map_dbl(1:n_sim, ~ sim_season(bat_avg = .350)),
  `.400` = future_map_dbl(1:n_sim, ~ sim_season(bat_avg = .400))
)
toc()

sim_compare_long <- gather(sim_compare, key = bat_avg, value = hit_streak)
write_rds(x = sim_compare_long,
          path = here("bat-streak", "sim_compare_long.Rds"))

# violin plots of distribution of simulated hit streaks
ggplot(sim_compare_long, aes(x = bat_avg, y = hit_streak)) +
  geom_violin()

sim_compare_long %>%
  group_by(bat_avg) %>%
  summarize(prop = sum(hit_streak > 56) / n())
