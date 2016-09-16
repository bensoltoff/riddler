library(tidyverse)

set.seed(11091987)

old <- 1:30
n_sims <- 10000

draft_pick_sim <- function(n_groups = 2, n_teams = 30, n_sims = 100){
  old <- 1:n_teams
  n_groups <- n_groups - 1
  
  sims <- replicate(n_sims, rbinom(length(old), size = n_groups, prob = .5)) %>%
    tbl_df %>%
    bind_cols(data_frame(old)) %>%
    gather(sim, outcome, -old) %>%
    group_by(sim) %>%
    arrange(sim, outcome, old) %>%
    mutate(new = row_number())
  
  return(sims)
}

# two groups
t2 <- draft_pick_sim(n_sims = 10000)

t2 %>%
  group_by(old) %>%
  summarize(new_mean = mean(new),
            new_median = median(new))


ggplot(t2, aes(old, new)) +
  geom_point(alpha = .1) +
  geom_smooth()

# three groups
t3 <- draft_pick_sim(n_groups = 3, n_sims = 10000)

t3 %>%
  group_by(old) %>%
  summarize(new_mean = mean(new),
            new_median = median(new))


ggplot(t3, aes(old, new)) +
  geom_point(alpha = .1) +
  geom_smooth()

# n groups
tn <- map_df(2:10, draft_pick_sim, n_sims = 10000, .id = "n_group") %>%
  mutate(n_group = as.character(as.numeric(n_group) + 1))

ggplot(tn, aes(old, new, color = n_group)) +
  geom_smooth()



