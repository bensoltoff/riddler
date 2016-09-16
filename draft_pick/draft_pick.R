library(tidyverse)

set.seed(11091987)

old <- 1:30
n_sims <- 10000

sims <- replicate(n_sims, rbinom(length(old), size = 1, prob = .5)) %>%
  tbl_df %>%
  bind_cols(data_frame(old)) %>%
  gather(sim, outcome, -old) %>%
  group_by(sim) %>%
  arrange(sim, outcome, old) %>%
  mutate(new = row_number())

ggplot(sims, aes(old, new)) +
  geom_point(alpha = .1) +
  geom_smooth()

sims %>%
  group_by(old) %>%
  summarize(new_mean = mean(new),
            new_median = median(new))



