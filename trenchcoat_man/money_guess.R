require(dplyr)
require(magrittr)
require(ggplot2)
require(ggrepel)

rm(list = ls())

set.seed(11091987)

# function to guess money amount using strategy
guess_money <- function(actual, initial, n_tries = 9, min_val = 1, max_val = 1000){
  # set iterator
  i <- 1
  
  # while i is less than the max number of guesses, find the median value
  # within the possible range. if guess is not correct, reset min_val or max_val
  # depending on info trenchcoat man provides
  while(i <= n_tries){
    if(i == 1){
      guess <- initial
    } else{
      guess <- round(mean(c(min_val, max_val)))
    }

    # if guess is correct, immediately exit the loop and return true
    # if guess is not correct:
    ## if actual is higher than guess, change min_val to guess
    ## if actual is lower than guess, change max_val to guess
    if(actual == guess){
      return(c(win = TRUE, round = i))
    } else if(actual > guess) {
      min_val <- guess
    } else if(actual < guess) {
      max_val <- guess
    }
    
    # iterate to next round if guess was incorrect
    i <- i + 1
  }
  
  # at this point still have not guessed the money amount, so lose
  # correct i since we didn't really guess the i-th time
  return(c(win = FALSE, round = i - 1))
}

# run guess_money on every value between 1 and 1000
# with starting values between 1 and 1000
min_val <- 1
max_val <- 1000
actual_vals <- min_val:max_val
guess_vals <- min_val:max_val

system.time({
  data <- expand.grid(actual = actual_vals, guess = guess_vals) %>%
    tbl_df
  
  result <- with(data, Vectorize(guess_money)(actual = actual, initial = guess,
                                              min_val = min_val, max_val = max_val))
  
  both <- bind_cols(data, t(result) %>%
                      as.data.frame)
})

# which guess has the best win rate?
both %>%
  group_by(guess) %>%
  summarize(win_rate = mean(win)) %>%
  ggplot(aes(guess, win_rate)) +
  geom_line() +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Initial Guess",
       y = "Win Rate") +
  theme_bw()

# which guess has the highest expected value?
exp_val <- both %>%
  group_by(guess) %>%
  summarize(win_rate = mean(win),
            exp_val = mean(actual * win)) %>%
  ungroup

exp_val_max <- exp_val %>%
  filter(exp_val == max(exp_val))

ggplot(exp_val, aes(guess, exp_val)) +
  geom_line() +
  geom_point(data = exp_val_max) +
  geom_text(data = exp_val_max, aes(label = paste0("$", guess)),
            hjust = -.25) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Initial Guess",
       y = "Average Winnings") +
  theme_bw()


# get expected values for 1:12 tries
guess_money_mult <- function(n_tries = 1, min_val = 1, max_val = 1000){
  actual_vals <- min_val:max_val
  guess_vals <- min_val:max_val
  
  data <- expand.grid(actual = actual_vals, guess = guess_vals) %>%
    tbl_df
  
  result <- with(data, Vectorize(guess_money)(actual = actual, initial = guess,
                                              n_tries = n_tries,
                                              min_val = min_val, max_val = max_val))
  
  both <- bind_cols(data, t(result) %>%
                      as.data.frame) %>%
    mutate(n_tries = n_tries)
  
  return(both)
}

system.time({
  tries_all <- lapply(1:12, function(x) guess_money_mult(n_tries = x)) %>%
    bind_rows
})

tries_all_exp <- tries_all %>%
  mutate(n_tries = factor(n_tries)) %>%
  group_by(guess, n_tries) %>%
  summarize(win_rate = mean(win),
            exp_val = mean(actual * win))

tries_all_exp_max <- tries_all_exp %>%
  group_by(n_tries) %>%
  filter(exp_val == max(exp_val)) %>%
  arrange(-exp_val) %>%
  slice(1)

ggplot(tries_all_exp, aes(guess, exp_val,
                          group = n_tries, color = n_tries)) +
  geom_line() +
  geom_point(data = tries_all_exp_max) +
  # geom_label_repel(data = tries_all_exp_max, aes(label = paste0("$", guess)),
  #           show.legend = FALSE) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_discrete(guide = guide_legend(reverse = TRUE)) +
  labs(x = "Initial Guess",
       y = "Expected Value",
       color = "Number of\nGuesses",
       group = "Number of\nGuesses") +
  theme_bw(base_size = 16)

