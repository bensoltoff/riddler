require(dplyr)
require(magrittr)
require(ggplot2)

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
actual_vals <- 1:1000
guess_vals <- 1:1000

system.time({
  data <- expand.grid(actual = actual_vals, guess = guess_vals) %>%
    tbl_df
  
  result <- with(data, Vectorize(guess_money)(actual = actual, initial = guess))
  
  both <- bind_cols(data, t(result) %>%
                      as.data.frame)
})

# what is the optimal starting guess?
both %>%
  group_by(guess) %>%
  summarize(win_rate = mean(win)) %>%
  ggplot(aes(guess, win_rate)) +
  geom_line()

both %>%
  group_by(guess) %>%
  summarize(win_rate = mean(win)) %>%
  arrange(-win_rate)

## account for how fast to victory
both %>%
  group_by(guess, round) %>%
  summarize(win_rate = mean(win),
            n = n())

