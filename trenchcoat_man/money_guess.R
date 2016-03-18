require(dplyr)
require(magrittr)
require(ggplot2)

rm(list = ls())

set.seed(11091987)

# number of trials
n_trial <- 100

# function to guess money amount using strategy
guess_money <- function(actual, n_tries = 9, min_val = 1, max_val = 1000){
  # set iterator
  i <- 1
  
  # while i is less than the max number of guesses, find the median value
  # within the possible range. if guess is not correct, reset min_val or max_val
  # depending on info trenchcoat man provides
  while(i <= n_tries){
    guess <- round(mean(c(min_val, max_val)))
    
    # if guess is correct, immediately exit the loop and return true
    # if guess is not correct:
    ## if actual is higher than guess, change min_val to guess
    ## if actual is lower than guess, change max_val to guess
    if(actual == guess){
      return(c(TRUE, i))
    } else if(actual > guess) {
      min_val <- guess
    } else if(actual < guess) {
      max_val <- guess
    }
    
    # iterate to next round if guess was incorrect
    # as long as this is not the last round
    if(i != n_tries){
      i <- i + 1
    }
  }
  
  # at this point still have not guessed the money amount, so lose
  return(c(FALSE, i))
}



