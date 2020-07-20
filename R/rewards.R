#' Random Reward Probability
#'
#' randRewardProb function generates a random number based on a uniform
#' distribution between 0.2 and 0.8.
#'
#' @return a random number between 0.2 and 0.8
#'
#'
#' @examples
#' \dontrun{
#' x <- dynqrandRewardProb() #x will be between 0.2 and 0.8
#' }
#'

randRewardProb <- function() {
  stats::runif(1, 0.2, 0.8)
}


#' Setup Rewards
#'
#' setupRewards function initializes the reward probabilities for each image at
#' the beginning of a session.
#'
#' The four probabilities are initialized as global variables:
#' \itemize{
#'     \item rewardProb$AR
#'     \item rewardProb$AL
#'     \item rewardProb$BR
#'     \item rewardProb$BL
#' }
#'
#' @return Returns the initial reward probabilities for each image
#' @export

setupRewards <- function() {

  #create initial reward probabilities for fractals AR, AL, BR, and BL
  rewardProb$AR <- randRewardProb() #initial reward probability from state RAR or LAR
  rewardProb$AL <- randRewardProb() #initial reward probability from state RAL or LAL
  rewardProb$BR <- randRewardProb() #initial reward probability from state RBR or LBR
  rewardProb$BL <- randRewardProb() #initial reward probability from state RBL or LBL

}


# updateRewardProb Function -----------------------------------------------
# updateRewardProb function takes in an image's reward probability and updates
# it according to a normal distribution with mean 0 and standard deviation of
# 0.025 per Solway's methods, with reflecting boundaries at 0.8 and 0.2. That
# is, according to Solway in personal communication:
#
# Each of the four second-stage images had independent probabilities. - A random
# probability was chosen for each for trial 1 in the valid range. - The
# probability for trial n+1 was: prob trial n + N(0, sd) - There were reflecting
# boundaries, meaning for example if prob on trial n was 0.7 and you drew +0.3,
# you would end up at 0.6 for trial n+1 (go up 0.1 to the 0.8 upper boundary,
# then down 0.2).
#
# This function takes in an image's reward probability, and returns a final
# probability to be reassigned to the inputted parameter according to the above description.

#' Update Reward Probabilities
#'
#' Update reward probabilities according to Solway et al. (2019) methods. It
#' takes in an image's reward probability, and returns a final probability to be
#' reassigned to the inputted parameter according to the above description.where
#' probabilities shift according to random gaussian noise with mean 0 and
#' standard deviation of 0.025. Reflecting boundaries are at 0.8 and 0.2.
#'
#'
#' @details According to personal communication with Solway: Each of the four
#'   second-stage images had independent probabilities.
#'
#'   \itemize{ \item A random probability was chosen for each for trial 1 in the
#'   valid range. \item The probability for trial n+1 was: prob trial n + N(0,
#'   sd) \item There were reflecting boundaries, meaning, for example, if prob
#'   on trial n was 0.7 and you drew +0.3, you would end up at 0.6 for trial n+1
#'   (go up 0.1 to the 0.8 upper boundary, then down 0.2).
#'
#'   }
#'
#' @param imageProb The current probability of receiving a reward for a given
#'   image.
#'
#' @return Updated probability
#'
#'
updateRewardProb <- function(imageProb){
  newProb <- imageProb + stats::rnorm(1, mean = 0, sd = 0.025)
  if(newProb > 0.8){
    boundary <- 0.8
    diff <- newProb - boundary
    finalProb <- boundary - diff
  } else if(newProb < 0.2) {
    boundary <- 0.2
    diff <- newProb - boundary
    finalProb <- boundary - diff
  } else if(newProb >= 0.2 | newProb <= 0.8){
    finalProb <- newProb
  }
  return(finalProb)
}


# get Reward function determines whether a reward is delivered (1) or not (0) based off of a
# binary payoff with probability either 0.2 or 0.8 that have, on each trial, a little bit of gaussian noise following
# the normal distribution with mean 0 and SD 0.025

#' Check for Reward Outcome
#'
#' the getReward function determines whether a reward is delivered (1) or not
#' (0) based off of a binary payoff with probability either 0.2 or 0.8 that
#' have, on each trial, a little bit of gaussian noise following the normal
#' distribution with mean 0 and SD 0.025. See \code{\link{updateRewardProb}}
#'
#' @param state The state, or image, for which a reward might be received.
#'
#' @return Binary reward (1 or 0) based off of the relevant state probability.
#' @export

getReward <- function(state) {

  #For each fractal, add a random value from the gaussian distribution with mean = 0 and sd = 0.025.
  #Double check that it is between 0.2 and 0.8. If not, then set it to 0.2 or 0.8
  #if a random number is less than or equal to that probability, reward is given (1) else, it's not (0).

  if(state == "RAR" | state == "LAR") {
    rewardProb$AR <- updateRewardProb(rewardProb$AR)
    if(stats::runif(1) <= rewardProb$AR){reward <- 1}else{reward <- 0} #assign reward
  }

  if(state == "RAL" | state == "LAL") {
    rewardProb$AL <- updateRewardProb(rewardProb$AL)
    if(stats::runif(1) <= rewardProb$AL){reward <- 1}else{reward <- 0} #assign reward
  }

  if(state == "RBR" | state == "LBR") {
    rewardProb$BR <- updateRewardProb(rewardProb$BR)
    if(stats::runif(1) <= rewardProb$BR){reward <- 1}else{reward <- 0} #assign reward
  }

  if(state == "RBL" | state == "LBL") {
    rewardProb$BL <- updateRewardProb(rewardProb$BL)
    if(stats::runif(1) <= rewardProb$BL){reward <- 1}else{reward <- 0} #assign reward
  }
  return(reward)
}




