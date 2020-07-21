#' Perform One Model-Free Trial
#'
#' The \code{oneTrial} function simulates one trial of the two-stage Markov task using model-free Q-learning.
#'
#' @param alpha The learning rate alpha.
#' @param gam The temporal discounting factor gamma.
#' @param epsilon The epsilon to be used in epsilon-greedy policy choices.
#' @param tau The tau (temperature) to be used in softmax policy choices.
#' @param softmax Logical: TRUE if softmax policy decisions should be used;
#'   FALSE if epsilon-greedy policy decisions should be used. By default,
#'   softmax is used.

#' @return A tibble with 8 rows and 18 columns. The 8 rows contain identical
#'   information \strong{except for the Qtable column.} They contain information
#'   about the states, actions, and rewards for one trial as well as meta data
#'   including the temporal discounting factor (gamma) learning rate (alpha),
#'   choice policy parameters (epsilon and tau), and probability of receiving a
#'   reward for each image.
#' @export
#'

oneTrial <- function(alpha = 0.1, gam = 0.9, epsilon = 0.1, tau = 0.08, softmax = TRUE) {

  #Second State:
  #get first action (from s1 to s2)
  action1 <- choice(state = "FC", epsilon = epsilon, tau = tau, softmax = softmax)

  #update Qtable after making the first action and before entering a new state
  updateQtable(state = "FC", action = action1, reward = 0, alpha = alpha, gamma = gam)

  #Get second state
  secondState <- getFirstTransition(state = "FC", action = action1)
  reward1 <- 0 #no reward for transitioning to state 2

  #get second action
  action2 <- choice(secondState, epsilon = epsilon, tau = tau, softmax = softmax)

  #update the Qtable after making the second action from the second state
  updateQtable(state = secondState, action = action2, reward = reward1, alpha = alpha, gamma = gam)

  #Run second transition
  secondTransition <- getSecondTransition(secondState, action2)
  thirdState <- secondTransition$state #pull the third state
  reward2 <- secondTransition$reward #pull the reward

  #update the Qtable after making second action and getting to third state
  updateQtable(state = thirdState, action = "none", reward = reward2, alpha = alpha, gamma = gam)

  #generate output dataframe
  output <- tidyr::tibble("State1" = "FC",
                          "Action1" = action1,
                          "State2" = secondState,
                          "Reward1" = reward1,
                          "Action2" = action2,
                          "State3" = thirdState,
                          "Reward2" = reward2,
                          "Gamma" = gam,
                          "Alpha" = alpha,
                          "Epsilon" = epsilon,
                          "Tau" = tau,
                          "probAR" = rewardProb$AR,
                          "probAL" = rewardProb$AL,
                          "probBR" = rewardProb$BR,
                          "probBL" = rewardProb$BL,
                          "Qtable" = Q$Table,
                          "QL" = Q$L,
                          "QR" = Q$R)

  return(output)
}
