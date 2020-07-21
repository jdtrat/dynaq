#' Get a Random Value from an Existing Dataframe
#'
#' Returns a character string of a random value from a supplied dataframe and
#' column name.
#'
#' @param df A dataframe to get the random value from
#' @param name The column name of the dataframe
#'
#' @return A random value from \code{df$name} as a character string.
#' @export
#'
randomPrevious <- function(df, name) {

  #get a random value between 1 and nrow(df) to act as an index.
  #this will be used for getting a previously visited action or state.
  randIndex <- base::round(stats::runif(1, min = 1, max = base::nrow(df)))

  #index the df and specific variable with a random value
  randValue <- df[randIndex, name]

  #return the random value
  return(base::as.character(randValue))

}



# simModel function -------------------------------------------------------
# The simModel function takes in the output of oneTrial and performs model-based simulations
# based on previously visited states. The input x is the number of simulations that are run (x is provided to generateData).

#' Perform One Model-Based Trial
#'
#' The \code{simModel} function simulates one trial of the two-stage Markov task
#' using a model-based approach (with a transition model), whose action values
#' get updated according to the Q-learning. The model-based simulations are
#' based on random previously visited states and previously taken actions. The x
#' parameter is the number of simulations that are run, in line with the Dyna
#' architecture.
#'
#' @param trialData The output of \code{\link{oneTrial}} (i.e. the last real
#'   experience)
#' @param modelAlpha The learning rate from simulated data.
#' @param gam The temporal discounting factor, gamma.
#' @param epsilon The epsilon to be used in epsilon-greedy policy choices.
#' @param tau The tau (temperature) to be used in softmax policy choices.
#' @param x The amount of simulations to be done. This is used to track the
#'   total number performed via the \code{\link{updateTransFunction}}.
#'
#' @return A tibble with 8 rows and 18 columns. The 8 rows contain identical
#'   information \strong{except for the Qtable column.} They contain information
#'   about the states, actions, and rewards for one trial as well as meta data
#'   including the temporal discounting factor (gamma) learning rate (alpha,
#'   specific to simulated experience), choice policy parameters (epsilon and
#'   tau), and probability of receiving a reward for each image.
#' @export
#'

simModel <- function(trialData, modelAlpha = 0.1, gam = 0.9, epsilon = 0.1, tau = 0.08, x) {

  #get the first transition data by transmuting the inputted trialData into
  #the quadtuple of State1, Action1, State2, Reward1.
  firstTrans <- trialData %>% dplyr::select(State1, Action1, State2, Reward1) %>%
    removeTable() #%>%
  #mutate(Action1 = recode(.$Action1, "right" = 1, "left" = 0)) #May want to recode now. Not sure. Keeping commented.


  #get the second transition data by transmuting the inputted trialData into
  #the quadtuple of State2, Action2, State3, Reward2.
  secondTrans <- trialData %>% dplyr::select(State2, Action2, State3, Reward2) %>%
    removeTable() #%>%
  #mutate(Action2 = recode(.$Action2, "right" = 1, "left" = 0)) #May want to recode now. Not sure. Keeping commented.

  action1 <- randomPrevious(firstTrans, "Action1") #random previously taken action in state 1

  #update Qtable after making the first action and before entering a new state
  updateQtable(state = "FC", action = action1, reward = 0, alpha = modelAlpha, gamma = gam)

  state2 <- getFirstTransition(state = "FC", action = action1, sim = TRUE, tFunction = trans$Function, transDF = firstTrans)
  reward1 <- 0 #no reward for transitioning to state 2

  #update the transition function with the new state information
  updateTransFunction(action = action1, statePrime = state2, alpha = modelAlpha, real = firstTrans, x = x)

  action2 <- randomPrevious(secondTrans, "Action2") #random previously taken action in state 2

  #update the Qtable after making the second action from the second state
  updateQtable(state = state2, action = action2, reward = reward1, alpha = modelAlpha, gamma = gam)

  #Run second transition
  transition2to3 <- getSecondTransition(state2, action2)
  state3 <- transition2to3$state #pull the third state
  reward2 <- transition2to3$reward #pull the reward

  #update the Qtable after making second action and getting to third state
  updateQtable(state = state3, action = "none", reward = reward2, alpha = modelAlpha, gamma = gam)

  #generate output dataframe
  output <- tidyr::tibble("State1" = "FC",
                         "Action1" = action1,
                         "State2" = state2,
                         "Reward1" = reward1,
                         "Action2" = action2,
                         "State3" = state3,
                         "Reward2" = reward2,
                         "Gamma" = gam,
                         "Alpha" = modelAlpha,
                         "Epsilon" = epsilon,
                         "Tau" = tau,
                         "probAR" = rewardProb$AR,
                         "probAL" = rewardProb$AL,
                         "probBR" = rewardProb$BR,
                         "probBL" = rewardProb$BL,
                         "Qtable" = Q$Table,
                         "QL" = Q$L,
                         "QR" = Q$R)

}



