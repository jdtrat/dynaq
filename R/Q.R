# setupQtable function ----------------------------------------------------
# setupQtable function simply initializes the Qtable for the beginning of a session.

setupQtable <- function() {

  #initialize Qtable as zeros.
  Q$Table <- base::matrix(c(rep(0.5, 16), rep(0,8)), ncol = 3, nrow = 8)

  #Qtable <- matrix(runif(24), ncol = 3, nrow = 8) #TO PRACTICE
  base::rownames(Q$Table) <- c("RAR", "RAL", "RBR", "RBL", "LAR", "LAL", "LBR", "LBL")
  base::colnames(Q$Table) <- c("State 1 (FC)","State 2 (A or B)", "Terminal State")

  Q$R <- base::max(Q$Table[1:4,1])

  Q$L <- base::max(Q$Table[5:8,1])

  #Vector of states and their Qtable indexes to be used when updating last state Q-values
  Q$thirdStateVec <- base::data.frame(stateName = c("RAR", "RAL", "RBR", "RBL", "LAR", "LAL", "LBR", "LBL"),
                                index = c(seq(1,8, by = 1)))

  #grid of state-action pairs and their indexes to use when updating second state Q-values
  Q$stateActionPair <- tidyr::expand_grid(stateName = c("RA", "RB", "LA", "LB"),
                                          actionOptions = c("right", "left")) %>%
    dplyr::mutate(index = dplyr::row_number())

}



#' Update QR and QL
#'
#' The \code{updateQRL} function will take in the action an agent performs and
#' update either \code{Q$R} or \code{Q$L} as appropriate.
#'
#' @param state The state the agent is currently in.
#' @param action The action (right or left) an agent performs.
#' @param alpha The learning rate alpha.
#' @param gamma The temporal discounting factor gamma.
#'
#' @return Updated \code{Q$R} or \code{Q$L} values.
#' @export
#'

updateQRL <- function(state, action, alpha, gamma) {

  if (state == "FC" && action == "right") {
    #update the Q value of going right at state FC
    Q$R <- Q$R + (alpha * (0 + (gamma * base::max(Q$Table[1:4,1]) - QR)))

  }else if (state == "FC" && action == "left") {
    #update the Q value of going left at state FC
    Q$L <- Q$L + (alpha * (0 + (gamma * base::max(Q$Table[5:8,1]) - QL)))
  }

}


#' Update Q-values at Second State
#'
#' This function takes the \code{Q$stateActionPair} dataframe initialized in the
#' background and filters it to pull the Q-table index that matches the
#' state-action pair. It then updates the Q-value according to the Q-learning
#' algorithm.
#'
#' @param state The state the agent is currently in.
#' @param action The action (right or left) an agent performs.
#' @param alpha The learning rate alpha.
#' @param gamma The temporal discounting factor gamma.
#'
#' @return Updated second state Q-value
#' @export
#'
updateQsecondState <- function(state, action, alpha, gamma) {

  index <- Q$stateActionPair %>%
    dplyr::filter(stringr::str_detect(stateName, state) & stringr::str_detect(actionOptions, action)) %>%
    dplyr::pull(index)

  Q$Table[index,1] <- Q$Table[index,1] + (alpha * (0 + (gamma * base::max(Q$Table[index,2])) - Q$Table[index,1]))

}




#' Update Q-values at Third State
#'
#' This function takes the \code{Q$thirdStateVec} dataframe initialized in the
#' background and filters it to pull the Q-table index that matches the
#' terminal state. It then updates the Q-value according to the Q-learning
#' algorithm.
#'
#' @param state The state the agent is currently in.
#' @param reward The reward an agent receives at the current state.
#' @param alpha The learning rate alpha.
#' @param gamma The temporal discounting factor gamma.
#'
#' @return Updated third state Q-value
#' @export
#'
updateQthirdState <- function(state, reward, alpha, gamma) {

  index <- Q$thirdStateVec %>%
    dplyr::filter(stringr::str_detect(stateName, state)) %>%
    dplyr::pull(index)

  #Update the Q values according to the Q-learning algorithm. max(0) because
  #next state is terminal/doesn't exist.
  Q$Table[index,2] <- Q$Table[index,2] + (alpha * (reward + (gamma * base::max(0)) - Q$Table[index,2]))
}


# updateQtable function ---------------------------------------------------
# updateQtable function takes in the state, reward, alpha and gamma value
# and updates the Q-table based on the results
# action must be provided for first and second states, but not for third state.

#' Update Q Table
#'
#' This function updates the Q-table values by calling the functions
#' \code{\link{updateQRL}} which updates the \code{Q$R} and \code{Q$L} values,
#' \code{\link{updateQsecondState}}, which updates the second state Q-values,
#' and \code{\link{updateQthirdState}}, which updates the third state Q-values.
#'
#' @param state The state the agent is currently in.
#' @param action The action (right or left) an agent performs.
#' @param reward The reward an agent receives at the current state.
#' @param alpha The learning rate alpha.
#' @param gamma The temporal discounting factor gamma.
#'
#'
#' @return Updated Q-values.
#' @export
#'

updateQtable <- function(state, action, reward, alpha = 0.1, gamma = 0.9) {

  updateQRL(state = state, action = action, alpha = alpha, gamma = gamma)
  updateQsecondState(state = state, action = action, alpha = alpha, gamma = gamma)
  updateQthirdState(state = state, reward = reward, alpha = alpha, gamma = gamma)

}

