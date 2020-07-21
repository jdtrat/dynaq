#' Create a random choice
#'
#' @return A string signifying an action either "right" or "left"
#' @export
#'

randomChoice <- function() {
  #create random value between 0 and 1
  p <- stats::runif(1)
  #if less than or equal to 0.5, make action left. Else, make action right.
  if(p <= 0.5) {action <- "left"} else if(p > 0.5) {action <- "right"}
  return(action)
}

#' Action according to Softmax
#'
#' @param smax The softmax argument (specifically the softmax probability of going right -- \code{smaxRight}).
#'
#' @return An action based on a random binomial distribution with probability smax.
#' @export
#'

smaxAction <- function(smax) {

  #get a 1 or 0 based off a binomial distribution with a probability according
  #to smaxRight. If it's 1, go right. Else, go left.
  if(stats::rbinom(1,1, smax)) {action <- "right"}else{action <- "left"}

  return(action)
}

# choice function ---------------------------------------------------------
# choice function calculates a choice based on the state and epsilon value by
# interfacing with the Q$Table. In its present iteration, it utilizes a simple
# epsilon greedy policy.

#' Choose an Action
#'
#'
#' @param state The state the agent is currently in.
#' @param epsilon The epsilon to be used in epsilon-greedy policy choices.
#' @param tau The tau (temperature) to be used in softmax policy choices.
#' @param softmax Logical: TRUE if softmax policy decisions should be used;
#'   FALSE if epsilon-greedy policy decisions should be used. By default,
#'   softmax is used.
#'
#' @return A string depicting an action (left or right) to take in the current
#'   state.
#' @export
#'

choice <- function(state, epsilon = 0.1, tau = 0.08, softmax = TRUE) {

  if(softmax) {

    #if state FC, and Q$R == Q$L, make a random choice. Else, calculate smaxRight and smaxLeft.
    if(state == "FC") {
      if(Q$L == Q$R) {action <- randomChoice()
      }else if (Q$L != Q$R) {
        smaxRight <- (exp(Q$R/tau))/(exp(Q$R/tau) + exp(Q$L/tau))
        smaxLeft <- (exp(Q$L/tau))/(exp(Q$L/tau) + exp(Q$R/tau))
        action <- smaxAction(smaxRight)
      }
    }

    #if all of these (or any individual permutations) are equal, make a random choice. Else, loop through
    #and calculate the smax for each state.
    if(base::all(Q$Table[,2] == 0.5)) {action <- randomChoice()

    }else if(Q$Table[1,2] == Q$Table[2,2]) {action <- randomChoice()

    }else if(Q$Table[3,2] == Q$Table[4,2]) {action <- randomChoice()

    }else if(Q$Table[5,2] == Q$Table[6,2]) {action <- randomChoice()

    }else if(Q$Table[7,2] == Q$Table[8,2]) {action <- randomChoice()

    }else if (state == "RA") {#STATE A AFTER GOING RIGHT IN FC
      smaxRight <- (exp(Q$Table[1,2]/tau))/(exp(Q$Table[1,2]/tau) + exp(Q$Table[2,2]/tau))
      smaxLeft <- (exp(Q$Table[2,2]/tau))/(exp(Q$Table[2,2]/tau) + exp(Q$Table[1,2]/tau))
      action <- smaxAction(smaxRight)

    }else if (state == "RB") {#STATE B AFTER GOING RIGHT IN FC
      smaxRight <- (exp(Q$Table[3,2]/tau))/(exp(Q$Table[3,2]/tau) + exp(Q$Table[4,2]/tau))
      smaxLeft <- (exp(Q$Table[4,2]/tau))/(exp(Q$Table[4,2]/tau) + exp(Q$Table[3,2]/tau))
      action <- smaxAction(smaxRight)

    }else if (state == "LA") {#STATE A AFTER GOING LEFT IN FC
      smaxRight <- (exp(Q$Table[5,2]/tau))/(exp(Q$Table[5,2]/tau) + exp(Q$Table[6,2]/tau))
      smaxLeft <- (exp(Q$Table[6,2]/tau))/(exp(Q$Table[6,2]/tau) + exp(Q$Table[5,2]/tau))
      action <- smaxAction(smaxRight)

    }else if (state == "LB") {#STATE B AFTER GOING LEFT IN FC
      smaxRight <- (exp(Q$Table[7,2]/tau))/(exp(Q$Table[7,2]/tau) + exp(Q$Table[8,2]/tau))
      smaxLeft <- (exp(Q$Table[8,2]/tau))/(exp(Q$Table[8,2]/tau) + exp(Q$Table[7,2]/tau))
      action <- smaxAction(smaxRight)}

  }else if(!softmax) {

    p <- stats::runif(1) #random probability

    #if it is the first state, compare Q$L and Q$Right S1
    #and make appropriate choice based on epsilon greedy.
    if(state == "FC") {
      if(Q$L == Q$R) {action <- randomChoice()}
      if(Q$L > Q$R && p > epsilon) {action <- "left"}
      if(Q$L < Q$R && p > epsilon) {action <- "right"}
      if(Q$L > Q$R && p <= epsilon) {action <- "right"}
      if(Q$L < Q$R && p <= epsilon) {action <- "left"}
    }

    #if it is one of the second states, compare relevant Qvalues
    #and make appropriate choice based on epsilon greedy.

    #if the Qvalues being compared have no value (are 0.5), make a random choice
    #all Q values are zero
    if(base::all(Q$Table[,2] == 0.5)) {action <- randomChoice()

    }else if(Q$Table[1,2] == Q$Table[2,2]) {action <- randomChoice()

    }else if(Q$Table[3,2] == Q$Table[4,2]) {action <- randomChoice()

    }else if(Q$Table[5,2] == Q$Table[6,2]) {action <- randomChoice()

    }else if(Q$Table[7,2] == Q$Table[8,2]) {action <- randomChoice()

    #create actions based on Qvalues

    #STATE A AFTER GOING RIGHT IN FC
    }else if (state == "RA" && Q$Table[1,2] > Q$Table[2,2] && p > epsilon) {action <- "right"

    }else if (state == "RA" && Q$Table[1,2] > Q$Table[2,2] && p <= epsilon) {action <- "left"

    }else if (state == "RA" && Q$Table[2,2] > Q$Table[1,2] && p > epsilon) {action <- "left"

    }else if (state == "RA" && Q$Table[2,2] > Q$Table[1,2] && p <= epsilon) {action <- "right"

    #STATE B AFTER GOING RIGHT IN FC
    }else if (state == "RB" && Q$Table[3,2] > Q$Table[4,2] && p > epsilon) {action <- "right"

    }else if (state == "RB" && Q$Table[3,2] > Q$Table[4,2] && p <= epsilon) {action <- "left"

    }else if (state == "RB" && Q$Table[4,2] > Q$Table[3,2] && p > epsilon) {action <- "left"

    }else if (state == "RB" && Q$Table[4,2] > Q$Table[3,2] && p <= epsilon) {action <- "right"

    #STATE A AFTER GOING LEFT IN FC
    }else if (state == "LA" && Q$Table[5,2] > Q$Table[6,2] && p > epsilon) {action <- "right"

    }else if (state == "LA" && Q$Table[5,2] > Q$Table[6,2] && p <= epsilon) {action <- "left"

    }else if (state == "LA" && Q$Table[6,2] > Q$Table[5,2] && p > epsilon) {action <- "left"

    }else if (state == "LA" && Q$Table[6,2] > Q$Table[5,2] && p <= epsilon) {action <- "right"

    #STATE B AFTER GOING LEFT IN FC
    }else if (state == "LB" && Q$Table[7,2] > Q$Table[8,2] && p > epsilon) {action <- "right"

    }else if (state == "LB" && Q$Table[7,2] > Q$Table[8,2] && p <= epsilon) {action <- "left"

    }else if (state == "LB" && Q$Table[8,2] > Q$Table[7,2] && p > epsilon) {action <- "left"

    }else if (state == "LB" && Q$Table[8,2] > Q$Table[7,2] && p <= epsilon) {action <- "right"}

  }

  #return the action
  return(action)

}









