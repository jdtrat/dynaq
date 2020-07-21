#' Generate Task Simulations
#'
#' Running this function is equivalent to running one agent on the two-stage
#' Markov decision task.
#'
#' @param numIter The number of trials per session.
#' @param x The amount of simulations to be done. This is the extent to which
#'   model-based activity occurs.
#' @param alpha The learning rate alpha.
#' @param gamma The temporal discounting factor gamma.
#' @param epsilon The epsilon to be used in epsilon-greedy policy choices.
#' @param tau The tau (temperature) to be used in softmax policy choices.
#' @param softmax Logical: TRUE if softmax policy decisions should be used;
#'   FALSE if epsilon-greedy policy decisions should be used. By default,
#'   softmax is used.
#'
#' @return If \code{x} is greater than 0, so there are some model-based
#'   simulations, then this function returns a list with two elements containing
#'   real (element 1) and simulated (element 2) data. These can be passed into
#'   the \code{\link{processSimData}} function to concatenate into one
#'   dataframe. If \code{x} is equal to 0, so there are no model-based
#'   simulations, then this function returns a dataframe with real experience
#'   data.
#' @export
#'

generateData <- function(numIter = 201, x = 10, alpha = 0.1, gamma = 0.9, epsilon = 0.1, tau = 0.08, softmax = TRUE) {
  setupRewards() #setup reward structure
  setupQtable() #initialize Qtable
  setupTransFunction() #initialize the transition function
  real <- NULL #initialize real
  simulate <- NULL #initialize simulate
  #for the number of trials bind the data in a dataframe

  if (x == 0) {sim <- FALSE} else if (x != 0) {sim <- TRUE}

  if(sim){
    for(i in 1:numIter) {
      real <- base::rbind(real, oneTrial(alpha = alpha, gam = gamma, epsilon = epsilon, tau = tau, softmax = softmax))
      for(j in 1:x) {
        simulate <- base::rbind(simulate, simModel(real, modelAlpha = alpha, epsilon = epsilon, tau = tau, gam = gamma, x = x))
      }
    }

    real <- real %>% dplyr::mutate(experience = "Real",
                            row = dplyr::row_number())
    simulate <- simulate %>% dplyr::mutate(experience = "Simulated",
                                    row = dplyr::row_number())

    return(base::list(real, simulate))

  }else if (!sim){
    for(i in 1:numIter) {
      real <- base::rbind(real, oneTrial(alpha = alpha, gam = gamma, epsilon = epsilon, tau = tau, softmax = softmax))
    }

    real <- real %>% dplyr::mutate(experience = "Real",
                                   row = dplyr::row_number())
    return(real)
  }
}
