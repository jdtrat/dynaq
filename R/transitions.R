# setupTransFunction -------------------------------------------------

#' Setup Transition Model
#'
#' @return Returns the initialized simulation counter \code{trans$numSims}, the
#'   transition function \code{trans$Function}, the transition table
#'   \code{trans$Table} which keeps track of the
#'   State/Action/StatePrime/Reward/source of experience (simulated or not), and
#'   the more detailed \code{trans$Track}, which keeps the running proportion of
#'   states observed as a result of the first action/transition.
#' @export
#'
#'
setupTransFunction <- function() {

  trans$numSims <- NULL

  #Initialize transition function for transitioning to "states" RA, RB, LA, and LB.
  trans$Function <- data.frame("tRA" = 0.5,
                               "tRB" = 0.5,
                               "tLA" = 0.5,
                               "tLB" = 0.5)
  #initialize the trans$Table with four different
  trans$Table <- tibble("State" = NA,
                        "Action" = NA,
                        "StatePrime" = NA,
                        "Reward" = NA,
                        "Sim" = NA)

  trans$Track <- NULL
}

#' Update Transition Model
#'
#' The updateTransFunction updates the transition model for simulated
#' experience. It updates the following: \itemize{ \item \code{trans$numSims}
#' which keeps track of the number of simulations currently run. \item
#' \code{trans$Function} which contains the model's estimate of transitioning to
#' different states from FC. \item The transition table \code{trans$Table}
#' which keeps track of the State/Action/StatePrime/Reward/source of experience
#' (simulated or not). \item \code{trans$Track} which keeps the
#' running proportion of states observed as a result of the first
#' action/transition (more detailed \code{trans$Table}). }
#'
#' @param action The action taken in the initial state
#' @param statePrime The state reached as a result of taking action a in the
#'   initial state.
#' @param alpha The learning rate for simulated data.
#' @param real The previously experienced "real" S, A, R, S' combination.
#' @param x The number of simulations to perform.
#'
#' @return Updates the trans environment objects with the latest values.
#' @export
#'
updateTransFunction <- function(action, statePrime, alpha, real = NULL, x) {
  if(is.null(real)) {
    stop("Real experience information is not supplied. Please define.")
  }

  #update the real experience as the latest real experience
  realExp <- tibble(tail(real, 1))
  names(realExp) <- c("State", "Action", "StatePrime", "Reward")
  realExp <- realExp %>% mutate("Sim" = 0)

  simExp <- tibble("State" = "FC", #initial state is always FC
                   "Action" = action,
                   "StatePrime" = statePrime,
                   "Reward" = 0, #there's never a reward presented at statePrime
                   "Sim" = 1)

  #if this is the first round, and trans$Table has all NAs from initialization
  if(sum(is.na(trans$Table)) == 5) {
    #bind the existing trans$Table (previous real and simulated experience)
    #with the new real experience and the simulated experience
    trans$Table <- bind_rows(trans$Table,
                             realExp,
                             simExp) %>% na.omit() #remove the empty trans$Table row when initializing

    trans$numSims <- 1 #one simulation has been done

    #otherwise if this isn't the first round
  }else if (sum(is.na(trans$Table)) != 5) {

    #check if trans$numSims is equal to x. If not, just keep track of the simulated experience and update trans$numSims
    if (trans$numSims != x) {

      trans$Table <- bind_rows(trans$Table, simExp)
      trans$numSims <- trans$numSims + 1

      #If trans$numSims is equal to x then bind the (new) real experience and simulated experience, and reset
      #the trans$numSims to 1.
    }else if (trans$numSims == x) {
      #bind the existing trans$Table (previous real and simulated experience)
      #with the new real experience and the simulated experience
      trans$Table <- bind_rows(trans$Table,
                               realExp,
                               simExp)
      trans$numSims <- 1
    }
  }

  #trans$Track keeps the running proportion of states observed as a result of the first action/transition
  trans$Track <- trans$Table %>% mutate(Action = recode(.$Action, "right" = 1, "left" = 0),
                                       Right = case_when(Action == 1 ~ 1, #if action is 1 (right) make Right = 1, otherwise Right = 0
                                                         TRUE ~ 0),
                                       Left = case_when(Action == 0 ~ 1, #if action is 0 (left) make Left = 1, otherwise Left = 0
                                                        TRUE ~ 0),

                                       #Track the states
                                       RB = case_when(Right == 1 & StatePrime == "RB" ~ 1,
                                                      TRUE ~ 0),
                                       RA = case_when(Right == 1 & StatePrime == "RA" ~ 1,
                                                      TRUE ~ 0),
                                       LB = case_when(Left == 1 & StatePrime == "LB" ~ 1,
                                                      TRUE ~ 0),
                                       LA = case_when(Left == 1 & StatePrime == "LA" ~ 1,
                                                      TRUE ~ 0),
                                       #get the proportion of the states observed
                                       propRB = cumsum(RB) / cumsum(Right),
                                       propRA = 1 - propRB,
                                       propLB = cumsum(LB) / cumsum(Left),
                                       propLA = 1 - propLB) %>%
    #ensure that the if one of these states hasn't been experienced in the simulation, that
    #the values will be 0 because they haven't been experienced.
    mutate(propRB = replace_na(propRB, 0),
           propRA = replace_na(propRA, 0),
           propLB = replace_na(propLB, 0),
           propLA = replace_na(propLA, 0))


  #update the transition function based on the latest cumulative sum of the proportion of going to RB/RA/LB/LA
  #based on going right/left
  trans$Function <- data.frame("tRA" = last(trans$Track$propRA),
                               "tRB" = last(trans$Track$propRB),
                               "tLA" = last(trans$Track$propLA),
                               "tLB" = last(trans$Track$propLB))
}







#' Get Transition from First to Second State
#'
#' \code{getFirstTransition} takes in a state (though unnecessary, as the task
#' structure dictates it will always be State "FC") and an action and returns
#' the next state based off them. If this is being called for actual experience,
#' that is all it does. simulated experience, is more complicated and described
#' below.
#'
#' If simulated experience, the function requires a transition function and
#' transition data from real experience. Using these, it determines the next
#' state based on the supplied transition function, \code{tFunction}, and prior
#' experience, \code{transDF}.
#'
#' If this is the first round, as indicated by an empty \code{trans$Table} global
#' variable (which tracks the prior state-action-reward information), then the
#' transition is chosen based off whatever was previously experienced during the
#' actual trial. It does this by calling the \code{randomPrevious}
#' function, passing in "State2", and observing what was really experienced.
#'
#' This is to say that an individual cannot experience a state in a simulation
#' that they have not previously visited. Similarly, if this is not the first
#' simulated experience, the function checks for which states have been visited
#' previously. If, for example, the action is right, and the agent has only
#' experienced State "RB," then they will only transition to that state in this
#' simulation. However, if they've experienced both States RA and RB, they could
#' transition to either based on the supplied transition \code{tFunction} (i.e.
#' the agent's model of the task structure).
#'
#'
#' @param state The first state.
#' @param action The action chosen in the first state.
#' @param sim Logical: TRUE if simulating experience; FALSE if not simulating
#'   experience.
#' @param tFunction Transition function that has the current probability of
#'   transitioning to States "A" or "B".
#' @param transDF Transition data from real experience as defined in
#'   \code{simModel}
#'
#' @return Next state.
#' @export

getFirstTransition <- function(state, action, sim = FALSE, tFunction = NULL, transDF = NULL) {
  p1 <- stats::runif(1) #set probability for transitioning to next state
  if(!sim) {
    if (state == state("FC") && action == "right" && p1 <= 0.7) {next_state <- state("RB")}
    if (state == state("FC") && action == "right" && p1 > 0.7) {next_state <- state("RA")}
    if (state == state("FC") && action == "left" && p1 <= 0.7) {next_state <- state("LA")}
    if (state == state("FC") && action == "left" && p1 > 0.7) {next_state <- state("LB")}
  }

  if(sim) {
    if(is.null(tFunction)) {
      stop("Transition function is not supplied. Please define.")
    }

    rightProb <- tFunction$tRB #get probability of going to tRB -- could've used tRA, they are inverse.
    leftProb <- tFunction$tLA #get probability of going to tLA -- could've used tLB, they are inverse.


    #if the action is right
    if (action == "right"){

      # if the action is right and the trans$Table has no values, then
      # get a random previous state that happens by going right. Double check that it
      # has an "R" with str_detect and if not, keep doing a random previously visited one
      # until it gets one with an "R". Then set next_state as tempState (whatever has the "R").
      if(sum(is.na(trans$Table)) == 5) {

        #get random previous state.
        tempState <- randomPrevious(transDF, "State2")

        while(!stringr::str_detect(tempState, "R")) {
          tempState <- randomPrevious(transDF, "State2")
        }

        if(stringr::str_detect(tempState, "R")){next_state <- tempState}

        #test to make sure we've experienced both states. If so, use the transition probability.
      }else if ("RB" %in% transDF$State2 & "RA" %in% transDF$State2) {
        if (p1 <= rightProb) {
          next_state <- state("RB")
        } else if(p1 > rightProb) {
          next_state <- state("RA")
        }
        #if not, then just assign the next state as the previously observed one.
      } else if ("RB" %in% transDF$State2 & !"RA" %in% transDF$State2) {
        next_state <- state("RB")
        #if not, then just assign the next state as the previously observed one.
      }else if (!"RB" %in% transDF$State2 & "RA" %in% transDF$State2) {
        next_state <- state("RA")
      }
      #similarly, if the action is left
    } else if (action == "left"){

      # if the action is left and the trans$Table has no values, then
      # get a random previous state that happens by going left. Double check that it
      # has an "L" with str_detect and if not, keep doing a random previously visited one
      # until it gets one with an "L". Then set next_state as tempState (whatever has the "L").
      if(sum(is.na(trans$Table)) == 5) {

        #get random previous state.
        tempState <- randomPrevious(transDF, "State2")

        while(!stringr::str_detect(tempState, "L")) {
          tempState <- randomPrevious(transDF, "State2")
        }

        if(stringr::str_detect(tempState, "L")){next_state <- tempState}
        #test to make sure we've experienced both possible states. If so, use transition probability.
      }else if("LB" %in% transDF$State2 & "LA" %in% transDF$State2) {
        if(p1 <= leftProb){
          next_state <- state("LA")
        }else if (p1 > leftProb) {
          next_state <- state("LB")
        }
        #if not, then just assign the next state as the previously observed one.
      }else if ("LB" %in% transDF$State2 & !"LA" %in% transDF$State2) {
        next_state <- state("LB")
        #if not, then just assign the next state as the previously observed one.
      }else if (!"LB" %in% transDF$State2 & "LA" %in% transDF$State2) {
        next_state <- state("LA")
      }

    }
  }

  return(next_state)
}

#' Get Transition from Second to Third State
#'
#' \code{getSecondTransition} takes in the current second state and action
#' chosen in that state and determines the final state and reward. This function
#' does not change whether or not the experience is real or simulated.
#'
#' @param state Current second state.
#' @param action Action taken in second state.
#'
#' @return Final state and reward.
#' @export
#'

getSecondTransition <- function(state, action) {

  #get next state if in state 1
  if (state == state("RA") && action == "right") {
    state <- state("RAR")
    reward <- getReward(state = "RAR")}
  if (state == state("RA") && action == "left") {
    state <- state("RAL")
    reward <- getReward(state = "RAL")}
  if (state == state("LA") && action == "right") {
    state <- state("LAR")
    reward <- getReward(state = "LAR")}
  if (state == state("LA") && action == "left") {
    state <- state("LAL")
    reward <- getReward(state = "LAL")}

  #get next state if in state 2
  if (state == state("RB") && action == "right") {
    state <- state("RBR")
    reward <- getReward(state = "RBR")}
  if (state == state("RB") && action == "left") {
    state <- state("RBL")
    reward <- getReward(state = "RBL")}
  if (state == state("LB") && action == "right") {
    state <- state("LBR")
    reward <- getReward(state = "LBR")}
  if (state == state("LB") && action == "left") {
    state <- state("LBL")
    reward <- getReward(state = "LBL")}

  output <- data.frame(state = state,
                       reward = reward)

  return(output)
}






