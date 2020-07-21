#' Remove Qtable From Data
#'
#' This function filters the inputted dataframe so we only get every 8th row.
#' Because there are 8 rows in the Q table, which we have in the output of
#' \code{\link{generateData}}, everything trial's information is repeated 8
#' times. This function uses a dplyr::filter for \code{(row_number() + 1 %% 8 ==
#' 1}, which returns the 1st, 9th, 17th, 25th, etc. rows which happen to be the
#' first one for the new action set.
#'
#' @param data Any dataframe that has a Qtable column, such as the output of
#'   \code{\link{oneTrial}} or \code{\link{simModel}}.
#'
#' @return The unique values of the data (minus the Qtable).
#' @export
#'

removeTable <- function(data) {

  data <- data %>% dplyr::filter((dplyr::row_number()+1) %% 8 == 1)

  return(data)

}


#' Manipulate Data
#'
#' This function manipulates the output of \code{\link{generateData}}. It
#' recodes actions as binary (right == 1, left == 0), calculates the proportion
#' of actions right or left as of each trial, determines if a transition is
#' common or not (inherent in the task structure), and filters the data to
#' remove the Qtable while retaining meta data such as the temporal discounting
#' factor (gamma) learning rate for simulated data (alpha), and the probability
#' of receiving a reward for each image.
#'
#' @param data The data to be manipulated. Typically, the output of
#'   \code{\link{generateData}}, though it could be the output of
#'   \code{\link{oneTrial}} or \code{\link{simModel}.}
#'
#' @return Manipulated data with 21 columns and however many trials were run
#'   (just real or with real and simulated experience combined).
#' @export
#'
manipulateData <- function(data) {

    manipulated <- data %>%
      dplyr::transmute(state1 = State1,
                       action1 = dplyr::recode(.$Action1, "right" = 1, "left" = 0), #recode action 1: right is 1; left is 0.
                       #get the proportion of action 1 that is right.
                       #this is the cumulative sum of actions that are right (recoded as 1)
                       #divided by the row number
                       propA1R = base::cumsum(action1) / dplyr::row_number(),
                       propA1L = 1 - propA1R, #prop of action 1 that is left (1 - prop right)
                       state2 = State2, #state 2
                       #determine if transition is common (1) or rare 0)
                       transition = dplyr::case_when(action1 == 1 & state2 == "RB" ~ 1, #if action 1 is right and state 2 is B, transition = 1
                                                     action1 == 1 & state2 == "RA" ~ 0, #if action 1 is right and state 2 is A, transition = 0
                                                     action1 == 0 & state2 == "LA" ~ 1, #if action 1 is left and state 2 is A, transition = 1
                                                     action1 == 0 & state2 == "LB" ~ 0, #if action 1 is left and state 2 is B, transition = 0
                                                     TRUE ~ 3), #else put 3
                       action2 = dplyr::recode(.$Action2, "right" = 1, "left" = 0), #recode action 2: right is 1; left is 0.
                       propA2R = base::cumsum(action2) / dplyr::row_number(), #prop of action 2 that is right (see logic in comment for action 1)
                       propA2L = 1 - propA2R, #prop of action 2 that is left (1 - prop right)
                       state3 = State3, #state 3
                       reward = Reward2, #reward or not
                       probAR = probAR, #reward probability for image AR
                       probAL = probAL, #reward probability for image AL
                       probBR = probBR, #reward probability for image BR
                       probBL = probBL, #reward probability for image BL
                       alpha = Alpha, #alpha
                       gamma = Gamma, #gamma
                       epsilon = epsilon, #epsilon
                       tau = Tau, #tau
                       experience = experience,
                       row = row) %>%
      removeTable()

  return(manipulated)
}

#' Interleave two dataframes
#'
#' @description The interleave function takes in two data frames, x and y. It
#'   interleaves every row in x with every n rows in y. That is, the first row
#'   of x is followed by n rows of y, and then the next row of x, and then the
#'   next n rows of y, etc.
#'
#'   The interleave function -- specific to this package -- adds two columns,
#'   \code{experience} and \code{row} that describe contain the source of
#'   experience (real or simulated) and the row number from each dataframe.
#'
#'   This is used in the \code{\link{processSimData}} function.
#'
#'
#' @param x The dataframe whose data should be in the first row.
#' @param y The dataframe whose data should be interleaved with parameter x.
#' @param n The amount of rows in y to follow each row in x. Should be divisible
#'   by y.
#'
#' @return A dataframe of length \code{x} + \code{y}.
#' @export
#'
#' @examples
#' \dontrun{
#' #Interleave by every row:
#' yesData <- dplyr::tibble("response" = base::rep("yes", 5))
#' noData <- dplyr::tibble("response" = base::rep("no", 5))
#'
#' responseData <- interleave(x = yesData, y = noData, n = 1)
#'
#' #Interleave by every 2 rows
#' firstSet <- dplyr::tibble("data" = base::seq(1,10))
#' secondSet <- dplyr::tibble("data" = base::seq(1,20))
#'
#' interleave(x = firstSet, y = secondSet, n = 2)
#' }
#'

interleave <- function(x, y, n) {

  if (base::names(x) !=  base::names(y)) {
    stop("Column names are not equal. Cannot interleave.")
  }

  #Add the experience and row number for easy validation
  x <- x %>% dplyr::mutate(experience = "Real",
                    row = dplyr::row_number())
  y <- y %>% dplyr::mutate(experience = "Simulated",
                    row = dplyr::row_number())

  #initialize counters
  i <- 1
  j <- 1
  combine <- NULL

  # if combine is NULL, then
  # just bind the first row of x
  # with the first n rows of y
  if (base::is.null(combine)) {
    xRow <- dplyr::slice(x, i)
    yRow <- dplyr::slice(y, j:(j + (n-1)))
    combine <- dplyr::bind_rows(xRow, yRow)
    j <- j + n
  }

  # for every row in x, starting at 2,
  # get the n rows in y and bind them in order.
  # save as combine.
  for (i in 2:base::nrow(x)) {
    xRow <- dplyr::slice(x, i)
    yRow <- dplyr::slice(y, j:(j + (n-1)))
    combine <- dplyr::bind_rows(combine, xRow, yRow)
    j <- j + n
  }

  return(combine)
}


# processSimData Function -------------------------------------------------
# processSimData function takes in the output of generateData if it's not pure model-free
# and then returns the unlisted version of simulation in order.
# x is the number of simulations per real trials from generateData.
# The output of preprocessSimData if removeTable = TRUE can be passed into the stayProbabilityPlot function, for instance.
# The output of preprocessSimData if removeTable = FALSE can be passed into getPlots, for instance.

#' Process Simulated Data
#'
#' This function takes in the output of generateData if x is greater than 0 and
#' returns a sorted version that has real and simulated experiences in the
#' appropriate order, either with or without the Qtable.
#'
#'
#'
#' @param data The output of \code{\link{generateData}} if it's not pure
#'   model-free.
#' @param x The number of simulations per real trial.
#' @param removeTable Logical: TRUE if the Qtable should be removed. FALSE if it
#'   should not be removed.
#'
#' @return A dataframe of real and simulated data in the appropriate order.
#'
#' If removeTable = TRUE, the output of this does not contain the Q table and
#' can be passed into functions such as stayProbabilityPlot.
#'
#' If removeTable = FALSE, the output of this contains the Q table and can be passed into functions such as getPlots.
#'
#' @export
#'
processSimData <- function(data, x, removeTable) {

  if(removeTable) {
    real <- data[[1]] %>% manipulateData()
    simulated <- data[[2]] %>% manipulateData()

    output <- interleave(real, simulated, n = x)

  }else if (!removeTable){output <- interleave(data[[1]], data[[2]], n = x)}

  return(output)

}

