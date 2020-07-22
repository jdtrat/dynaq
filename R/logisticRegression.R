# logSetup Function -------------------------------------------------------
# the logSetup function takes in a dataset and sets it up for logistic regression.
# it mutates the input data frame so reward and transition refer to the last trial's reward and or transition structure
# and the stay column means that the (first) action on this trial is equal to the (first) action on the last trial.
# If the data is pure Q-learning, it should be the result of manipulateData. If it's not, it should be the processed
# sim data without a Qtable.

#' Manipulate the Data for Logistic Regression
#'
#' This function takes in the output of \code{\link{generateData}} (see parameter data for
#' specifics) and sets it up for logistic regression by mutating the dataframe
#' so reward and transition refer to the last trial's reward and transition type
#' and the stay column means that the (first) action on trial n is equal to the
#' (first) action on trial n = 1. This function gets called in
#' \code{\link{getLogFit}} and \code{\link{getLogPreds}} as needed.
#'
#' @param data The output of \code{\link{generateData}}; if pure Q-learning, it manipulateData
#'   should be called first. If it's DynaQ, the data should be processed and the
#'   Q table removed.
#'
#' @return A dataframe with 23 columns. New ones include \code{lastAction1}
#'   (numeric) that tracks the action taken in the previous trial,
#'   \code{lastReward} that tracks whether the last trial was rewarded,
#'   (numeric), and \code{stay} (factor) which tracks whether trial n and n+1
#'   have the same first action.
#' @export
#'

logSetup <- function(data) {

  data %>% mutate(lastAction1 = dplyr::lag(action1), #create lastAction by lagging action1
                  lastReward = dplyr::lag(reward), #get whether last trial was rewarded by lagging reward
                  #stay = 1 if lastAction == action1, 0 otherwise
                  stay = dplyr::case_when(lastAction1 == action1 ~ "Yes",
                                   TRUE ~ "No")) %>%
    #mutate transition to equal the last trial's transition
    #make reward = lastReward
    mutate(transition = dplyr::lag(transition),
           transition = dplyr::recode(transition, `0` = -1),
           reward = lastReward,
           reward = dplyr::recode(reward, `0` = -1),
           stay = base::as.factor(stay)) %>%
    stats::na.omit()

}

#' Get the Logistic Regression Fit
#'
#' This function takes in a training data set and performs a logistic regression
#' model to predict the stay probability as a function of whether the last trial
#' was rewarded or not, whether the last trial had a common or rare transition,
#' and the interaction between the two. This can be run by itself,
#' but is called by the function \code{\link{getLogPreds}}.
#'
#' @param data The data can \emph{either} be the output of
#'   \code{\link{logSetup}} \emph{or} the output of \code{\link{generateData}}.
#'   If the latter, make sure that manipulateData is called on pure Q-learning
#'   simulations. Similarly, for DynaQ simulations, make sure the data has been
#'   processed and the Q table has been removed.
#'
#' @return This returns a tidymodels workflow object that has been fitted to
#'   predict the stay probability based on the reward and transition types.
#'
#' @export
#'
getLogFit <- function(data) {

  #Check to see if the data has been setup for logistic regression. If not, set
  #it up.
  if (base::sum(stringr::str_detect(base::names(data), "stay")) == 0) {
    data <-logSetup(data)
  }

  #create model specification as a logistic regression with glm engine
  logSpec <- parsnip::logistic_reg() %>%
    parsnip::set_engine("glm")

  #define a recipe predicting stay based on reward and transition from training
  #data. step_interact creates an additional interaction term between reward and
  #transition according to the new paper by Feher de Silva and Hare.
  logRecipe <- recipes::recipe(stay ~ reward + transition, data = data) %>%
    recipes::step_interact(terms = ~reward:transition)

  #create a workflow and add the model spec and recipe
  logWF <- workflows::workflow() %>%
    workflows::add_model(logSpec) %>%
    workflows::add_recipe(logRecipe)

  #fit the workflow on the training data
  fitWF <- parsnip::fit(logWF, data = data)

  return(fitWF)

}

# getLogPreds Function -----------------------------------------------------
# getLogPreds function takes in a training and testing dataset and
# calls the getFit function to perform a logistic regression model based
# on the training dataset to predict stay probability as a function of reward, transition, and transition*reward.
# It outputs the testing dataset bound with a column predStay bound to it.

#' Get the Logistic Regression Predictions
#'
#' This function takes in simulation data (see parameter data for more
#' information) and calls the \code{\link{logSetup}} function if necessary
#' before calling the \code{\link{getLogFit}} function to fit a logistic
#' regression model predicting the probability of staying based on whether the
#' last trial was rewarded or not, whether the last trial had a common or rare
#' transition, and the interaction between the two. It then uses the predict
#' function and binds the predictions to the original (manipulated with
#' \code{\link{logSetup}}) dataframe.
#'
#' @param data The data can \emph{either} be the output of
#'   \code{\link{logSetup}} \emph{or} the output of \code{\link{generateData}}.
#'   If the latter, make sure that manipulateData is called on pure Q-learning
#'   simulations. Similarly, for DynaQ simulations, make sure the data has been
#'   processed and the Q table has been removed.
#'
#' @return The original (modified with \code{\link{logSetup}}) data frame plus
#'   two columns \code{predNoStay} and \code{predStay}.
#' @export
#'

getLogPreds <- function(data) {

  #Check to see if the data has been setup for logistic regression. If not, set
  #it up.
  if (base::sum(stringr::str_detect(base::names(data), "stay")) == 0) {
    data <- logSetup(data)
  }

  fit <- getLogFit(data)

  #get the predictions for the testing data
  predicted <- stats::predict(fit, data, type = "prob")

  #bind the testing dataset and the predictions
  #rename .pred_class as predStay
  output <- dplyr::bind_cols(data, predicted) %>%
    dplyr::rename(predNoStay = .pred_No,
           predStay = .pred_Yes)

  return(output)
}

