% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logisticRegression.R
\name{getLogFit}
\alias{getLogFit}
\title{Get the Logistic Regression Fit}
\usage{
getLogFit(data)
}
\arguments{
\item{data}{The data can \emph{either} be the output of
\code{\link{logSetup}} \emph{or} the output of \code{\link{generateData}}.
If the latter, make sure that manipulateData is called on pure Q-learning
simulations. Similarly, for DynaQ simulations, make sure the data has been
processed and the Q table has been removed.}
}
\value{
This returns a tidymodels workflow object that has been fitted to
  predict the stay probability based on the reward and transition types.
}
\description{
This function takes in a training data set and performs a logistic regression
model to predict the stay probability as a function of whether the last trial
was rewarded or not, whether the last trial had a common or rare transition,
and the interaction between the two. This can be run by itself,
but is called by the function \code{\link{getLogPreds}}.
}
