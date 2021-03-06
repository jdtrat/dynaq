% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{getQGraphicValues}
\alias{getQGraphicValues}
\title{Get Q Values for Plotting}
\usage{
getQGraphicValues(Qtable, image)
}
\arguments{
\item{Qtable}{The Qtable from a simulation (i.e. the output of
\code{\link{generateData}})}

\item{image}{The image ID to be selected (either "AR", "AL", "BR", or "BL").}
}
\value{
A dataframe with 201 rows and 4 columns that have the row number
  (trial number), Q values for both of the paths leading to an image and the
  average of those Q values.
}
\description{
This function gets the Q values so we can plot them over time. It filters the
relevant Q values for the image supplied, transmutes it so there's only the
name column and the second state Q value. It then groups by name and adds a
row (trial) number, pivoting the data wider so there is one column for each
of Q values (one for each possible image path). It then calculates the mean
of those two Q values and puts that in a column with the same name as the
image input. For instance, the Q value of AR is the mean of the Q values for
RAR and LAR.
}
