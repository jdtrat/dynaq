% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processSimulations.R
\name{interleave}
\alias{interleave}
\title{Interleave two dataframes}
\usage{
interleave(x, y, n)
}
\arguments{
\item{x}{The dataframe whose data should be in the first row.}

\item{y}{The dataframe whose data should be interleaved with parameter x.}

\item{n}{The amount of rows in y to follow each row in x. Should be divisible
by y.}
}
\value{
A dataframe of length \code{x} + \code{y}.
}
\description{
The interleave function takes in two data frames, x and y. It
  interleaves every row in x with every n rows in y. That is, the first row
  of x is followed by n rows of y, and then the next row of x, and then the
  next n rows of y, etc.

  The interleave function -- specific to this package -- adds two columns,
  \code{experience} and \code{row} that describe contain the source of
  experience (real or simulated) and the row number from each dataframe.

  This is used in the \code{\link{processSimData}} function.
}
\examples{
\dontrun{
#Interleave by every row:
yesData <- dplyr::tibble("response" = base::rep("yes", 5))
noData <- dplyr::tibble("response" = base::rep("no", 5))

responseData <- interleave(x = yesData, y = noData, n = 1)

#Interleave by every 2 rows
firstSet <- dplyr::tibble("data" = base::seq(1,10))
secondSet <- dplyr::tibble("data" = base::seq(1,20))

interleave(x = firstSet, y = secondSet, n = 2)
}

}
