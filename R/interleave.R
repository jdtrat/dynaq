#' Interleave two dataframes
#'
#' The interleave function takes in two data frames, x and y. It interleaves
#' every row in x with every n rows in y. That is, the first row of x is
#' followed by n rows of y, and then the next row of x, and then the next n rows
#' of y, etc.
#'
#'
#' @param x The dataframe whose data should be in the first row.
#' @param y The dataframe whose data should be interleaved with parameter x.
#' @param n The amount of rows in y to follow each row in x.
#'   Should be divisible by y.
#' @param check Logical. If \code{TRUE}, data will be appended with two columns
#'   \emph{source} and \emph{row} which can be used to check the output
#'   dataframe.
#'
#' @return A dataframe of length \code{x} + \code{y}.
#' @export
#'
#' @examples
#'
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
#'

interleave <- function(x, y, n, check = FALSE) {

  if (base::names(x) !=  base::names(y)) {
    stop("Column names are not equal. Cannot interleave.")
  }


  if (check) {

    #Add the source and row number for easy validation
    x <- x %>% dplyr::mutate(source = "x",
                             row = dplyr::row_number())
    y <- y %>% dplyr::mutate(source = "y",
                             row = dplyr::row_number())
  }

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
