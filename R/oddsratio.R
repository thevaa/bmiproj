#' Odds ratio for 2x2 table with Confidence Interval
#'
#' This functions prints a list of odds ratio and confidence interval for a 2x2
#' count data
#'
#' @param x a vector of length 4 with the data in the order of a, b, c, d
#' @param CI a numeric vector indicating the two sided confidence interval
#' @return a list of two elements: element 1 is the odds ratio, element 2 is a vector of length
#' two of lower bound and upper bounds of the odds ratio
#' @author Thevaa Chandereng
#' @seealso \code{mean}
#' @export
oddsratio <- function(x, CI = 0.95){
  stopifnot(all(sapply(x, is.numeric)))
  stopifnot(length(x) == 4)
  odds <- (x[4] * x[1]) / (x[2] * x[3])
  log.se.odds <- sqrt(1/x[1] + 1/x[2] + 1/x[3]  + 1/x[4])
  z <- qnorm(1-(1- CI)/2)
  upper <- exp(log(odds) + log.se.odds * z)
  lower <- exp(log(odds) - log.se.odds * z)
  return(list(odds = odds, conf.int = c(lower, upper)))
}
