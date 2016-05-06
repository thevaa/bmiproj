#' Recurrence Relationship for Mean
#'
#'  This functions updates the mean using old mean and new value
#'
#'   @param mu a numeric value of old mean
#'   @param x a numeric value for the new value to be added to the old mean
#'   @param n a numeric value for the number of data including the new data
#'   @return a numeric value of the updated mean
#'   @author Thevaa Chandereng
#'   @details
#'   This function updates the mean using recurrence relationship between old mean and the new
#'   value.
#'   @seealso \code{mean}
#'   @export
#'
new_mean <- function(mu, x, n){
  stopifnot(all(sapply(c(mu, x, n), is.numeric)))
  mu.new <- mu + (x - mu) / n
  return(mu.new)
}
