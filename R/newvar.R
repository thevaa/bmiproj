#' Recurrence Relationship for Sample Variance
#'
#'  This functions updates the sample variance using old sample variance, old mean and new value
#'
#'   @param oldvar a numeric value of old sample variance
#'   @param n a numeric value for the number of data including the new data
#'   @param new a numeric value of the new data point
#'   @param mu a numeric value of the old mean
#'   @return a numeric value of the updated sample variance
#'   @author Thevaa Chandereng
#'   @details
#'   This function updates the sample variance using recurrence relationship between old mean, old sample variance and the new
#'   value.
#'   @seealso \code{var}
#'   @seealso \code{new_mean}
#'   @export
#'
newvar <- function(oldvar, n, mu, new){
  stopifnot(all(sapply(c(oldvar, n, mu, new), is.numeric)))
  sam_var <- ((n - 2) / (n - 1)) * oldvar + ((new - mu)^2) / n
  return(sam_var)
}

