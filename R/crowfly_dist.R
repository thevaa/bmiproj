#' Crowfly distance between two points
#'
#'  This functions calculates the crowfly distance between two points (lattitude and logitude).
#'  The output is in miles.
#'
#'   @param long1 a numeric value for the longitude of point 1
#'   @param lat1 a numeric value for the latitude of point 1
#'   @param long2 a numeric value for the longitude of point 2
#'   @param lat2 a numeric value for the latitude of point 2
#'   @return a numeric value of the distance between point 1 and 2
#'   @author Thevaa Chandereng
#'
crowfly_dist <- function (long1, lat1, long2, lat2){
  stopifnot(sapply(c(long1, lat1, long2, lat2), is.numeric))
  stopifnot(all(c(long1, long2) < 180 & c(long1, long2) > -180))
  stopifnot(all(c(lat1, lat2) < 90 & c(lat1, lat2) > -90))
  rad <- pi / 180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat / 2))^2 + cos(a1) * cos(b1) * (sin(dlon / 2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d / 1.60934)
}
