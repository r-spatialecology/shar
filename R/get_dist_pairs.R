#' get_dist_pairs
#'
#' @description Distance between points
#'
#' @param X ppp object
#' @param rmax Numeric with maximum distance
#'
#' @details
#' Returns matrix with point pairs and distances between them.
#'
#' @seealso
#' \code{\link{pcf.ppp}}
#'
#' @return matrix
#'
#' @keywords internal
get_dist_pairs <- function(X, rmax){

  dist_observed <- spatstat.geom::closepairs(X = X, rmax = rmax, what = "ijd", twice = TRUE)

  cbind(dist_observed$i, dist_observed$j, dist_observed$d)

}
