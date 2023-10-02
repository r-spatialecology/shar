#' calc_gest
#'
#' @description Calculate Gest
#'
#' @param dist matrix with distance pairs.
#' @param r vector with distances r.
#' @param n_points numeric with number of points
#'
#' @details
#' Calculates Gest based on distances created with \code{get_dist_pairs}.
#'
#' @seealso
#' \code{\link{get_dist_pairs}}
#'
#' @return data.frame
#'
#' @aliases calc_gest
#' @rdname calc_gest
#'
#' @keywords internal
calc_gest <- function(dist, r, n_points){

  mat <- matrix(nrow = n_points, ncol = n_points, data = Inf)
  mat[dist[, 1:2]] <- dist[, 3]

  distances_min <- apply(X = mat, MARGIN = 2, FUN = min, na.rm = TRUE)

  hist_min <- graphics::hist(distances_min, breaks = r, plot = FALSE)

  data.frame(r = hist_min$mids, edf = cumsum(hist_min$counts) / n_points)

}
