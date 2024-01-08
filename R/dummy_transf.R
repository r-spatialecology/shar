#' dummy_transf
#'
#' @description Tranfsorm to dummy variables
#'
#' @param f Result of the calc_moments_full function which represents
#' product-moment contribution of a point at coordinates x, y with marks,
#' for the whole point pattern.
#'
#' @details
#' Function for the transformation of variables to dummy variables and back
#'
#' @return matrix
#'
#' @keywords internal
to_dummy <- function(f) {
  x <- matrix(0, length(f), nlevels(f), dimnames=list(names(f), levels(f)))
  x[cbind(seq_along(f), as.integer(f))] <- 1
  x
}

from_dummy <- function(x, levels=colnames(x)) {
  f <- as.integer(x %*% seq_along(levels))
  levels(f) <- levels
  class(f) <- "factor"
  f
}
