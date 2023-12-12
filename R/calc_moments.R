#' calc_moments
#'
#' @description calc_moments (internal)
#'
#' @param fn Determination of the weightings of the mark correlation functions.
#' @param p Defines the initial state of the new ponit pattern.
#' @param x x Coordinate of the points from the reference point pattern.
#' @param y y Coordinate of the points from the reference point pattern.
#' @param mark Marks the currently viewed point pattern.
#' @param kernel Result of the kernel calculation, calculated with the
#' calc_kernels function.
#' @param rmax_bw Maximum distance at which the summary statistics are
#' evaluated + Bandwidth with which the kernels are scaled, so that this is the
#'  standard deviation of the smoothing kernel.
#' @param r is a sequence from rmin to rmax in rcount steps.
#' @param exclude
#'
#' @details
#' Definition of the product-moment function for calculating the contribution
#' of a point at the coordinates x, y with marking.
#'
#' @return matrix
#'
#' @aliases calc_moments
#' @rdname calc_moments
#'
#' @aliases reconstruct_algorithm
#' @rdname reconstruct_algorithm
#'
#' @keywords internal
#'
#'
calc_moments <- function(fn,
                         p,
                         exclude=NULL,
                         x,
                         y,
                         mark,
                         kernel,
                         rmax_bw,
                         r)
  {
  d2 <- (p$x-x)^2 + (p$y-y)^2
  use <-  d2 <= rmax_bw^2
  use[exclude] <- FALSE
  z <- crossprod(p$mark[use, , drop = FALSE],
                 outer(sqrt(d2[use]), r, function(d, r) kernel(r, d)))
  z[fn$i, , drop = FALSE] * mark[fn$j] + z[fn$j, , drop = FALSE] * mark[fn$i]
  }

calc_moments_full <- function(fn,
                              p,
                              kernel,
                              rmax_bw,
                              r)
  {
  f <- 0
  for (i in seq_len(nrow(p))) {
    f <- f + calc_moments(fn, p, i:nrow(p), p$x[i], p$y[i], p$mark[i, ],
                          kernel, rmax_bw, r)
    }
  rownames(f) <- paste(names(fn$i), names(fn$j), sep = ":")
  f
  }
