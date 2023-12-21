#' select_kernel
#'
#' @description Kernel selection
#'
#' @param kernel_arg  Parameter of the function
#' reconstruct_pattern_multi_trait_marks, specifies the kernel to be used to
#' calculate the energy, possible kernels can be: Gaussian, Epanechnikov,
#' Rectangular, Cumulative.
#' @param bw Bandwidth with which the kernels are scaled, so that this is the
#' standard deviation of the smoothing kernel.
#' @param rmax Maximum distance at which the summary statistics are evaluated.
#' @param divisor Divisor in the smoothing kernel, d or r.
#'
#' @details
#' Returns the function of the selected kernel, which is then used to
#' calculate the kernel.
#'
#' @return list
#'
#' @aliases select_kernel
#' @rdname select_kernel
#'
#' @keywords internal
#'
select_kernel <- function(kernel_arg, bw, rmax, divisor) {
  kernel <- switch(kernel_arg,
    epanechnikov = {
      a <- bw * sqrt(5)
      rmax_bw <- rmax + a
      switch(divisor,
        {
          rmax_bw <- sqrt(rmax^2 + a/pi)
          function(r, d) pmax.int(0, 1 - ((r^2-d^2)*pi/a)^2) * 0.75/a
        },
        none = function(r, d) pmax.int(0, 1 - ((r-d)/a)^2) * 0.75/a,
        r = function(r, d) pmax.int(0, 1 - ((r-d)/a)^2) * 0.75/(a*2*pi*r),
        d = function(r, d) pmax.int(0, 1 - ((r-d)/a)^2) * 0.75/(a*2*pi*d)
      )
    },
    rectangular =, box = {
      a <- bw * sqrt(3)
      rmax_bw <- rmax + a
      switch(divisor,
        {
          rmax_bw <- sqrt(rmax^2 + a/pi)
          function(r, d) stats::dunif((r^2-d^2)*pi,-a,+a)
        },
        none = function(r, d) stats::dunif(r,d-a,d+a),
        r = function(r, d) stats::dunif(r,d-a,d+a)/(2*pi*r),
        d = function(r, d) stats::dunif(r,d-a,d+a)/(2*pi*d)
      )
    },
    gaussian = {
      rmax_bw <- Inf
      switch(divisor,
        function(r, d) stats::dnorm((r^2-d^2)*pi,0,sd=bw),
        none = function(r, d) stats::dnorm(r,d,sd = bw),
        r = function(r, d) stats::dnorm(r,d,sd = bw)/ (2*pi*r),
        d = function(r, d) stats::dnorm(r,d,sd = bw)/ (2*pi*d)
      )
    },
    cumulative = {
      rmax_bw <- rmax
      switch(divisor,
        function(r, d) as.numeric(d <= r),
        none = function(r, d) as.numeric(d <= r),
        r = function(r, d) (d <= r) / (2*pi*r),
        d = function(r, d) (d <= r) / (2*pi*d)
      )
    }
  )
  list(kernel, rmax_bw)
}


