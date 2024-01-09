#' compute_statistics
#'
#' @description Compute summary statistics
#'
#' @param x,y x and y coordinates of the points from the reference point pattern.
#' @param k Vector of values k; used only if Dk is included in w_statistics below.
#' @param xr,yr x and y extension of the observation window (start, end).
#' @param w_statistics  vector of named weights for optional spatial statistics
#' from the \code{spatstat} package to be included in the energy calculation. This may
#' include Dk, K, Hs, pcf.
#' @param bw,divisor,kernel_arg,r Several parameters related to summary function.
#'
#' @details
#' Compute optional spatial statistics using the spatstat package.
#'
#' @return list
#'
#' @keywords internal
compute_statistics <- function(x, y, k, xr, yr, w_statistics, bw, divisor, kernel_arg, r) {

  stat <- names(w_statistics)
  names(stat) <- stat
  lapply(stat, function(name) switch(name,
    # Calculation of the Dk(r)-function, if this is to be taken into account for the energy calculation.
    Dk = {
      nnd_ <- as.matrix(spatstat.geom::nndist(x, y, k=k))
      apply(nnd_, 2, function(z) cumsum(graphics::hist(z[z <= max(r)], breaks = c(-Inf, max(r)), plot = FALSE) $ count) / length(z))
    },

    # Calculation of the K(r)-function, if this is to be taken into account for the energy calculation.
    K = {
      kest<-spatstat.explore::Kest(spatstat.geom::ppp(x,y,window=spatstat.geom::owin(xr,yr)), rmax=max(r), correction="none")
      kest$un
    },

    # Calculation of the pcf(r)-function (spherical contact distribution), if this is to be taken into account for the energy calculation.
    pcf = {
      pcfest<-spatstat.explore::pcf.ppp(spatstat.geom::ppp(x,y,window=spatstat.geom::owin(xr,yr)), r=c(0,r), kernel=kernel_arg, divisor=divisor, bw=bw, correction="none")
      pcfest$un
    },
    # Calculation of the Hs(r)-function (pair correlation function), if this is to be taken into account for the energy calculation.
    Hs = {
      hest<-spatstat.explore::Hest(spatstat.geom::ppp(x,y,window=spatstat.geom::owin(xr,yr)), correction="none")
      hest$raw
    },
    # wrong selection
    stop("unknown statistic")
    )
  )
}
