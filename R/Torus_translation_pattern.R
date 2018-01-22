#' Torus translation
#'
#' Shift of the point pattern in x and y direction treating the observation window as a torus
#' @param x_shift [\code{numeric(1)}] Shift in x direction
#' @param y_shift [\code{numeric(1)}] Shift in y direction
#' @return ppp object of the spatstat package

#' @export
# Torus.Translation.Pattern <- function(pattern, x_shift=0, y_shift=0){
#   xoff <- min(pattern$window$xrange)
#   yoff <- min(pattern$window$yrange)
#   xsc <- (max(pattern$window$xrange) - xoff)
#   ysc <- (max(pattern$window$yrange) - yoff)
#   pattern$x <- pattern$x - xoff
#   pattern$y <- pattern$y - yoff
#   pattern$x <- pattern$x + x_shift
#   pattern$y <- pattern$y + y_shift
#   pattern$x <- (pattern$x%%xsc) + xoff
#   pattern$y <- (pattern$y%%ysc) + yoff
#
#   return(pattern)
# }


