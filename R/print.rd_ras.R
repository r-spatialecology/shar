#' print.rd_ras
#'
#' @description Print method for rd_ras object
#'
#' @param x Random patterns.
#' @param ... Arguments passed to cat
#'
#' @details
#' Printing method for random patterns created with \code{\link{randomize_raster}}.
#'
#' @seealso
#' \code{\link{randomize_raster}}
#'
#' @examples
#' \dontrun{
#' landscape_classified <- classify_habitats(landscape, classes = 5)
#' landscape_random <- randomize_raster(landscape_classified, n_random = 19)
#'
#' print(landscape_random)
#' }
#'
#' @aliases print.rd_ras
#' @rdname print.rd_ras

#' @export
print.rd_ras <- function(x,
                         ...) {

  # set length observed pattern to 0 and
  # return warning that energy can't be calculated
  if (!methods::is(x$observed, "RasterLayer")) {

    number_raster_obs <- 0

    includes_observed <- "NA"
  }

  # observed pattern is present
  else {

    number_raster_obs <- 1

    includes_observed <- "included"
  }

  # get extent of window
  extent_window <- paste0(c(raster::xmin(x$randomized[[1]]),
                            raster::xmax(x$randomized[[1]]),
                            raster::ymin(x$randomized[[1]]),
                            raster::ymax(x$randomized[[1]])),
                          collapse = " ")

  # get number of randomized patterns plus observed pattern
  number_raster <- length(x$randomized) + number_raster_obs

  # print result
  cat(paste0("No. of raster: ", number_raster, "\n",
             "Method: ", x$method, "\n",
             "Observed pattern: ", includes_observed, "\n",
             "Extent: ", extent_window, " (xmin, xmax, ymin, ymax) \n"), ...)
}
