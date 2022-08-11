#' print.rd_ras
#'
#' @description Print method for rd_ras object
#'
#' @param x rd_ras object with randomized raster.
#' @param ... Arguments passed to \code{cat}.
#'
#' @details
#' Printing method for random patterns created with \code{\link{randomize_raster}} or
#' \code{\link{translate_raster}}.
#'
#' @seealso
#' \code{\link{randomize_raster}} \cr
#' \code{\link{translate_raster}}
#'
#' @return void
#'
#' @examples
#' \dontrun{
#' landscape_classified <- classify_habitats(terra::rast(landscape), n = 5, style = "fisher")
#' landscape_random <- randomize_raster(landscape_classified, n_random = 19)
#'
#' print(landscape_random)
#' }
#'
#' @aliases print.rd_ras
#' @rdname print.rd_ras
#'
#' @export
print.rd_ras <- function(x, ...) {

  # check if observed raster is included
  if (!methods::is(x$observed, "SpatRaster")) {

    number_raster_obs <- 0

    includes_observed <- "NA"

  # observed pattern is present
  } else {

    number_raster_obs <- 1

    includes_observed <- "included"

  }

  # get extent of window
  extent_window <- paste0(c(terra::xmin(x$randomized[[1]]), terra::xmax(x$randomized[[1]]),
                            terra::ymin(x$randomized[[1]]), terra::ymax(x$randomized[[1]])),
                          collapse = " ")

  # get number of randomized patterns plus observed pattern
  number_raster <- length(x$randomized) + number_raster_obs

  # print result
  cat(paste0("No. of raster: ", number_raster, "\n",
             "Method: ", x$method, "\n",
             "Observed pattern: ", includes_observed, "\n",
             "Extent: ", extent_window, " (xmin, xmax, ymin, ymax) \n"), ...)
}
