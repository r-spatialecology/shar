#' plot.rd_ras
#'
#' @description Plot method for rd_ras object
#'
#' @param x rd_ras object with randomized raster.
#' @param n Integer with number or vector of ids of randomized raster to plot.
#' See Details section for more information.
#' @param col Vector with color palette used for plotting.
#' @param nrow,ncol Integer with number of rows and columns of plot grid.
#' @param verbose Logical if messages are printed.
#' @param ... Not used.
#'
#' @details
#' Function to plot randomized raster. If \code{n} is a single number, \code{n} randomized
#' raster will be sampled to plot. If \code{n} is a vector, the corresponding raster
#' will be plotted. \code{col, nrow, ncol} are passed to \code{plot}.
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
#' plot(landscape_random)
#' }
#'
#' @aliases plot.rd_ras
#' @rdname plot.rd_ras
#'
#' @export
plot.rd_ras <- function(x, n = NULL, col, verbose = TRUE, nrow, ncol, ...) {

  # check if class is correct
  if (!inherits(x = x, what = "rd_ras")) {

    stop("Class of 'raster' must be 'rd_ras'.", call. = FALSE)

  }

  # check if observed is present
  if (!inherits(x = x$observed, what = "SpatRaster")) {

    stop("Input must include 'observed' raster.", call. = FALSE)

  }

  habitats <- sort(table(terra::values(x$observed), useNA = "no")) # get table of habitats

  # print warning if more than 10 classes are present
  if (length(habitats) > 10) {

    warning("The raster has more than 10 classes. Please make sure discrete classes are provided.",
            call. = FALSE)

  }

  # get randomized pattern
  subset_raster <- sample_randomized(randomized = x$randomized, n = n,
                                     verbose = verbose)

  # add observed raster to subset
  subset_raster$observed <- x$observed

  # stack rasters
  raster_stack <- terra::rast(subset_raster)

  # plot result
  terra::plot(raster_stack, col = col, nc = ncol, nr = nrow, colNA = "grey")

  invisible()
}
