#' plot_randomized_raster
#'
#' @description Plot randomized raster
#'
#' @param raster List with randomized raster
#' @param n Number of randomized rasters to plot. See details for more information.
#' @param col Color palette used for plotting.
#' @param nrow,ncol Number of rows and columns.
#' @param verbose Print messages.
#'
#' @details
#' Function to plot randomized rasters. If n is only a single number, n randomized
#' rasters will be sampled. If n is a vector, the corresponding rasters will be plotted.
#'
#' @return plot
#'
#' @examples
#' \dontrun{
#' landscape_classes <- classify_habitats(raster = landscape, classes = 5)
#' landscape_random <- randomize_raster(raster = landscape_classes, n_random = 19)
#'
#' plot_randomized_raster(landscape_random)
#'
#' palette <- viridis::viridis(n = 5)
#' plot_randomized_raster(landscape_random, n = 5, col = palette, nrow = 3, ncol = 2)
#' }
#'
#' @aliases plot_randomized_raster
#' @rdname plot_randomized_raster
#'
#' @export
plot_randomized_raster <- function(raster,
                                   n = NULL,
                                   col,
                                   verbose = TRUE,
                                   nrow, ncol){

  # check if class is correct
  if (class(raster) != "rd_ras") {

    stop("Class of 'raster' must be 'rd_ras'.", call. = FALSE)
  }

  # check if observed is present
  if (!methods::is(raster$observed, "RasterLayer")) {

    stop("Input must include 'observed' raster.", call. = FALSE)
  }

  habitats <- sort(table(raster$observed@data@values, useNA = "no")) # get table of habitats

  # print warning if more than 10 classes are present
  if (verbose) {

    if (length(habitats) > 10) {

      warning("The raster has more than 10 classes. Please make sure discrete classes are provided.",
              call. = FALSE)
    }
  }

  # set n if not provided by user
  if (is.null(n)) {

    # check if less than 3 randomized raster are present
    if (length(raster$randomized) < 4) {

      # set n to numer of randomized raster
      n <- length(raster$randomized)

      # print message
      if (verbose) {

        message("> Setting n = ", n)
      }
    }

    # more than 3 randomized rasters
    else {

      # set n to 3
      n <- 3

      # print message
      if (verbose) {

        message("> Setting n = ", n)
      }
    }
  }

  # vector provided, subset rasters with corresponding ID
  if (length(n) > 1) {

    # check if any ID is larger than length of list
    if (any(n > length(raster$randomized))) {

      # remove not valid IDs
      n <- n[n <= length(raster$randomized)]

      if (length(n) == 0) {

        stop("Please provide at least on valid ID for n.", call. = FALSE)
      }

      if (verbose) {

        warning("Using only n IDs that are present in randomized data.", call. = FALSE)
      }

    }

    subset_raster <- raster$randomized[n]
  }

  # only one number provided for n
  else {

    # n larger than number of randomized rasters
    if (n > length(raster$randomized)) {

      # check if less than 3 randomized raster present
      if (length(raster$randomized) < 4) {

        n <- length(raster$randomized)
      }

      # more than 3 randomized raster
      else{

        n <- 3
      }

      # print warning
      if (verbose) {

        warning("'n' larger than number of randomized rasters - setting n = ", n, ".",
                call. = FALSE)
      }
    }

    # sample raster
    subset_raster <- sample(x = raster$randomized, size = n)
  }

  # add observed raster to subset
  subset_raster$observed <- raster$observed

  # stack rasters
  raster_stack <- raster::stack(subset_raster)

  # plot result
  raster::plot(raster_stack, col = col, nc = ncol, nr = nrow,
               colNA = "grey")

  invisible()
}
