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
#' landscape_classified <- classify_habitats(landscape, classes = 5)
#' landscape_random <- randomize_raster(landscape_classified, n_random = 19)
#' plot(landscape_random)
#' }
#'
#' @aliases plot.rd_ras
#' @rdname plot.rd_ras

#' @export
plot.rd_ras <- function(x, n = NULL, col, verbose = TRUE, nrow, ncol, ...) {

  # check if class is correct
  if (!inherits(x = x, what = "rd_ras")) {

    stop("Class of 'raster' must be 'rd_ras'.", call. = FALSE)

  }

  # check if observed is present
  if (!methods::is(x$observed, "RasterLayer")) {

    stop("Input must include 'observed' raster.", call. = FALSE)

  }

  habitats <- sort(table(x$observed@data@values, useNA = "no")) # get table of habitats

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
    if (length(x$randomized) < 4) {

      # set n to numer of randomized raster
      n <- length(x$randomized)

      # print message
      if (verbose) {

        message("> Setting n = ", n)

      }

      # more than 3 randomized rasters
    } else {

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
    if (any(n > length(x$randomized))) {

      # remove not valid IDs
      n <- n[n <= length(x$randomized)]

      if (length(n) == 0) {

        stop("Please provide at least on valid ID for n.", call. = FALSE)

      }

      if (verbose) {

        warning("Using only n IDs that are present in randomized data.", call. = FALSE)

      }
    }

    subset_raster <- x$randomized[n]

    # only one number provided for n
  } else {

    # n larger than number of randomized rasters
    if (n > length(x$randomized)) {

      # check if less than 3 randomized raster present
      if (length(x$randomized) < 4) {

        n <- length(x$randomized)

        # more than 3 randomized raster
      } else {

        n <- 3

      }

      # print warning
      if (verbose) {

        warning("'n' larger than number of randomized rasters - setting n = ", n, ".",
                call. = FALSE)

      }
    }

    # sample raster
    subset_raster <- sample(x = x$randomized, size = n)

  }

  # add observed raster to subset
  subset_raster$observed <- x$observed

  # stack rasters
  raster_stack <- raster::stack(subset_raster)

  # plot result
  raster::plot(raster_stack, col = col, nc = ncol, nr = nrow, colNA = "grey")

  invisible()
}
