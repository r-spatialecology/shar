#' sample_randomized
#'
#' @description Sample randomized list
#'
#' @param randomized List with randomized raster or patterns.
#' @param n Integer with number or vector of ids of randomized pattern to plot.
#' @param verbose Logical if progress report is printed.
#'
#' @details
#' Get list with \code{n} randomized raster or patterns. If \code{n} is a single number,
#' \code{n} randomized elements will be sampledt. If \code{n} is a vector, the
#' corresponding elements will be returned.
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' sample_randomized(randomized = reconstruction$randomized, n = c(5, 10, 15))
#' }
#'
#' @aliases sample_randomized
#' @rdname sample_randomized
#'
#' @keywords internal
sample_randomized <- function(randomized, n = NULL, verbose = TRUE) {

  # get number of randomized elements
  n_randomized <- length(randomized)

  # set n if not provided by user
  if (is.null(n)) {

    # check if less than 3 randomized elements are present
    n <- ifelse(test = n_randomized < 4, yes = n_randomized, no = 3)

    # print message
    if (verbose) {

      message("> Setting n = ", n)

    }

    # sample elements
    subset_randomized <- sample(x = randomized, size = n)

  # sample n elements
  } else if (length(n) == 1) {

    # check if n is larger than elements
    if (n > n_randomized) {

      # check if less than 3 randomized elements are present
      n <- ifelse(test = n_randomized < 4, yes = n_randomized, no = 3)

      # print message
      if (verbose) {

        # return warning
        warning("n larger than number of randomize eleements. Setting n = ", n, ".",
                call. = FALSE)

        }
    }

    # sample elements
    subset_randomized <- sample(x = randomized, size = n, replace = FALSE)

  # use vector of id
  } else if (length(n) > 1) {

    # check if any ID is larger than length of list
    if (any(n > n_randomized)) {

      # remove not valid IDs
      n <- n[n <= n_randomized]

      # stop if no n is present in elements
      if (length(n) == 0) {

        stop("Please provide at least on valid ID for n.", call. = FALSE)

      }

      # return warning that some ids were removed
      if (verbose) {

        warning("Using only IDs that are present in randomized data.", call. = FALSE)

      }
    }

    # sample elements
    subset_randomized <- randomized[n]

  }

  # return final list
  return(subset_randomized)
}
