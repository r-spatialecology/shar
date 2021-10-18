#' list_to_randomized
#'
#' @description Convert list to rd_* object.
#'
#' @param list List
#' @param observed Observed
#'
#' @details
#' Convert list of randomized point pattern or raster layer to a rd_* object that
#' can be used with all functions of the package. The main purpose of this utility function
#' is to allow an easy parallelization of the randomization approach.
#'
#' For more information, please see the "Parallelization" article.
#'
#' @seealso
#' \code{\link{randomize_raster}} \cr
#' \code{\link{translate_raster}} \cr
#' \code{\link{reconstruct_pattern_homo}} \cr
#' \code{\link{reconstruct_pattern_hetero}} \cr
#' \code{\link{reconstruct_pattern_cluster}}
#'
#' @return rd_pat, rd_ras
#'
#' @examples
#' \dontrun{
#' fit_list <- lapply(X = 1:39, FUN = function(i) {fit_point_process(pattern = species_a,
#' n_random = 1, simplify = TRUE, return_input = FALSE, verbose = FALSE)})
#'
#' list_to_randomized(list = fit_list, observed = species_a)
#' }
#'
#' @aliases list_to_randomized
#' @rdname list_to_randomized
#'
#' @export
list_to_randomized <- function(list, observed = NULL) {

  # get classes of input
  list_class <- vapply(X = list, FUN = function(i) class(i), FUN.VALUE = character(1))

  # create class of output
  # check if ppp
  if (all(list_class == "ppp")) {

    # check if pattern is marked
    ppp_mark <- vapply(X = list, FUN = spatstat.geom::is.marked, FUN.VALUE = logical(1))

    if (all(ppp_mark)) {

      result_class <- "rd_mar"

    } else {

      result_class <- "rd_pat"

    }

  # randomized raster
  } else if (all(list_class == "RasterLayer")) {

    result_class <- "rd_ras"

  } else {

    stop("Please provide list of either 'ppp' or 'RasterLayer' objects.", call. = FALSE)

  }

  # return observed if present or NA if not
  if (is.null(observed)) {

    observed <- "NA"

  }

  # set names
  names(list) <- paste0("randomized_", seq_along(list))

  # create empty iterations list
  iterations_list <- as.list(rep(NA, times = length(list)))

  # combine to one list
  result <- list(randomized = list, observed = observed,
                 method = "list_to_randomized()", energy_df = "NA",
                 stop_criterion = "NA", iterations = iterations_list)

  class(result) <- result_class

  return(result)
}
