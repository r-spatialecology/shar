#' plot_energy
#'
#' @description Plot energy of pattern reconstruction
#'
#' @param pattern List with reconstructed patterns.
#' @param col Vector with colors. Must be as long as n_random.
#'
#' @details
#' The function plots the decrease of the energy over time, i.e. the iterations.
#' This can help to identify if enough max_runs where chosen for the reconstruction.
#'
#' @seealso
#' \code{\link{calculate_energy}} \cr
#' \code{\link{reconstruct_pattern_homo}} \cr
#' \code{\link{reconstruct_pattern_hetero}} \cr
#' \code{\link{reconstruct_pattern_cluster}} \cr
#' \code{\link{plot_randomized_pattern}}
#'
#' @examples
#' \dontrun{
#' pattern_recon <- reconstruct_pattern_homo(species_a, n_random = 3, max_runs = 1000)
#' plot_energy(pattern_recon)
#'
#' marks_sub <- spatstat::subset.ppp(species_a, select = dbh)
#' marks_recon <- reconstruct_pattern_marks(pattern_recon$randomized[[1]], marks_sub,
#' n_random = 1, max_runs = 1000)
#' plot_energy(marks_recon)
#' }
#'
#' @aliases plot_energy
#' @rdname plot_energy
#'
#' @export
plot_energy <- function(pattern,
                        col = NULL){

  # check if class is correct
  if (!class(pattern) %in% c("rd_pat", "rd_mar")) {

    stop("Class of 'pattern' must be 'rd_pat' or 'rd_mar'.",
         call. = FALSE)
  }

  if (all(pattern$energy_df == "NA")) {

    stop("There is no 'energy_df' slot. Please use pattern reconstruction for valid input data.",
         call. = FALSE)
  }

  # get number of rows
  range_i <- range(vapply(X = pattern$energy_df,
                          FUN = function(x) range(x$i), FUN.VALUE = numeric(2)))

  range_energy <- range(vapply(X = pattern$energy_df,
                               FUN = function(x) range(x$energy), FUN.VALUE = numeric(2)))

  # sample colors
  if (is.null(col)) {

    col <- rep("black", times = length(pattern$energy_df))
  }

  # plot pcf
  graphics::plot(NULL,
                 xlim = range_i,
                 ylim = range_energy,
                 main = "Energy over iterations",
                 xlab = "Iterations",
                 ylab = "Energy")

  for (i in seq_along(pattern$energy_df)) {

    graphics::lines(x = pattern$energy_df[[i]][, 1],
                    y = pattern$energy_df[[i]][, 2],
                    col = col[[i]])

  }
}
