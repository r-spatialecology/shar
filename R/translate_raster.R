#' translate_raster
#'
#' @description Torus translation
#'
#' @param raster SpatRaster with discrete habitat classes.
#' @param steps_x,steps_y Integer with number of steps (cells) the raster is translated
#' into the corresponding direction. If both are null, all possible combinations are used
#' resulting in n = ((50 + 1) * (50 + 1)) - 4 rasters.
#' @param return_input Logical if the original input data is returned.
#' @param simplify Logical if only the raster will be returned if \code{n_random = 1}
#' and \code{return_input = FALSE}.
#' @param verbose Logical if progress report is printed.
#'
#' @details
#' Torus translation test as described in Harms et al. (2001). The raster is shifted
#' in all four cardinal directions by steps equal to the raster resolution. If a cell
#' exits the extent on one side, it enters the extent on the opposite side.
#'
#' The method does not allow any NA values to be present in the SpatRaster.
#'
#' @seealso
#' \code{\link{randomize_raster}}
#'
#' @return rd_ras
#'
#' @examples
#' \dontrun{
#' landscape_classified <- classify_habitats(terra::rast(landscape), n = 5, style = "fisher")
#'
#' landscape_random <- translate_raster(landscape_classified)
#' landscape_random_sub <- translate_raster(landscape_classified,
#' steps_x = 1:10, steps_y = 1:5)
#' }
#'
#' @references
#' Harms, K.E., Condit, R., Hubbell, S.P., Foster, R.B., 2001. Habitat associations
#' of trees and shrubs in a 50-ha neotropical forest plot. Journal of Ecology 89, 947–959.
#' <https://doi.org/10.1111/j.1365-2745.2001.00615.x>
#'
#' @export
translate_raster <- function(raster, steps_x = NULL, steps_y = NULL,
                             return_input = TRUE, simplify = FALSE,
                             verbose = TRUE) {

  # stop if NA are present
  if (anyNA(terra::values(raster, mat = FALSE))) {

    stop("NA values are not allowed for 'translate_raster()'.", call. = FALSE)

  }

  habitats <- sort(table(terra::values(raster, mat = FALSE))) # get table of habitats

  # print warning if more than 10 classes are present
  if (length(habitats) > 10) {

    warning("The raster has more than 10 classes. Please make sure discrete classes are provided.",
            call. = FALSE)

  }

  # use all possible combinations
  if (is.null(steps_x) & is.null(steps_y)) {

    steps_x <- seq(from = 0, to = terra::ncol(raster), by = 1) # all steps in x-direction

    steps_y <- seq(from = 0, to = terra::nrow(raster), by = 1) # all steps in y-direction

    steps_xy <- expand.grid(x = steps_x, y = steps_y) # grid with all possible x-y combinations

    # remove combinations identical to original raster
    steps_xy <- steps_xy[-c(1, length(steps_x), max(steps_x) * length(steps_y) + 1, length(steps_x) * length(steps_y)),]

  } else {

    if (is.null(steps_x)) {steps_x <- 0}

    if (is.null(steps_y)) {steps_y <- 0}

    steps_xy <- expand.grid(x = steps_x, y = steps_y) # grid with all possible x-y combinations

    # remove combinations identical to original raster
    remove_id <- c(which(steps_xy[, 1] + steps_xy[, 2] == 0),
                   which(steps_xy[, 1] + steps_xy[, 2] ==  terra::ncol(raster) + terra::nrow(raster)),
                   which(steps_xy[, 1] == 0 & steps_xy[, 2] == terra::ncol(raster)),
                   which(steps_xy[, 2] == 0 & steps_xy[, 1] == terra::nrow(raster)))

    if (length(remove_id) > 0) {

      steps_xy <- steps_xy[-remove_id, ]

    }
  }

  matrix_raster <- terra::as.matrix(raster, wide = TRUE, na.rm = TRUE) # convert to matrix

  # loop through all possible steps
  result_list <- lapply(seq_len(nrow(steps_xy)), function(current_row) {

    x_shift <- steps_xy[current_row, 1] - (ncol(matrix_raster) *
                                             (steps_xy[current_row, 1] %/% ncol(matrix_raster)))

    y_shift <- steps_xy[current_row, 2] - (nrow(matrix_raster) *
                                             (steps_xy[current_row, 2] %/% nrow(matrix_raster)))

    if (x_shift == 0) {

      matrix_shifted <- matrix_raster

    } else {

      matrix_shifted <- cbind(matrix_raster[, (x_shift + 1):dim(matrix_raster)[2]],
                              matrix_raster[, seq_len(x_shift)])

    }

    if (y_shift == 0) {

      matrix_shifted <- matrix_shifted

    } else {

      matrix_shifted <- rbind(matrix_shifted[(y_shift + 1):dim(matrix_shifted)[1], ],
                                 matrix_shifted[seq_len(y_shift), ])

    }

    # convert back to raster
    raster_shifted <- terra::rast(matrix_shifted, crs = terra::crs(raster),
                                  extent = terra::ext(raster))

    names(raster_shifted) <- "layer"

    # print progress
    if (verbose) {

      message("\r> Progress: n_random: ", current_row, "/", nrow(steps_xy), "\t\t",
              appendLF = FALSE)

    }

    return(raster_shifted)

  })

  n_random <- length(result_list)

  # set names of randomization randomized_1 ... randomized_n
  names(result_list) <- paste0("randomized_", seq_along(result_list))

  # combine to one list
  randomization <- list(randomized = result_list, observed = raster,
                        method = "translate_raster()")

  # set class of result
  class(randomization) <- "rd_ras"

  # remove input if return_input = FALSE
  if (!return_input) {

    # set observed to NA
    randomization$observed <- "NA"

    # check if output should be simplified
    if (simplify) {

      # not possible if more than one raster is present
      if (n_random > 1) {

        warning("'simplify = TRUE' not possible for 'n_random > 1'.",
                call. = FALSE)

      # only one random raster is present that should be returend
      } else if (n_random == 1) {

        randomization <- randomization$randomized[[1]]

      }
    }
  }

  # return input if return_input = TRUE
  else {

    # return warning if simply = TRUE because not possible if return_input = TRUE (only verbose = TRUE)
    if (simplify) {

      warning("'simplify = TRUE' not possible for 'return_input = TRUE'.", call. = FALSE)

    }
  }

  # write result in new line if progress was printed
  if (verbose) {

    message("\r")

  }

  return(randomization)
}
