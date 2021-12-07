#' randomize_raster
#'
#' @description Randomized-habitats procedure
#'
#' @param raster RasterLayer with discrete habitat classes.
#' @param n_random Integer with number of randomizations.
#' @param directions Interger with cells neighbourhood rule: 4 (rook's case), 8 (queen's case).
#' @param return_input Logical if the original input data is returned.
#' @param simplify Logical if only the raster will be returned if \code{n_random = 1}
#' and \code{return_input = FALSE}.
#' @param verbose Logical if progress report is printed.
#'
#' @details
#' The function randomizes a habitat map with discrete classes (as RasterLayer) as proposed
#' by Harms et al. (2001) as “randomized-habitats procedure”. The algorithm starts with an
#' empty habitat map and starts to assign random neighbouring cells to each habitat
#' (in increasing order of abundance in observed map). We modified the procedure
#' slightly by increasing a probability to jump to a non-neighbouring cell as the
#' current patch becomes larger.
#'
#' In case the RasterLayer contains NA cells, this needs to be reflected in the observation
#' window of the point pattern as well (i.e., no point locations possible in these areas).
#'
#' @seealso
#' \code{\link{translate_raster}}
#'
#' @return rd_ras
#'
#' @examples
#' \dontrun{
#' landscape_classified <- classify_habitats(landscape, classes = 5)
#' landscape_random <- randomize_raster(landscape_classified, n_random = 19)
#' }
#'
#' @aliases randomize_raster
#' @rdname randomize_raster
#'
#' @references
#' Harms, K.E., Condit, R., Hubbell, S.P., Foster, R.B., 2001. Habitat associations of
#' trees and shrubs in a 50-ha neotropical forest plot. Journal of Ecology 89, 947–959.
#' <https://doi.org/10.1111/j.1365-2745.2001.00615.x>
#'
#'@export
randomize_raster <- function(raster,
                             n_random = 1,
                             directions = 4,
                             return_input = TRUE,
                             simplify = FALSE,
                             verbose = TRUE){

  # warning if NA are present
  if (anyNA(raster@data@values)) {

    warning("NA values present. Please make sure the observation window of the point pattern reflects this.", call. = FALSE)

  }

  # check if n_random is >= 1
  if (!n_random >= 1) {

    stop("n_random must be >= 1.", call. = FALSE)

  }

  # set names of randomization randomized_1 ... randomized_n
  names_randomization <- paste0("randomized_", seq_len(n_random))

  habitats <- sort(table(raster@data@values, useNA = "no")) # get table of habitats

  # print warning if more than 10 classes are present
  if (verbose) {

    if (length(habitats) > 10) {

      warning("The raster has more than 10 classes. Please make sure discrete classes are provided.",
              call. = FALSE)

    }
  }

  n_cells <- sum(habitats) # number of cells

  # create n_random rasters
  result_list <- lapply(seq_len(n_random), function(current_raster) {

    random_matrix <- raster::as.matrix(raster) # new raster without values

    random_matrix[!is.na(random_matrix)] <- -999 # set all non-NAs to unique number

    # loop through habitats but last one (all remaining cells will be last)
    for (current_habitat in seq_len(length(habitats) - 1)) {

      k <- 0 # counter since last jump

      habitat_id <- as.numeric(names(habitats[current_habitat])) # get value of current habitat

      random_cell <- sample(x = which(random_matrix == -999), size = 1) # random cell which is still -999

      random_matrix[random_cell] <- habitat_id # assign habitat to cell

      # loop until same number as number in original raster are assigned (break criterion within loop)
      for (i in seq_len(habitats[current_habitat] - 1)) {

        # increases as loop continious and increases prob to jump to non-neighbouring cell
        ratio <- k / n_cells

        r <- stats::runif(n = 1, min = 0, max = 1) # random number

        # assign value to neighbouring patch
        if (r >= ratio) {

          # cells already assigned to habitat
          cells_habitat <- which(random_matrix == habitat_id,
                                 arr.ind = TRUE, useNames = FALSE)

          # get neighbour cells
          neighbours <- create_neighbourhood(cells = cells_habitat, matrix = random_matrix,
                                             directions = directions)

          # all neighbouring cells that are -999
          empty_neighbours <- which(random_matrix[neighbours] == -999,
                                    arr.ind = TRUE, useNames = FALSE)

          # neighbours without habitat and inside plot present
          if (length(empty_neighbours) > 0) {

            # sample random neighbour
            random_neighbour <- sample(x = empty_neighbours, size = 1)

            # get matrix index of sampled neighbour
            random_neighbour <- matrix(neighbours[random_neighbour, ], ncol = 2)

            # assign cell to habitat
            random_matrix[random_neighbour] <- habitat_id

            k <- k + 1 # count since laste time jumped

          # no neighbour with habitat and inside plot present
          } else{

            # random cell which is still -999
            random_cell <- sample(x = which(random_matrix == -999), size = 1)

            # assign habitat to cell
            random_matrix[random_cell] <- habitat_id

            k <- 0 # set counter since last jump zero

          }

        # jump to random starting cell
        } else {

          # random cell which is still -999
          random_cell <- sample(x = which(random_matrix == -999), size = 1)

          # assign habitat to cell
          random_matrix[random_cell] <- habitat_id

          k <- 0 # set counter since last jump zero

        }

        # print progess
        if (verbose) {

          message("\r> Progress: n_random: ", current_raster, "/", n_random,
                  " || habitats: " , current_habitat, "/", length(habitats), "\t\t",
                  appendLF = FALSE) # add habitat and number empty cells

        }
      }
    }

    # cells not assigned to any habitat yet
    empty_cells <- which(random_matrix == -999, arr.ind = TRUE, useNames = FALSE)

    # assign all still empty cells to last habitat
    random_matrix[empty_cells] <- as.numeric(names(habitats[length(habitats)]))

    # convert back to raster
    random_raster <- raster::setValues(x = raster, values = random_matrix)

    return(random_raster)

  })

  names(result_list) <- names_randomization

  # combine to one list
  randomization <- list(randomized = result_list, observed = raster,
                        method = "randomize_raster()")

  # set class of result
  class(randomization) <- "rd_ras"

  # remove input if return_input = FALSE
  if (!return_input) {

    # set observed to NA
    randomization$observed <- "NA"

    # check if output should be simplified
    if (simplify) {

      # not possible if more than one raster is present
      if (n_random > 1 && verbose) {

        warning("'simplify = TRUE' not possible for 'n_random > 1'.",
                call. = FALSE)

      # only one random raster is present that should be returend
      } else if (n_random == 1) {

        randomization <- randomization$randomized[[1]]

      }
    }

  # return input if return_input = TRUE
  } else {

    # return warning if simply = TRUE because not possible if return_input = TRUE (only verbose = TRUE)
    if (simplify && verbose) {

      warning("'simplify = TRUE' not possible for 'return_input = TRUE'.", call. = FALSE)

    }
  }

  # write result in new line if progress was printed
  if (verbose) {

    message("\r")

  }

  return(randomization)
}
