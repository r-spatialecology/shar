#' randomization_algorithm
#'
#' @description Randomization algorithm
#'
#' @param raster RasterLayer
#' @param direction he number of directions in which cells should be connected:
#' 4 (rook's case), 8 (queen's case), 16 (knight and one-cell queen moves),
#' or 'bishop' to connect cells with one-cell diagonal moves. Or a neigborhood matrix (see Details)
#' @param n_random Number of randomized RasterLayers
#' @param verbose Print progress report
#'
#' @details
#' Randomize habitat map
#'
#' @seealso
#' \code{\link{translate_raster}} \cr
#' \code{\link{adjacent}}
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' landscape <- NLMR::nlm_fbm(ncol = 50, nrow = 50, user_seed = 1)
#' landscape_classified <- SHAR::classify_habitats(landscape, classes = 5)
#' landscape_random <- randomization_algorithm(landscape_classified, n_random = 9)
#' }
#'
#' @aliases randomization_algorithm
#' @rdname randomization_algorithm
#'
#' @references
#' Harms, K. E., Condit, R., Hubbell, S. P., & Foster, R. B. (2001). Habitat associations
#' of trees and shrubs in a 50-ha neotropical forest plot. Journal of Ecology, 89(6), 947–959.
#'
#' @export
randomization_algorithm <- function(raster, direction = 8, n_random = 19,
                                    verbose = FALSE){

  result <- lapply(1:n_random, function(current_raster) {

  n_cells_plot <- length(raster::Which(!is.na(raster), cells = TRUE)) # all cells within plot
  random_raster <- raster::setValues(raster, NA) # new raster without values
  random_raster <- raster::mask(random_raster, raster) # mask new random raster to plot area

  habitats <- sort(table(raster::values(raster)))

  for(current_habitat in 1:(length(habitats) - 1)){ # loop for habitats

    k <- 0 # counter since last jump
    true_counter <- vector()

    random_cell <- sample(raster::Which(is.na(random_raster), cells = TRUE), size = 1)

    random_raster[random_cell] <- as.numeric(names(habitats[current_habitat])) # assign habitat to cell

    while(TRUE){

      ratio <- k / n_cells_plot
      r <- runif(n = 1, min = 0, max = 1)

      if(r >= ratio){ # neighbouring patch

        cells_habitat <- raster::Which(random_raster == as.numeric(names(habitats[current_habitat])), cells = TRUE) # cells already assigned to habitat

        zero_neighbours <- raster::adjacent(x = random_raster,
                                            cells = cells_habitat,
                                            target = raster::Which(is.na(random_raster), cells = TRUE),
                                            direction = direction,
                                            pairs = FALSE,
                                            include = FALSE) # neighbours of cell already assigned to habitat

        if(length(zero_neighbours) > 0){ # neighbours without habitat and inside plot present

          random_cell <- sample(zero_neighbours, size = 1) # random number
          true_counter <- c(true_counter, random_cell %in% raster::Which(is.na(random_raster), cells = TRUE))

          random_raster[random_cell] <- as.numeric(names(habitats[current_habitat])) # assign habitat to cell
          k <- k +1 # count since laste time jumped
        }

        else{ # no neighbour with habitat and inside plot present

          random_cell <- sample(x = raster::Which(is.na(random_raster), cells = TRUE), size = 1) # random number
          true_counter <- c(true_counter, random_cell %in% raster::Which(is.na(random_raster), cells = TRUE))

          random_raster[random_cell] <- as.numeric(names(habitats[current_habitat])) # assign habitat
          k <- 0 # set counter since last jump zero
        }
      }

      else{ # jump to random patch

        random_cell <- sample(x = raster::Which(is.na(random_raster), cells = TRUE), size = 1) # random number
        true_counter <- c(true_counter, random_cell %in% raster::Which(is.na(random_raster), cells = TRUE))

        random_raster[random_cell] <- as.numeric(names(habitats[current_habitat])) # assign habitat
        k <- 0 # set counter since last jump zero
      }

      if(length(raster::Which(random_raster == as.numeric(names(habitats[current_habitat])), cells = TRUE)) == habitats[current_habitat]){
        break
      }

      if(verbose == TRUE) {
        cat(paste0("\rProgress: n_random: ", current_raster, "/", n_random,
                   "|| habitats:" , current_habitat, "/", length(habitats),
                   "|| cells: ", length(zero_neighbours))) # add habitat and number empty cells
      }
    }
  }

  empty_cells <- raster::Which(is.na(random_raster), cells = TRUE)
  random_raster[empty_cells] <- as.numeric(names(habitats[length(habitats)]))

  return(random_raster) # return results
  })

  result[[length(result) + 1]] <- raster
  names(result) <-  c(rep(paste0("randomized_", 1:(length(result)-1))), "observed")

  return(result)
}

