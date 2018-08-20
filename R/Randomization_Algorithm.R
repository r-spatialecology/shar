#' Internal help function
#'
#' Randomization algorithm assigning habitat to random neighbours
#' @param raster [\code{raster(1)}] Raster of the raster object with discrete habitats
#' @param number_neighbours [\code{numeric(1)}] Number of random neigbhours. See ?raster::adjacent() for more details
#'
#' @return Raster of the raster package with randomized habitats

#' @export
randomization_algorithm <- function(raster, number_neighbours = 8){

  # if(raster::nlayers(raster)==2){raster <- raster[[2]]} # original raster habitats

  n_cells_plot <- length(raster::Which(!is.na(raster), cells = TRUE)) # all cells within plot
  random_raster <- raster::setValues(raster, NA) # new raster without values
  random_raster <- raster::mask(random_raster, raster) # mask new random raster to plot area

  habitats <- sort(table(raster::values(raster)))

  for(current_habitat in 1:(length(habitats)-1)){ # loop for habitats

    k <- 0 # counter since last jump
    true_counter <- vector()

    random_cell <- raster::Which(is.na(random_raster), cells = TRUE) %>%
      sample(size = 1)

    random_raster[random_cell] <- as.numeric(names(habitats[current_habitat])) # assign habitat to cell

    while(TRUE){

      ratio <- k / n_cells_plot
      r <- runif(n = 1, min = 0, max = 1)

      if(r >= ratio){ # neighbouring patch

        cells_habitat <- raster::Which(random_raster == as.numeric(names(habitats[current_habitat])), cells = TRUE) # cells already assigned to habitat

        zero_neighbours <- raster::adjacent(x = random_raster,
                                            cells = cells_habitat,
                                            target = raster::Which(is.na(random_raster), cells = TRUE),
                                            direction = number_neighbours,
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
    }
  }

  empty_cells <- raster::Which(is.na(random_raster), cells = TRUE)
  random_raster[empty_cells] <- as.numeric(names(habitats[length(habitats)]))

  return(random_raster) # return results
}

