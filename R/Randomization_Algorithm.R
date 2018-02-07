#' Internal help function
#'
#' Randomization algorithm assigning habitat to random neighbours
#' @param raster [\code{raster(1)}] Raster of the raster object with discrete habitats
#' @param number_neighbours [\code{numeric(1)}] Number of random neigbhours. See ?raster::adjacent() for more details
#'
#' @return Raster of the raster package with randomized habitats

#' @export
Randomization.Algorithm <- function(raster, number_neighbours=8){
  if(raster::nlayers(raster)==2){raster <- raster[[2]]} # original raster habitats

  n.cells.plot <- length(raster::Which(!is.na(raster), cells=T)) # all cells within plot
  random.neighbour <- raster::setValues(raster, 0) # new raster without values
  random.neighbour <- raster::mask(random.neighbour, raster) # mask new random raster to plot area

  habitats <- sort(table(raster::getValues(raster))) # table with increasing number of habitat patches

  for(i in 1:length(habitats)){ # loop for habitats
    k <- 0 # counter since last jump

    cell <- raster::Which(random.neighbour==0, cells=T) %>%
      length() %>%
      runif(n=1, min=1, max=.) %>%
      round(0)

    #cell <- raster::Which(random.neighbour==0, cells=T)[r] # random cell without habitat
    random.neighbour[cell] <- as.numeric(names(habitats)[i]) # assign habitat to cell

    for(j in 2:habitats[[i]]){ # loop for number of patches habitat
      ratio <- k/n.cells.plot
      r <- runif(n=1, min=0, max=1)

      if(r>ratio){ # neighbouring patch
        cells.habitat<-raster::Which(random.neighbour==as.numeric(names(habitats)[i]), cells=T) # cells already assigned to habitat
        neighbours <- raster::adjacent(x=random.neighbour, cells=cells.habitat, pairs=F, direction=number_neighbours) # neighbours of cell already assigned to habitat
        zero.neighbours <- neighbours[random.neighbour[neighbours]==0 & !is.na(random.neighbour[neighbours])] # neighbours without habitat and inside plot

        if(length(zero.neighbours)>0){ # neighbours without habitat and inside plot present
          r <- round(runif(n=1, min=1, max=length(zero.neighbours)),0) # random number
          cell <- zero.neighbours[r] # random neighbour
          random.neighbour[cell] <- as.numeric(names(habitats)[i]) # assign habitat to cell
          k <- k +1 # count since laste time jumped
        }

        else{ # no neighbour with habitat and inside plot present
          r <- round(runif(n=1, min=1, max=length(raster::Which(random.neighbour==0, cells=T))),0) # random number
          cell <- raster::Which(random.neighbour==0, cells=T)[r] # random cell without habitat and inside plot
          random.neighbour[cell] <- as.numeric(names(habitats)[i]) # assign habitat
          k <- 0 # set counter since last jump zero
        }
      }

      else{ # jump to random patch
        r <- round(runif(n=1, min=1, max=length(raster::Which(random.neighbour==0, cells=T))),0) # random number
        cell <-raster::Which(random.neighbour==0, cells=T)[r] # random cell without habitat and inside plot
        random.neighbour[cell] <- as.numeric(names(habitats)[i]) # assign habitat
        k <- 0 # set counter since last jump zero
      }
    }
  }
  return(random.neighbour) # return results
}
