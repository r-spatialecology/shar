#' Create simulation pattern
#'
#' Algorithm to create simulation pattern with six species with different species-habitat associations.
#' \cr Species 1: Positive associations (Poisson process)
#' \cr Species 2: Positive associations (Thomas process)
#' \cr Species 3: Negative associations (Poisson process)
#' \cr Species 4: Negative associations (Thomas process)
#' @param raster [\code{raster(1)}]\cr Raster object of the raster package with habitats
#' @param number_points [\code{numeric(1)}]\cr Number of points for each species (alpha=0)
#' @param alpha [\code{numeric(1)}]\cr Strength of species-habitat association
#' @return ppp object of the spatstat package with simulated species

#' @export
Create.Simulation.Pattern <- function(raster, number_points=100, alpha=0.3){

  owin_raster <- raster %>%
    raster::rasterToPolygons(fun=function(x) !is.na(x), na.rm=T, dissolve=T) %>%
    maptools::unionSpatialPolygons(ID=rep(1, times=length(.))) %>%
    maptools::as.owin.SpatialPolygons()

  # Species 1: Positive associations (Poisson)
  habitat_1 <- sample(x=seq(min(raster::values(raster)):max(raster::values(raster))), size=1)
  species_1 <- SHAR::Create.Simulation.Species(raster=raster, type="positive", process="Poisson",
                                               habitat=habitat_1, number_points=number_points, alpha=alpha,
                                               species_code=1, verbose=F)

  # Species 2: Positive associations (Thomas process)
  habitat_2 <- sample(x=seq(min(raster::values(raster)):max(raster::values(raster))), size=1)
  species_2 <- SHAR::Create.Simulation.Species(raster=raster, type="positive", process="Thomas",
                                               habitat=habitat_2, number_points=number_points, alpha=alpha,
                                               species_code=2, verbose=F)

  # Species 3: Negative associations (Poisson)
  habitat_3 <- sample(x=seq(min(raster::values(raster)):max(raster::values(raster))), size=1)
  species_3 <- SHAR::Create.Simulation.Species(raster=raster, type="negative", process="Poisson",
                                                 habitat=habitat_3, number_points=number_points, alpha=alpha,
                                                 species_code=3, verbose=F)

  # Species 4: Negative associations habitat 4 (Thomas process)
  habitat_4 <- sample(x=seq(min(raster::values(raster)):max(raster::values(raster))), size=1)
  species_4 <- SHAR::Create.Simulation.Species(raster=raster, type="negative", process="Thomas",
                                               habitat=habitat_4, number_points=number_points, alpha=alpha,
                                               species_code=4, verbose=F)

  simulation_pattern <- spatstat::superimpose(species_1, species_2,
                                              species_3, species_4,
                                              W=owin_raster)

  return(simulation_pattern)
}
