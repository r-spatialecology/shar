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
#' @param complex [\code{Logical(1)}]\cr If TRUE, a clusters based at a regular grid is created (complex pattern)
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

  # else{
  #   # Species 1 - No associations
  #   species_1 <- SHAR::Pattern.Complex(owin=owin_habitats)
  #   marks_1 <- data.frame(Species=rep("Species_1", species_1$n), Code=rep(1, species_1$n))
  #   spatstat::marks(species_1) <- marks_1
  #
  #   # Species 2 - Positive associations habitat 2
  #   species_2_a <- SHAR::Pattern.Complex(owin=owin_habitats)
  #
  #   species_2 <- raster %>%
  #     raster::rasterToPolygons(fun=function(x){x==2}, dissolve=T) %>%
  #     maptools::as.owin.SpatialPolygons() %>%
  #     spatstat::runifpoint(n=floor(species_2_a$n*m_pos), win=.) %>%
  #     spatstat::superimpose(species_2_a, W=owin_habitats)
  #
  #   marks_2 <- data.frame(Species=rep("Species_2", species_2$n), Code=rep(2, species_2$n))
  #   spatstat::marks(species_2) <- marks_2
  #
  #   # Species 3 - Negative associations habitat 3
  #   species_3_a <- SHAR::Pattern.Complex(owin=owin_habitats)
  #
  #   owin_habitat_3 <- raster %>%
  #     raster::rasterToPolygons(fun=function(x){x==3}, dissolve=3) %>%
  #     maptools::as.owin.SpatialPolygons()
  #
  #   species_3_b <- species_3_a[!spatstat::inside.owin(x=species_3_a, w=owin_habitat_3)]
  #   species_3_c <- spatstat::rthin(species_3_a[spatstat::inside.owin(x=species_3_a, w=owin_habitat_3)], m_neg)
  #
  #   species_3 <- spatstat::superimpose(species_3_b, species_3_c, W=owin_habitats)
  #   marks_3 <- data.frame(Species=rep("Species_3", species_3$n), Code=rep(3, species_3$n))
  #   spatstat::marks(species_3) <- marks_3
  #
  #   simulation_pattern <- spatstat::superimpose(species_1, species_2, species_3,
  #                                               W=owin_raster)
  # }
  return(simulation_pattern)
}
