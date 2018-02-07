#' Create simulation pattern
#'
#' Algorithm to create simulation pattern with six species with different species-habitat associations.
#' Species 1: No associations (Poisson process)
#' Species 2: Positive associations habitat 2 (Poisson process)
#' Species 3: Positive associations habitat 3 (Thomas process)
#' Species 4: Negative associations habitat 3 (Poisson process)
#' Species 5: Negative associations habitat 5 (Thomas process)
#' Species 6: No associations (Thomas process)
#' @param raster [\code{raster(1)}]\cr Raster object of the raster package with habitats
#' @param number_points [\code{numeric(1)}]\cr Number of points for each species (alpha=0)
#' @param alpha [\code{numeric(1)}]\cr Strength of species-habitat association
#' @param complex [\code{Logical(1)}]\cr If TRUE, a clusters based at a regular grid is created (complex pattern)
#' @return ppp object of the spatstat package with simulated species

#' @export
Create.Simulation.Pattern <- function(raster, number_points=100, alpha=0.3, complex=F){

  # inside<- raster::rasterToPolygons(raster, fun=function(x){!is.na(x)}, na.rm=T, dissolve=T)
  # inside <- maptools::unionSpatialPolygons(inside, IDs=rep(1, times=length(inside)))
  # owin_habitats <- maptools::as.owin.SpatialPolygons(inside)

  owin_habitats <- raster %>%
    raster::rasterToPolygons(fun=function(x) !is.na(x), na.rm=T, dissolve=T) %>%
    maptools::unionSpatialPolygons(ID=rep(1, times=length(.))) %>%
    maptools::as.owin.SpatialPolygons()

  m_pos <- alpha
  m_neg <- 1-alpha

  lambda <- number_points / spatstat::area(owin_habitats)
  scale <- mean(diff(owin_habitats$yrange), diff(owin_habitats$xrange)) / 50

  if(complex==F){
    # Species 1: No associations (Poisson)
    species_1 <- spatstat::rpoispp(lambda=lambda, nsim=1, drop=T, win=owin_habitats)
    marks_1 <- data.frame(Species=rep("Species_1", species_1$n), Code=rep(1, species_1$n))
    spatstat::marks(species_1) <- marks_1

    # Species 2: Positive associations habitat 2 (Poisson process)
    species_2a <- spatstat::rpoispp(lambda=lambda, nsim=1, drop=T, win=owin_habitats)

    species_2 <- raster %>%
      raster::rasterToPolygons(fun=function(x){x==2}, dissolve=T) %>%
      maptools::as.owin.SpatialPolygons() %>%
      spatstat::runifpoint(n=floor(species_2a$n*m_pos), win=.) %>%
      spatstat::superimpose.ppp(species_2a, W=owin_habitats)

    marks_2 <- data.frame(Species=rep("Species_2", species_2$n), Code=rep(2, species_2$n))
    spatstat::marks(species_2) <- marks_2

    # Species 3: Positive associations habitat 3 (Thomas process)
    species_3a <- spatstat::rThomas(kappa=lambda/5, scale=scale, mu=5, win=owin_habitats)

    species_3 <- raster %>%
      raster::rasterToPolygons(fun=function(x){x==3}, dissolve=T) %>%
      maptools::as.owin.SpatialPolygons() %>%
      spatstat::runifpoint(n=floor(species_3a$n*m_pos), win=.) %>%
      spatstat::superimpose(species_3a, W=owin_habitats)

    marks_3 <- data.frame(Species=rep("Species_3", species_3$n), Code=rep(3, species_3$n))
    spatstat::marks(species_3) <- marks_3

    # Species 4: Negative associations habitat 4 (Poisson process)
    species_4a <- spatstat::rpoispp(lambda=lambda, nsim=1, drop=T, win=owin_habitats)

    owin_habitat_4 <- raster %>%
      raster::rasterToPolygons(fun=function(x){x==4}, dissolve=T) %>%
      maptools::as.owin.SpatialPolygons()

    species_4b <- species_4a[!spatstat::inside.owin(x=species_4a, w=owin_habitat_4)]
    species_4c <- spatstat::rthin(species_4a[spatstat::inside.owin(x=species_4a, w=owin_habitat_4)], m_neg)

    species_4 <- spatstat::superimpose(species_4b, species_4c, W=owin_habitats)
    marks_4 <- data.frame(Species=rep("Species_4", species_4$n), Code=rep(4, species_4$n))
    spatstat::marks(species_4) <- marks_4

    # Species 5: Negative associations habitat 5 (Thomas process)
    species_5a <- spatstat::rThomas(kappa=lambda/5, scale=scale, mu=5, win=owin_habitats)

    owin_habitat_5 <- raster %>%
      raster::rasterToPolygons(fun=function(x){x==5}, dissolve=T) %>%
      maptools::as.owin.SpatialPolygons()

    species_5b <- species_5a[!spatstat::inside.owin(x=species_5a, w=owin_habitat_5)]
    species_5c <- spatstat::rthin(species_5a[spatstat::inside.owin(x=species_5a, w=owin_habitat_5)], m_neg)

    species_5 <- spatstat::superimpose(species_5b, species_5c, W=owin_habitats)
    marks_5 <- data.frame(Species=rep("Species_5", species_5$n), Code=rep(5, species_5$n))
    spatstat::marks(species_5) <- marks_5

    # Species 6: No associations (Thomas process)
    species_6 <- spatstat::rThomas(kappa=lambda/5, scale=scale, mu=5, win=owin_habitats)
    marks_6 <- data.frame(Species=rep("Species_6", species_6$n), Code=rep(6, species_6$n))
    spatstat::marks(species_6) <- marks_6

    simulation_pattern <- spatstat::superimpose(species_1, species_2, species_3,
                                                species_4, species_5, species_6, W=owin_habitats)
  }

  else{
    # Species 1 - No associations
    species_1 <- SHAR::Pattern.Complex(owin=owin_habitats)
    marks_1 <- data.frame(Species=rep("Species_1", species_1$n), Code=rep(1, species_1$n))
    spatstat::marks(species_1) <- marks_1

    # Species 2 - Positive associations habitat 2
    species_2_a <- SHAR::Pattern.Complex(owin=owin_habitats)

    species_2 <- raster %>%
      raster::rasterToPolygons(fun=function(x){x==2}, dissolve=T) %>%
      maptools::as.owin.SpatialPolygons() %>%
      spatstat::runifpoint(n=floor(species_2_a$n*m_pos), win=.) %>%
      spatstat::superimpose(species_2_a, W=owin_habitats)

    marks_2 <- data.frame(Species=rep("Species_2", species_2$n), Code=rep(2, species_2$n))
    spatstat::marks(species_2) <- marks_2

    # Species 3 - Negative associations habitat 3
    species_3_a <- SHAR::Pattern.Complex(owin=owin_habitats)

    owin_habitat_3 <- raster %>%
      raster::rasterToPolygons(fun=function(x){x==3}, dissolve=3) %>%
      maptools::as.owin.SpatialPolygons()

    species_3_b <- species_3_a[!spatstat::inside.owin(x=species_3_a, w=owin_habitat_3)]
    species_3_c <- spatstat::rthin(species_3_a[spatstat::inside.owin(x=species_3_a, w=owin_habitat_3)], m_neg)

    species_3 <- spatstat::superimpose(species_3_b, species_3_c, W=owin_habitats)
    marks_3 <- data.frame(Species=rep("Species_3", species_3$n), Code=rep(3, species_3$n))
    spatstat::marks(species_3) <- marks_3

    simulation_pattern <- spatstat::superimpose(species_1, species_2, species_3,
                                                W=owin_habitats)
  }
  return(simulation_pattern)
}
