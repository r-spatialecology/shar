#' Create simulation species
#'
#' Algorithm to create simulation species with choosen characteristics
#' @param raster [\code{raster(1)}]\cr Raster object of the raster package with habitats
#' @param type [\code{string(1)}]\cr 'positive' or 'negative' associations
#' @param process [\code{string(1)}]\cr Process type to chose. Either 'Poisson' or 'Thomas'
#' @param number_points [\code{numeric(1)}]\cr Number of points for each species (alpha=0)
#' @param alpha [\code{numeric(1)}]\cr Strength of species-habitat association
#' @param species_code [\code{numeric{1}}]\cr Species code to number species
#' @param verbose [\code{logic(1)}]\cr Print advanced error message
#'
#' @return ppp object of the spatstat package with simulated species

#' @export
Create.Simulation.Species <- function(raster, type, process, habitat, number_points=100, alpha=0.3,
                                      species_code=0, verbose=T, neutral=F){

  owin_overall <- raster %>%
    raster::rasterToPolygons(fun=function(x) !is.na(x), na.rm=T, dissolve=T) %>%
    maptools::unionSpatialPolygons(ID=rep(1, times=length(.))) %>%
    maptools::as.owin.SpatialPolygons()

  lambda <- number_points / spatstat::area(owin_overall)
  scale <- mean(diff(owin_overall$yrange), diff(owin_overall$xrange)) / 50

  if(type=="positive"){
    m_pos <- alpha

    if(process=="Poisson"){
      pattern_a <- spatstat::rpoispp(lambda=lambda, nsim=1, drop=T, win=owin_overall)

      pattern <- raster %>%
        raster::rasterToPolygons(fun=function(x){x==habitat}, dissolve=T) %>%
        maptools::as.owin.SpatialPolygons() %>%
        spatstat::runifpoint(n=floor(pattern_a$n*m_pos), win=.) %>%
        spatstat::superimpose.ppp(pattern_a, W=owin_overall)

      marks_pattern <- data.frame(Species=rep(paste0("Poisson_positive_", habitat), pattern$n),
                                    Species_code=species_code,
                                    Habitat=rep(habitat, pattern$n))
      spatstat::marks(pattern) <- marks_pattern
    }

    else if(process=="Thomas"){
      pattern_a <- spatstat::rThomas(kappa=lambda/5, scale=scale, mu=5, win=owin_overall)

      pattern <- raster %>%
        raster::rasterToPolygons(fun=function(x){x==habitat}, dissolve=T) %>%
        maptools::as.owin.SpatialPolygons() %>%
        spatstat::runifpoint(n=floor(pattern_a$n*m_pos), win=.) %>%
        spatstat::superimpose(pattern_a, W=owin_overall)

      marks_pattern <- data.frame(Species=rep(paste0("Thomas_positive_", habitat), pattern$n),
                                    Species_code=species_code,
                                    Habitat=rep(habitat, pattern$n))
      spatstat::marks(pattern) <- marks_pattern
    }

    else{
      if(verbose==T){print("Please select either 'Poisson' or 'Thomas' as process")}
      pattern <- NULL
    }
  }

  else if (type=="negative"){
    m_neg <- 1-alpha

    if(process=="Poisson"){
      pattern_a <- spatstat::rpoispp(lambda=lambda, nsim=1, drop=T, win=owin_overall)

      owin_pattern <- raster %>%
        raster::rasterToPolygons(fun=function(x){x==habitat}, dissolve=T) %>%
        maptools::as.owin.SpatialPolygons()

      pattern_b <- pattern_a[!spatstat::inside.owin(x=pattern_a, w=owin_pattern)]
      pattern_c <- spatstat::rthin(pattern_a[spatstat::inside.owin(x=pattern_a, w=owin_pattern)], m_neg)

      pattern <- spatstat::superimpose(pattern_b, pattern_c, W=owin_overall)

      marks_pattern <- data.frame(Species=rep(paste0("Poisson_negative_", habitat), pattern$n),
                                    Species_code=species_code,
                                    Habitat=rep(habitat, pattern$n))
      spatstat::marks(pattern) <- marks_pattern

    }

    else if(process=="Thomas"){
      pattern_a <- spatstat::rThomas(kappa=lambda/5, scale=scale, mu=5, win=owin_overall)

      owin_pattern <- raster %>%
        raster::rasterToPolygons(fun=function(x){x==habitat}, dissolve=T) %>%
        maptools::as.owin.SpatialPolygons()

      pattern_b <- pattern_a[!spatstat::inside.owin(x=pattern_a, w=owin_pattern)]
      pattern_c <- spatstat::rthin(pattern_a[spatstat::inside.owin(x=pattern_a, w=owin_pattern)], m_neg)

      pattern <- spatstat::superimpose(pattern_b, pattern_c, W=owin_overall)

      marks_pattern <- data.frame(Species=rep(paste0("Thomas_negative_", habitat), pattern$n),
                                    Species_code=species_code,
                                    Habitat=rep(habitat, pattern$n))
      spatstat::marks(pattern) <- marks_pattern
    }

    else{
      if(verbose==T){print("Please select either 'Poisson' or 'Thomas' as process")}
      pattern <- NULL
    }
  }

  else if(type=="neutral"){
    if(process=="Poisson"){
      pattern <- spatstat::runifpoint(n=number_points, win=owin_overall)
      marks_pattern <- data.frame(Species=rep("Poisson_neutral", pattern$n),
                                  Species_code=species_code)
      spatstat::marks(pattern) <- marks_pattern
    }

    else if(process=="Thomas"){
      pattern <- spatstat::rThomas(kappa=lambda/5, scale=scale, mu=5, win=owin_overall)
      marks_pattern <- data.frame(Species=rep("Thomas_neutral", pattern$n),
                                  Species_code=species_code)
      spatstat::marks(pattern) <- marks_pattern
    }

    else{
      if(verbose==T){print("Please select either 'Poisson' or 'Thomas' as process")}
      pattern <- NULL
    }
  }

  else{
    if(verbose==T){print("Please select either 'positive', 'negative' or 'neutral' as type")}
    pattern <- NULL
  }
  return(pattern)
}
