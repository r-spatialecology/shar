library(dplyr)
library(NLMR)
library(maptools)
library(spatstat)

#### Create example data ####

set.seed(42)

# Create landscape
landscape <- NLMR::nlm_fbm(ncol = 50,
                           nrow = 50,
                           resolution = 20,
                           fract_dim = 1,
                           user_seed = 42)

landscape_class <- shar::classify_habitats(landscape, classes = 5)

# Create species with negative
pattern_a <- spatstat::runifpoint(n = 100, win = spatstat::owin(c(0, 1000), c(0, 1000)))

owin_pattern <- raster::rasterToPolygons(landscape_class, fun = function(x){x == 4}, dissolve = TRUE) %>%
  maptools::as.owin.SpatialPolygons()

pattern_b <- pattern_a[!spatstat::inside.owin(x = pattern_a, w = owin_pattern)]
pattern_c <- spatstat::rthin(pattern_a[spatstat::inside.owin(x = pattern_a, w = owin_pattern)], 0)

species_a <- spatstat::superimpose(pattern_b, pattern_c,
                                   W = spatstat::owin(c(0, 1000), c(0, 1000)))

marks_df_a <- data.frame(status = factor(sample(c("dead", "alive"), size = species_a$n, replace = TRUE)),
                        dbh = runif(n = species_a$n, min = 5, max = 65))

spatstat::marks(species_a) <- marks_df_a

# Create species with positive associations
pattern <- spatstat::runifpoint(n = 100, win = spatstat::owin(c(0, 1000), c(0, 1000)))

species_b <- raster::rasterToPolygons(landscape_class, fun = function(x){x == 5}, dissolve = TRUE) %>%
  maptools::as.owin.SpatialPolygons() %>%
  spatstat::runifpoint(n = floor(pattern$n * 1), win=.) %>%
  spatstat::superimpose.ppp(pattern, W = spatstat::owin(c(0, 1000), c(0, 1000)))

marks_df_b <- factor(sample(c("dominant", "understorey"), size = species_b$n, replace = TRUE))

spatstat::marks(species_b) <- marks_df_b

devtools::use_data(landscape, overwrite = TRUE)
devtools::use_data(species_a, overwrite = TRUE)
devtools::use_data(species_b, overwrite = TRUE)
