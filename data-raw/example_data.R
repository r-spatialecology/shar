library(dplyr)
library(NLMR)
library(maptools)
library(usethis)
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

owin_pattern <- raster::rasterToPolygons(landscape_class,
                                         fun = function(x){x == 4},
                                         dissolve = TRUE) %>%
  maptools::as.owin.SpatialPolygons()

pattern_b <- pattern_a[!spatstat::inside.owin(x = pattern_a, w = owin_pattern)]
pattern_c <- spatstat::rthin(pattern_a[spatstat::inside.owin(x = pattern_a,
                                                             w = owin_pattern)], 0)

species_a <- spatstat::superimpose(pattern_b, pattern_c,
                                   W = spatstat::owin(c(0, 1000), c(0, 1000)))

marks_df_a <- data.frame(status = factor(sample(c("dead", "alive"),
                                                size = species_a$n, replace = TRUE)),
                        dbh = runif(n = species_a$n, min = 5, max = 65))

spatstat::marks(species_a) <- marks_df_a

# Create species with positive associations
pattern <- spatstat::runifpoint(n = 100, win = spatstat::owin(c(0, 1000), c(0, 1000)))

species_b <- raster::rasterToPolygons(landscape_class,
                                      fun = function(x){x == 5}, dissolve = TRUE) %>%
  maptools::as.owin.SpatialPolygons() %>%
  spatstat::runifpoint(n = floor(pattern$n * 1), win = .) %>%
  spatstat::superimpose.ppp(pattern, W = spatstat::owin(c(0, 1000), c(0, 1000)))

marks_df_b <- factor(sample(c("dominant", "understorey"),
                            size = species_b$n, replace = TRUE))

spatstat::marks(species_b) <- marks_df_b

# translate raster
torus_trans <- translate_raster(raster = landscape_class, verbose = FALSE)

# use randomization algorithm
random_walk <- randomize_raster(raster = landscape_class,
                                n_random = 39, verbose = FALSE)

# use gamma test
gamma_test <- fit_point_process(pattern = species_b, process = "cluster",
                                n_random = 39, verbose = FALSE)

# use pattern reconstruction
reconstruction <- reconstruct_pattern_cluster(pattern = species_b,
                                              n_random = 39, verbose = FALSE)

#### Save data ####

# save landscape
usethis::use_data(landscape, overwrite = TRUE)

# save species data
usethis::use_data(species_a, overwrite = TRUE)

usethis::use_data(species_b, overwrite = TRUE)

# save random landscape data
usethis::use_data(torus_trans, overwrite = TRUE)

usethis::use_data(random_walk, overwrite = TRUE)

# save random point data
usethis::use_data(gamma_test, overwrite = TRUE)

usethis::use_data(reconstruction, overwrite = TRUE)
