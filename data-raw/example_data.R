library(dplyr)
library(NLMR)
library(usethis)
library(sf)
library(spatstat)
library(terra)

library(shar)

#### Create example data ####

set.seed(42)

# Create landscape
landscape <- NLMR::nlm_fbm(ncol = 50, nrow = 50, resolution = 20,
                           fract_dim = 1.5, user_seed = 42) %>%
  terra::rast()

landscape_class <- classify_habitats(landscape, n = 5, style = "fisher")

# Create species with negative
pattern_a <- spatstat.random::runifpoint(n = 250, win = spatstat.geom::owin(c(0, 1000), c(0, 1000)))

# get habitat 4 as owin
owin_pattern <- terra::as.polygons(landscape_class) %>%
  terra::subset(.$layer == 4) %>%
  sf::st_as_sf() %>%
  spatstat.geom::as.owin()

# create pattern with no point in habitat 4
species_a <- pattern_a[!spatstat.geom::inside.owin(x = pattern_a, w = owin_pattern)]

# create pattern with no point in habitat 4
species_a <- spatstat.random::rthin(pattern_a[spatstat.geom::inside.owin(x = pattern_a,
                                                                       w = owin_pattern)],
                                  P = 0.05) %>%
  spatstat.geom::superimpose.ppp(species_a, W = spatstat.geom::owin(c(0, 1000), c(0, 1000)))

marks_df_a <- data.frame(status = factor(sample(c("dead", "alive"),
                                                size = species_a$n, replace = TRUE)),
                        dbh = runif(n = species_a$n, min = 5, max = 65))

spatstat.geom::marks(species_a) <- marks_df_a

# Create species with positive associations
pattern_b <- spatstat.random::runifpoint(n = floor(species_a$n / 2),
                                         win = spatstat.geom::owin(c(0, 1000), c(0, 1000)))

# create pattern with more points in habitat 5
species_b <- terra::as.polygons(landscape_class) %>%
  terra::subset(.$layer == 5) %>%
  sf::st_as_sf() %>%
  spatstat.geom::as.owin() %>%
  spatstat.random::runifpoint(n = floor(pattern_b$n * 1), win = .) %>%
  spatstat.geom::superimpose.ppp(pattern_b, W = spatstat.geom::owin(c(0, 1000), c(0, 1000)))

marks_df_b <- factor(sample(c("dominant", "understorey"),
                            size = species_b$n, replace = TRUE))

spatstat.geom::marks(species_b) <- marks_df_b

# set number of repetitions
n_random <- 39

# translate raster
torus_trans <- translate_raster(raster = landscape_class)

# use randomization algorithm
random_walk <- randomize_raster(raster = landscape_class, n_random = n_random)

# use gamma test
gamma_test <- fit_point_process(pattern = species_b, n_random = n_random, process = "cluster")

# use pattern reconstruction
reconstruction <- reconstruct_pattern(pattern = species_b, n_random = n_random,
                                      e_threshold = 0.05)

#### Save data ####

overwrite <- TRUE

# save landscape
landscape <- terra::wrap(landscape)
usethis::use_data(landscape, overwrite = overwrite)

# save species data
usethis::use_data(species_a, overwrite = overwrite)

usethis::use_data(species_b, overwrite = overwrite)

# save random landscape data
torus_trans <- pack_randomized(raster = torus_trans)
usethis::use_data(torus_trans, overwrite = overwrite)

random_walk <- pack_randomized(raster = random_walk)
usethis::use_data(random_walk, overwrite = overwrite)

# save random point data
usethis::use_data(gamma_test, overwrite = overwrite)

usethis::use_data(reconstruction, overwrite = overwrite)
