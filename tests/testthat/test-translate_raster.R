testthat::context("Translate raster")

# n_random <- (raster::nrow(landscape) + 1) * (raster::ncol(landscape) + 1)  - 4

# create landscape
landscape_classified <- shar::classify_habitats(raster = shar::landscape,
                                                classes = 3)

# normal translation
landscape_random <- shar::translate_raster(raster = landscape_classified,
                                           verbose = FALSE)

# no input
landscape_random_ni <- shar::translate_raster(raster = landscape_classified,
                                              return_input = FALSE,
                                              verbose = FALSE)

# create landscape wrong extent
landscape_wrong <- raster::crop(shar::landscape, raster::extent(0, 1000, 0, 500))

# classify landscape wrong extent
landscape_classified_wrong <- shar::classify_habitats(landscape_wrong, classes = 3)

# torus translation with provided steps
landscape_random_steps <- shar::translate_raster(raster = landscape_classified,
                                                 steps_x = 1:5, steps_y = 1:5,
                                                 verbose = FALSE,
                                                 return_input = FALSE)

# simplified raster
landscape_random_simple <- shar::translate_raster(raster = landscape_classified,
                                                  steps_x = 1, steps_y = 5,
                                                  simplify = TRUE,
                                                  verbose = FALSE,
                                                  return_input = FALSE)

################################################################################

testthat::test_that("Output is a long as n_random for translate_raster", {

  testthat::expect_length(landscape_random,
                          n = 2598)
})

testthat::test_that("Output includes randomizations and original pattern for translate_raster", {

  testthat::expect_named(landscape_random,
                         expected = c(paste0("randomized_", 1:2597), "observed"))

  testthat::expect_equal(landscape_random[[2598]],
                         expected = landscape_classified)
})

testthat::test_that("Input raster can not be returned for translate_raster", {

  random_stack <- raster::stack(lapply(landscape_random_ni, function(x) x))

  comparison <- as.matrix(raster::values(abs(random_stack - landscape_classified)))

  check <- any(apply(comparison, 2, function(x) all(x == 0)))

  testthat::expect_false(check)
})

testthat::test_that("Providing steps is working for translate_raster", {

  testthat::expect_length(landscape_random_steps,
                          n = 25)
})

testthat::test_that("simplify is working for translate_raster", {

  testthat::expect_is(landscape_random_simple,
                      class = "RasterLayer")
})

testthat::test_that("Error if nrow != ncol for translate_raster", {

  testthat::expect_error(shar::translate_raster(raster = landscape_classified_wrong,
                                                verbose = FALSE),
                         regexp  = "Torus translation only works for raster with nrow == ncol.",
                         fixed = TRUE)
})
