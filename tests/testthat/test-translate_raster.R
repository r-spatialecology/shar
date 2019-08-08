testthat::context("test-translate_raster")

# n_random <- (raster::nrow(landscape) + 1) * (raster::ncol(landscape) + 1)  - 4

# create landscape
landscape_classified <- shar::classify_habitats(raster = shar::landscape,
                                                classes = 5)

# normal translation
landscape_random <- shar::translate_raster(raster = landscape_classified,
                                           verbose = FALSE)

# torus translation with provided steps
landscape_random_steps <- shar::translate_raster(raster = landscape_classified,
                                                 steps_x = 1:3, steps_y = 1:3,
                                                 verbose = FALSE,
                                                 return_input = FALSE)

# simplified raster
landscape_random_simple <- shar::translate_raster(raster = landscape_classified,
                                                  steps_x = 1, steps_y = 5,
                                                  simplify = TRUE,
                                                  verbose = FALSE,
                                                  return_input = FALSE)

# create landscape wrong extent
landscape_wrong <- raster::crop(shar::landscape, raster::extent(0, 1000, 0, 500))

# classify landscape wrong extent
landscape_classified_wrong <- shar::classify_habitats(landscape_wrong, classes = 5)


################################################################################

testthat::test_that("Output is a long as n_random for translate_raster", {

  testthat::expect_length(landscape_random$randomized,
                          n = 2597)
})

testthat::test_that("Output includes randomizations and original pattern for translate_raster", {

  testthat::expect_named(landscape_random$randomized,
                         expected = paste0("randomized_", 1:2597))

  testthat::expect_equal(landscape_random$observed,
                         expected = landscape_classified)
})

testthat::test_that("Input raster can not be returned for translate_raster", {

  check <- vapply(X = landscape_random_steps$randomized, FUN = function(x) {

    landscape_diff <- landscape_classified - x

    all(raster::values(landscape_diff) == 0)
  }, FUN.VALUE = logical(1))

  testthat::expect_false(all(check))
})

testthat::test_that("Providing steps is working for translate_raster", {

  testthat::expect_length(landscape_random_steps$randomized,
                          n = 9)
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

testthat::test_that("Warning if more than 10 classes are present for translate_raster", {

  testthat::expect_warning(shar::translate_raster(raster = landscape,
                                                  steps_x = 5, steps_y = 5),
                           regexp  = "The raster has more than 10 classes. Please make sure discrete classes are provided.",
                           fixed = TRUE)
})
