testthat::context("test-randomize_raster")

landscape_classified <- shar::classify_habitats(raster = shar::landscape,
                                                classes = 5)

# normal random
landscape_random <- shar::randomize_raster(raster = landscape_classified,
                                           n_random = 1,
                                           verbose = FALSE)

# simplify output
raster_random_simple <- shar::randomize_raster(raster = landscape_classified,
                                               n_random = 1,
                                               simplify = TRUE,
                                               return_input = FALSE,
                                               verbose = FALSE)

################################################################################

testthat::test_that("Output is as long as n_random for randomize_raster", {

  testthat::expect_is(landscape_random, class = "rd_ras")

  testthat::expect_length(landscape_random$randomized, n = 1)
})

testthat::test_that("Output includes randomizations and original pattern for randomize_raster", {

  testthat::expect_named(landscape_random$randomized,
                         expected = "randomized_1")

  testthat::expect_equal(landscape_random$observed,
                         expected = landscape_classified)
})

testthat::test_that("Input raster can not be returned for randomize_raster", {

  landscape_diff <- landscape_classified - raster_random_simple

  check <- all(raster::values(landscape_diff) == 0)

  testthat::expect_false(check)
})

testthat::test_that("simplify works for randomize_raster", {

  testthat::expect_is(raster_random_simple, "RasterLayer")
})

testthat::test_that("randomize_raster returns error of n_random < 1", {

  testthat::expect_error(shar::randomize_raster(raster = landscape_classified,
                                                n_random = 0,
                                                verbose = FALSE),
                         regexp = "n_random must be >= 1.",
                         fixed = TRUE)
})

testthat::test_that("randomize_raster returns all warnings", {

  testthat::expect_warning(shar::randomize_raster(raster = landscape_classified,
                                                  n_random = 1,
                                                  simplify = TRUE),
                           regexp = "'simplify = TRUE' not possible for 'return_input = TRUE'.",
                           fixed = TRUE)

  testthat::expect_warning(shar::randomize_raster(raster = landscape_classified,
                                                  n_random = 2,
                                                  simplify = TRUE,
                                                  return_input = FALSE),
                           regexp = "'simplify = TRUE' not possible for 'n_random > 1'.",
                           fixed = TRUE)

  testthat::expect_warning(shar::randomize_raster(raster = landscape,
                                                  n_random = 1),
                           regexp = "The raster has more than 10 classes. Please make sure discrete classes are provided.",
                           fixed = TRUE)
})
