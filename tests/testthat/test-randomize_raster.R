testthat::context("randomize_raster")

landscape_classified <- SHAR::classify_habitats(raster = SHAR::landscape,
                                                classes = 3)

testthat::test_that("Output is as long as n_random for randomize_raster", {

  landscape_random <- SHAR::randomize_raster(raster = landscape_classified,
                                             n_random = 3)

  testthat::expect_type(landscape_random, type = "list")
  testthat::expect_length(landscape_random, n = 4)
})

testthat::test_that("Output includes randomizations and original pattern for randomize_raster", {


  landscape_random <- SHAR::randomize_raster(raster = landscape_classified,
                                             n_random = 3)

  testthat::expect_named(landscape_random,
                         expected = c(paste0("randomized_", 1:3), "observed"))

  testthat::expect_equal(landscape_random[[4]],
                         expected = landscape_classified)
})

testthat::test_that("Input raster can not be returned for randomize_raster", {

  landscape_random <- SHAR::randomize_raster(raster = landscape_classified,
                                             n_random = 3,
                                             return_input = FALSE)

  testthat::expect_false(any(list(landscape_classified) %in% landscape_random))
})

testthat::test_that("All optional arguments can be used for randomize_raster", {

  landscape_random <- SHAR::randomize_raster(raster = landscape_classified,
                                             n_random = 3,
                                             verbose = TRUE)

  testthat::expect_type(landscape_random, type = "list")
  testthat::expect_length(landscape_random, n = 4)
})

testthat::test_that("simplify wokrs for randomize_raster", {

  raster_random <- SHAR::randomize_raster(raster = landscape_classified,
                                          n_random = 1,
                                          simplify = TRUE,
                                          return_input = FALSE)

  testthat::expect_is(raster_random, "RasterLayer")
})

testthat::test_that("randomize_raster returns error of n_random < 1", {

  testthat::expect_error(SHAR::randomize_raster(raster = landscape_classified,
                                                n_random = 0),
                         regexp = "n_random must be >= 1.")
})

testthat::test_that("randomize_raster returns all warnings", {

  testthat::expect_warning(SHAR::randomize_raster(raster = landscape_classified,
                                                  n_random = 3,
                                                  simplify = TRUE,
                                                  verbose = TRUE),
                           regexp = "'simplify = TRUE' not possible for 'return_input = TRUE'.")

  testthat::expect_warning(SHAR::randomize_raster(raster = landscape_classified,
                                                  n_random = 3,
                                                  simplify = TRUE,
                                                  return_input = FALSE,
                                                  verbose = TRUE),
                           regexp = "'simplify = TRUE' not possible for 'n_random > 1'.")
})

