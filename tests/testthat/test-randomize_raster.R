testthat::context("randomize_raster")

landscape_classified <- shar::classify_habitats(raster = shar::landscape,
                                                classes = 3)

landscape_random <- shar::randomize_raster(raster = landscape_classified,
                                           n_random = 3,
                                           verbose = FALSE)

testthat::test_that("Output is as long as n_random for randomize_raster", {

  testthat::expect_type(landscape_random, type = "list")

  testthat::expect_length(landscape_random, n = 4)
})

testthat::test_that("Output includes randomizations and original pattern for randomize_raster", {

  testthat::expect_named(landscape_random,
                         expected = c(paste0("randomized_", 1:3), "observed"))

  testthat::expect_equal(landscape_random[[4]],
                         expected = landscape_classified)
})

testthat::test_that("Input raster can not be returned for randomize_raster", {

  landscape_random <- shar::randomize_raster(raster = landscape_classified,
                                             n_random = 2,
                                             return_input = FALSE,
                                             verbose = FALSE)

  classified_df <- raster::as.data.frame(landscape_classified, xy = TRUE)
  random_df <- raster::as.data.frame(raster::stack(landscape_random), xy = TRUE)

  check <- any(c(all(random_df$randomized_1 == classified_df$layer),
                 all(random_df$randomized_2 == classified_df$layer)))

  testthat::expect_false(check)
})

testthat::test_that("simplify works for randomize_raster", {

  raster_random <- shar::randomize_raster(raster = landscape_classified,
                                          n_random = 1,
                                          simplify = TRUE,
                                          return_input = FALSE,
                                          verbose = FALSE)

  testthat::expect_is(raster_random, "RasterLayer")
})

testthat::test_that("randomize_raster returns error of n_random < 1", {

  testthat::expect_error(shar::randomize_raster(raster = landscape_classified,
                                                n_random = 0,
                                                verbose = FALSE),
                         regexp = "n_random must be >= 1.")
})

testthat::test_that("randomize_raster returns all warnings", {

  testthat::expect_warning(shar::randomize_raster(raster = landscape_classified,
                                                  n_random = 1,
                                                  simplify = TRUE),
                           regexp = "'simplify = TRUE' not possible for 'return_input = TRUE'.")

  testthat::expect_warning(shar::randomize_raster(raster = landscape_classified,
                                                  n_random = 2,
                                                  simplify = TRUE,
                                                  return_input = FALSE),
                           regexp = "'simplify = TRUE' not possible for 'n_random > 1'.")
})

