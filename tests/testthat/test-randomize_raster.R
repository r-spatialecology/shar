context("randomize_raster")

landscape_classified <- SHAR::classify_habitats(raster = SHAR::landscape,
                                                classes = 3)

test_that("Output is as long as n_random for randomize_raster", {

  landscape_random <- SHAR::randomize_raster(raster = landscape_classified,
                                             n_random = 3)

  expect_type(landscape_random, type = "list")
  expect_length(landscape_random, n = 4)
})

test_that("Output includes randomizations and original pattern for randomize_raster", {


  landscape_random <- SHAR::randomize_raster(raster = landscape_classified,
                                             n_random = 3)

  expect_named(landscape_random, expected = c(paste0("randomized_", 1:3), "observed"))

  expect_equal(landscape_random[[4]], expected = landscape_classified)
})

test_that("Input raster can not be returned for randomize_raster", {

  landscape_random <- SHAR::randomize_raster(raster = landscape_classified,
                                             n_random = 3,
                                             return_input = FALSE)

  expect_false(any(list(landscape_classified) %in% landscape_random))
})

test_that("All optional arguments can be used for randomize_raster", {

  landscape_random <- SHAR::randomize_raster(raster = landscape_classified,
                                             n_random = 3,
                                             verbose = TRUE)

  expect_type(landscape_random, type = "list")
  expect_length(landscape_random, n = 4)
})

test_that("randomize_raster returns error of n_random < 1", {

  expect_error(SHAR::randomize_raster(raster = landscape_classified,
                                      n_random = 0),
               regexp = "n_random must be >= 1.")
})
