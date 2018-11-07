context("randomize_raster")

test_that("Output is a long as n_random for randomize_raster", {

  landscape_classified <- SHAR::classify_habitats(raster = SHAR::landscape,
                                                  classes = 3)

  landscape_random <- SHAR::randomize_raster(raster = landscape_classified,
                                             n_random = 2)

  expect_length(landscape_random, n = 3)
})

test_that("Output includes randomizations and original pattern for randomize_raster", {

  landscape_classified <- SHAR::classify_habitats(raster = SHAR::landscape,
                                                  classes = 3)

  landscape_random <- SHAR::randomize_raster(raster = landscape_classified,
                                             n_random = 2)

  expect_named(landscape_random, expected = c(paste0("randomized_", 1:2), "observed"))

  expect_equal(landscape_random[[3]], expected = landscape_classified)
})

test_that("All optional arguments can be used for randomize_raster", {

  landscape_classified <- SHAR::classify_habitats(raster = SHAR::landscape,
                                                  classes = 3)

  landscape_random <- SHAR::randomize_raster(raster = landscape_classified,
                                             n_random = 2,
                                             direction = 4,
                                             verbose = TRUE)

  expect_type(landscape_random, type = "list")
})

test_that("Input raster can not be returned for randomize_raster", {

  landscape_classified <- SHAR::classify_habitats(raster = SHAR::landscape,
                                                  classes = 3)

  landscape_random <- SHAR::randomize_raster(raster = landscape_classified,
                                             n_random = 2,
                                             return_input = FALSE)

  expect_named(landscape_random, expected = paste0("randomized_", 1:2))

  expect_false(any(list(landscape_classified) %in% landscape_random))
})
