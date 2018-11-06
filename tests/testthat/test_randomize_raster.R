context("Randomize raster")

landscape_input <- NLMR::nlm_fbm(nrow = 20,
                                 ncol = 20,
                                 fract_dim = 1.0,
                                 user_seed = 42)

landscape_classified <- SHAR::classify_habitats(landscape_input, classes = 5)

n_random <- 3
names_random <- paste0("randomized_", 1:n_random)

test_that("Output is a long as n_random for randomize_raster", {

  landscape_random <- SHAR::randomize_raster(raster = landscape_classified,
                                             n_random = n_random)

  expect_length(landscape_random, n = n_random + 1)

})

test_that("Output includes randomizations and original pattern for randomize_raster", {

  landscape_random <- SHAR::randomize_raster(raster = landscape_classified,
                                             n_random = n_random)

  expect_named(landscape_random, expected = c(names_random, "observed"))

  expect_equal(landscape_random[[n_random + 1]], expected = landscape_classified)

})

test_that("All optional arguments can be used for randomize_raster", {

  landscape_random <- SHAR::randomize_raster(raster = landscape_classified,
                                             n_random = n_random,
                                             direction = 4,
                                             verbose = TRUE)

  expect_type(landscape_random, type = "list")
})

test_that("Input raster can not be returned for randomize_raster", {

  landscape_random <- SHAR::randomize_raster(raster = landscape_classified,
                                             n_random = n_random,
                                             return_input = FALSE)

  expect_named(landscape_random, expected = names_random)
})
