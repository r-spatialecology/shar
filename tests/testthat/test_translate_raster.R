context("Translate raster")

nrow <- 20
ncol <- 20

landscape_input <- NLMR::nlm_fbm(nrow = nrow, ncol = ncol,
                                 fract_dim = 1.0,
                                 user_seed = 42)

landscape_classified <- SHAR::classify_habitats(landscape_input, classes = 5)

n_random <- (nrow + 1) * (ncol + 1)  - 4
names_random <- paste0("randomized_", 1:n_random)

test_that("Output is a long as n_random for translate_raster", {

  landscape_random <- SHAR::translate_raster(raster = landscape_classified)

  expect_length(landscape_random, n = n_random + 1)

})

test_that("Output includes randomizations and original pattern for translate_raster", {

  landscape_random <- SHAR::translate_raster(raster = landscape_classified)

  expect_named(landscape_random, expected = c(names_random, "observed"))

  expect_equal(landscape_random[[n_random + 1]], expected = landscape_classified)

})

test_that("Input raster can not be returned for translate_raster", {

  landscape_random <- SHAR::translate_raster(raster = landscape_classified,
                                             return_input = FALSE)

  expect_named(landscape_random, expected = names_random)
})

test_that("Error if nrow != ncol for translate_raster", {

  landscape_input <- NLMR::nlm_fbm(nrow = 20, ncol = 30,
                                   fract_dim = 1.0,
                                   user_seed = 42)

  landscape_classified <- SHAR::classify_habitats(landscape_input, classes = 5)

  expect_error(SHAR::translate_raster(raster = landscape_classified),
               regexp  = "Torus translation only works for raster with nrow == ncol")
})
