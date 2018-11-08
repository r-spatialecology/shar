context("Translate raster")

# n_random <- (raster::nrow(landscape) + 1) * (raster::ncol(landscape) + 1)  - 4

test_that("Output is a long as n_random for translate_raster", {

  landscape_classified <- SHAR::classify_habitats(raster = SHAR::landscape,
                                                  classes = 3)

  landscape_random <- SHAR::translate_raster(raster = landscape_classified)

  expect_length(landscape_random, n = 2598)
})

test_that("Output includes randomizations and original pattern for translate_raster", {

  landscape_classified <- SHAR::classify_habitats(raster = SHAR::landscape,
                                                  classes = 3)

  landscape_random <- SHAR::translate_raster(raster = landscape_classified)

  expect_named(landscape_random, expected = c(paste0("randomized_", 1:2597), "observed"))

  expect_equal(landscape_random[[2598]], expected = landscape_classified)

})

test_that("Input raster can not be returned for translate_raster", {

  landscape_classified <- SHAR::classify_habitats(raster = SHAR::landscape,
                                                  classes = 3)

  landscape_random <- SHAR::translate_raster(raster = landscape_classified,
                                             return_input = FALSE)

  expect_named(landscape_random, expected = paste0("randomized_", 1:2597))

  expect_false(any(list(landscape_classified) %in% landscape_random))
})

test_that("Error if nrow != ncol for translate_raster", {

  landscape_wrong <- raster::crop(SHAR::landscape, raster::extent(0, 1000, 0, 500))

  landscape_classified <- SHAR::classify_habitats(landscape_wrong, classes = 3)

  expect_error(SHAR::translate_raster(raster = landscape_classified),
               regexp  = "Torus translation only works for raster with nrow == ncol")
})
