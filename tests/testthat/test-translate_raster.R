testthat::context("Translate raster")

# n_random <- (raster::nrow(landscape) + 1) * (raster::ncol(landscape) + 1)  - 4

landscape_classified <- shar::classify_habitats(raster = shar::landscape,
                                                classes = 3)

landscape_random <- shar::translate_raster(raster = landscape_classified,
                                           verbose = FALSE)

landscape_wrong <- raster::crop(shar::landscape, raster::extent(0, 1000, 0, 500))

landscape_classified_wrong <- shar::classify_habitats(landscape_wrong, classes = 3)

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

  landscape_random <- shar::translate_raster(raster = landscape_classified,
                                             return_input = FALSE,
                                             verbose = FALSE)

  random_stack <- raster::stack(landscape_random)

  comparison <- as.matrix(raster::values(abs(random_stack - landscape_classified)))

  check <- any(apply(comparison, 2, function(x) all(x == 0)))

  testthat::expect_false(check)
})

testthat::test_that("Error if nrow != ncol for translate_raster", {

  testthat::expect_error(shar::translate_raster(raster = landscape_classified_wrong,
                                                verbose = FALSE),
                         regexp  = "Torus translation only works for raster with nrow == ncol.")
})
