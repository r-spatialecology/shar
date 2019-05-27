testthat::context("test-plot_randomized_raster")

landscape_classified <- shar::classify_habitats(raster = shar::landscape,
                                                classes = 5)

raster_random <- shar::randomize_raster(raster = landscape_classified,
                                        n_random = 1,
                                        verbose = FALSE)

raster_random_ni <- shar::randomize_raster(raster = landscape_classified,
                                           n_random = 1,
                                           return_input = FALSE,
                                           verbose = FALSE)

raster_random_cont <- shar::randomize_raster(raster = landscape,
                                             n_random = 1,
                                             verbose = FALSE)

################################################################################

testthat::test_that("plot_randomized_raster returns plot", {

  testthat::expect_null(shar::plot_randomized_raster(raster_random,
                                                     verbose = FALSE))
})

testthat::test_that("plot_randomized_raster returns plot if n vector is specified", {

  testthat::expect_null(shar::plot_randomized_raster(raster_random,
                                                     n = c(1, 2),
                                                     verbose = FALSE))
})

testthat::test_that("plot_randomized_raster returns error if observed is missing", {

  testthat::expect_error(shar::plot_randomized_raster(raster_random_ni,
                                                      verbose = FALSE),
                         regexp = "Input must include 'observed' raster.",
                         fixed = TRUE)
})

testthat::test_that("plot_randomized_rasters returns error if wrong class ", {

  testthat::expect_error(shar::plot_randomized_raster(list(shar::species_a,
                                                           shar::species_b),
                                                       verbose = FALSE),
                         regexp = "Class of 'raster' must be 'rd_ras'.",
                         fixed = TRUE)
})

testthat::test_that("plot_randomized_rasters returns warning if more than 10 classes are present", {

  testthat::expect_warning(shar::plot_randomized_raster(raster = raster_random_cont),
                           regexp = "The raster has more than 10 classes. Please make sure discrete classes are provided.",
                           fixed = TRUE)
})


