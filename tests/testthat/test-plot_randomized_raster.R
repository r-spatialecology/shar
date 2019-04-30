testthat::context("test-plot_randomized_raster")

landscape_classified <- shar::classify_habitats(raster = shar::landscape,
                                                classes = 5)

raster_random <- shar::randomize_raster(raster = landscape_classified,
                                        n_random = 3,
                                        verbose = FALSE)

testthat::test_that("plot_randomized_raster returns plot", {

  shar::plot_randomized_raster(raster_random,
                               verbose = FALSE)
})

testthat::test_that("plot_randomized_raster returns plot if n vector is specified", {

  shar::plot_randomized_raster(raster_random,
                               n = c(2, 3),
                               verbose = FALSE)
})

testthat::test_that("plot_randomized_raster returns error if observed is missing", {

  raster_random <- shar::randomize_raster(raster = landscape_classified,
                                          n_random = 3,
                                          return_input = FALSE,
                                          verbose = FALSE)

  testthat::expect_error(shar::plot_randomized_raster(raster_random,
                                                      verbose = FALSE),
                         regexp = "Input must include 'observed' raster.")
})


testthat::test_that("plot_randomized_rasters returns error if wrong class ", {

  testthat::expect_error(shar::plot_randomized_raster(list(shar::species_a,
                                                           shar::species_b),
                                                       verbose = FALSE),
                         regexp = "Class of 'raster' must be 'rd_ras'.",)
})
