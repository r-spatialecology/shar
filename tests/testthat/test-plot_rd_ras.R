# context("test-plot_rd_ras")

landscape_classified <- classify_habitats(raster = terra::rast(landscape), n = 5,
                                          style = "fisher")

raster_random <- translate_raster(raster = landscape_classified,
                                  steps_x = 1:2, steps_y = 1:1, verbose = FALSE)

raster_random_large <- translate_raster(raster = landscape_classified,
                                        steps_x = 1:2, steps_y = 1:2, verbose = FALSE)

raster_random_ni <- translate_raster(raster = landscape_classified,
                                     steps_x = 1:1, steps_y = 1:1,
                                     return_input = FALSE, verbose = FALSE)

################################################################################

test_that("plot returns plot", {

  expect_null(plot(raster_random, verbose = FALSE))

})

test_that("plot returns plot if n vector is specified", {

  expect_null(plot(raster_random, n = c(1, 2, 5), verbose = FALSE))

  expect_null(plot(raster_random_large, verbose = FALSE))

  expect_null(plot(raster_random, n = 100, verbose = FALSE))

  expect_null(plot(raster_random_large, n = 100, verbose = FALSE))

})

test_that("plot returns error if observed is missing", {

  expect_error(plot(raster_random_ni, verbose = FALSE),
               regexp = "Input must include 'observed' raster.")

})

test_that("plot returns error if wrong id are selected ", {

  expect_error(plot(raster_random, n = c(100, 101, 102), verbose = FALSE),
               regexp = "Please provide at least on valid ID for n.")

})
