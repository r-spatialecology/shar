testthat::context("test-plot_rd_ras")

landscape_classified <- classify_habitats(raster = terra::rast(landscape), n = 5, style = "fisher")

raster_random <- translate_raster(raster = landscape_classified,
                                  steps_x = 1:2, steps_y = 1:1, verbose = FALSE)

raster_random_large <- translate_raster(raster = landscape_classified,
                                        steps_x = 1:2, steps_y = 1:2, verbose = FALSE)

raster_random_ni <- translate_raster(raster = landscape_classified,
                                     steps_x = 1:1, steps_y = 1:1,
                                     return_input = FALSE, verbose = FALSE)

raster_random_cont <- translate_raster(raster = terra::rast(landscape),
                                       steps_x = 1:1, steps_y = 1:1, verbose = FALSE)

################################################################################

testthat::test_that("plot returns plot", {

  testthat::expect_null(plot(raster_random, verbose = FALSE))
})

testthat::test_that("plot returns plot if n vector is specified", {

  testthat::expect_null(plot(raster_random, n = c(1, 2, 5), verbose = FALSE))

  testthat::expect_null(plot(raster_random_large, verbose = FALSE))

  testthat::expect_null(plot(raster_random, n = 100, verbose = FALSE))

  testthat::expect_null(plot(raster_random_large, n = 100, verbose = FALSE))
})

testthat::test_that("plot returns error if observed is missing", {

  testthat::expect_error(plot(raster_random_ni, verbose = FALSE),
                         regexp = "Input must include 'observed' raster.",
                         fixed = TRUE)
})

testthat::test_that("plot returns error if wrong id are selected ", {

  testthat::expect_error(plot(raster_random, n = c(100, 101, 102), verbose = FALSE),
                         regexp = "Please provide at least on valid ID for n.",
                         fixed = TRUE)
})

testthat::test_that("plot returns warning if more than 10 classes are present", {

  testthat::expect_warning(plot(raster_random_cont),
                           regexp = "The raster has more than 10 classes. Please make sure discrete classes are provided.",
                           fixed = TRUE)
})
