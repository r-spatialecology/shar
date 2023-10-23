# testthat::context("test-pack_randomized")

landscape_classified <- classify_habitats(terra::rast(landscape), n = 5, style = "fisher")
landscape_classified[terra::values(landscape_classified) != 1] <- 2

landscape_random <- randomize_raster(landscape_classified, n_random = 2)
landscape_ni <- randomize_raster(landscape_classified, n_random = 2, return_input = FALSE)

x <- pack_randomized(raster = landscape_random)
x_ni <- pack_randomized(raster = landscape_random)


################################################################################

testthat::test_that("pack_randomized wraps raster", {

  testthat::expect_s4_class(object = x$observed, class = "PackedSpatRaster")
  testthat::expect_true(all(sapply(x$randomized, inherits, what = "PackedSpatRaster")))
  testthat::expect_true(all(sapply(x_ni$randomized, inherits, what = "PackedSpatRaster")))

})

# testthat::context("test-pack_randomized")

y <- unpack_randomized(raster = x)
y_ni <- unpack_randomized(raster = x_ni)


testthat::test_that("unpack_randomized unwraps raster", {

  testthat::expect_s4_class(object = y$observed, class = "SpatRaster")
  testthat::expect_true(all(sapply(y$randomized, inherits, what = "SpatRaster")))
  testthat::expect_true(all(sapply(y_ni$randomized, inherits, what = "SpatRaster")))

})


