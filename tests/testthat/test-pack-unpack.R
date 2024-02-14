# context("test-pack_randomized")

landscape_classified <- classify_habitats(terra::rast(landscape), n = 5, style = "fisher")
landscape_classified[terra::values(landscape_classified) != 1] <- 2

landscape_random <- randomize_raster(landscape_classified, n_random = 2, verbose = FALSE)
landscape_ni <- randomize_raster(landscape_classified, n_random = 2,
                                 return_input = FALSE, verbose = FALSE)

x <- pack_randomized(raster = landscape_random)
x_ni <- pack_randomized(raster = landscape_random)

################################################################################

test_that("pack_randomized wraps raster", {

  expect_s4_class(object = x$observed, class = "PackedSpatRaster")
  expect_true(all(sapply(x$randomized, inherits, what = "PackedSpatRaster")))
  expect_true(all(sapply(x_ni$randomized, inherits, what = "PackedSpatRaster")))

})

# context("test-pack_randomized")

y <- unpack_randomized(raster = x)
y_ni <- unpack_randomized(raster = x_ni)


test_that("unpack_randomized unwraps raster", {

  expect_s4_class(object = y$observed, class = "SpatRaster")
  expect_true(all(sapply(y$randomized, inherits, what = "SpatRaster")))
  expect_true(all(sapply(y_ni$randomized, inherits, what = "SpatRaster")))

})


