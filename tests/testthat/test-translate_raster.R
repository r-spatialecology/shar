# context("test-translate_raster")

# n_random <- (terra::nrow(terra::rast(landscape)) + 1) *
#   terra::ncol(terra::rast(landscape)) + 1)  - 4

# create landscape
landscape_classified <- classify_habitats(raster = terra::rast(landscape), n = 5, style = "fisher")

# normal translation
landscape_random <- translate_raster(raster = landscape_classified, verbose = FALSE)

# torus translation with provided steps
landscape_random_steps <- translate_raster(raster = landscape_classified,
                                           steps_x = 1:3, steps_y = 1:3,
                                           verbose = FALSE, return_input = FALSE)

# simplified raster
landscape_random_simple <- translate_raster(raster = landscape_classified,
                                            steps_x = 1, steps_y = 5,
                                            simplify = TRUE, verbose = FALSE,
                                            return_input = FALSE)

# create landscape wrong extent
landscape_wrong <- landscape_classified

# classify landscape wrong extent
landscape_wrong[1:50] <- NA

################################################################################

test_that("Output is a long as n_random for translate_raster", {

  expect_length(landscape_random$randomized, n = 2597)

})

test_that("Output includes randomizations and original pattern for translate_raster", {

  expect_named(landscape_random$randomized, expected = paste0("randomized_", 1:2597))

  expect_equal(landscape_random$observed, expected = landscape_classified)

})

test_that("Input raster can not be returned for translate_raster", {

  check <- vapply(X = landscape_random_steps$randomized, FUN = function(x) {

    landscape_diff <- landscape_classified - x

    all(terra::values(landscape_diff) == 0)
  }, FUN.VALUE = logical(1))

  expect_false(all(check))

})

test_that("Providing steps is working for translate_raster", {

  expect_length(landscape_random_steps$randomized, n = 9)

})

test_that("simplify is working for translate_raster", {

  expect_s4_class(landscape_random_simple, class = "SpatRaster")

})

test_that("Warning if more than 10 classes are present for translate_raster", {

  expect_warning(translate_raster(raster = terra::rast(landscape), steps_x = 5, steps_y = 5,
                                  verbose = FALSE),
                 regexp  = "The raster has more than 10 classes. Please make sure discrete classes are provided.")

})

test_that("Stop if NA are present", {

  expect_error(translate_raster(raster = landscape_wrong, steps_x = 5, steps_y = 5),
               regexp = "NA values are not allowed for 'translate_raster\\()'.")

})
