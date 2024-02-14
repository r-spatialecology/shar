# context("test-randomize_raster")

landscape_classified <- classify_habitats(raster = terra::rast(landscape), n = 5, style = "fisher")

# normal random
landscape_random <- randomize_raster(raster = landscape_classified, n_random = 1,
                                     verbose = FALSE)

# simplify output
raster_random_simple <- randomize_raster(raster = landscape_classified, n_random = 1,
                                         simplify = TRUE, return_input = FALSE,
                                         verbose = FALSE)

# create landscape wrong extent
landscape_wrong <- landscape_classified

# classify landscape wrong extent
landscape_wrong[1:50] <- NA

################################################################################

test_that("Output is as long as n_random for randomize_raster", {

  expect_s3_class(landscape_random, class = "rd_ras")

  expect_length(landscape_random$randomized, n = 1)

})

test_that("Output includes randomizations and original pattern for randomize_raster", {

  expect_named(landscape_random$randomized, expected = "randomized_1")

  expect_equal(landscape_random$observed, expected = landscape_classified)

})

test_that("Input raster can not be returned for randomize_raster", {

  landscape_diff <- landscape_classified - raster_random_simple

  check <- all(terra::values(landscape_diff) == 0)

  expect_false(check)

})

test_that("simplify works for randomize_raster", {

  expect_s4_class(raster_random_simple, "SpatRaster")

})

test_that("randomize_raster returns error of n_random < 1", {

  expect_error(randomize_raster(raster = landscape_classified, n_random = 0,
                                verbose = FALSE), regexp = "n_random must be >= 1.")

})

test_that("randomize_raster returns all warnings", {

  expect_warning(randomize_raster(raster = landscape_classified, n_random = 1,
                                  simplify = TRUE, verbose = FALSE),
                 regexp = "'simplify = TRUE' not possible for 'return_input = TRUE'.")

  expect_warning(randomize_raster(raster = landscape_classified, n_random = 2,
                                  simplify = TRUE, return_input = FALSE, verbose = FALSE),
                 regexp = "'simplify = TRUE' not possible for 'n_random > 1'.")

  expect_warning(randomize_raster(raster = terra::rast(landscape), n_random = 1,
                                  verbose = FALSE),
                 regexp = "The raster has more than 10 classes. Please make sure discrete classes are provided.")

})

test_that("Warning if NA are present", {

  expect_warning(randomize_raster(raster = landscape_wrong, n_random = 1, verbose = FALSE),
                 regexp = "NA values present. Please make sure the observation window of the point pattern reflects this.")

})
