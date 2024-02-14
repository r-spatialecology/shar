# context("test-list_to_randomized")

pattern_random <- lapply(X = 1:3, function(i) {
  fit_point_process(pattern = species_b, n_random = 1,
                    return_input = FALSE, simplify = TRUE,
                    verbose = FALSE)
  })

pattern_conv <- list_to_randomized(list = pattern_random,
                                   observed = species_b)

landscape_classified <- classify_habitats(raster = terra::rast(landscape),
                                          n = 3, style = "fisher")

raster_random <- lapply(X = 1:3, function(i) {
  randomize_raster(raster = landscape_classified, n_random = 1,
                   return_input = FALSE, simplify = TRUE, verbose = FALSE)})

raster_conv <- list_to_randomized(list = raster_random, observed = landscape_classified)

################################################################################

test_that("Output has correct class", {

  expect_s3_class(object = pattern_conv, class = "rd_pat")

  expect_s3_class(object = raster_conv, class = "rd_ras")

})

test_that("Output has correct method", {

  expect_equal(object = pattern_conv$method, expected = "list_to_randomized()")

  expect_equal(object = raster_conv$method, expected = "list_to_randomized()")

})

test_that("Output is a long as input list", {

  expect_length(object = pattern_conv$randomized, n = 3)

  expect_length(object = raster_conv$randomized, n = 3)

})

test_that("Output includes randomizations and original objects", {

  expect_named(object = pattern_conv$randomized, expected = paste0("randomized_", 1:3))

  expect_equal(object = pattern_conv$observed, expected = species_b)

  expect_named(object = raster_conv$randomized, expected = paste0("randomized_", 1:3))

  expect_equal(object = raster_conv$observed, expected = landscape_classified)

})

test_that("list_to_randomized returns errors", {

  expect_error(object = list_to_randomized(list = species_b),
               regexp = "Please provide list of either 'ppp' or 'SpatRaster' objects.",)

})

test_that("list_to_randomized works with results_habitat_associations", {

  res_a <- results_habitat_association(pattern = pattern_conv, raster = landscape_classified,
                                       verbose = FALSE)

  res_b <- results_habitat_association(pattern = species_b, raster = raster_conv,
                                       verbose = FALSE)

  expect_s3_class(object = res_a, class = "data.frame")

  expect_s3_class(object = res_b, class = "data.frame")

})
