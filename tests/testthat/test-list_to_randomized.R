testthat::context("test-list_to_randomized")

pattern_random <- lapply(X = 1:3, function(i) {
  shar::fit_point_process(pattern = shar::species_b, n_random = 1,
                          return_input = FALSE, simplify = TRUE, verbose = FALSE)})

pattern_conv <- shar::list_to_randomized(list = pattern_random,
                                         observed = shar::species_b)


landscape_classified <- shar::classify_habitats(raster = shar::landscape,
                                                classes = 3)

raster_random <- lapply(X = 1:3, function(i) {
  shar::randomize_raster(raster = landscape_classified, n_random = 1,
                         return_input = FALSE, simplify = TRUE, verbose = FALSE)})

raster_conv <- shar::list_to_randomized(list = raster_random,
                                        observed = landscape_classified)

################################################################################


testthat::test_that("Output has correct class", {

  testthat::expect_s3_class(object = pattern_conv, class = "rd_pat")

  testthat::expect_s3_class(object = raster_conv, class = "rd_ras")

})

testthat::test_that("Output has correct method", {

  testthat::expect_equal(object = pattern_conv$method, expected = "list_to_randomized()")

  testthat::expect_equal(object = raster_conv$method, expected = "list_to_randomized()")

})

testthat::test_that("Output is a long as input list", {

  testthat::expect_length(object = pattern_conv$randomized, n = 3)

  testthat::expect_length(object = raster_conv$randomized, n = 3)

})

testthat::test_that("Output includes randomizations and original objects", {

  testthat::expect_named(object = pattern_conv$randomized,
                         expected = paste0("randomized_", 1:3))

  testthat::expect_equal(object = pattern_conv$observed, expected = shar::species_b)

  testthat::expect_named(object = raster_conv$randomized,
                         expected = paste0("randomized_", 1:3))

  testthat::expect_equal(object = raster_conv$observed,
                         expected = landscape_classified)
})

testthat::test_that("list_to_randomized returns errors", {

  testthat::expect_error(object = shar::list_to_randomized(list = shar::species_b),
                         regexp = "Please provide list of either 'ppp' or 'RasterLayer' objects.",)

})

testthat::test_that("list_to_randomized works with results_habitat_associations", {

  res_a <- shar::results_habitat_association(pattern = pattern_conv,
                                             raster = landscape_classified)

  res_b <- shar::results_habitat_association(pattern = shar::species_b,
                                             raster = raster_conv)

  testthat::expect_s3_class(object = res_a, class = "data.frame")

  testthat::expect_s3_class(object = res_b, class = "data.frame")

})

