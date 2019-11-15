testthat::context("test-results_habitat_association")

set.seed(42)

random_a <- shar::fit_point_process(pattern = shar::species_a,
                                    n_random = 199,
                                    verbose = FALSE)

random_a_ni <- shar::fit_point_process(pattern = shar::species_a,
                                       n_random = 199,
                                       return_input = FALSE,
                                       verbose = FALSE)

landscape_classified <- shar::classify_habitats(raster = shar::landscape,
                                                classes = 5)

raster_random <- shar::randomize_raster(landscape_classified,
                                        n_random = 1,
                                        verbose = FALSE)

raster_random_ni <- shar::randomize_raster(landscape_classified,
                                           n_random = 1,
                                           return_input = FALSE,
                                           verbose = FALSE)

raster_random_cont <- shar::randomize_raster(landscape,
                                             n_random = 1,
                                             verbose = FALSE)

result_pattern <- shar::results_habitat_association(raster = landscape_classified,
                                                    pattern = random_a,
                                                    verbose = FALSE)

result_raster <- shar::results_habitat_association(raster = raster_random,
                                                   pattern = shar::species_a,
                                                   verbose = FALSE)

landscape_wrong <- raster::crop(x = landscape_classified,
                                y = raster::extent(c(0, 500, 0, 500)))

landscape_wrong <- shar::randomize_raster(landscape_wrong,
                                          n_random = 1,
                                          verbose = FALSE)

pattern_wrong <- shar::species_b[, spatstat::owin(xrange = c(0, 500), yrange = c(0, 500))]

pattern_wrong <- shar::fit_point_process(pattern = pattern_wrong,
                                         n_random = 199,
                                         verbose = FALSE)

################################################################################

testthat::test_that("results_habitat_association returns one row for each habitat", {

  testthat::expect_equal(nrow(result_pattern), expected = 5)

  testthat::expect_equal(nrow(result_raster), expected = 5)
})

testthat::test_that("results_habitat_association lo is < hi", {

  testthat::expect_true(all(result_pattern$lo <= result_pattern$hi))

  testthat::expect_true(all(result_raster$lo <= result_raster$hi))
})

testthat::test_that("results_habitat_association returns correct association", {

  result_ns <- dplyr::filter(result_pattern, significance == "n.s.")
  result_pos <- dplyr::filter(result_pattern, significance == "positive")
  result_neg <- dplyr::filter(result_pattern, significance == "negative")

  ns_tf <- result_ns$count >= result_ns$lo & result_ns$count <= result_ns$hi
  pos_tf <- result_pos$count > result_pos$hi
  neg_tf <- result_neg$count < result_neg$lo

  testthat::expect_true(all(c(ns_tf, pos_tf, neg_tf)))
})

testthat::test_that("results_habitat_association returns warning if significance_threshold is not meaningful", {

  testthat::expect_warning(shar::results_habitat_association(raster = landscape_classified,
                                                             pattern = random_a,
                                                             significance_level = 0.75),
                           regexp = "Make sure 'signifcance_level' is meaningful (e.g. 'significance_level = 0.05').",
                           fixed = TRUE)
})

testthat::test_that("results_habitat_association returns warning if more than 10 classes are present", {

  testthat::expect_warning(shar::results_habitat_association(raster = landscape,
                                                             pattern = random_a),
                           regexp = "The raster has more than 10 classes. Please make sure discrete classes are provided.",
                           fixed = TRUE)

  testthat::expect_warning(shar::results_habitat_association(raster = raster_random_cont,
                                                             pattern = shar::species_a),
                           regexp = "The raster has more than 10 classes. Please make sure discrete classes are provided.",
                           fixed = TRUE)
})

testthat::test_that("results_habitat_association returns error if input is wrong", {

  testthat::expect_error(shar::results_habitat_association(raster = landscape_classified,
                                                           pattern = shar::species_a,
                                                           verbose = FALSE),
                         regexp = "Class of 'pattern' or 'raster' must be either 'rd_pat' or 'rd_ras'.",
                         fixed = TRUE)

  testthat::expect_error(shar::results_habitat_association(raster = raster_random,
                                                           pattern = random_a,
                                                           verbose = FALSE),
                         regexp = "Please provide only one randomized input.",
                         fixed = TRUE)

  testthat::expect_error(shar::results_habitat_association(raster = raster_random_ni,
                                                           pattern = shar::species_a,
                                                           verbose = FALSE),
                         regexp = "The observed raster needs to be included in the input 'raster'.",
                         fixed = TRUE)

testthat::expect_error(shar::results_habitat_association(raster = landscape_classified,
                                                         pattern = random_a_ni,
                                                         verbose = FALSE),
                       regexp = "The observed pattern needs to be included in the input 'pattern'.",
                       fixed = TRUE)
})

testthat::test_that("results_habitat_association returns error if extent is not identical", {


  testthat::expect_warning(shar::results_habitat_association(pattern = shar::species_a,
                                                             raster = landscape_wrong,
                                                             verbose = FALSE),
                         regexp = "Extent of 'pattern' and 'raster' are not identical",
                         fixed = TRUE)

  testthat::expect_warning(shar::results_habitat_association(pattern = pattern_wrong,
                                                             raster = landscape_classified,
                                                             verbose = FALSE),
                         regexp = "Extent of 'pattern' and 'raster' are not identical",
                         fixed = TRUE)

})
