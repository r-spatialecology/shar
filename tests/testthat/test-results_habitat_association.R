testthat::context("test-results_habitat_association")

set.seed(42)

random_a <- fit_point_process(pattern = species_a, n_random = 199,
                              verbose = FALSE)

random_a_ni <- fit_point_process(pattern = species_a, n_random = 199,
                                 return_input = FALSE, verbose = FALSE)

landscape_classified <- classify_habitats(raster = landscape, classes = 5)

raster_random <- randomize_raster(landscape_classified, n_random = 1, verbose = FALSE)

raster_random_ni <- randomize_raster(landscape_classified, n_random = 1,
                                     return_input = FALSE, verbose = FALSE)

raster_random_cont <- randomize_raster(landscape, n_random = 1, verbose = FALSE)

result_pattern <- results_habitat_association(raster = landscape_classified,
                                              pattern = random_a, verbose = FALSE)

result_raster <- results_habitat_association(raster = raster_random, pattern = species_a,
                                             verbose = FALSE)

landscape_wrong <- raster::crop(x = landscape_classified,
                                y = raster::extent(c(0, 500, 0, 500)))

landscape_wrong <- randomize_raster(landscape_wrong, n_random = 1, verbose = FALSE)

landscape_wrong_b <- landscape_classified

landscape_wrong_b[1:50] <- NA

pattern_wrong <- species_b[, spatstat.geom::owin(xrange = c(0, 500), yrange = c(0, 500))]

pattern_wrong <- fit_point_process(pattern = pattern_wrong, n_random = 199,
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

  testthat::expect_warning(results_habitat_association(raster = landscape_classified,
                                                       pattern = random_a,
                                                       significance_level = 0.75),
                           regexp = "Make sure 'signifcance_level' is meaningful (e.g. 'significance_level = 0.05').",
                           fixed = TRUE)
})

testthat::test_that("results_habitat_association returns warning if more than 10 classes are present", {

  testthat::expect_warning(results_habitat_association(raster = landscape,
                                                       pattern = random_a),
                           regexp = "The raster has more than 10 classes. Please make sure discrete classes are provided.")

  testthat::expect_warning(results_habitat_association(raster = raster_random_cont,
                                                       pattern = species_a),
                           regexp = "The raster has more than 10 classes. Please make sure discrete classes are provided.")
})

testthat::test_that("results_habitat_association returns error if input is wrong", {

  testthat::expect_error(results_habitat_association(raster = landscape_classified,
                                                     pattern = species_a, verbose = FALSE),
                         regexp = "Class of 'pattern' or 'raster' must be either 'rd_pat' or 'rd_ras'.")

  testthat::expect_error(results_habitat_association(raster = raster_random,
                                                     pattern = random_a, verbose = FALSE),
                         regexp = "Please provide only one randomized input.")

  testthat::expect_error(results_habitat_association(raster = raster_random_ni,
                                                     pattern = species_a, verbose = FALSE),
                         regexp = "The observed raster needs to be included in the input 'raster'.")

testthat::expect_error(results_habitat_association(raster = landscape_classified,
                                                   pattern = random_a_ni, verbose = FALSE),
                       regexp = "The observed pattern needs to be included in the input 'pattern'.")
})

testthat::test_that("results_habitat_association returns error if extent is not identical", {

  testthat::expect_warning(results_habitat_association(pattern = species_a,
                                                       raster = landscape_wrong, verbose = FALSE),
                         regexp = "Extent of 'pattern' and 'raster' are not identical.")

  testthat::expect_warning(results_habitat_association(pattern = pattern_wrong,
                                                       raster = landscape_classified, verbose = FALSE),
                         regexp = "Extent of 'pattern' and 'raster' are not identical.")

})

testthat::test_that("results_habitat_association returns warning if NA present", {

  testthat::expect_warning(results_habitat_association(pattern = random_a,
                                                       raster = landscape_wrong_b,
                                                       verbose = FALSE),
                           regexp = "NA values present. Please make sure the observation window of the point pattern reflects this.")
})
