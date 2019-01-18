testthat::context("test-results_habitat_association")

set.seed(42)

random_a <- shar::fit_point_process(pattern = shar::species_b, n_random = 199)

landscape_classified <- shar::classify_habitats(raster = shar::landscape,
                                                classes = 5)

raster_random <- shar::randomize_raster(landscape_classified,
                                        n_random = 3)

testthat::test_that("results_habitat_association returns one row for each habitat", {

  result <- shar::results_habitat_association(raster = landscape_classified,
                                              pattern = random_a)

  testthat::expect_equal(nrow(result), expected = 5)
})

testthat::test_that("results_habitat_association lo is < hi", {

  result <- shar::results_habitat_association(raster = landscape_classified,
                                              pattern = random_a)

  testthat::expect_true(all(result$lo < result$hi))
})

testthat::test_that("results_habitat_association returns correct association", {

  result <- shar::results_habitat_association(raster = landscape_classified,
                                              pattern = random_a)

  result_ns <- dplyr::filter(result, significance == "n.s.")
  result_pos <- dplyr::filter(result, significance == "positive")
  result_neg <- dplyr::filter(result, significance == "negative")

  ns_tf <- result_ns$count >= result_ns$lo & result_ns$count <= result_ns$hi
  pos_tf <- result_pos$count > result_pos$hi
  neg_tf <- result_neg$count < result_neg$lo

  testthat::expect_true(all(c(ns_tf, pos_tf, neg_tf)))
})

testthat::test_that("results_habitat_association returns warning if significance_threshold is not meaningful", {

  testthat::expect_warning(shar::results_habitat_association(raster = landscape_classified,
                                                             pattern = random_a,
                                                             significance_level = 0.75))
})

testthat::test_that("results_habitat_association returns error if input is named wrong", {

  testthat::expect_error(shar::results_habitat_association(raster = landscape_classified,
                                                           pattern = unname(random_a)),
                         regexp = "Input must named 'randomized_1' to 'randomized_n' and includ 'observed' raster.")
})

testthat::test_that("results_habitat_association returns error if wrong input is provided", {

  testthat::expect_error(shar::results_habitat_association(raster = landscape_classified,
                                                           pattern = random_a[[1]]),
                         regexp = "Please provide either randomized point patterns or randomized rasters.")
})

testthat::test_that("results_habitat_association works for random raster", {

  result <- shar::results_habitat_association(raster = raster_random,
                                              pattern = shar::species_b)

  testthat::expect_equal(nrow(result), expected = 5)
  testthat::expect_true(all(result$lo < result$hi))
})

