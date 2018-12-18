context("test-results_habitat_association")

set.seed(42)

random_a <- SHAR::fit_point_process(pattern = SHAR::species_b, n_random = 199)

landscape_classified <- SHAR::classify_habitats(raster = SHAR::landscape,
                                                classes = 5)

raster_random <- SHAR::randomize_raster(landscape_classified,
                                        n_random = 3)

test_that("results_habitat_association returns one row for each habitat", {

  result <- results_habitat_association(raster = landscape_classified,
                                        pattern = random_a)

  expect_equal(nrow(result), expected = 5)
})

test_that("results_habitat_association lo is < hi", {

  result <- results_habitat_association(raster = landscape_classified,
                                        pattern = random_a)

  expect_true(all(result$lo < result$hi))
})

test_that("results_habitat_association returns correct association", {

  result <- results_habitat_association(raster = landscape_classified,
                                        pattern = random_a)

  result_ns <- dplyr::filter(result, significance == "n.s.")
  result_pos <- dplyr::filter(result, significance == "positive")
  result_neg <- dplyr::filter(result, significance == "negative")

  ns_tf <- result_ns$count >= result_ns$lo & result_ns$count <= result_ns$hi
  pos_tf <- result_pos$count > result_pos$hi
  neg_tf <- result_neg$count < result_neg$lo

  expect_true(all(c(ns_tf, pos_tf, neg_tf)))
})

test_that("results_habitat_association returns warning if significance_threshold is not meaningful", {

  expect_warning(results_habitat_association(raster = landscape_classified,
                                             pattern = random_a,
                                             significance_level = 0.75))
})

test_that("results_habitat_association returns error if input is named wrong", {


  expect_error(results_habitat_association(raster = landscape_classified,
                                           pattern = unname(random_a)),
               regexp = "Input must named 'randomized_1' to 'randomized_n' and includ 'observed' raster.")
})

test_that("results_habitat_association returns error if wrong input is provided", {

  expect_error(results_habitat_association(raster = landscape_classified,
                                             pattern = random_a[[1]]),
               regexp = "Please provide either randomized point patterns or randomized rasters.")
})

test_that("results_habitat_association works for random raster", {

  result <- results_habitat_association(raster = raster_random,
                                        pattern = species_b)

  expect_equal(nrow(result), expected = 5)
  expect_true(all(result$lo < result$hi))
})

