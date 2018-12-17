context("test-results_habitat_association")

set.seed(42)
random_a <- SHAR::fit_point_process(pattern = SHAR::species_b, n_random = 199)

landscape_classified <- SHAR::classify_habitats(raster = SHAR::landscape,
                                                classes = 5)

result <- results_habitat_association(raster = landscape_classified,
                                      pattern = random_a,
                                      verbose = FALSE)

test_that("results_habitat_association returns one row for each habitat", {

  expect_equal(nrow(result), expected = 5)
})

test_that("results_habitat_association lo is < hi", {

  expect_true(all(result$lo < result$hi))
})

test_that("results_habitat_association returns correct association", {

  result_ns <- dplyr::filter(result, significance == "n.s.")
  result_pos <- dplyr::filter(result, significance == "positive")
  result_neg <- dplyr::filter(result, significance == "negative")

  ns_tf <- result_ns$count >= result_ns$lo & result_ns$count <= result_ns$hi
  pos_tf <- result_pos$count > result_pos$hi
  neg_tf <- result_neg$count < result_neg$lo

  expect_true(all(c(ns_tf, pos_tf, neg_tf)))
})
