context("test-results_habitat_association")

test_that("results_habitat_association returns one row for each habitat", {

  random_a <- SHAR::fit_point_process(pattern = SHAR::species_a, n_random = 199)

  landscape_classified <- SHAR::classify_habitats(raster = SHAR::landscape,
                                                  classes = 3)

  result <- results_habitat_association(raster = landscape_classified,
                                        pattern = random_a)

  expect_equal(nrow(result), expected = 3)
})


test_that("results_habitat_association lo is < hi", {

  random_b <- SHAR::fit_point_process(pattern = SHAR::species_b, n_random = 199)

  landscape_classified <- SHAR::classify_habitats(raster = SHAR::landscape,
                                                  classes = 3)

  result <- results_habitat_association(raster = landscape_classified,
                                        pattern = random_b)

  expect_true(all(result$lo < result$hi))
})

test_that("results_habitat_association returns correct association", {

  random_a <- SHAR::fit_point_process(pattern = SHAR::species_a, n_random = 199)

  landscape_classified <- SHAR::classify_habitats(raster = SHAR::landscape,
                                                  classes = 3)

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
