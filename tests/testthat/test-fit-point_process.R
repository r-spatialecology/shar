testthat::context("test-fit_point_process")

# normal
pattern_random <- shar::fit_point_process(pattern = shar::species_b,
                                          n_random = 3,
                                          verbose = FALSE)

# cluster process
pattern_random_cluster <- shar::fit_point_process(pattern = shar::species_b,
                                                  n_random = 3,
                                                  process = "cluster",
                                                  verbose = FALSE)

# no input
pattern_random_ni <- shar::fit_point_process(pattern = shar::species_a,
                                             n_random = 3,
                                             return_input = FALSE,
                                             verbose = FALSE)

# simple output
pattern_random_simple <- shar::fit_point_process(pattern = shar::species_a,
                                                 n_random = 1,
                                                 return_input = FALSE,
                                                 simplify = TRUE,
                                                 verbose = FALSE)

################################################################################

testthat::test_that("Output is a long as n_random for fit_point_process", {

  testthat::expect_length(pattern_random$randomized,
                          n = 3)

  testthat::expect_length(pattern_random_cluster$randomized,
                          n = 3)
})

testthat::test_that("Output includes randomizations and original pattern for fit_point_process", {

  testthat::expect_named(pattern_random$randomized,
                         expected = paste0("randomized_", 1:3))

  testthat::expect_equal(pattern_random$observed,
                         expected = spatstat::unmark(shar::species_b))

  testthat::expect_named(pattern_random_cluster$randomized,
                         expected = paste0("randomized_", 1:3))

  testthat::expect_equal(pattern_random_cluster$observed,
                         expected = spatstat::unmark(shar::species_b))
})

testthat::test_that("Fitted patterns have same number of points for cluster process", {

  testthat::expect_true(all(vapply(pattern_random$randomized,
                                   FUN.VALUE = logical(1),
                                   function(x) x$n == shar::species_b$n)))

  testthat::expect_true(all(vapply(pattern_random_cluster$randomized,
                                   FUN.VALUE = logical(1),
                                   function(x) x$n == shar::species_b$n)))
})

testthat::test_that("Input pattern can not be returned for fit_point_process", {

  testthat::expect_equal(object = pattern_random_ni$observed,
                         expected = "NA")
})

testthat::test_that("simplify works for fit_point_process", {

  testthat::expect_is(pattern_random_simple, "ppp")
})

testthat::test_that("fit_point_process returns errors", {

  testthat::expect_error(shar::fit_point_process(pattern = shar::species_b,
                                                 n_random = -10,
                                                 verbose = FALSE),
                         regexp = "n_random must be >= 1.",
                         fixed = TRUE)

  testthat::expect_error(shar::fit_point_process(pattern = shar::species_b,
                                                 n_random = 19,
                                                 process = "not_valid",
                                                 verbose = FALSE),
                         regexp = "Please select either 'poisson' or 'cluster'.",
                         fixed = TRUE)
})

testthat::test_that("fit_point_process returns warnings", {

  testthat::expect_warning(shar::fit_point_process(pattern = shar::species_a,
                                                   n_random = 3,
                                                   return_input = FALSE,
                                                   simplify = TRUE),
                           regexp = "'simplify = TRUE' not possible for 'n_random > 1'.",
                           fixed = TRUE)

  testthat::expect_warning(shar::fit_point_process(pattern = shar::species_a,
                                                   n_random = 1,
                                                   simplify = TRUE),
                           regexp = "'simplify = TRUE' not possible for 'return_input = TRUE'.",
                           fixed = TRUE)
})

