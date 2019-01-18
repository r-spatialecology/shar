testthat::context("Fit point process")

testthat::test_that("Output is a long as n_random for fit_point_process", {

  pattern_random <- shar::fit_point_process(pattern = shar::species_a,
                                            n_random = 19)

  testthat::expect_length(pattern_random, n = 20)
})

testthat::test_that("Output includes randomizations and original pattern for fit_point_process", {

  pattern_random <- shar::fit_point_process(pattern = shar::species_a,
                                            n_random = 19)

  testthat::expect_named(pattern_random, expected = c(paste0("randomized_", 1:19), "observed"))

  testthat::expect_equal(pattern_random[[20]], expected = spatstat::unmark(shar::species_a))
})

testthat::test_that("Fitted patterns have same number of points for cluster process", {

  pattern_random <- shar::fit_point_process(pattern = shar::species_b,
                                            n_random = 199,
                                            process = "cluster",
                                            return_input = FALSE)


  testthat::expect_true(all(vapply(pattern_random,
                                   FUN.VALUE = logical(1),
                                   function(x) x$n == shar::species_b$n)))
})

testthat::test_that("Input pattern can not be returned for fit_point_process", {

  pattern_random <- shar::fit_point_process(pattern = shar::species_b,
                                            n_random = 19,
                                            return_input = FALSE)

  testthat::expect_false(any(shar::species_b %in% pattern_random))
})

testthat::test_that("simplify works for fit_point_process", {

  pattern_random <- shar::fit_point_process(pattern = shar::species_a,
                                            n_random = 1,
                                            return_input = FALSE,
                                            simplify = TRUE)

  testthat::expect_is(pattern_random, "ppp")
})

testthat::test_that("fit_point_process returns errors", {

  testthat::expect_error(shar::fit_point_process(pattern = shar::species_b,
                                                 n_random = -10),
                         regexp = "n_random must be >= 1.")

  testthat::expect_error(shar::fit_point_process(pattern = shar::species_b,
                                                 n_random = 19,
                                                 process = "not_valid"),
                         regexp = "Please select either 'poisson' or 'cluster'.")
})

testthat::test_that("fit_point_process returns warnings", {

  testthat::expect_warning(shar::fit_point_process(pattern = shar::species_b,
                                                   n_random = 19,
                                                   return_input = FALSE,
                                                   simplify = TRUE,
                                                   verbose = TRUE),
                           regexp = "'simplify = TRUE' not possible for 'n_random > 1'.")

  testthat::expect_warning(shar::fit_point_process(pattern = shar::species_b,
                                                   n_random = 1,
                                                   simplify = TRUE,
                                                   verbose = TRUE),
                           regexp = "'simplify = TRUE' not possible for 'return_input = TRUE'.")
})

