testthat::context("Fit point process")

pattern_random_b <- shar::fit_point_process(pattern = shar::species_b,
                                            n_random = 3)

pattern_random_b_cluster <- shar::fit_point_process(pattern = shar::species_b,
                                                    n_random = 3,
                                                    process = "cluster")

testthat::test_that("Output is a long as n_random for fit_point_process", {

  testthat::expect_length(pattern_random_b,
                          n = 4)

  testthat::expect_length(pattern_random_b_cluster,
                          n = 4)
})

testthat::test_that("Output includes randomizations and original pattern for fit_point_process", {

  testthat::expect_named(pattern_random_b,
                         expected = c(paste0("randomized_", 1:3), "observed"))

  testthat::expect_equal(pattern_random_b[[4]],
                         expected = spatstat::unmark(shar::species_b))

  testthat::expect_named(pattern_random_b_cluster,
                         expected = c(paste0("randomized_", 1:3), "observed"))

  testthat::expect_equal(pattern_random_b_cluster[[4]],
                         expected = spatstat::unmark(shar::species_b))
})

testthat::test_that("Fitted patterns have same number of points for cluster process", {

  testthat::expect_true(all(vapply(pattern_random_b,
                                   FUN.VALUE = logical(1),
                                   function(x) x$n == shar::species_b$n)))

  testthat::expect_true(all(vapply(pattern_random_b_cluster,
                                   FUN.VALUE = logical(1),
                                   function(x) x$n == shar::species_b$n)))
})

testthat::test_that("Input pattern can not be returned for fit_point_process", {

  pattern_random_a <- shar::fit_point_process(pattern = shar::species_a,
                                              n_random = 3,
                                              return_input = FALSE)

  testthat::expect_false(any(shar::species_a %in% pattern_random_a))
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

  testthat::expect_warning(shar::fit_point_process(pattern = shar::species_a,
                                                   n_random = 3,
                                                   return_input = FALSE,
                                                   simplify = TRUE,
                                                   verbose = TRUE),
                           regexp = "'simplify = TRUE' not possible for 'n_random > 1'.")

  testthat::expect_warning(shar::fit_point_process(pattern = shar::species_a,
                                                   n_random = 1,
                                                   simplify = TRUE,
                                                   verbose = TRUE),
                           regexp = "'simplify = TRUE' not possible for 'return_input = TRUE'.")
})

