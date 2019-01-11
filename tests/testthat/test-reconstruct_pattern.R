testthat::context("reconstruct_pattern")

testthat::test_that("Output is a long as n_random for reconstruct_pattern", {

  pattern_recon <- SHAR::reconstruct_pattern(pattern = SHAR::species_b,
                                             n_random = 3,
                                             max_runs = 10)

  testthat::expect_type(pattern_recon, type = "list")
  testthat::expect_length(pattern_recon, n = 4)
})

testthat::test_that("Output includes randomizations and original pattern for reconstruct_pattern", {

  pattern_recon <- SHAR::reconstruct_pattern(pattern = SHAR::species_b,
                                             n_random = 3,
                                             max_runs = 10)

  testthat::expect_named(pattern_recon,
                         expected = c(paste0("randomized_", c(1: 3)), "observed"))

  testthat::expect_equal(pattern_recon[[4]],
                         expected = spatstat::unmark(SHAR::species_b))
})

testthat::test_that("Reconstructed patterns have same number of points for fitting = TRUE", {

  pattern_recon <- SHAR::reconstruct_pattern(pattern = SHAR::species_a,
                                             n_random = 199,
                                             max_runs = 1,
                                             fitting = TRUE,
                                             return_input = FALSE)

  testthat::expect_true(all(vapply(pattern_recon,
                                   FUN.VALUE = logical(1),
                                   function(x) x$n == SHAR::species_a$n)))
})

testthat::test_that("Input pattern can not be returned for reconstruct_pattern", {

  pattern_recon <- SHAR::reconstruct_pattern(pattern = SHAR::species_b,
                                             n_random = 3,
                                             max_runs = 10,
                                             return_input = FALSE)

  testthat::expect_false(any(SHAR::species_b %in% pattern_recon))
})

testthat::test_that("All optional arguments can be used for reconstruct_pattern", {

  pattern_recon <- SHAR::reconstruct_pattern(pattern = SHAR::species_a,
                                             n_random = 3,
                                             max_runs = 10,
                                             fitting = TRUE,
                                             comp_fast = TRUE,
                                             verbose = TRUE,
                                             plot = TRUE)

  testthat::expect_type(pattern_recon, type = "list")
  testthat::expect_length(pattern_recon, n = 4)
})

testthat::test_that("simplify works for reconstruct_pattern", {

  pattern_recon <- SHAR::reconstruct_pattern(pattern = SHAR::species_a,
                                             n_random = 1,
                                             max_runs = 10,
                                             return_input = FALSE,
                                             simplify = TRUE)

  testthat::expect_is(pattern_recon, "ppp")
})

testthat::test_that("reconstruct_pattern returns error if n_random < 1", {

  testthat::expect_error(SHAR::reconstruct_pattern(pattern = SHAR::species_b, n_random = -5),
                         regexp = "n_random must be >= 1.")
})

testthat::test_that("reconstruct_pattern returns warnings", {

  testthat::expect_warning(SHAR::reconstruct_pattern(pattern = SHAR::species_a,
                                                     n_random = 3,
                                                     max_runs = 10,
                                                     return_input = FALSE,
                                                     simplify = TRUE,
                                                     verbose = TRUE),
                           regexp = "'simplify = TRUE' not possible for 'n_random > 1'.")

  testthat::expect_warning(SHAR::reconstruct_pattern(pattern = SHAR::species_a,
                                                     n_random = 1,
                                                     max_runs = 10,
                                                     simplify = TRUE,
                                                     verbose = TRUE),
                           regexp = "'simplify = TRUE' not possible for 'return_input = TRUE'.")
})
