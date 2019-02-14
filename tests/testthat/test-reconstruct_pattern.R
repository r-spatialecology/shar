testthat::context("reconstruct_pattern")

pattern_recon <- shar::reconstruct_pattern(pattern = shar::species_a,
                                           n_random = 3,
                                           max_runs = 1,
                                           fitting = TRUE,
                                           verbose = FALSE)

testthat::test_that("Output is a long as n_random for reconstruct_pattern", {

  testthat::expect_type(pattern_recon, type = "list")

  testthat::expect_length(pattern_recon, n = 4)
})

testthat::test_that("Output includes randomizations and original pattern for reconstruct_pattern", {

  testthat::expect_named(pattern_recon,
                         expected = c(paste0("randomized_", c(1: 3)), "observed"))

  testthat::expect_equal(pattern_recon[[4]],
                         expected = spatstat::unmark(shar::species_a))
})

testthat::test_that("Reconstructed patterns have same number of points", {

  testthat::expect_true(all(vapply(pattern_recon,
                                   FUN.VALUE = logical(1),
                                   function(x) x$n == shar::species_a$n)))
})

testthat::test_that("Input pattern can not be returned for reconstruct_pattern", {

  pattern_recon <- shar::reconstruct_pattern(pattern = shar::species_b,
                                             n_random = 2,
                                             max_runs = 1,
                                             return_input = FALSE,
                                             verbose = FALSE)

  testthat::expect_false(any(shar::species_a %in% pattern_recon))
})

testthat::test_that("Argument comp_fast = TRUE is working", {

  pattern_recon <- shar::reconstruct_pattern(pattern = shar::species_a,
                                             n_random = 1,
                                             max_runs = 1,
                                             comp_fast = 0,
                                             verbose = FALSE)

  testthat::expect_type(pattern_recon, type = "list")

  testthat::expect_length(pattern_recon, n = 2)
})

testthat::test_that("Reconstruction stops if e_threshold is reached", {

  pattern_recon <- shar::reconstruct_pattern(pattern = shar::species_b,
                                             e_threshold = 0.5,
                                             n_random = 19,
                                             verbose = FALSE)

  energy <- shar::calculate_energy(pattern_recon, verbose = FALSE)

  testthat::expect_true(object = all(energy < 0.5))
})


testthat::test_that("simplify works for reconstruct_pattern", {

  pattern_recon <- shar::reconstruct_pattern(pattern = shar::species_a,
                                             n_random = 1,
                                             max_runs = 1,
                                             return_input = FALSE,
                                             simplify = TRUE,
                                             verbose = FALSE)

  testthat::expect_is(pattern_recon, "ppp")
})

testthat::test_that("reconstruct_pattern returns error if n_random < 1", {

  testthat::expect_error(shar::reconstruct_pattern(pattern = shar::species_b,
                                                   n_random = -5,
                                                   verbose = FALSE),
                         regexp = "n_random must be >= 1.")
})

testthat::test_that("reconstruct_pattern returns warnings", {

  testthat::expect_warning(shar::reconstruct_pattern(pattern = shar::species_a,
                                                     n_random = 2,
                                                     max_runs = 1,
                                                     return_input = FALSE,
                                                     simplify = TRUE,
                                                     verbose = FALSE),
                           regexp = "'simplify = TRUE' not possible for 'n_random > 1'.")

  testthat::expect_warning(shar::reconstruct_pattern(pattern = shar::species_a,
                                                     n_random = 1,
                                                     max_runs = 1,
                                                     simplify = TRUE,
                                                     verbose = FALSE),
                           regexp = "'simplify = TRUE' not possible for 'return_input = TRUE'.")
})
