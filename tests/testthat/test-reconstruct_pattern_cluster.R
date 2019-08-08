testthat::context("test-reconstruct_pattern_cluster")

# normal reconstruction
pattern_recon <- shar::reconstruct_pattern_cluster(pattern = shar::species_b,
                                                   n_random = 3,
                                                   max_runs = 1,
                                                   verbose = FALSE)

pattern_recon_ni <- shar::reconstruct_pattern_cluster(pattern = shar::species_b,
                                                      n_random = 2,
                                                      max_runs = 1,
                                                      return_input = FALSE,
                                                      verbose = FALSE)

pattern_recon_comp_fast <- shar::reconstruct_pattern_cluster(pattern = shar::species_b,
                                                             n_random = 1,
                                                             max_runs = 1,
                                                             comp_fast = 0,
                                                             verbose = FALSE)

pattern_recon_energy <- shar::reconstruct_pattern_cluster(pattern = shar::species_b,
                                                          e_threshold = 0.1,
                                                          n_random = 3,
                                                          verbose = FALSE)

pattern_recon_simple <- shar::reconstruct_pattern_cluster(pattern = shar::species_b,
                                                          n_random = 1,
                                                          max_runs = 1,
                                                          return_input = FALSE,
                                                          simplify = TRUE,
                                                          verbose = FALSE)

################################################################################

testthat::test_that("Output is a long as n_random for reconstruct_pattern_cluster", {

  testthat::expect_is(pattern_recon, class = "rd_pat")

  testthat::expect_type(pattern_recon$randomized, type = "list")

  testthat::expect_length(pattern_recon$randomized, n = 3)
})

testthat::test_that("Output includes randomizations and original pattern for reconstruct_pattern_cluster", {

  testthat::expect_named(pattern_recon$randomized,
                         expected = paste0("randomized_", c(1:3)))

  testthat::expect_equal(pattern_recon$observed,
                         expected = spatstat::unmark(shar::species_b))
})

testthat::test_that("Reconstructed patterns have same number of points", {

  testthat::expect_true(all(vapply(pattern_recon$randomized,
                                   FUN.VALUE = logical(1),
                                   function(x) x$n == shar::species_b$n)))
})

testthat::test_that("Input pattern can not be returned for reconstruct_pattern_cluster", {

  testthat::expect_equal(object = pattern_recon_ni$observed,
                         expected = "NA")
})

testthat::test_that("Argument comp_fast = TRUE is working", {

  testthat::expect_is(pattern_recon_comp_fast, "rd_pat")
})

testthat::test_that("Reconstruction stops if e_threshold is reached", {

  energy <- shar::calculate_energy(pattern_recon_energy, verbose = FALSE)

  testthat::expect_true(object = all(energy < 0.1))
})


testthat::test_that("simplify works for reconstruct_pattern_cluster", {

  testthat::expect_is(pattern_recon_simple, "ppp")
})

testthat::test_that("reconstruct_pattern_cluster returns error if n_random < 1", {

  testthat::expect_error(shar::reconstruct_pattern_cluster(pattern = shar::species_b,
                                                          n_random = -5,
                                                          verbose = FALSE),
                         regexp = "n_random must be >= 1.",
                         fixed = TRUE)
})

testthat::test_that("reconstruct_pattern_cluster returns error if weights are wrong ", {

  testthat::expect_error(shar::reconstruct_pattern_cluster(pattern = shar::species_b,
                                                           weights = c(0, 0),
                                                           verbose = FALSE),
                         regexp = "The sum of 'weights' must be 0 < sum(weights) <= 1.",
                         fixed = TRUE)

  testthat::expect_error(shar::reconstruct_pattern_cluster(pattern = shar::species_b,
                                                           weights = c(1, 1),
                                                           verbose = FALSE),
                         regexp = "The sum of 'weights' must be 0 < sum(weights) <= 1.",
                         fixed = TRUE)
})

testthat::test_that("reconstruct_pattern_cluster returns warnings", {

  testthat::expect_warning(shar::reconstruct_pattern_cluster(pattern = shar::species_b,
                                                             n_random = 2,
                                                             max_runs = 1,
                                                             return_input = FALSE,
                                                             simplify = TRUE),
                           regexp = "'simplify = TRUE' not possible for 'n_random > 1'.",
                           fixed = TRUE)

  testthat::expect_warning(shar::reconstruct_pattern_cluster(pattern = shar::species_b,
                                                             n_random = 1,
                                                             max_runs = 1,
                                                             simplify = TRUE),
                           regexp = "'simplify = TRUE' not possible for 'return_input = TRUE'.",
                           fixed = TRUE)
})
