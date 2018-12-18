context("reconstruct_pattern")

test_that("Output is a long as n_random for reconstruct_pattern", {

  pattern_recon <- SHAR::reconstruct_pattern(pattern = SHAR::species_b,
                                             n_random = 3,
                                             max_runs = 10)

  expect_type(pattern_recon, type = "list")
  expect_length(pattern_recon, n = 4)
})

test_that("Output includes randomizations and original pattern for reconstruct_pattern", {

  pattern_recon <- SHAR::reconstruct_pattern(pattern = SHAR::species_b,
                                             n_random = 3,
                                             max_runs = 10)

  expect_named(pattern_recon, expected = c(paste0("randomized_", c(1: 3)), "observed"))

  expect_equal(pattern_recon[[4]], expected = SHAR::species_b)
})

test_that("Input pattern can not be returned for reconstruct_pattern", {

  pattern_recon <- SHAR::reconstruct_pattern(pattern = SHAR::species_b,
                                             n_random = 3,
                                             max_runs = 10,
                                             return_input = FALSE)

  expect_false(any(SHAR::species_b %in% pattern_recon))
})

test_that("reconstruct_pattern returns error if n_random < 1", {

  expect_error(SHAR::reconstruct_pattern(pattern = SHAR::species_b, n_random = -5),
               regexp = "n_random must be >= 1.")
})

test_that("All optional arguments can be used for reconstruct_pattern", {

  pattern_recon <- SHAR::reconstruct_pattern(pattern = SHAR::species_a,
                                             n_random = 3,
                                             max_runs = 10,
                                             fitting = TRUE,
                                             comp_fast = TRUE,
                                             verbose = TRUE,
                                             plot = TRUE)

  expect_type(pattern_recon, type = "list")
  expect_length(pattern_recon, n = 4)
})
