context("reconstruct_pattern")

test_that("Output is a long as n_random for reconstruct_pattern", {

  pattern_recon <- SHAR::reconstruct_pattern(pattern = SHAR::species_a,
                                             n_random = 19,
                                             max_runs = 100)

  expect_length(pattern_recon, n = 20)
})

test_that("Output includes randomizations and original pattern for reconstruct_pattern", {

  pattern_recon <- SHAR::reconstruct_pattern(pattern = SHAR::species_b,
                                             n_random = 19,
                                             max_runs = 100)

  expect_named(pattern_recon, expected = c(paste0("randomized_", 1:19), "observed"))

  expect_equal(pattern_recon[[20]], expected = SHAR::species_b)

})

test_that("All optional arguments can be used for reconstruct_pattern", {

  pattern_recon <- SHAR::reconstruct_pattern(pattern = SHAR::species_a,
                                             n_random = 39,
                                             max_runs = 50,
                                             fitting = TRUE,
                                             comp_fast = TRUE)

  expect_type(pattern_recon, type = "list")
})

test_that("Input pattern can not be returned for reconstruct_pattern", {

  pattern_recon <- SHAR::reconstruct_pattern(pattern = SHAR::species_b,
                                             n_random = 19,
                                             max_runs = 100,
                                             return_input = FALSE)

  expect_named(pattern_recon, expected = paste0("randomized_", 1:19))

  expect_false(any(SHAR::species_b %in% pattern_recon))
})
