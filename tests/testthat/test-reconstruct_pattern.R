context("reconstruct_pattern")

pattern_recon <- SHAR::reconstruct_pattern(pattern = SHAR::species_b,
                                           n_random = 2,
                                           max_runs = 10)

test_that("Output is a long as n_random for reconstruct_pattern", {

  expect_length(pattern_recon, n = 3)
})

test_that("Output includes randomizations and original pattern for reconstruct_pattern", {

  expect_named(pattern_recon, expected = c(paste0("randomized_", c(1,2)), "observed"))

  expect_equal(pattern_recon[[3]], expected = SHAR::species_b)
})

# test_that("All optional arguments can be used for reconstruct_pattern", {
#
#   pattern_recon <- SHAR::reconstruct_pattern(pattern = SHAR::species_a,
#                                              n_random = 2,
#                                              max_runs = 10,
#                                              fitting = TRUE,
#                                              comp_fast = TRUE)
#
#   expect_type(pattern_recon, type = "list")
# })

test_that("Input pattern can not be returned for reconstruct_pattern", {

  pattern_recon <- SHAR::reconstruct_pattern(pattern = SHAR::species_b,
                                             n_random = 2,
                                             max_runs = 10,
                                             return_input = FALSE)

  expect_false(any(SHAR::species_b %in% pattern_recon))
})
