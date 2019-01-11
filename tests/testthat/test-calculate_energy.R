testthat::context("test-calculate_energy")

testthat::test_that("calculate_energy returns energy for all randomizations", {

  pattern_random <- SHAR::fit_point_process(pattern = SHAR::species_a,
                                            n_random = 19)

  testthat::expect_length(SHAR::calculate_energy(pattern_random), n = 19)
})

testthat::test_that("calculate_energy returns mean ", {

  pattern_random <- SHAR::fit_point_process(pattern = SHAR::species_a,
                                            n_random = 19)

  mean_energy <- mean(SHAR::calculate_energy(pattern_random))

  testthat::expect_equal(SHAR::calculate_energy(pattern_random,
                                                return_mean = TRUE),
                         expected = mean_energy)
})

testthat::test_that("calculate_energy can use comp_fast = TRUE ", {

  pattern_random <- SHAR::fit_point_process(pattern = SHAR::species_a,
                                            n_random = 19)

  expect_length(SHAR::calculate_energy(pattern_random,
                                       comp_fast = TRUE), n = 19)
})

testthat::test_that("calculate_energy returns error if observed not included ", {

  pattern_random <- SHAR::fit_point_process(pattern = SHAR::species_a,
                                            n_random = 19,
                                            return_input = FALSE)

  testthat::expect_error(SHAR::calculate_energy(pattern_random),
                         regexp = "Input must named 'randomized_1' to 'randomized_n' and includ 'observed' pattern.")
})
