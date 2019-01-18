testthat::context("test-calculate_energy")

testthat::test_that("calculate_energy returns energy for all randomizations", {

  pattern_random <- shar::fit_point_process(pattern = shar::species_a,
                                            n_random = 19)

  testthat::expect_length(shar::calculate_energy(pattern_random), n = 19)
})

testthat::test_that("calculate_energy returns mean ", {

  pattern_random <- shar::fit_point_process(pattern = shar::species_a,
                                            n_random = 19)

  mean_energy <- mean(shar::calculate_energy(pattern_random))

  testthat::expect_equal(shar::calculate_energy(pattern_random,
                                                return_mean = TRUE),
                         expected = mean_energy)
})

testthat::test_that("calculate_energy can use comp_fast = TRUE ", {

  pattern_random <- shar::fit_point_process(pattern = shar::species_a,
                                            n_random = 19)

  expect_length(shar::calculate_energy(pattern_random,
                                       comp_fast = TRUE), n = 19)
})

testthat::test_that("calculate_energy returns error if observed not included ", {

  pattern_random <- shar::fit_point_process(pattern = shar::species_a,
                                            n_random = 19,
                                            return_input = FALSE)

  testthat::expect_error(shar::calculate_energy(pattern_random),
                         regexp = "Input must named 'randomized_1' to 'randomized_n' and includ 'observed' pattern.")
})
