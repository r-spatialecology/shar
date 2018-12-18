context("test-calculate_energy")

test_that("calculate_energy returns energy for all randomizations", {

  pattern_random <- SHAR::fit_point_process(pattern = SHAR::species_a,
                                            n_random = 19)

  expect_length(calculate_energy(pattern_random), n = 19)
})

test_that("calculate_energy returns mean ", {

  pattern_random <- SHAR::fit_point_process(pattern = SHAR::species_a,
                                            n_random = 19)

  mean_energy <- mean(calculate_energy(pattern_random))

  expect_equal(calculate_energy(pattern_random, return_mean = TRUE),
               expected = mean_energy)
})

test_that("calculate_energy can use comp_fast = TRUE ", {

  pattern_random <- SHAR::fit_point_process(pattern = SHAR::species_a,
                                            n_random = 19)

  expect_length(calculate_energy(pattern_random, comp_fast = TRUE), n = 19)
})

test_that("calculate_energy returns error if observed not included ", {

  pattern_random <- SHAR::fit_point_process(pattern = SHAR::species_a,
                                            n_random = 19,
                                            return_input = FALSE)

  expect_error(calculate_energy(pattern_random),
               regexp = "Input must named 'randomized_1' to 'randomized_n' and includ 'observed' pattern.")
})
