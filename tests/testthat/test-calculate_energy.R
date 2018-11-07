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
