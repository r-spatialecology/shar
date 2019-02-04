testthat::context("test-calculate_energy")

pattern_random_a <- shar::fit_point_process(pattern = shar::species_a,
                                            n_random = 3)

pattern_random_b <- shar::fit_point_process(pattern = shar::species_b,
                                            n_random = 3,
                                            return_input = FALSE)

testthat::test_that("calculate_energy returns energy for all randomizations", {

  testthat::expect_length(shar::calculate_energy(pattern_random_a),
                          n = 3)
})

testthat::test_that("calculate_energy returns mean ", {

  mean_energy <- mean(shar::calculate_energy(pattern_random_a))

  testthat::expect_equal(shar::calculate_energy(pattern_random_a,
                                                return_mean = TRUE),
                         expected = mean_energy)
})

testthat::test_that("calculate_energy can use comp_fast ", {

  expect_length(shar::calculate_energy(pattern_random_a,
                                       comp_fast = 50), n = 3)
})

testthat::test_that("calculate_energy returns error if observed not included ", {

  testthat::expect_error(shar::calculate_energy(pattern_random_b),
                         regexp = "Input must named 'randomized_1' to 'randomized_n' and includ 'observed' pattern.")
})
