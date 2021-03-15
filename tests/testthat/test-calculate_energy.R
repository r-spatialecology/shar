testthat::context("test-calculate_energy")

pattern_random_a <- shar::fit_point_process(pattern = shar::species_a,
                                            n_random = 3, verbose = FALSE)

pattern_random_b <- shar::fit_point_process(pattern = shar::species_b,
                                            n_random = 3,
                                            return_input = FALSE, verbose = FALSE)

marks_sub <- spatstat.geom::subset.ppp(species_a, select = dbh)

marks_recon <- shar::reconstruct_pattern_marks(pattern_random_a$randomized[[1]],
                                               marks_sub,
                                               n_random = 3, max_runs = 10,
                                               verbose = FALSE)

marks_recon_NA  <- marks_recon

marks_recon_NA$energy_df <- "NA"

################################################################################

testthat::test_that("calculate_energy returns energy for all randomizations", {

  testthat::expect_length(shar::calculate_energy(pattern_random_a, verbose = FALSE),
                          n = 3)
})

testthat::test_that("calculate_energy uses weights", {

  unweighted <- shar::calculate_energy(pattern_random_a,
                                       return_mean = TRUE,
                                       verbose = FALSE)

  weighted <- shar::calculate_energy(pattern_random_a,
                                     weights = c(0, 1),
                                     return_mean = TRUE,
                                     verbose = FALSE)

  testthat::expect_false(unweighted == weighted)
})


testthat::test_that("calculate_energy returns mean ", {

  mean_energy <- mean(shar::calculate_energy(pattern_random_a, verbose = FALSE))

  testthat::expect_equal(shar::calculate_energy(pattern_random_a,
                                                return_mean = TRUE,
                                                verbose = FALSE),
                         expected = mean_energy)
})

testthat::test_that("calculate_energy can use comp_fast ", {

  testthat::expect_length(shar::calculate_energy(pattern_random_a,
                                                 comp_fast = 50,
                                                 verbose = FALSE),
                          n = 3)
})

testthat::test_that("calculate_energy returns works for reconstructed marks", {

  testthat::expect_length(shar::calculate_energy(marks_recon, verbose = FALSE),
                          n = 3)

  mean_energy <- mean(shar::calculate_energy(marks_recon, verbose = FALSE))

  testthat::expect_equal(shar::calculate_energy(marks_recon,
                                                return_mean = TRUE,
                                                verbose = FALSE),
                         expected = mean_energy)

  testthat::expect_equal(shar::calculate_energy(marks_recon_NA,
                                                return_mean = TRUE,
                                                verbose = FALSE),
                         expected = mean_energy)
  })

testthat::test_that("calculate_energy returns error if observed not included", {

  testthat::expect_error(shar::calculate_energy(pattern_random_b, verbose = FALSE),
                         regexp = "Input must include 'observed' pattern.",
                         fixed = TRUE)
})

testthat::test_that("calculate_energy returns error if wrong class ", {

  testthat::expect_error(shar::calculate_energy(list(shar::species_a,
                                                     shar::species_b), verbose = FALSE),
                         regexp = "Class of 'pattern' must be 'rd_pat' or 'rd_mar'.",
                         fixed = TRUE)
})

testthat::test_that("calculate_energy returns error if weights are wrong ", {

  testthat::expect_error(shar::calculate_energy(pattern_random_a,
                                                weights = c(0, 0),
                                                return_mean = TRUE,
                                                verbose = FALSE),
                         regexp = "The sum of 'weights' must be 0 < sum(weights) <= 1.",
                         fixed = TRUE)

  testthat::expect_error(shar::calculate_energy(pattern_random_a,
                                                weights = c(1, 1),
                                                return_mean = TRUE,
                                                verbose = FALSE),
                         regexp = "The sum of 'weights' must be 0 < sum(weights) <= 1.",
                         fixed = TRUE)
})
