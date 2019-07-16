testthat::context("test-print.rd_pat")

pattern_random <- shar::fit_point_process(pattern = shar::species_a,
                                          n_random = 3,
                                          verbose = FALSE)

energy_random <- round(shar::calculate_energy(pattern_random,
                                        return_mean = TRUE,
                                        verbose = FALSE),
                       digits = 4)

pattern_recon <- shar::reconstruct_pattern(pattern = shar::species_a,
                                           n_random = 3,
                                           max_runs = 10,
                                           verbose = FALSE)

energy_recon <- round(shar::calculate_energy(pattern_recon,
                                             return_mean = TRUE,
                                             verbose = FALSE),
                       digits = 4)

################################################################################

testthat::test_that("print.rd_pat works for fitted pattern", {

  testthat::skip("skip")

  output <- testthat::capture_output(shar::print.rd_pat(pattern_random))

  testthat::expect_equal(object = output,
                         expected = paste0("No. of pattern: 4\nMean energy: ",
                                           energy_random,
                                           "\nMethod: fit_point_process()\nObserved pattern: included\nIterations (mean): NA"))
})


testthat::test_that("print.rd_pat works for reconstructed pattern", {

  testthat::skip("skip")

  output <- testthat::capture_output(shar::print.rd_pat(pattern_recon))

  testthat::expect_equal(object = output,
                         expected = paste0("No. of pattern: 4\nMean energy: ",
                                           energy_recon,
                                           "\nMethod: reconstruct_pattern()\nObserved pattern: included\nIterations (mean): 10"))
})
