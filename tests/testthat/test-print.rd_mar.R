testthat::context("test-print.rd_mar")

pattern_recon <- shar::reconstruct_pattern(pattern = shar::species_a,
                                           n_random = 1,
                                           max_runs = 10,
                                           return_input = FALSE,
                                           simplify = TRUE,
                                           verbose = FALSE)

marks_recon <- shar::reconstruct_marks(pattern = pattern_recon,
                                       marked_pattern = spatstat::subset.ppp(shar::species_a,
                                                                             select = dbh),
                                       n_random = 3,
                                       max_runs = 10,
                                       verbose = FALSE)

energy_recon <- round(shar::calculate_energy(marks_recon,
                                             return_mean = TRUE,
                                             verbose = FALSE),
                      digits = 4)

################################################################################

testthat::test_that("print.rd_mar works for reconstructed pattern", {

  testthat::skip("skip")

  output <- testthat::capture_output(shar::print.rd_mar(marks_recon))

  testthat::expect_equal(object = output,
                         expected = paste0("No. of pattern: 4\nMean energy: ",
                                           energy_recon,
                                           "\nMethod: reconstruct_marks()\nObserved pattern: included\nIterations (mean): 10"))
})
