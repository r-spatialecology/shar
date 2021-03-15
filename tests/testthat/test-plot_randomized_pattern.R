testthat::context("test-plot_randomized_pattern")

pattern_random <- shar::fit_point_process(shar::species_a,
                                          n_random = 3,
                                          verbose = FALSE)

pattern_random_ni <- shar::fit_point_process(shar::species_b,
                                             n_random = 3,
                                             return_input = FALSE,
                                             verbose = FALSE)

pattern_random_marks <- pattern_random$randomized[[1]]

marks_sub <- spatstat.geom::subset.ppp(species_a, select = dbh)

marks_recon <- shar::reconstruct_pattern_marks(pattern = pattern_random_marks,
                                               marked_pattern = marks_sub,
                                               n_random = 3, max_runs = 1,
                                               verbose = FALSE)

################################################################################

testthat::test_that("plot_randomized_pattern returns plot", {

  testthat::expect_null(shar::plot_randomized_pattern(pattern_random,
                                                      verbose = FALSE,
                                                      ask = FALSE))

  testthat::expect_null(shar::plot_randomized_pattern(pattern_random, what = "pp",
                                                      verbose = FALSE,
                                                      ask = FALSE))
})

testthat::test_that("plot_randomized_pattern returns error if observed is missing", {

  testthat::expect_error(shar::plot_randomized_pattern(pattern_random_ni,
                                                       verbose = FALSE),
                         regexp = "Input must include 'observed' pattern.",
                         fixed = TRUE)
})

testthat::test_that("plot_randomized_pattern uses comp_fast", {

  testthat::expect_null(shar::plot_randomized_pattern(pattern_random,
                                                      comp_fast = 50,
                                                      verbose = FALSE,
                                                      ask = FALSE))
})

testthat::test_that("plot_randomized_patterns works for reconstructed marks", {

  testthat::expect_null(shar::plot_randomized_pattern(marks_recon,
                                                      verbose = FALSE))
})

testthat::test_that("plot_randomized_patterns returns error if what is wrong", {

  testthat::expect_error(shar::plot_randomized_pattern(pattern_random,
                                                       what = "wrong",
                                                       verbose = FALSE),
                         regexp = "Please select either what = 'sf' or what = 'pp'.",
                         fixed = TRUE)
})

testthat::test_that("plot_randomized_patterns returns error if wrong class ", {

  testthat::expect_error(shar::plot_randomized_pattern(list(shar::species_a,
                                                            shar::species_b),
                                                       verbose = FALSE),
                         regexp = "Class of 'pattern' must be 'rd_pat' or 'rd_mar'.",
                         fixed = TRUE)
})
