testthat::context("test-plot_randomized_pattern")

pattern_random_a <- shar::fit_point_process(shar::species_a, n_random = 3,
                                            verbose = FALSE)

pattern_random_b <- shar::fit_point_process(shar::species_b,
                                            n_random = 3,
                                            return_input = FALSE,
                                            verbose = FALSE)

pattern_random_marks <- pattern_random_a[[1]]

marks_sub <- spatstat::subset.ppp(species_a, select = dbh)

marks_recon <- shar::reconstruct_marks(pattern = pattern_random_marks,
                                       marked_pattern = marks_sub,
                                       n_random = 3, max_runs = 1,
                                       verbose = FALSE)

testthat::test_that("plot_randomized_pattern returns plot", {

  shar::plot_randomized_pattern(pattern_random_a,
                                verbose = FALSE,
                                ask = FALSE)

  shar::plot_randomized_pattern(pattern_random_a, what = "pp",
                                verbose = FALSE,
                                ask = FALSE)
})

testthat::test_that("plot_randomized_pattern returns error if observed is missing", {

  testthat::expect_error(shar::plot_randomized_pattern(pattern_random_b,
                                                       verbose = FALSE),
                         grep = "Input must include 'observed' pattern.",
                         fixed = TRUE)
})

testthat::test_that("plot_randomized_pattern uses comp_fast", {

  shar::plot_randomized_pattern(pattern_random_a,
                                comp_fast = 50,
                                verbose = FALSE)
})

testthat::test_that("plot_randomized_patterns works for reconstructed marks", {

  shar::plot_randomized_pattern(marks_recon,
                                verbose = FALSE)
})

testthat::test_that("plot_randomized_patterns returns error if what is wrong", {

  testthat::expect_error(shar::plot_randomized_pattern(marks_recon,
                                                       what = "wrong",
                                                       verbose = FALSE),
                         grep = "Please select either what = 'sf' or what = 'pp'.",
                         fixed = TRUE)
})

testthat::test_that("plot_randomized_patterns returns error if wrong class ", {

  testthat::expect_error(shar::plot_randomized_pattern(list(shar::species_a,
                                                            shar::species_b),
                                                       verbose = FALSE),
                         grep = "Class of 'pattern' must be 'rd_pat' or 'rd_mar'.",
                         fixed = TRUE)
})
