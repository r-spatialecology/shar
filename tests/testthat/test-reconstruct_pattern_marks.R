testthat::context("test-reconstruct_pattern_marks")

pattern_recon <- shar::reconstruct_pattern_homo(shar::species_a,
                                                n_random = 1,
                                                return_input = FALSE,
                                                simplify = TRUE,
                                                max_runs = 1,
                                                verbose = FALSE)

marks_sub <- spatstat.geom::subset.ppp(shar::species_a, select = dbh)

# normal reconstruction
marks_recon <- shar::reconstruct_pattern_marks(pattern = pattern_recon,
                                               marked_pattern = marks_sub,
                                               n_random = 3,
                                               max_runs = 1,
                                               verbose = FALSE)

# no input
marks_recon_ni <- shar::reconstruct_pattern_marks(pattern = pattern_recon,
                                                  marked_pattern = marks_sub,
                                                  n_random = 3,
                                                  max_runs = 1,
                                                  return_input = FALSE,
                                                  verbose = FALSE)

# simplify = TRUE
marks_recon_simple <- shar::reconstruct_pattern_marks(pattern = pattern_recon,
                                                      marked_pattern = marks_sub,
                                                      n_random = 1,
                                                      max_runs = 1,
                                                      return_input = FALSE,
                                                      simplify = TRUE,
                                                      verbose = FALSE)

marks_recon_energy <- shar::reconstruct_pattern_marks(pattern = pattern_recon,
                                                      marked_pattern = marks_sub,
                                                      n_random = 3,
                                                      e_threshold = 0.1,
                                                      verbose = FALSE)

################################################################################

testthat::test_that("Output is a long as n_random for reconstruct_pattern_marks", {

  testthat::expect_is(marks_recon, class = "rd_mar")

  testthat::expect_type(marks_recon$randomized, type = "list")

  testthat::expect_length(marks_recon$randomized, n = 3)
})

testthat::test_that("Output includes randomizations and original pattern for reconstruct_pattern_marks", {

  testthat::expect_named(marks_recon$randomized,
                         expected = paste0("randomized_", c(1:3)))

  testthat::expect_equal(marks_recon$observed, expected = marks_sub)
})

testthat::test_that("Input pattern can not be returned for reconstruct_pattern_marks", {

  testthat::expect_equal(object = marks_recon_ni$observed,
                         expected = "NA")
})

testthat::test_that("Only pattern can be returned for simplify = TRUE", {

  testthat::expect_is(marks_recon_simple, "ppp")
})

testthat::test_that("Reconstruction stops if e_threshold is reached", {

  energy <- shar::calculate_energy(marks_recon_energy, verbose = FALSE)

  testthat::expect_true(all(energy < 0.1 & energy > 0.01))

  testthat::expect_true(all(marks_recon_energy$stop_criterion == "e_threshold/no_change"))
})

testthat::test_that("All errors are returned for reconstruct_pattern_marks", {

  testthat::expect_error(shar::reconstruct_pattern_marks(pattern = pattern_recon,
                                                         marked_pattern = marks_sub,
                                                         n_random = -5,
                                                         max_runs = 1,
                                                         verbose = FALSE),
                         regexp = "n_random must be >= 1.",
                         fixed = TRUE)

  testthat::expect_error(shar::reconstruct_pattern_marks(pattern = pattern_recon,
                                                         marked_pattern = pattern_recon,
                                                         n_random = 3,
                                                         max_runs = 1,
                                                         verbose = FALSE),
                         regexp = "'pattern' must be unmarked and 'marked_pattern' marked",
                         fixed = TRUE)

  testthat::expect_error(shar::reconstruct_pattern_marks(pattern = marks_sub,
                                                         marked_pattern = marks_sub,
                                                         n_random = 3,
                                                         max_runs = 1,
                                                         verbose = FALSE),
                         regexp = "'pattern' must be unmarked and 'marked_pattern' marked",
                         fixed = TRUE)

  testthat::expect_error(shar::reconstruct_pattern_marks(pattern = spatstat.geom::unmark(shar::species_b),
                                                         marked_pattern = marks_sub,
                                                         n_random = 3,
                                                         max_runs = 1,
                                                         verbose = FALSE),
                         regexp = "'pattern' and 'pattern' must have same window and number of points",
                         fixed = TRUE)


  testthat::expect_error(shar::reconstruct_pattern_marks(pattern = pattern_recon,
                                                         marked_pattern = spatstat.geom::subset.ppp(shar::species_a,
                                                                                               select = status),
                                                         n_random = 3, max_runs = 1),
                         regexp = "marks must be 'numeric'",
                         fixed = TRUE)
})

testthat::test_that("All warnings are returned for reconstruct_pattern_marks", {

  testthat::expect_warning(shar::reconstruct_pattern_marks(pattern = pattern_recon,
                                                           marked_pattern = marks_sub,
                                                           n_random = 2, max_runs = 1,
                                                           return_input = FALSE,
                                                           simplify = TRUE),
                           regexp = "'simplify = TRUE' not possible for 'n_random > 1'",
                           fixed = TRUE)

  testthat::expect_warning(shar::reconstruct_pattern_marks(pattern = pattern_recon,
                                                           marked_pattern = marks_sub,
                                                           n_random = 1, max_runs = 1,
                                                           simplify = TRUE),
                           regexp = "'simplify = TRUE' not possible for 'return_input = TRUE'",
                           fixed = TRUE)
})
