testthat::context("reconstruct_marks")

pattern_recon <- SHAR::reconstruct_pattern(SHAR::species_a, n_random = 1,
                                           max_runs = 10)[[1]]
marks_sub <- spatstat::subset.ppp(SHAR::species_a, select = dbh)

testthat::test_that("Output is a long as n_random for reconstruct_marks", {

  marks_recon <- SHAR::reconstruct_marks(pattern = pattern_recon,
                                         marked_pattern = marks_sub,
                                         n_random = 3, max_runs = 10)

  testthat::expect_type(marks_recon, type = "list")
  testthat::expect_length(marks_recon, n = 4)
})

testthat::test_that("Output includes randomizations and original pattern for reconstruct_marks", {

  marks_recon <- SHAR::reconstruct_marks(pattern = pattern_recon,
                                         marked_pattern = marks_sub,
                                         n_random = 3, max_runs = 10)

  testthat::expect_named(marks_recon,
                         expected = c(paste0("randomized_", c(1: 3)), "observed"))

  testthat::expect_equal(marks_recon[[4]], expected = marks_sub)
})

testthat::test_that("Input pattern can not be returned for reconstruct_marks", {

  marks_recon <- SHAR::reconstruct_marks(pattern = pattern_recon,
                                         marked_pattern = marks_sub,
                                         n_random = 3, max_runs = 10,
                                         return_input = FALSE)

  testthat::expect_false(any(marks_sub %in% marks_recon))
})

testthat::test_that("Only pattern can be returned for simplify = TRUE", {

  marks_recon <- SHAR::reconstruct_marks(pattern = pattern_recon,
                                         marked_pattern = marks_sub,
                                         n_random = 1, max_runs = 10,
                                         return_input = FALSE, simplify = TRUE)

  testthat::expect_is(marks_recon, "ppp")
})

testthat::test_that("All optional arguments can be used for reconstruct_marks", {

  marks_recon <- SHAR::reconstruct_marks(pattern = pattern_recon,
                                         marked_pattern = marks_sub,
                                         n_random = 3, max_runs = 10,
                                         plot = TRUE)


  testthat::expect_type(marks_recon, type = "list")
  testthat::expect_length(marks_recon, n = 4)
})

testthat::test_that("All errors are returned for reconstruct_marks", {

  testthat::expect_error(SHAR::reconstruct_marks(pattern = pattern_recon,
                                                 marked_pattern = marks_sub,
                                                 n_random = -5, max_runs = 10),
                         regexp = "n_random must be >= 1.")

  testthat::expect_error(SHAR::reconstruct_marks(pattern = pattern_recon,
                                                 marked_pattern = pattern_recon,
                                                 n_random = 3, max_runs = 10),
                         regexp = "'pattern' must be unmarked and 'marked_pattern' marked")

  testthat::expect_error(SHAR::reconstruct_marks(pattern = marks_sub,
                                                 marked_pattern = marks_sub,
                                                 n_random = 3, max_runs = 10),
                         regexp = "'pattern' must be unmarked and 'marked_pattern' marked")

  testthat::expect_error(SHAR::reconstruct_marks(pattern = spatstat::unmark(SHAR::species_b),
                                                 marked_pattern = marks_sub,
                                                 n_random = 3, max_runs = 10),
                         regexp = "'pattern' and 'pattern' must have same window and number of points")


  testthat::expect_error(SHAR::reconstruct_marks(pattern = pattern_recon,
                                                 marked_pattern = spatstat::subset.ppp(SHAR::species_a,
                                                                                       select = status),
                                                 n_random = 3, max_runs = 10),
                         regexp = "marks must be 'numeric'")
})

test_that("All warnings are returned for reconstruct_marks", {

  expect_warning(SHAR::reconstruct_marks(pattern = pattern_recon,
                                         marked_pattern = marks_sub,
                                         n_random = 2, max_runs = 10,
                                         return_input = FALSE,
                                         simplify = TRUE,
                                         verbose = TRUE),
                 regexp = "'simplify = TRUE' not possible for 'n_random > 1'")

  expect_warning(SHAR::reconstruct_marks(pattern = pattern_recon,
                                         marked_pattern = marks_sub,
                                         n_random = 1, max_runs = 10,
                                         simplify = TRUE,
                                         verbose = TRUE),
                 regexp = "'simplify = TRUE' not possible for 'return_input = TRUE'")
})
