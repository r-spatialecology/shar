# context("test-reconstruct_pattern_marks")

pattern_recon <- reconstruct_pattern(species_a, n_random = 1, return_input = FALSE,
                                     simplify = TRUE, max_runs = 1,
                                     verbose = FALSE)

marks_sub <- spatstat.geom::subset.ppp(species_a, select = dbh)

# normal reconstruction
marks_recon <- reconstruct_pattern_marks(pattern = pattern_recon, marked_pattern = marks_sub,
                                         n_random = 3, max_runs = 1, verbose = FALSE)

# no input
marks_recon_ni <- reconstruct_pattern_marks(pattern = pattern_recon, marked_pattern = marks_sub,
                                            n_random = 3, max_runs = 1,
                                            return_input = FALSE, verbose = FALSE)

# simplify = TRUE
marks_recon_simple <- reconstruct_pattern_marks(pattern = pattern_recon, marked_pattern = marks_sub,
                                                n_random = 1, max_runs = 1,
                                                return_input = FALSE, simplify = TRUE,
                                                verbose = FALSE)

marks_recon_energy <- reconstruct_pattern_marks(pattern = pattern_recon, marked_pattern = marks_sub,
                                                n_random = 3, e_threshold = 0.1,
                                                verbose = FALSE)

pattern_recon_empty <- pattern_recon[-c(1:pattern_recon$n)]

################################################################################

test_that("Output is a long as n_random for reconstruct_pattern_marks", {

  expect_s3_class(marks_recon, class = "rd_mar")

  expect_type(marks_recon$randomized, type = "list")

  expect_length(marks_recon$randomized, n = 3)

})

test_that("Output includes randomizations and original pattern for reconstruct_pattern_marks", {

  expect_named(marks_recon$randomized, expected = paste0("randomized_", c(1:3)))

  expect_equal(marks_recon$observed, expected = marks_sub)

})

test_that("Input pattern can not be returned for reconstruct_pattern_marks", {

  expect_true(object = is.na(marks_recon_ni$observed))

})

test_that("Only pattern can be returned for simplify = TRUE", {

  expect_s3_class(marks_recon_simple, "ppp")

})

test_that("Reconstruction stops if e_threshold is reached", {

  energy <- calculate_energy(marks_recon_energy, verbose = FALSE)

  expect_true(all(energy < 0.1 & energy > 0.01))

  expect_true(all(marks_recon_energy$stop_criterion %in% c("e_threshold", "no_change")))

})

test_that("All errors are returned for reconstruct_pattern_marks", {

  expect_error(reconstruct_pattern_marks(pattern = pattern_recon, marked_pattern = marks_sub,
                                         n_random = -5, max_runs = 1, verbose = FALSE),
               regexp = "n_random must be >= 1.")

  expect_error(reconstruct_pattern_marks(pattern = pattern_recon, marked_pattern = pattern_recon,
                                         n_random = 3, max_runs = 1, verbose = FALSE),
               regexp = "'pattern' must be unmarked and 'marked_pattern' marked")

  expect_error(reconstruct_pattern_marks(pattern = marks_sub, marked_pattern = marks_sub,
                                         n_random = 3, max_runs = 1, verbose = FALSE),
               regexp = "'pattern' must be unmarked and 'marked_pattern' marked")

  expect_error(reconstruct_pattern_marks(pattern = pattern_recon,
                                         marked_pattern = spatstat.geom::subset.ppp(species_a,
                                                                                    select = status),
                                         n_random = 3, max_runs = 1),
               regexp = "marks must be 'numeric'")

  expect_error(reconstruct_pattern_marks(pattern = pattern_recon_empty, marked_pattern = marks_sub,
                                         verbose = FALSE),
               regexp = "At least one of the observed patterns contain no points.")

})

test_that("All warnings are returned for reconstruct_pattern_marks", {

  expect_warning(reconstruct_pattern_marks(pattern = pattern_recon, marked_pattern = marks_sub,
                                           n_random = 2, max_runs = 1, return_input = FALSE,
                                           simplify = TRUE, verbose = FALSE),
                 regexp = "'simplify = TRUE' not possible for 'n_random > 1'")

  expect_warning(reconstruct_pattern_marks(pattern = pattern_recon, marked_pattern = marks_sub,
                                           n_random = 1, max_runs = 1, simplify = TRUE,
                                           verbose = FALSE),
                 regexp = "'simplify = TRUE' not possible for 'return_input = TRUE'")

})
