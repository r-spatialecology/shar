# context("test-fit_point_process")

# normal
pattern_random <- fit_point_process(pattern = species_b, n_random = 3,
                                    verbose = FALSE)

# cluster process
pattern_random_cluster <- fit_point_process(pattern = species_b, n_random = 3,
                                            process = "cluster", verbose = FALSE)

# no input
pattern_random_ni <- fit_point_process(pattern = species_a, n_random = 3,
                                       return_input = FALSE, verbose = FALSE)

# simple output
pattern_random_simple <- fit_point_process(pattern = species_a, n_random = 1,
                                           return_input = FALSE, simplify = TRUE,
                                           verbose = FALSE)

# return parameters
pattern_random_para_a <- fit_point_process(pattern = species_b, n_random = 3,
                                           process = "poisson", return_para = TRUE,
                                           verbose = FALSE)

# return parameters
pattern_random_para_b <- fit_point_process(pattern = species_b, n_random = 3,
                                           process = "cluster", return_para = TRUE,
                                           verbose = FALSE)

################################################################################

test_that("Output is a long as n_random for fit_point_process", {

  expect_length(pattern_random$randomized, n = 3)

  expect_length(pattern_random_cluster$randomized, n = 3)

})

test_that("Output includes randomizations and original pattern for fit_point_process", {

  expect_named(pattern_random$randomized, expected = paste0("randomized_", 1:3))

  expect_equal(pattern_random$observed, expected = spatstat.geom::unmark(species_b))

  expect_named(pattern_random_cluster$randomized, expected = paste0("randomized_", 1:3))

  expect_equal(pattern_random_cluster$observed, expected = spatstat.geom::unmark(species_b))

})

test_that("Fitted patterns have same number of points for cluster process", {

  expect_true(all(vapply(pattern_random$randomized, FUN.VALUE = logical(1),
                         function(x) x$n == species_b$n)))

  expect_true(all(vapply(pattern_random_cluster$randomized, FUN.VALUE = logical(1),
                         function(x) x$n == species_b$n)))

})

test_that("Input pattern can not be returned for fit_point_process", {

  expect_true(object = is.na(pattern_random_ni$observed))

})

test_that("simplify works for fit_point_process", {

  expect_s3_class(pattern_random_simple, "ppp")

})

test_that("Parameters can be returned", {

  expect_named(object = pattern_random_para_a$para, expected = c("number_points", "lambda"))

  expect_named(object = pattern_random_para_b$para,
               expected = c("number_parents", "number_points", "cluster_area"))

  expect_true(object = is.na(pattern_random$param))

})

test_that("fit_point_process returns errors", {

  expect_error(fit_point_process(pattern = species_b, n_random = -10, verbose = FALSE),
               regexp = "n_random must be >= 1.")

  expect_error(fit_point_process(pattern = species_b, n_random = 19, process = "not_valid",
                                 verbose = FALSE),
               regexp = "Please select either 'poisson' or 'cluster'.")

})

test_that("fit_point_process returns warnings", {

  expect_warning(fit_point_process(pattern = species_a, n_random = 3, return_input = FALSE,
                                   simplify = TRUE, verbose = FALSE),
                 regexp = "'simplify = TRUE' not possible for 'n_random > 1'.")

  expect_warning(fit_point_process(pattern = species_a, n_random = 1, simplify = TRUE,
                                   verbose = FALSE),
                 regexp = "'simplify = TRUE' not possible for 'return_input = TRUE'.")

})
