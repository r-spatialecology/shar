# context("test-print")

pattern_reconstruction <- reconstruct_pattern(pattern = species_a, n_random = 3, max_runs = 1,
                                              verbose = FALSE)

pattern_fitted <- fit_point_process(pattern = species_a, n_random = 3,
                                    verbose = FALSE)

marks_sub <- spatstat.geom::subset.ppp(species_a, select = dbh)

marks_reconstruction <- reconstruct_pattern_marks(pattern = pattern_reconstruction$randomized[[1]],
                                                  marked_pattern = marks_sub,
                                                  n_random = 3, max_runs = 1,
                                                  verbose = FALSE)

# random landscape
landscape_classified <- classify_habitats(raster = terra::rast(landscape), n = 5, style = "fisher")

landscape_random <- translate_raster(raster = landscape_classified,
                                     steps_x = 1, steps_y = 1, verbose = FALSE,
                                     return_input = FALSE)

################################################################################

test_that("print.rd_pat works", {

  expect_output(print(pattern_reconstruction))
  expect_output(print(pattern_fitted))

})

test_that("print.rd_mar works", {

  expect_output(print(marks_reconstruction))

})

test_that("print.rd_ras works", {

  expect_output(print(landscape_random))

})
