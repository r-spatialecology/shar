testthat::context("test-print")

pattern_reconstruction <- shar::reconstruct_pattern_homo(pattern = shar::species_a,
                                                         n_random = 3,
                                                         max_runs = 1,
                                                         verbose = FALSE)

pattern_fitted <- shar::fit_point_process(pattern = shar::species_a,
                                          n_random = 3,
                                          verbose = FALSE)

marks_sub <- spatstat.geom::subset.ppp(shar::species_a, select = dbh)

marks_reconstruction <- shar::reconstruct_pattern_marks(pattern = pattern_reconstruction$randomized[[1]],
                                                        marked_pattern = marks_sub,
                                                        n_random = 3,
                                                        max_runs = 1,
                                                        verbose = FALSE)

# random landscape
landscape_classified <- shar::classify_habitats(raster = shar::landscape,
                                                classes = 5)

landscape_random <-  shar::translate_raster(raster = landscape_classified,
                                            steps_x = 1, steps_y = 1,
                                            verbose = FALSE,
                                            return_input = FALSE)

################################################################################

testthat::test_that("print.rd_pat works", {

  testthat::expect_output(print(pattern_reconstruction))
  testthat::expect_output(print(pattern_fitted))
})

testthat::test_that("print.rd_mar works", {

  testthat::expect_output(print(marks_reconstruction))
})

testthat::test_that("print.rd_ras works", {

  testthat::expect_output(print(landscape_random))
})
