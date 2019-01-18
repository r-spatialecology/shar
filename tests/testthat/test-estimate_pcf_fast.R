testthat::context("test-estimate_pcf_fast")

pcf <- shar::estimate_pcf_fast(pattern = shar::species_b)

testthat::test_that("estimate_pcf returns spatstat.fv object", {
  testthat::expect_is(pcf, "fv")
})
