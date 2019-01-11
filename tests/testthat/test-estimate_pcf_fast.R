testthat::context("test-estimate_pcf_fast")

testthat::test_that("estimate_pcf returns spatstat.fv object", {

  pcf <- SHAR::estimate_pcf_fast(pattern = SHAR::species_b)

  testthat::expect_is(pcf, "fv")
})
