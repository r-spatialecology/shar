# context("test-classint_to_vector")

x <- classInt::classIntervals(var = stats::runif(n = 100), style = "fisher", n = 5)

################################################################################

test_that("classint_to_vector returns n breaks", {

  present_classes <- classint_to_vector(x = x, digits = 3)

  expect_length(present_classes, n = 5)

})
