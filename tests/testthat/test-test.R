test_that("myncurve function works as expected", {

  result = capture.output(myncurve(mu = 0, sigma = 1, a = 2))
  expect_match(result, "Area =")

})
