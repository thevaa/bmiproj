context("odds ratio")
test_that("the 2x2 odds ratio is", {
  expect_equal(odds.ratio(c(5,5,5,5))$odds, 1)
  expect_equal(odds.ratio(c(10,5,5,10))$odds, 4)
  expect_equal(odds.ratio(c(400, 400, 400, 400), 0.90), c(0.8483302, 1.1787863))
  expect_error(odds.ratio(c(50, NA, "c", 3)))
})
