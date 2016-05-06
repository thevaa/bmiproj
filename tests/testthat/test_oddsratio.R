context("odds ratio")
test_that("the 2x2 odds ratio is", {
  expect_equal(oddsratio(c(5,5,5,5))$odds, 1)
  expect_equal(oddsratio(c(10,5,5,10))$odds, 4)
  expect_equal(oddsratio(c(400, 400, 400, 400), 0.90)$conf.int, c(0.8483301741208367, 1.1787863151706770))
  expect_error(oddsratio(c(50, NA, "c", 3)))
})
