context("new mean")
test_that("new updated mean is", {
  expect_equal(new_mean(5, 7, 4), 5.5)
  expect_equal(new_mean(10, 2, 100), 9.92)
  expect_equal(new_mean(9, 4, 5), 8)
  expect_error(new_mean(5, 4, "c"))
})
