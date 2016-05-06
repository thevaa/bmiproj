context("recurring sample variance")
test_that("new sample variance is", {
  expect_equal(newvar(5, 5, 6, 5), 3.95)
  expect_equal(newvar(5, 5, 5, 5), 3.75)
  expect_error(newvar(5, 4, "c", 7))
})


newvar(5, 5, 6, 5)
 3.95
