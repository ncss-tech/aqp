context("aqp package environment")

test_that("defaults", {
  expect_equal(getOption(".aqp.show.n.cols"), 10)
  options(.aqp.show.n.cols = 100)
  expect_equal(getOption(".aqp.show.n.cols"), 100)
})
