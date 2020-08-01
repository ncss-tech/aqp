context("aqp package environment")

test_that("defaults", {
  expect_equal(getOption(".aqp.show.n.cols"), 10)
  options(.aqp.show.n.cols = 100)
  expect_equal(getOption(".aqp.show.n.cols"), 100)
  expect_silent(aqp:::.onLoad("foo","bar")) # libname and pkgname not used at present
})
