context("segment")


test_that("data.frame interface works as expected", {
  
  # init local copy of sample data
  data(sp1)
  
  # trimming
  z <- segment(sp1, intervals = c(0, 10, 20, 30), trim = TRUE, hzdepcols = c('top', 'bottom'))
  
  # correct object type and segment label
  expect_true(inherits(z, 'data.frame'))
  expect_true('segment_id' %in% names(z))
  
  # no triming
  z <- segment(sp1, intervals = c(0, 10, 20, 30), trim = FALSE, hzdepcols = c('top', 'bottom'))
  
  # correct object type and segment label
  expect_true(inherits(z, 'data.frame'))
  expect_true('segment_id' %in% names(z))
  
})


test_that("SPC interface works as expected", {
  
  # init local copy of sample data
  data(sp1)
  depths(sp1) <- id ~ top + bottom
  
  # trimming
  z <- segment(sp1, intervals = c(0, 10, 20, 30), trim = TRUE)
  
  expect_true(inherits(z, 'SoilProfileCollection'))
  expect_true('segment_id' %in% horizonNames(z))
  
  # no trimming
  z <- segment(sp1, intervals = c(0, 10, 20, 30), trim = FALSE)
  
  expect_true(inherits(z, 'SoilProfileCollection'))
  expect_true('segment_id' %in% horizonNames(z))
  
})


