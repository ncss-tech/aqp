context("PMS2Munsell")

test_that('basic functionality', {
  
  codes <- c(NA, "7630-c", "102-c")
  x <- PMS2Munsell(codes)
  
  expect_true(inherits(x, 'data.frame'))
  expect_true(ncol(x) == 4)
  expect_true(nrow(x) == length(codes))
  
  expect_true(is.na(x$munsell[1]))
  expect_equal(x$munsell[2], '5R 3/4')
  expect_equal(x$munsell[3], '5Y 9/13')
  
})

