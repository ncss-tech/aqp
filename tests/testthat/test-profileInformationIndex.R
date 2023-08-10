context("profile information metrics")

test_that(".pii works", {
  
  # constant data
  x <- .pii(i = rep(1, 10), baseline = TRUE, numericDigits = 4)
  expect_true(inherits(x, c('numeric', 'integer')))
  expect_true(length(x) == 1)
  
  # changes in r-devel / memCompress() give slightly different results
  # expect_equal(x, 1L)
  
  # simple data
  x <- .pii(i = as.integer(c(1:10)), baseline = TRUE, numericDigits = 4)
  expect_true(inherits(x, c('numeric', 'integer')))
  expect_true(length(x) == 1)
  
  # changes in r-devel / memCompress() give slightly different results
  # expect_equal(x, 2L, tolerance = 1)
  
  # complex data
  x <- .pii(i = letters, baseline = TRUE, numericDigits = 4)
  expect_true(inherits(x, c('numeric', 'integer')))
  expect_true(length(x) == 1)
  
  # changes in r-devel / memCompress() give slightly different results
  # expect_equal(x, 3.615, tolerance = 0.001)
  
})


test_that("basic operation", {
  
  a <- data.frame(id = 'A', top = 0, bottom = 100, p = 5)
  b <- data.frame(
    id = 'B', 
    top = c(0, 10, 20, 30, 40, 50), 
    bottom = c(10, 20, 30, 40, 50, 100), 
    p = rep(5, times = 6)
  )
  
  x <- rbind(a, b)
  depths(x) <- id ~ top + bottom
  
  
  # changes in r-devel / memCompress() give slightly different results
  # adapt test after this is sorted out
  
  p <- profileInformationIndex(x, vars = c('p'), method = 'sum')
  expect_true(inherits(p, c('numeric', 'integer')))
  expect_true(length(p) == 2)
  
  # expect_equal(as.vector(p), c(3, 4.3), tolerance = 0.1)
  
  p <- profileInformationIndex(x, vars = c('p'), method = 'mean')
  expect_true(inherits(p, c('numeric', 'integer')))
  expect_true(length(p) == 2)
  
  # expect_equal(as.vector(p), c(0, 0.458), tolerance = 0.1)
  
  p <- profileInformationIndex(x, vars = c('p'), method = 'median')
  expect_true(inherits(p, c('numeric', 'integer')))
  expect_true(length(p) == 2)
  
  # expect_equal(as.vector(p), c(0, 0.529), tolerance = 0.1)
  
  p <- profileInformationIndex(x, vars = c('p'), method = 'sum', baseline = FALSE)
  expect_true(inherits(p, c('numeric', 'integer')))
  expect_true(length(p) == 2)
  
  # expect_equal(as.vector(p), c(29, 62))
  
  p <- profileInformationIndex(x, vars = c('p'), method = 'mean', baseline = FALSE)
  expect_true(inherits(p, c('numeric', 'integer')))
  expect_true(length(p) == 2)
  
  # expect_equal(as.vector(p), c(8.666, 19.666), tolerance = 0.1)
  
  p <- profileInformationIndex(x, vars = c('p'), method = 'median', baseline = FALSE)
  expect_true(inherits(p, c('numeric', 'integer')))
  expect_true(length(p) == 2)
  
  # expect_equal(as.vector(p), c(8, 23))
  
})


