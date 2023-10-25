context("profile information metrics")

## TODO: finish these
test_that("helper functions", {
  
  v <- aqp:::.prepareVector(1:10, d = 10)
  expect_equal(length(v), 1)
  expect_equal(nchar(v), 11)
  
  # v <- aqp:::.prepareVector(letters, d = 10)
  # v <- aqp:::.prepareVector(TRUE, d = 10)
  # v <- aqp:::.prepareVector(factor(c('A', 'Z')), d = 10)
  # 
  # b <- aqp:::.prepareBaseline(1:10, d = 10, type = 'numeric')
  # 
  # x <- 1:10
  # i <- aqp:::.prepareVariable(x, removeNA = TRUE, numericDigits = 4)
  # 
  # x <- c('A', 'C', 'C', 'D')
  # i <- aqp:::.prepareVariable(x, removeNA = TRUE, numericDigits = 4)
  # 
  # x <- factor(c('A', 'C', 'C', 'D'))
  # i <- aqp:::.prepareVariable(x, removeNA = TRUE, numericDigits = 4)
  # 
  # x <- c(TRUE, TRUE, FALSE)
  # i <- aqp:::.prepareVariable(x, removeNA = TRUE, numericDigits = 4)
  
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
  
  
  # memCompress() give slightly different results across platforms
  # adapt test after this is sorted out
  
  p <- profileInformationIndex(x, vars = 'p', method = 'joint', baseline = FALSE)
  expect_true(inherits(p, c('numeric', 'integer')))
  expect_true(length(p) == 2)
  
  # expect_equal(as.vector(p), c(3, 4.3), tolerance = 0.1)
  
  p <- profileInformationIndex(x, vars = c('p'), method = 'joint')
  expect_true(inherits(p, c('numeric', 'integer')))
  expect_true(length(p) == 2)
  
  # expect_equal(as.vector(p), c(0, 0.458), tolerance = 0.1)
  
  p <- profileInformationIndex(x, vars = c('p'), method = 'joint')
  expect_true(inherits(p, c('numeric', 'integer')))
  expect_true(length(p) == 2)
  
  # expect_equal(as.vector(p), c(0, 0.529), tolerance = 0.1)
  
  p <- profileInformationIndex(x, vars = c('p'), method = 'joint', baseline = FALSE)
  expect_true(inherits(p, c('numeric', 'integer')))
  expect_true(length(p) == 2)
  
  # expect_equal(as.vector(p), c(29, 62))
  
  p <- profileInformationIndex(x, vars = c('p'), method = 'joint', baseline = FALSE)
  expect_true(inherits(p, c('numeric', 'integer')))
  expect_true(length(p) == 2)
  
  # expect_equal(as.vector(p), c(8.666, 19.666), tolerance = 0.1)
  
  p <- profileInformationIndex(x, vars = c('p'), method = 'joint', baseline = FALSE)
  expect_true(inherits(p, c('numeric', 'integer')))
  expect_true(length(p) == 2)
  
  # expect_equal(as.vector(p), c(8, 23))
  
})


