context("find/fixOverlap")

# example sequence with "overlap"
x <- c(1, 2, 3, 3.4, 3.5, 5, 6, 10)


## TODO: more tests for electrostatic simulation

test_that(".electricForce() works as expected", {
  
  
  
  # vector displacement
  e <- aqp:::.electricForce(Q1 = 1, Q2 = 1, Qk = 0.5, d = 5)
  expect_equal(length(e), 1)
  expect_true(is.vector(e))
  expect_equal(e, 0.01980198)
  
  e <- aqp:::.electricForce(Q1 = 1, Q2 = 1, Qk = 0.5, d = c(2, 5))
  expect_equal(length(e), 2)
  expect_true(is.vector(e))
  expect_equal(e, c(0.11764706, 0.01980198))
  
  e <- aqp:::.electricForce(Q1 = c(1, 1), Q2 = c(1, 1), Qk = 0.5, d = c(2, 5))
  expect_equal(length(e), 2)
  expect_true(is.vector(e))
  expect_equal(e, c(0.11764706, 0.01980198))
  
  
  # pair-wise matrix displacement
  e <- aqp:::.electricForce(Q1 = 1, Q2 = 1, Qk = 0.5, d = matrix(c(NA, 1, 1, NA), nrow = 2))
  expect_true(is.matrix(e))
  expect_equal(dim(e), c(2, 2))
  expect_equivalent(na.omit(unique(as.vector(e))), 0.4)
  
  # equal charges
  e <- aqp:::.electricForce(Q1 = c(1, 1), Q2 = c(1, 1), Qk = 0.5, d = matrix(c(NA, 1, 1, NA), nrow = 2))
  expect_true(is.matrix(e))
  expect_equal(dim(e), c(2, 2))
  expect_equivalent(na.omit(unique(as.vector(e))), 0.4)
  
  
  # pair-wise matrix displacement
  # unequal charges
  m <- matrix(
  c(NA, 1, 1,
  1, NA, 1,
  1, 1, NA),
  nrow = 3, byrow = TRUE,
  )
  
  e <- aqp:::.electricForce(Q1 = c(1, 1, 2), Q2 = c(1, 1, 2), Qk = 0.5, d = m)
  expect_true(is.matrix(e))
  expect_equal(dim(e), c(3, 3))
  expect_equivalent(na.omit(unique(as.vector(e))), c(0.4, 1.6))
  
  
  ## errors
  
  # charge vectors not the same length
  expect_error(
    aqp:::.electricForce(Q1 = 1, Q2 = c(1, 2), Qk = 0.5, d = matrix(c(NA, 1, 1, NA), nrow = 2)), 
    regexp = 'charge vectors must be of the same length'
  )
  
  # charge vectors wrong size for displacement matrix
  expect_error(
    aqp:::.electricForce(Q1 = c(1, 1, 1) , Q2 = c(1, 1, 1), Qk = 0.5, d = matrix(c(NA, 1, 1, NA), nrow = 2)),
    regexp = 'charge vectors must be of length ncol\\(d\\)'
  )
  
  # not square
  expect_error(
    aqp:::.electricForce(Q1 = c(1, 1, 1) , Q2 = c(1, 1, 1), Qk = 0.5, d = matrix(c(NA, 1, 1, 1, 1, NA), nrow = 2)),
    regexp = 'displacement matrix must be square'  
  )
  
})




test_that("overlapMetrics", {
  
  x <- c(1, 2, 3, 3.4, 3.5, 5, 6, 10)
  
  # no overlap with 0 threshold
  z <- overlapMetrics(x, thresh = 0)
  expect_true(length(z$idx) == 0)
  
  # none yet
  z <- overlapMetrics(x, thresh = 0.1)
  expect_true(length(z$idx) == 0)
  
  # a little
  z <- overlapMetrics(x, thresh = 0.3)
  expect_true(length(z$idx) == 2)
  expect_equal(x[z$idx], c(3.4, 3.5))
  
  # more
  z <- overlapMetrics(x, thresh = 1)
  expect_true(length(z$idx) == 3)
  expect_equal(x[z$idx], c(3, 3.4, 3.5))
  
  # all
  z <- overlapMetrics(x, thresh = 2)
  expect_true(length(z$idx) == 7)
})


test_that("fixOverlap, simple cases", {
  
  # stochastic element, don't test on CRAN
  skip_on_cran()
  
  # basic interface
  z <- fixOverlap(x, thresh = 0.3, min.x = 1, max.x = 10)
  
  # numeric vector
  expect_true(inherits(z, 'numeric'))
  
  # same length
  expect_true(length(z) == length(x))
  
  # row order preserved
  expect_true(all(rank(x) == rank(z)))
  
  # boundary conditions
  expect_true(min(z) >= 1)
  expect_true(max(z) <= 10)
  
})


test_that("electrostatic simulation", {
  
  # simple
  x <- c(1, 2, 3, 3.4, 3.5, 5, 6, 10)
  z <- fixOverlap(x, thresh = 0.3, method = 'E', q = 1, trace = TRUE)
  
  # list
  expect_true(inherits(z, 'list'))
  
  # should converge
  expect_true(z$converged)
  
  # more difficult
  z <- fixOverlap(x, thresh = 0.7, method = 'E', q = 1, trace = TRUE)
  
  # list
  expect_true(inherits(z, 'list'))
  
  # should converge
  expect_true(z$converged)
  
})



test_that("fixOverlap, trace = TRUE", {
  
  # stochastic element, don't test on CRAN
  skip_on_cran()
  
  # verbose
  z <- fixOverlap(x, thresh = 0.3, trace = TRUE)
  
  # list
  expect_true(inherits(z, 'list'))
  
})

