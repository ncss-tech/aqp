context("find/fixOverlap")

# example sequence with "overlap"
x <- c(1, 2, 3, 3.4, 3.5, 5, 6, 10)


## TODO: more tests for electrostatic simulation

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

