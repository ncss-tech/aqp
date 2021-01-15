context("colorQuantiles")

data(sp5)


test_that("colorQuantiles works as expected", {
  
  # compute marginal quantiles and L1 median
  cq <- colorQuantiles(sp5$soil_color, p = c(0.05, 0.5, 0.95))
  
  # expected output
  expect_true(inherits(cq, 'list'))
  expect_true(all(names(cq) == c('marginal', 'L1')))
  expect_true(nrow(cq$marginal) == 3)
  
})


test_that("plotColorQuantiles works as expected", {
  
  # compute marginal quantiles and L1 median
  cq <- colorQuantiles(sp5$soil_color, p = c(0.05, 0.5, 0.95))
  
  p <- plotColorQuantiles(cq)
  
  # expecte outuput
  expect_true(inherits(p, 'trellis'))
  
})
