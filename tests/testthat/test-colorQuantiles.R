context("colorQuantiles")

data(sp5)


test_that("colorQuantiles works as expected", {
  
  skip_if_not_installed('Gmedian')
  
  # compute marginal quantiles and L1 median
  cq <- colorQuantiles(sp5$soil_color, p = c(0.05, 0.5, 0.95))
  
  # expected output
  expect_true(inherits(cq, 'list'))
  expect_true(all(names(cq) == c('marginal', 'L1')))
  expect_true(nrow(cq$marginal) == 3)
  
})


test_that("plotColorQuantiles works as expected", {
  
  skip_if_not_installed('Gmedian')
  
  # compute marginal quantiles and L1 median
  cq <- colorQuantiles(sp5$soil_color, p = c(0.05, 0.5, 0.95))
  
  p <- plotColorQuantiles(cq)
  
  # expected output
  expect_true(inherits(p, 'trellis'))
  
})

test_that("output is correct", {
  
  skip_if_not_installed('Gmedian')
  
  # compute marginal quantiles and L1 median
  cq <- colorQuantiles(sp5$soil_color, p = c(0.05, 0.5, 0.95))
  
  # L1 CIELAB coordinates
  expect_equal(cq$L1$p, 0.5, tolerance = 0.001)
  expect_equal(cq$L1$L, 32.96, tolerance = 0.01)
  expect_equal(cq$L1$A, 6.38, tolerance = 0.01)
  expect_equal(cq$L1$B, 9.75, tolerance = 0.01)
  
  # L1 Munsell color
  expect_match(cq$L1$L1_chip, '5YR 3/2', fixed = TRUE)
  
})



