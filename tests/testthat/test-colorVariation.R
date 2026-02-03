context("colorVariation()")

test_that("works as expected", {
  
  # all NA
  v <- colorVariation(NA)
  expect_equal(v, NA)
  
  # too few obs
  v <- colorVariation(c(NA, '10YR 2/2'))
  expect_equal(v, 0)
  
  # frequency
  m <- c('10YR 3/3', '10YR 4/4', '10YR 4/4', '5GY 6/8')
  v <- colorVariation(m, method = 'frequency')
  
  # strip attributes
  expect_equal(round(as.vector(v)), 10.0)
  
  # check attributes
  expect_equal(attr(v, 'most frequent'), '10YR 4/4')
  

  # centroid
  v <- colorVariation(m, method = 'centroid')
  
  # strip attributes
  expect_equal(round(as.vector(v)), 13.0)
  
  # check attributes
  expect_equal(attr(v, 'centroid'), '5Y 4/4')
  
})


