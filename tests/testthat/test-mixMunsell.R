context("mixing Munsell colors")


test_that("mixMunsell works as expected", {


  ## error conditions
  # invalid Munsell notation
  expect_error(mixMunsell(c('10YR 5/3', '10YR 3/99')))
  
  # singleton
  x <- mixMunsell(c('10YR 5/3'))
  expect_true(inherits(x, 'data.frame'))
  expect_true(ncol(x) == 2)
  expect_true(nrow(x) == 1)
  expect_true(x$munsell == '10YR 5/3')
  
  # standard usage
  x <- mixMunsell(c('10YR 5/3', '10YR 3/2'))
  expect_true(x$munsell == '10YR 4/2')
    
})



