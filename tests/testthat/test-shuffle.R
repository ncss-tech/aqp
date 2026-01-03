context("shuffle method for SoilProfileCollection objects")


test_that("basic functionality", {
  
  data('osd', package = 'aqp')
  o <- osd
  
  o.d <- shuffle(o, mode = 'data')
  o.h <- shuffle(o, mode = 'horizon')
  o.h2 <- shuffle(o, mode = 'horizon', replace = TRUE)
  
  expect_true(inherits(o.d, 'SoilProfileCollection'))
  expect_true(inherits(o.h, 'SoilProfileCollection'))
  expect_true(inherits(o.h2, 'SoilProfileCollection'))
  
})
