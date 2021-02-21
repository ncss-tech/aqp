context("dice (update to slice)")

test_that("basic functionality", {
  
  data(sp4, package = 'aqp')
  depths(sp4) <- id ~ top + bottom
  
  # SPC
  s <- dice(sp4)
  # as a data.frame
  s.d <- dice(sp4, SPC = FALSE)
  
  # did it work?
  expect_true(inherits(s, 'SoilProfileCollection'))
  expect_true(inherits(s.d, 'data.frame'))
  
  # new metadata columns
  
  # arguments
  
  # there should be 42 horizon slices
  expect_equal(nrow(horizons(s[1, ])),  42)
  
})



