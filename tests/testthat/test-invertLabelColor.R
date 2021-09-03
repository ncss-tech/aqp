context("invertLabelColor")


test_that("works as expected", {
  
  cols <- parseMunsell(c('10YR 2/1', '10YR 6/8', NA, NA))
  icols <- invertLabelColor(cols)
  
  # white
  expect_equal(icols[1], '#FFFFFF')
  
  # black
  expect_equal(icols[2], '#000000')
  
  # NA input -> black
  expect_equal(icols[3], '#000000')
  expect_equal(icols[4], '#000000')
  
})
