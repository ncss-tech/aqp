context("estimateSoilColor")

# note, not symmetric

test_that("works as expected", {
  
  # moist -> dry
  x <- estimateSoilColor(hue = '10YR', value = 3, chroma = 3, sourceMoistureState = 'moist')
  
  expect_equal(x$hue, '10YR')
  expect_equal(x$value, 4)
  expect_equal(x$chroma, 4)
  
  # dry -> moist
  x <- estimateSoilColor(hue = '10YR', value = 5, chroma = 3, sourceMoistureState = 'dry')
  
  expect_equal(x$hue, '7.5YR')
  expect_equal(x$value, 4)
  expect_equal(x$chroma, 2)
  
})
