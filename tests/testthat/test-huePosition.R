context("Munsell Hue position")

## sample data
x <- c('2.5YR', '7.5YR', '10YR', '5BG')

## tests

test_that("huePosition works as expected", {
  
  z <- huePosition(x)
  
  # manually counted on the Munsell wheel
  # https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/ref/?cid=nrcs142p2_053569
  expect_equal(z, c(4, 6, 7, 21))
  
  # bogus input should result in NA
  expect_true(is.na(huePosition('10YR 3/3')))
  
})
