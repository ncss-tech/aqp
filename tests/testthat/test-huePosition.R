context("Munsell hue position")

## sample data
m1 <- c('10YR 6/3', '7.5YR 3/3', '10YR 2/2', '7.5YR 3/4')
m2 <- c('5YR 3/4', '7.5YR 4/4', '2.5YR 2/2', '7.5YR 6/3')

## tests


test_that("huePosition works as expected", {
  
  x <- c('2.5YR', '7.5YR', '10YR', '5BG')
  z <- huePosition(x)
  
  ## basic operation / vectorization
  # manually counted on the Munsell wheel
  # https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/ref/?cid=nrcs142p2_053569
  # huePositionCircle(chroma = 15)
  expect_equal(z, c(4, 6, 7, 21))
  
  ## bogus input should result in NA
  expect_true(is.na(huePosition('10YR 3/3')))
  expect_true(is.na(huePosition('5RR')))
  
  ## all hues
  h <- huePosition(returnHues = TRUE)
  expect_true(length(h) == 40)
  
  ## neutral hues require a special argument
  expect_true(is.na(huePosition('N')))
  expect_true((huePosition('N', includeNeutral = TRUE)) == 41)
  h <- huePosition(returnHues = TRUE, includeNeutral = TRUE)
  expect_true(length(h) == 41)
  
  ## shift origin
  # huePositionCircle(chroma = 15)
  expect_true(
    huePosition(x = '10GY', origin = '10Y') == 5L
  )
  
  # vectorization
  expect_true(
    all(
      huePosition(x, origin = '5P') == c(12L, 14L, 15L, 29L)  
    )
  )
  
  
  ## search direction
  # CW
  expect_true(
    huePosition(x = '5P', direction = 'cw') == 33L
  )
  
  # CCW
  expect_true(
    huePosition(x = '5P', direction = 'ccw') == 9L
  )
  
  
  ## new origin + search direction
  # huePositionCircle(hues = huePosition(returnHues = TRUE, origin = '10Y', direction = 'ccw'), chroma = 15)
  expect_true(
    all(
      huePosition(x = x, origin = '10Y', direction = 'ccw') == c(8L, 6L, 5L, 31L)
    )
  )
  
  
})


test_that("huePositionCircle works as expected", {
  res <- huePositionCircle(hues = huePosition(returnHues = TRUE, origin = '10Y', direction = 'ccw'), chroma = 15)
  
  expect_true(inherits(res, 'data.frame'))
})

