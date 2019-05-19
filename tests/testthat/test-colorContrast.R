context("Munsell Color Contrast")

## sample data
m1 <- c('10YR 3/3', '7.5YR 3/3', '10YR 2/2', '7.5YR 3/3')
m2 <- c('5YR 3/4', '7.5YR 4/4', '7.5YR 2/2', '7.5YR 4/3')

## tests

test_that("colorContrast works as expected", {
  
  # contrast metrics
  d <- colorContrast(m1, m2)
  
  # output should be a data.frame
  expect_true(class(d) == 'data.frame')
  expect_true(nrow(d) == length(m1))
  
  # first two columns should contain original colors
  expect_true(all(d$m1 == m1))
  expect_true(all(d$m2 == m2))
  
})

test_that("colorContrast fails as expected", {
  
  # m1/m2 not same length ---> error
  expect_error(colorContrast(m1[1], m2))
  
  # bogus hues -> dH and dE00 are NA
  d <- colorContrast('10FG 2/3', '4ZZ 4/5')
  expect_true(is.na(d$dH))
  expect_true(is.na(d$dE00))
  
  # bogus Munsell colors, all NA
  d <- colorContrast('123sdf', '345gg')
  expect_true(all(is.na(d[, -c(1:2)])))
})


test_that("valid results", {
  
  # contrast metrics
  d <- colorContrast(m1, m2)
  
  # hand-checked
  expect_equal(d$dH, c(2, 0, 1, 0))
  expect_equal(d$dV, c(0, 1, 0, 1))
  expect_equal(d$dC, c(1, 1, 0, 0))
  
  ## TODO add some less-common colors
  
})

