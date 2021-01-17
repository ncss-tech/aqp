context("mixing Munsell colors")


test_that("mixMunsell works as expected", {


  ## error conditions
  expect_error(mixMunsell(c(NA, '10YR 3/4')))

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

  # TODO: tests to "fix"

  # mixMunsell-patch #183 to fix this (bad weights when length(x) != length(unique(x)))
  expect_warning(mixMunsell(c('10YR 5/3', '10YR 3/2', '10YR 5/3')))

  # also: propose allowing 0 weights to filter NA

  # Error in mixMunsell(c(NA, "10YR 3/4"), w = c(0, 1)) :
  # input must be valid Munsell notation, neutral hues not supported
  expect_error(mixMunsell(c(NA, '10YR 3/4'), w = c(0, 1)))

  # also: pretty sure this output does not make sense
  # mixMunsell(c('10YR 5/3', '10YR 3/2'), w = 1)
  # munsell   distance
  # 1 7.5GY 2/4 0.07119279
})



