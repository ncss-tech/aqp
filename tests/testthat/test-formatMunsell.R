context("Munsell formatting")


test_that("formatMunsell() works as expected", {
  
  
  .res <- formatMunsell('10YR', 3, 4)
  expect_equal(.res, '10YR 3/4')
  
  # no automatic conversion to neutral, use launderMunsell() for that
  .res <- formatMunsell('10YR', 3, 0)
  expect_equal(.res, '10YR 3/0')
  
  .res <- formatMunsell('N', 3, 4, neutralConvention = 'zero')
  expect_equal(.res, 'N 3/0')
  
  # chroma can be an empty string
  .res <- formatMunsell('N', 3, '', neutralConvention = 'zero')
  expect_equal(.res, 'N 3/0')
  
  .res <- formatMunsell('N', 3, 4, neutralConvention = 'empty')
  expect_equal(.res, 'N 3/')
  
  .res <- formatMunsell('N', 3, NA, neutralConvention = 'empty')
  expect_equal(.res, 'N 3/')
  
  .res <- formatMunsell('N', 3, 0, neutralConvention = 'empty')
  expect_equal(.res, 'N 3/')
  
  .res <- formatMunsell('5YR', NA, 4)
  expect_equal(.res, NA_character_)
  
  # missing chroma, not neutral hue => NA
  .res <- formatMunsell('5YR', 3, NA)
  expect_equal(.res, NA_character_)
  
  .res <- formatMunsell(NA, 6, 4)
  expect_equal(.res, NA_character_)
  
  .res <- formatMunsell('5Z', 6, 4)
  expect_equal(.res, NA_character_)
  
})

test_that("launderMunsell() works as expected", {
  
  .res <- launderMunsell('10YR 3/4')
  expect_equal(.res, '10YR 3/4')
  
  .res <- launderMunsell('10YR 3/0')
  expect_equal(.res, 'N 3/0')
  
  # missing chroma, not neutral hue => NA
  .res <- launderMunsell('10YR 3/')
  expect_equal(.res, NA_character_)
  
  .res <- launderMunsell('N 8/')
  expect_equal(.res, 'N 8/0')
  
  .res <- launderMunsell('N 8/0', neutralConvention = 'empty')
  expect_equal(.res, 'N 8/')
  
})
