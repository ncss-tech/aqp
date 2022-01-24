context("horizon boundary interpretation")

test_that("hzDistinctnessCodeToOffset", {
  
  # terms
  expect_equal(
    hzDistinctnessCodeToOffset(c('very abrupt', 'abrupt', 'clear', 'gradual', 'diffuse')),
    c(0.25, 1.00, 2.50, 7.50, 10.00)
  )
  
  # case insensitivity
  expect_equal(
    hzDistinctnessCodeToOffset(c('Very Abrupt', 'Abrupt', 'Clear', 'Gradual', 'Diffuse')),
    c(0.25, 1.00, 2.50, 7.50, 10.00)
  )
  
  # codes
  expect_equal(
    hzDistinctnessCodeToOffset(c('V', 'A', 'C', 'G', 'D')),
    c(0.25, 1.00, 2.50, 7.50, 10.00)
  )
  
  # bogus -> 0
  expect_equal(
    hzDistinctnessCodeToOffset(c('very very', 'A', 'C', 'G', 'zzz')),
    c(0, 1.00, 2.50, 7.50, 0)
  )
  
  # NA -> 0
  expect_equal(
    hzDistinctnessCodeToOffset(NA),
    0
  )
  
  
})


test_that("hzTopographyCodeToLineType", {
  
  # terms
  expect_equal(
    hzTopographyCodeToLineType(c('smooth', 'wavy', 'irregular', 'broken')),
    c(1, 2, 3, 4)
  )
  
  # case insensitivity
  expect_equal(
    hzTopographyCodeToLineType(c('Smooth', 'Wavy', 'Irregular', 'Broken')),
    c(1, 2, 3, 4)
  )
  
  # codes
  expect_equal(
    hzTopographyCodeToLineType(c('B', 'I', 'W', 'S')),
    c(4, 3, 2, 1)
  )
  
  # bogus -> 1
  expect_equal(
    hzTopographyCodeToLineType(c('clowns')),
    1
  )
  
  # NA -> 1
  expect_equal(
    hzTopographyCodeToLineType(NA),
    1
  )
  
})

test_that("hzTopographyCodeToOffset", {
  
  # terms
  expect_equal(
    hzTopographyCodeToOffset(c('smooth', 'wavy', 'irregular', 'broken')),
    c(0, 4, 8, 12)
  )
  
  # case insensitivity
  expect_equal(
    hzTopographyCodeToOffset(c('Smooth', 'Wavy', 'Irregular', 'Broken')),
    c(0, 4, 8, 12)
  )
  
  # codes
  expect_equal(
    hzTopographyCodeToOffset(c('B', 'I', 'W', 'S')),
    c(12, 8, 4, 0)
  )
  
  # bogus -> 0
  expect_equal(
    hzTopographyCodeToOffset(c('clowns')),
    0
  )
  
  # NA -> 0
  expect_equal(
    hzTopographyCodeToOffset(NA),
    0
  )
  
})



