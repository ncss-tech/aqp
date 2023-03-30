context("sieve")


test_that("basic functionality", {
  
  # obvious diameters
  expect_equal(sieve(c(75, 150, 450, 1000)), c('gravel', 'cobbles', 'stones', 'boulders'))
  
  # right on the line
  expect_equal(sieve(c(76)), 'cobbles')
  
  # NA
  expect_equal(sieve(c(76, NA)), c('cobbles', NA_character_))
  
  # flat
  expect_equal(sieve(c(76), flat = TRUE), 'channers')
  
  # prefix
  expect_equal(sieve(c(76), prefix = 'chewy_'), 'chewy_cobbles')
  
  # custom sieves
  expect_equal(sieve(c(50, 100), sieves = c('teaspoon' = 76, 'tablespoon' = 250)), c('teaspoon', 'tablespoon'))
  
})

