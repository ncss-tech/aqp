context("sieve")


test_that("basic functionality, USDA simplified system", {
  
  # obvious diameters
  expect_equal(fragmentSieve(c(75, 150, 450, 1000)), c('gravel', 'cobbles', 'stones', 'boulders'))
  
  # right on the line
  expect_equal(fragmentSieve(c(76)), 'cobbles')
  
  # NA
  expect_equal(fragmentSieve(c(76, NA)), c('cobbles', NA_character_))
  
  # flat
  expect_equal(fragmentSieve(c(76), flat = TRUE), 'channers')
  
  # prefix
  expect_equal(fragmentSieve(c(76), prefix = 'chewy_'), 'chewy_cobbles')
  
  # custom sieves
  expect_equal(fragmentSieve(c(50, 100), sieves = c('teaspoon' = 76, 'tablespoon' = 250)), c('teaspoon', 'tablespoon'))
  
  # ordered factors
  expect_true(
    inherits(
      fragmentSieve(c(5, 4, 7, 75, 150, 450, 1000), ordered = TRUE),
      'factor'
    )
  )
  
})

test_that("other systems", {
  
  x <- c(4, 35, 150, 400, 650)
  
  # USDA simplified
  expect_equal(fragmentSieve(x), c('gravel', 'gravel', 'cobbles', 'stones', 'boulders'))
  
  # USDA
  expect_equal(fragmentSieve(x, sys = 'USDA'), c('fine_gravel', 'coarse_gravel', 'cobbles', 'stones', 'boulders'))
  
  # International
  expect_equal(fragmentSieve(x, sys = 'international'), c('gravel', 'stones', 'stones', 'stones', 'stones'))
  
  # unified
  expect_equal(fragmentSieve(x, sys = 'unified'), c('fine_gravel', 'coarse_gravel', 'cobbles', 'boulders', 'boulders'))
  
  # AASHTO
  expect_equal(fragmentSieve(x, sys = 'AASHTO'), c('fine_gravel', 'coarse_gravel', 'broken_rock', 'broken_rock', 'broken_rock'))
  expect_equal(fragmentSieve(x, sys = 'AASHTO', rounded = TRUE), c('fine_gravel', 'coarse_gravel', 'boulders', 'boulders', 'boulders'))
  
  # modified wentworth
  expect_equal(fragmentSieve(x, sys = 'mod.wentworth'), c('pebbles', 'pebbles', 'cobbles', 'boulders', 'boulders'))
  
})


