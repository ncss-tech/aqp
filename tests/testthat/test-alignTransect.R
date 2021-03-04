context("alignTransect")

data("sierraTransect")

# split transects
g <- subset(sierraTransect, transect == 'Granite')
a <- subset(sierraTransect, transect == 'Andesite')

# basic functionality
test_that("alignTransect works as expected", {
  
  # CRAN safe
  
  # simple examples, data are pre-sorted by elevation
  p <- alignTransect(g$elev, 1, length(g), fix = FALSE)
  
  # structure
  expect_true(inherits(p, 'list'))
  expect_true(length(p) == 3)
  
  # known output
  expect_true(all(p$order == 1:7))
})

# basic functionality
test_that("more complex input", {
  
  # CRAN safe
  
  # more interesting, data are not pre-sorted by elevation
  p <- alignTransect(a$elev, 1, length(a), fix = FALSE)
  
  # structure
  expect_true(inherits(p, 'list'))
  expect_true(length(p) == 3)
  
  # known output
  expect_true(all(p$order == c(7, 6, 5, 4, 3, 2, 1)))
  
})


## TODO: add a couple more with more complex ordering
