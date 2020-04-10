context("split method for SoilProfileCollection objects")

## sample data
data(sp6)
depths(sp6) <- id ~ top + bottom

# fake grouping var
sp6$g <- factor(rep(c('A', 'B'), each=3))

## tests

test_that("site-level grouping factor", {
  
  # standard, grouping factor
  s <- split(sp6, 'g')
  
  # result should be a list
  expect_true(inherits(s, 'list'))
  
  # two goups
  expect_true(length(s) == 2)
  
  # three profiles / group
  expect_equivalent(sapply(s, length), c(3,3))
})

test_that("identity split", {
  
  # idname used
  s <- split(sp6)
  
  # result should be a list
  expect_true(inherits(s, 'list'))
  
  # as many groups as profiles in original
  expect_true(length(s) == length(sp6))
  
  # 1 profiles / group
  expect_equivalent(sapply(s, length), rep(1, times=length(sp6)))
})


## errors
test_that("split fails as expected", {
  
  # group variable that is not site-level attribute
  expect_error(split(sp6, f = 'XXX'))
})

## warnings


