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
  # s1 <- split(sp6) # doesn't work due to base generic
  s2 <- split(sp6, idname(sp6))
  s3 <- split(sp6, profile_id(sp6))
  s4 <- split(sp6, f = NULL)
  
  # result should be a list
  # expect_true(inherits(s1, 'list'))
  expect_true(inherits(s2, 'list'))
  expect_true(inherits(s3, 'list'))
  expect_true(inherits(s4, 'list'))

  # as many groups as profiles in original
  # expect_true(length(s1) == length(s2))
  expect_true(length(s2) == length(s3))
  expect_true(length(s3) == length(s4))
  expect_true(length(s4) == length(sp6))
  
  # 1 profile / group
  expect_equivalent(sapply(s2, length), rep(1, times = length(sp6)))
})


## errors
test_that("split fails as expected", {
  
  # group variable that is not site-level attribute
  expect_error(split(sp6, f = 'XXX'))
})

## warnings


