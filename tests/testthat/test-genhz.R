context("generalize horizon names")

## sample data
x <- c('A', 'AC', 'Bt1', '^AC', 'C', 'BC', 'CB')

## tests

test_that("basic pattern matching", {
  
  # the third pattern will steal from the second
  n <- c('A', '^AC', 'C')
  p <- c('A', '\\^A', 'C')
  res <- generalize.hz(x, new = n, pat=p, non.matching.code = 'not-used')
  
  # matching only text, not factor levels
  expect_equal(as.character(res), c('A', 'C', 'not-used', 'C', 'C', 'C', 'C'))
  
  # check levels: these should match the ording of `n` + non matching code
  expect_equal(levels(res), c('A', '^AC', 'C', 'not-used'))
})


test_that("advanced pattern matching, requires perl", {
  
  # the third pattern may steal from the second
  n <- c('A', '^AC', 'C')
  # A -- ^A -- C without preceding A
  p <- c('A', '\\^A', '(?<!A)C')
  
  # the last pattern requires perl-compatible REGEX
  # error without perl=TRUE
  expect_error(generalize.hz(x, new = n, pat=p, non.matching.code = 'not-used'))
  
  # this should work
  res <- generalize.hz(x, new = n, pat=p, non.matching.code = 'not-used', perl=TRUE)
  
  # matching only text, not factor levels
  expect_equal(as.character(res), c('A', 'A', 'not-used', '^AC', 'C', 'C', 'C'))
  
  # check levels: these should match the ording of `n` + non matching code
  expect_equal(levels(res), c('A', '^AC', 'C', 'not-used'))
})

## TODO:
# more complex patterns: '/', lithologic discontinuities, look-ahead, anchoring, etc.

