context("Metrics of soil color contrast")

## sample data
m1 <- c('10YR 6/3', '7.5YR 3/3', '10YR 2/2', '7.5YR 3/4')
m2 <- c('5YR 3/4', '7.5YR 4/4', '2.5YR 2/2', '7.5YR 6/3')

## tests


## tests

test_that("contrastClass works as expected", {

  ## hand-done tests

  # 10YR 6/3 vs 5YR 3/4
  x <- contrastClass(v1=6, c1=3, v2=3, c2=4, dH=2, dV=3, dC=1, verbose = TRUE)
  expect_true(x$faint$res == 'Prominent')
  expect_equivalent(unlist(x$faint[, c('f.case1', 'f.case2', 'f.case3', 'low.value.chroma')]),  c(FALSE, FALSE, FALSE, FALSE))
  expect_equivalent(unlist(x$distinct[, c('d.case1', 'd.case2', 'd.case3')]),  c(FALSE, FALSE, FALSE))

  # 7.5YR 3/3 vs 7.5YR 4/4
  x <- contrastClass(v1=3, c1=3, v2=4, c2=4, dH=0, dV=1, dC=1, verbose = TRUE)
  expect_true(x$faint$res == 'Faint')
  expect_equivalent(unlist(x$faint[, c('f.case1', 'f.case2', 'f.case3', 'low.value.chroma')]),  c(TRUE, FALSE, FALSE, FALSE))
  expect_equivalent(unlist(x$distinct[, c('d.case1', 'd.case2', 'd.case3')]),  c(FALSE, FALSE, FALSE))

  # 10YR 2/2 vs 2.5YR 2/2
  x <- contrastClass(v1=2, c1=2, v2=2, c2=2, dH=0, dV=0, dC=0, verbose = TRUE)
  expect_true(x$faint$res == 'Faint')
  expect_equivalent(unlist(x$faint[, c('f.case1', 'f.case2', 'f.case3', 'low.value.chroma')]),  c(TRUE, FALSE, FALSE, TRUE))
  expect_equivalent(unlist(x$distinct[, c('d.case1', 'd.case2', 'd.case3')]),  c(FALSE, FALSE, FALSE))

  # 7.5YR 3/4 vs 7.5YR 6/3
  x <- contrastClass(v1=3, c1=4, v2=5, c2=3, dH=0, dV=3, dC=1, verbose = TRUE)
  expect_true(x$faint$res == 'Distinct')
  expect_equivalent(unlist(x$faint[, c('f.case1', 'f.case2', 'f.case3', 'low.value.chroma')]),  c(FALSE, FALSE, FALSE, FALSE))
  expect_equivalent(unlist(x$distinct[, c('d.case1', 'd.case2', 'd.case3')]),  c(TRUE, FALSE, FALSE))

  # Error: inputs must all have the same length
  expect_error(x <- contrastClass(v1=3, c1=4, v2=5, c2=3, dH=0, dV=3, dC=numeric(0), verbose = TRUE))

  ## TODO: test entire range of rules/cases

})


test_that("colorContrast works as expected", {

  # contrast metrics
  d <- colorContrast(m1, m2)

  # output should be a data.frame
  expect_true(inherits(d, 'data.frame'))
  expect_true(nrow(d) == length(m1))

  # first two columns should contain original colors
  expect_true(all(d$m1 == m1))
  expect_true(all(d$m2 == m2))

  # color contrast should be an ordered factor
  expect_true(is.factor(d$cc))
  expect_true(is.ordered(d$cc))

})

test_that("colorContrast fails as expected", {

  # m1/m2 not same length ---> error
  expect_error(colorContrast(m1[1], m2))

  # bogus hues -> dH and dE00 are NA
  d <- colorContrast('10FG 2/3', '4ZZ 4/5')
  expect_true(is.na(d$dH))
  expect_true(is.na(d$dE00))

  # bogus Munsell colors, all NA
  d <- colorContrast(m1 = '123sdf', m2 = '345gg')
  expect_true(all(is.na(d[, -c(1:2)])))
})


test_that("valid results", {

  # contrast metrics
  d <- colorContrast(m1, m2)

  # hand-checked
  expect_equal(d$dH, c(2, 0, 3, 0))
  expect_equal(d$dV, c(3, 1, 0, 3))
  expect_equal(d$dC, c(1, 1, 0, 1))
  expect_equal(as.character(d$cc), c('Prominent', 'Faint', 'Faint', 'Distinct'))

  ## TODO add some less-common colors

})



test_that("neutral hues", {
  
  # contrast metrics
  d <- colorContrast('N 3/', 'N 6/')
  
  # hand-checked
  expect_equal(d$dH, 0)
  expect_equal(d$dV, 3)
  expect_equal(d$dC, 0)
  expect_equal(as.character(d$cc), 'Distinct')
  
})


