context("quickSPC")

x.l <- list(
  id = 'A',
  depths = c(25, 33, 100, 150),
  name = c('A', 'Bw', 'Bt', 'Cr'),
  clay = c(12, 15, 22, 25)
)

x.c <- 'A-Bt1-Bt2-Bt3-Cr-R'

x.c.multiple <- c(
  'A-Bt1-Bt2-Bt3-Cr-R', 
  'A-C1-C2-C3-C4-Ab', 
  'Ap-A-A/E-E-Bhs-Cr'
)

x.c.mode2 <- c(
    'AAA|BwBwBwBw|CCCCCCC|CdCdCdCd',
    'ApAp|AA|E|BhsBhs|Bw1Bw1|CCCCC',
    'A|Bt1Bt1Bt1|Bt2Bt2Bt2|Bt3|Cr|RRRRR'
    )


test_that("list template", {
  s <- quickSPC(x.l)
  
  expect_true(inherits(s, 'SoilProfileCollection'))
  expect_equal(length(s), 1)
  expect_equal(nrow(s), 4)
  expect_equal(hzdesgnname(s), 'name')
  
})

test_that("list template, error condition", {
  
  # ID spec
  expect_error(s <- quickSPC(x.l, id = 'xxx'))
  
  # depths spec
  expect_error(s <- quickSPC(x.l, d = 'xxx'))
  
})


test_that("character template, mode 1", {
  s <- quickSPC(x.c)
  
  expect_true(inherits(s, 'SoilProfileCollection'))
  expect_equal(length(s), 1)
  expect_equal(nrow(s), 6)
  expect_equal(hzdesgnname(s), 'name')
  
  # function is vectorized over these kind of templates
  s <- quickSPC(x.c.multiple)
  
  expect_true(inherits(s, 'SoilProfileCollection'))
  expect_equal(length(s), 3)
  
  # garbage input, but still valid template
  s <- quickSPC('A-4-zz-12')
  expect_true(inherits(s, 'SoilProfileCollection'))
  
})

test_that("character template, mode 1, error condition", {
  
  # invalid hz sequence
  expect_error(quickSPC('A-C--'))
  
})


test_that("character template, mode 2", {
  s <- quickSPC(x.c.mode2[1])
  
  expect_true(inherits(s, 'SoilProfileCollection'))
  expect_equal(length(s), 1)
  expect_equal(nrow(s), 4)
  expect_equal(hzdesgnname(s), 'name')
  
  # function is vectorized over these kind of templates
  s <- quickSPC(x.c.mode2)
  
  expect_true(inherits(s, 'SoilProfileCollection'))
  expect_equal(length(s), 3)
  
  # garbage input, but still valid template
  s <- quickSPC('A|4|zz|12')
  expect_true(inherits(s, 'SoilProfileCollection'))
  
})

test_that("character template, mode 2, error condition", {
  
  # invalid hz sequence
  expect_error(quickSPC('A|C||'))
  
  # invalid specification / mixing modes
  expect_error(quickSPC('A|C||--'), regexp = 'incorrect')
  
})

test_that("character template, other errors", {

  # invalid specification / mixing modes
  expect_error(quickSPC('A|C||--'), regexp = 'incorrect')
  
})


