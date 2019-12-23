context("evalMissingData")

# setup horizon-level data: data are from lab sampled pedons
# sprinkle-in some NA
# added fake contact to the bottom via R, Cr, Cd
d <- read.csv(
  textConnection('series,name,top,bottom,clay,frags,ph
auburn,A,0,3,21,6,NA
auburn,AB,3,15,21,13,5.6
auburn,Bw,15,25,20,9,5.8
auburn,R,25,47,21,28,5.8
dunstone,A,0,5,16,13,6
dunstone,AB,5,17,17,19,6.3
dunstone,Bt,17,31,20,6,6.3
dunstone,Cr,31,41,21,15,6.3
sobrante,A,0,5,18,0,5.8
sobrante,Ab,5,10,16,2,5.7
sobrante,Bt1,10,28,15,21,5.8
sobrante,Bt2,28,51,NA,NA,NA
sobrante,Cd,51,74,20,12,6.2'), stringsAsFactors=FALSE)

# establish site-level data
s <- data.frame(
  series=c('auburn', 'dunstone', 'sobrante'), 
  precip=c(24, 30, 32),
  stringsAsFactors=FALSE
)

# upgrade to SoilProfile Collection object
depths(d) <- series ~ top + bottom
site(d) <- s


## tests

test_that("runs as expected", {
  
  # does it run?
  e.rel <- evalMissingData(d, vars = c('clay', 'frags', 'ph'), name = 'name', method = 'relative')
  e.abs <- evalMissingData(d, vars = c('clay', 'frags', 'ph'), name = 'name', method = 'absolute')
  
  # result is a numeric vector
  expect_true(inherits(e.rel, 'numeric'))
  expect_true(inherits(e.abs, 'numeric'))
  
  # there should be no NA
  expect_true(all(is.na(e.rel) == FALSE))
  expect_true(all(is.na(e.abs) == FALSE))
  
  # check against hand-computed results
  # as.vector removes names attr
  # auburn
  expect_equal(as.vector(e.rel[1]), 22 / 25, tolerance=0.001)
  expect_equal(as.vector(e.abs[1]), 22 , tolerance=0.001)
  
  # dunstone
  expect_equal(as.vector(e.rel[2]), 1.000, tolerance=0.001)
  expect_equal(as.vector(e.abs[2]), 31, tolerance=0.001)
  
  # sobrante
  expect_equal(as.vector(e.rel[3]), 28 / 51, tolerance=0.001)
  expect_equal(as.vector(e.abs[3]), 28 , tolerance=0.001)
  
})


test_that("expected errors", {
  
  # bad horizon name
  expect_error(evalMissingData(d, vars = c('clay', 'frags', 'ph'), name = 'namae'))
  
  # bad var spec
  expect_error(evalMissingData(d, vars = c('clay', 'frags', 'a'), name = 'name'))
})


