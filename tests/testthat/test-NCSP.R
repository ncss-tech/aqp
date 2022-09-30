context("NCSP algorithm")

data("jacobs2000")
x <- jacobs2000

test_that('error conditions', {

  # x must be a SPC
  expect_error(
    NCSP(3L, vars = c('sand', 'clay'))
  )

  # vars must exist in site / horizon
  expect_error(
    NCSP(x, vars = c('sand', 'tacos'))
  )

  # depthSequence must be reasonable
  expect_error(
    NCSP(x, vars = c('sand', 'clay'), maxDepth = -5)
  )

})


# setup horizon-level data: data are from lab sampled pedons
d <- read.csv(
  textConnection('series,top,bottom,clay,frags,ph
auburn,0,3,21,6,5.6
auburn,3,15,21,13,5.6
auburn,15,25,20,9,5.8
auburn,25,47,21,28,5.8
dunstone,0,5,16,13,6
dunstone,5,17,17,19,6.3
dunstone,17,31,20,6,6.3
dunstone,31,41,21,15,6.3
sobrante,0,5,18,0,5.8
sobrante,5,10,16,2,5.7
sobrante,10,28,15,21,5.8
sobrante,28,51,18,13,6.2
sobrante,51,74,20,12,6.2')
)

# establish site-level data
s <- data.frame(
  series = c('auburn', 'dunstone', 'sobrante'), 
  precip = c(24, 30, 32)
)

# generate fake horizon names with clay / frags / ph
d$name <- with(d, paste(clay, frags, ph, sep='/'))

# upgrade to SoilProfile Collection object
depths(d) <- series ~ top + bottom
site(d) <- s
hzdesgnname(d) <- 'name'

## tests

test_that("NCSP works as expected", {
  
  # compute betwee-profile dissimilarity, no depth weighting
  d.dis <- suppressMessages(NCSP(d, vars = c('clay', 'ph', 'frags'), k = 0, maxDepth = 61))
  m <- as.matrix(d.dis)
  
  # results should be 3x3 distance matrix
  # this is a cluster package object
  expect_true(inherits(d.dis, c('dissimilarity', 'dist')))
  expect_equal(dim(m), c(3, 3))
  
  ## TODO: manually check these
  # known output
  expect_equivalent(diag(m), c(0, 0, 0))
  expect_equal(m[1,2], 33.03225, tolerance=0.00001)
  expect_equal(m[1,3], 49.02092, tolerance=0.00001)
  expect_equal(m[2,3], 45.94683, tolerance=0.00001)
})


