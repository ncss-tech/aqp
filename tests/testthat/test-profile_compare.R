context("profile_compare")

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
sobrante,51,74,20,12,6.2'), stringsAsFactors=FALSE)

# establish site-level data
s <- data.frame(
  series=c('auburn', 'dunstone', 'sobrante'), 
  precip=c(24, 30, 32),
  stringsAsFactors=FALSE
)

# generate fake horizon names with clay / frags / ph
d$name <- with(d, paste(clay, frags, ph, sep='/'))

# upgrade to SoilProfile Collection object
depths(d) <- series ~ top + bottom
site(d) <- s

## tests

test_that("profile_compare works as expected", {
  
  skip_if_not_installed("scales")
  
  # compute between-profile dissimilarity, no depth weighting
  # warning is expected because this is deprecated
  expect_warning({
    d.dis <- suppressMessages(profile_compare(d, vars=c('clay', 'ph', 'frags'), k=0, 
                                              max_d=61, replace_na=TRUE, add_soil_flag=TRUE))
  })
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


