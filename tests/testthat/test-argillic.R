context("argillic bounds and clay increase")

data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

p <- sp1[1]
attr <- 'prop' # clay contents % 

#implicitly testing the basic logic of this function :)
threshold.fun <- crit.clay.argillic 

#standard for argillic
vertical.distance=30

test_that("get.increase.matrix() (used for getArgillicBounds())", {
  m <- get.increase.matrix(p, attr, threshold.fun, vertical.distance) 
  
  # correct data type?
  expect_true(inherits(m, 'matrix'))
  
  # is square matrix?
  expect_true(nrow(m) == ncol(m))
  
  # no TRUE in lower triangle?
  expect_true(all(!(m * lower.tri(m))))
  
  # correct number of eluvial horizons identified in first illuvial column?
  # this is also a test of the underlying crit.clay.argillic
  expect_equal(colSums(m)[4], 2)
})

test_that("argillic.clay.increase() (used for getArgillicBounds())", {
  #make sure the main wrapper method for the increase functions works
  d <- argillic.clay.increase.depth(p, 'prop')
  
  # this is also a test of the underlying crit.clay.argillic
  expect_equal(d, 49)
})

test_that("getArgillicBounds()", {
  d <- getArgillicBounds(p, hzdesgn='name', attr='prop')
  
  # this makes sure estimateSoilDepth() isn't broken...
  expect_equivalent(d, c(49, 89))
})
