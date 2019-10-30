context("denormalize - redundant horizon attributes from site")

data(sp1, package = 'aqp')

# create a SoilProfileCollection from horizon data
depths(sp1) <- ~ top + bottom

# create random site-level attribute `sitevar` with a binary (0/1) outcome
sp1$sitevar <- round(runif(length(sp1)))

test_that("denormalize result is 1:1 with horizons", {
# use denormalize() to create a mirror of sitevar in the horizon table
# name the attribute something different (e.g. `hz.sitevar`) to prevent collision with the site attribute
# the attributes can have the same name but you will then need site() or horizons() to access explicitly
  sp1.hz.sitevar <- denormalize(sp1, 'sitevar')
  
# compare number of horizons to number of values in denormalize result
  expect_equal(nrow(sp1), length(sp1.hz.sitevar)) # check that the output is 1:1 with horizon
  
  sp1$hz.sitevar <- sp1.hz.sitevar
})
