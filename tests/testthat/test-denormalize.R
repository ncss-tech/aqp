context("denormalize - redundant horizon attributes from site")

data(sp1, package = 'aqp')

# create a SoilProfileCollection from horizon data
depths(sp1) <- id ~ top + bottom

# create random site-level attribute `sitevar` with a binary (0/1) outcome
sp1$sitevar <- round(runif(length(sp1)))

test_that("denormalize result is 1:1 with horizons", {
  # use denormalize() to create a mirror of sitevar in the horizon table
  # name the attribute something different (e.g. `hz.sitevar`) to prevent collision with the site attribute
  # the attributes can have the same name but you will then need site() or horizons() to access explicitly
  sp1.hz.sitevar <- denormalize(sp1, 'sitevar')

  expect_error(sp1.hz.sitevar <- denormalize(sp1, 'foo'))

  # compare number of horizons to number of values in denormalize result
  expect_equal(nrow(sp1), length(sp1.hz.sitevar)) # check that the output is 1:1 with horizon

  sp1$hz.sitevar <- sp1.hz.sitevar
})

test_that("round trip normalize/denormalize", {
  library(aqp)
  
  data(sp3)
  depths(sp3) <- id ~ top + bottom
  
  # create site var -- unique at site level
  site(sp3)$foo <- profile_id(sp3)
  
  # denormalize site var to horizon var (leaves foo in site)
  expect_error({sp3$foo <- denormalize(sp3, "foo")})
  
  # need to create a new variable for hz-denorm var
  sp3$foo2 <- denormalize(sp3, "foo")
  
  # inspect
  plot(sp3, color="foo2")
  
  # normalize to site (removes foo2 in horizon)
  site(sp3) <- ~ foo2
  
  # expected TRUE
  expect_true(all(sp3$foo == sp3$foo2))
  expect_true(all(sp3$foo2 == profile_id(sp3)))
  
  # commence the breakin'
  
  # make another `foo3`
  sp3$foo3 <- denormalize(sp3, "foo")
  
  # not appropriate for normalization (1:1 with horizon, not site)
  sp3$foo4 <- 1:nrow(sp3)
  
  # do that SPC dirty...
  expect_warning(site(sp3) <- ~ foo3 + foo4)
  
  # still valid
  expect_true(spc_in_sync(sp3)$valid)
  
  # didn't do anything
  expect_equal(length(sp3$foo3), nrow(sp3))
  expect_equal(length(sp3$foo4), nrow(sp3))
})
