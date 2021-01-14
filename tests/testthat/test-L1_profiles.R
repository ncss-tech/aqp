context("L1 profiles")

## TODO: think about a good test dataset


test_that("works as expected", {
  
  # overly simplistic example data
  p <- lapply(as.character(1:20), random_profile, method = 'LPP', SPC = TRUE)
  p <- combine(p)
  
  # init site data with groups
  site(p)$group <- rep(letters[1:2], times = 10)
  p$group <- factor(p$group)
  
  # set horizon designation
  horizons(p)$name <- 'XX'
  hzdesgnname(p) <- 'name'
  
  # try it
  z <- L1_profiles(p, fm = group ~ p1 + p2 + p3, method = 'constant', maxDepthRule = 'max', maxDepthConstant = 100)
  
  # plotSPC(z, color = 'p1', divide.hz = FALSE, name = NA, width = 0.2)
  
  
  # result is an SPC
  expect_true(inherits(z, 'SoilProfileCollection'))
  
  # conservation of group IDs
  expect_true(all(profile_id(z) == c('a', 'b')))
  
})



