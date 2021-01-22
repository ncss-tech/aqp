context("aggregateSoilDepth")

data(sp1)
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

# set horizon designation in SPC
hzdesgnname(sp1) <- 'name'


## tests

test_that("works as expected", {
  
  a <- aggregateSoilDepth(sp1, groups = 'group', crit.prob = 0.9)
  
  # structure
  expect_true(inherits(a, 'data.frame'))
  expect_true(ncol(a) == 3)
  expect_true(nrow(a) == length(unique(sp1$group)))
  
  # values
  expect_true(all(a$group == c(1, 2)))
  
  
  ## TODO: cross-reference with getSoilDepthClass
  ## these don't currently give the same results
  
  # site(sp1) <- getSoilDepthClass(sp1)
  # tapply(sp1$depth, sp1$group, quantile, probs = c(0.9))
  
})



