context("SoilProfileCollection object construction / deconstruction")



test_that("SPC construction from a data.frame", {
  
  data(sp1, package = 'aqp')
  depths(sp1) <- id ~ top + bottom
  site(sp1) <- ~ group
  
  # did it work?
  expect_match(class(sp1), 'SoilProfileCollection')
  
  # ID correctly initialized?
  expect_equal(idname(sp1), 'id')
  expect_true(length(profile_id(sp1)) == length(sp1))
  
  # ID in the correct order?
  expect_identical(profile_id(sp1), site(sp1)[[idname(sp1)]])
  
  # depth names?
  expect_equal(horizonDepths(sp1), c('top', 'bottom'))
  
  # site-level attributes correctly initialized?
  expect_true(length(sp1$group) == length(sp1))
  
  # correct number of profiles and horizons?
  expect_equal(length(sp1), 9)
  expect_equal(nrow(sp1), 60)
})


test_that("SPC deconstruction into a data.frame", {
  
  data(sp1, package = 'aqp')
  depths(sp1) <- id ~ top + bottom
  site(sp1) <- ~ group
  
  h <- horizons(sp1)
  s <- site(sp1)
  d <- as(sp1, 'data.frame')
  
  expect_match(class(h), 'data.frame')
  expect_match(class(s), 'data.frame')
  expect_match(class(d), 'data.frame')
})


test_that("SPC subsetting ", {
  
  data(sp1, package = 'aqp')
  depths(sp1) <- id ~ top + bottom
  site(sp1) <- ~ group
  
  # profile subsets
  expect_match(class(sp1[1, ]), 'SoilProfileCollection')
  expect_match(class(sp1[1:5, ]), 'SoilProfileCollection')
  
  # profile and horizon subsets
  expect_match(class(sp1[1, 1]), 'SoilProfileCollection')
  
  # there should only be 1 profile and 1 horizon
  expect_equal(length(sp1[1, 1]), 1)
  expect_equal(nrow(sp1[1, 1]), 1)
  
  # there should be 5 profiles and 1 horizon / profile
  expect_equal(length(sp1[1:5, 1]), 5)
  expect_equal(nrow(sp1[1:5, 1]), 5)
})
