context("slice method for SoilProfileCollection objects")



test_that("basic slice functionality", {
  
  data(sp1, package = 'aqp')
  depths(sp1) <- id ~ top + bottom
  site(sp1) <- ~ group
  
  # basic slice
  s <- slice(sp1, fm = 0:100 ~ ., top.down = TRUE, just.the.data = FALSE, strict = TRUE)
  # as a data.frame
  s.d <- slice(sp1, fm = 0:100 ~ ., top.down = TRUE, just.the.data = TRUE, strict = TRUE)
  
  # did it work?
  expect_true(inherits(s, 'SoilProfileCollection'))
  expect_true(inherits(s.d, 'data.frame'))
  
  # there should be 101 horizon slices
  expect_equal(nrow(horizons(s[1, ])),  101)
  
  # ID correctly initialized?
  expect_equal(idname(s), 'id')
  expect_true(length(profile_id(s)) == length(s))
  expect_equivalent(profile_id(s), profile_id(sp1))
  
  # ID in the correct order?
  expect_identical(profile_id(s), site(s)[[idname(s)]])
  
  # depth names?
  expect_equal(horizonDepths(s), horizonDepths(sp1))
  
  # site-level attributes correctly initialized?
  expect_true(length(s$group) == length(s))
  
  # original horizon IDs
  expect_true('hzID' %in% horizonNames(s))
  
  # new slice IDs
  expect_true('sliceID' %in% hzidname(s))
})



