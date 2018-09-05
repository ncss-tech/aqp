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
  expect_match(class(s), 'SoilProfileCollection')
  expect_match(class(s.d), 'data.frame')
  
  # there should be 101 horizon slices
  expect_equal(nrow(horizons(s[1, ])),  101)
  
  # ID correctly initialized?
  expect_equal(idname(sp1), 'id')
  expect_true(length(profile_id(sp1)) == length(sp1))
  
  # ID in the correct order?
  expect_identical(profile_id(sp1), site(sp1)[[idname(sp1)]])
  
  # depth names?
  expect_equal(horizonDepths(sp1), c('top', 'bottom'))
  
  # site-level attributes correctly initialized?
  expect_true(length(sp1$group) == length(sp1))
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

