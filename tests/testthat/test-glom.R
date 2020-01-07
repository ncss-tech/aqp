context("glomming clods - ragged horizon intersection for SPCs")

data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

p <- sp1[6]
attr <- 'prop' # clay contents % 

test_that("intersection of horizons by depth", {
    # intersection at a single depth should return only one horizon
    expect_equal(sum(clod.hz.ids(p, 50) %in% hzID(p)), 1)
    
    # intersection from 25 to 100 should return four horizons
    expect_equal(sum(clod.hz.ids(p, 25, 100) %in% hzID(p)), 4)
})

test_that("glom by depth returns a SPC clod", {
  # glom 'gloms' your input SPC `p`'s horizons (by depths specified) into a 'clod'
  
  # currently "clods" can be either represented as an SPC, or a data.frame with just
  # the horizons that are contained within the "clod".
  foo <- glom(p, 25, 100)
  # and returns an SPC
  expect_true(inherits(foo, 'SoilProfileCollection'))
  
  # within that SPC there should be only one profile
  expect_equal(length(foo), 1)
  
  # and that profile should have 4 horizons in 25-100cm
  expect_equal(nrow(foo), 4) 
})

test_that("glom by depth returns a data.frame clod", {
  # glom 'gloms' your input SPC `p`'s horizons (by depths specified) into a 'clod'
  
  # currently "clods" can be either represented as an SPC, or a data.frame with just
  # the horizons that are contained within the "clod".
  foo <- glom(p, 25, 100, as.data.frame = TRUE)
  
  # and returns an data.frame
  expect_true(inherits(foo, 'data.frame'))
  
  # within that data.frame, length() returns 18
  expect_equal(length(foo), 18)
  
  # and that data.frame should have 4 horizons (rows) in 25-100cm
  expect_equal(nrow(foo), 4) 
})

