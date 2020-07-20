context("glomming clods - ragged horizon intersection for SPCs")

data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

p <- sp1[6]
attr <- 'prop' # clay contents % 

test_that("intersection of horizons by depth", {
    # intersection at a single depth should return only one horizon
    expect_equal(sum(glom(p, 50, ids = TRUE) %in% hzID(p)), 1)
    
    # intersection from 25 to 100 should return four horizons
    expect_equal(sum(glom(p, 25, 100, ids = TRUE) %in% hzID(p)), 4)
    
    # glom tests with a degenerate case
    test <- data.frame(id = 1, top = 0, bottom = 100)
    depths(test) <- id ~ top + bottom
    
    # default glom
    expect_silent(t1 <- glom(test, 25, 50))
    expect_equal(t1$bottom - t1$top, 100)
    
    # thickest-modality default glom
    expect_silent(t1 <- glom(test, 25, 50, modality = "thickest"))
    expect_equal(t1$bottom - t1$top, 100)
    
    # truncate glom
    expect_silent(t1 <- glom(test, 25, 50, truncate = TRUE))
    expect_equal(t1$bottom - t1$top, 25)
    
    # thickest-modality truncate glom
    expect_silent(t1 <- glom(test, 25, 50, truncate = TRUE, modality = "thickest"))
    expect_equal(t1$bottom - t1$top, 25)
    
    # invert glom
    expect_silent(t1 <- glom(test, 25, 50, invert = TRUE))
    expect_equal(t1$bottom - t1$top, 100)
    
    # invert + truncate glom
    expect_silent(t1 <- glom(test, 25, 50, invert = TRUE, truncate = TRUE))
    expect_equal(t1$bottom - t1$top, c(25, 50))
    
    # thickest-modality invert + truncate glom
    expect_silent(t1 <- glom(test, 25, 50, invert = TRUE, truncate = TRUE, modality = "thickest"))
    expect_equal(t1$bottom - t1$top, 50)
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
  foo <- glom(p, 25, 100, df = TRUE)
  
  # and returns an data.frame
  expect_true(inherits(foo, 'data.frame'))
  
  # within that data.frame, length() returns 18
  expect_equal(length(foo), 18)
  
  # and that data.frame should have 4 horizons (rows) in 25-100cm
  expect_equal(nrow(foo), 4) 
})

test_that("glom truncate = TRUE works as expected", {
  # glom 'gloms' your input SPC `p`'s horizons (by depths specified) into a 'clod'
  
  # get truncated clod
  foo <- glom(p, 25, 100, truncate=TRUE)
  
  # and returns an data.frame
  expect_true(inherits(foo, 'SoilProfileCollection'))
  
  ## test that:
  # shallowest top truncated to z1
  # deepest bottom truncated to z2
  expect_equal(min(foo$top), 25)
  expect_equal(max(foo$bottom), 100)
  
})


