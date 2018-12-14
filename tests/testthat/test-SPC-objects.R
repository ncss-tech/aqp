context("SoilProfileCollection init, methods, coercion")

## make sample data
data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

sp1$x <- seq(-119, -120, length.out = length(sp1))
sp1$y <- seq(38, 39, length.out = length(sp1))

## tests

test_that("SPC construction from a data.frame", {
  
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
  
  # diagnostic slot should be initialized as an empty data.frame
  sp1.dh <- diagnostic_hz(sp1)
  expect_equal(class(sp1.dh), 'data.frame')
  expect_equal(nrow(sp1.dh), 0)
})

test_that("SPC data.frame interface", {
  
  # init site-level data
  sp1$x <- seq(-119, -120, length.out = length(sp1))
  sp1$y <- seq(38, 39, length.out = length(sp1))
  
  # init hz-level data
  sp1$z <- runif(n = nrow(sp1))
  
  expect_equal(length(sp1$x), length(sp1))
  expect_equal(length(sp1$z), nrow(sp1))
})

test_that("SPC deconstruction into a data.frame", {
  
  # do it here
  h <- horizons(sp1)
  s <- site(sp1)
  d <- as(sp1, 'data.frame')
  
  expect_match(class(h), 'data.frame')
  expect_match(class(s), 'data.frame')
  expect_match(class(d), 'data.frame')
})


test_that("SPC subsetting ", {
  
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


test_that("SPC spatial operations ", {
  
  # init / extract coordinates
  sp::coordinates(sp1) <- ~ x + y
  co <- sp::coordinates(sp1)
  
  # coordinates should be a matrix
  expect_equal(class(co), 'matrix')
  # as many rows as length and 2 columns
  expect_equal(dim(co), c(length(sp1), 2))
  # coordinates should be removed from @site
  expect_true(all( ! dimnames(co)[[2]] %in% siteNames(sp1)))
  
  # CRS
  sp::proj4string(sp1) <- '+proj=longlat +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0'
  sp::proj4string(sp1)
  
  # we should get back the same thing we started with
  expect_equal(sp::proj4string(sp1), '+proj=longlat +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0')
  
  # down-grade site+sp
  sp1.spdf <- suppressMessages(as(sp1, 'SpatialPointsDataFrame'))
  expect_match(class(sp1.spdf), 'SpatialPointsDataFrame')
  
  ## TODO: this generates spurious messages
  # implicity down-grade to SPDF via hz-subsetting
  # sp1.spdf <- suppressMessages(sp1[, 1])
  # expect_match(class(sp1.spdf), 'SpatialPointsDataFrame')
  
})

test_that("SPC misc. ", {
  
  # units
  depth_units(sp1) <- 'inches' 
  expect_equal(depth_units(sp1), 'inches')
  
  # metadata
  m <- metadata(sp1)
  m$citation <- 'this is a citation'
  metadata(sp1) <- m
  expect_equal(class(metadata(sp1)), 'data.frame')
  expect_equal(ncol(metadata(sp1)), 2)
  
})


