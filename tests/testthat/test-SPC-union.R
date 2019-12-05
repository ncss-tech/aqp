context("SoilProfileCollection union method")

## make sample data
data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

sp1$x <- seq(-119, -120, length.out = length(sp1))
sp1$y <- seq(38, 39, length.out = length(sp1))

sp::coordinates(sp1) <- ~ x + y
sp::proj4string(sp1) <- '+proj=longlat +datum=WGS84'

test_that("basic union tests", {
  
  # test data
  x <- sp1
  y <- sp1
  
  # alter horizon and site data in copy
  y$random <- runif(length(y))
  y$chroma <- NULL
  
  # add diagnostic hz
  diagnostic_hz(y) <- data.frame(id='P001', type='pizza')
  
  # this should not work, IDs aren't unqiue
  expect_error(union(list(x, y)))
  
  # fix IDs manually
  profile_id(y) <- sprintf("%s-copy", profile_id(y))
  
  # this should work
  z <- union(list(x,y))
  
  expect_true(inherits(z, 'SoilProfileCollection'))
  expect_equal(length(z), length(x) + length(y))
  
  # full site/hz names
  expect_equal(siteNames(z), unique(c(siteNames(x), siteNames(y))))
  expect_equal(horizonNames(z), unique(c(horizonNames(x), horizonNames(y))))
  
  # diagnostic features
  expect_equal(diagnostic_hz(z)[[idname(z)]], 'P001-copy')
  
})




test_that("non-conformal union tests", {
  
  # random data
  ids <- sprintf("%02d", 1:5)
  x <- plyr::ldply(ids, random_profile, n=c(6, 7, 8), n_prop=1, method='LPP', 
                   lpp.a=5, lpp.b=15, lpp.d=5, lpp.e=5, lpp.u=25)
  
  depths(x) <- id ~ top + bottom
  
  # more random data
  ids <- sprintf("%s", letters[1:5])
  y <- plyr::ldply(ids, random_profile, n=c(6, 7, 8), n_prop=4, method='LPP', 
                   lpp.a=5, lpp.b=15, lpp.d=5, lpp.e=5, lpp.u=25)
  
  # alter ID, top, bottom column names
  y$pID <- y$id
  y$hztop <- y$top
  y$hzbot <- y$bottom
  
  y$id <- NULL
  y$top <- NULL
  y$bottom <- NULL
  
  depths(y) <- pID ~ hztop + hzbot
  
  # very different data
  data('jacobs2000', package = 'aqp')
  
  # alter depth units
  depth_units(y) <- 'in'
  
  # should throw an error
  expect_error(union(list(x, y)), "inconsistent depth units")
  
  # reset depth units
  depth_units(y) <- 'cm'
  
  # attempt union
  z <- union(list(x, y, jacobs2000))
  
  # there should be a total of 17 profiles in the union
  expect_equal(sum(sapply(list(x, y, jacobs2000), length)), 17)
  expect_equal(length(z), sum(sapply(list(x, y, jacobs2000), length)))
  
  
})


test_that("union with non-conformal spatial data", {
  
  # test data
  x <- sp1
  y <- sp1
  z <- sp1
  
  # alter CRS, this generates an sp warning
  expect_warning(sp::proj4string(y) <- '+proj=utm +zone=10 +datum=NAD83')
  
  # should throw an error
  expect_error(union(list(x,y)), 'inconsistent CRS')
  
  # make IDs unique
  profile_id(y) <- sprintf("%s-copy", profile_id(y))
  profile_id(z) <- sprintf("%s-copy-copy", profile_id(z))
  
  # remove sp data for one
  z@sp <- new('SpatialPoints')
  
  # remove all CRS
  sp::proj4string(x) <- ''
  sp::proj4string(y) <- ''
  sp::proj4string(z) <- ''
  
  # should throw an error
  expect_error(union(list(x, y, z)), 'non-conformal point geometry')
  
  # drop spatial data and no error
  res <- union(list(x, y, z), drop.spatial = TRUE)
  expect_true(inherits(res, 'SoilProfileCollection'))
  
  ## TODO: different coordinate names
})


test_that("filtering NULL elements", {
  
  # test data
  x <- sp1
  y <- sp1
  profile_id(y) <- sprintf("%s-copy", profile_id(y))
  
  # add NULLs
  s <- list(NULL, x, y, NULL)
  
  # this should work
  res <- union(s)
  expect_true(inherits(res, 'SoilProfileCollection'))
})

