context("SoilProfileCollection combination methods")

## make sample data
data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

sp1$x <- seq(-119, -120, length.out = length(sp1))
sp1$y <- seq(38, 39, length.out = length(sp1))

initSpatial(sp1) <- ~ x + y
prj(sp1) <- 'OGC:CRS84'

test_that("basic combination tests", {

  # test data
  x <- sp1
  y <- sp1

  # alter horizon and site data in copy
  y$random <- runif(length(y))
  y$chroma <- NULL

  # add diagnostic hz
  diagnostic_hz(y) <- data.frame(id = 'P001', type = 'pizza')

  # this should not work, IDs aren't unqiue
  expect_error(combine(list(x, y)))

  # fix IDs manually
  profile_id(y) <- sprintf("%s-copy", profile_id(y))

  # this should work
  z <- combine(list(x, y))
  
  # as should this
  z2 <- combine(x, y)

  expect_true(inherits(z, 'SoilProfileCollection'))
  expect_true(inherits(z2, 'SoilProfileCollection'))
  expect_equal(length(z), length(x) + length(y))
  expect_equal(length(z2), length(x) + length(y))

  # full site/hz names
  expect_equal(siteNames(z), unique(c(siteNames(x), siteNames(y))))
  expect_equal(horizonNames(z), unique(c(horizonNames(x), horizonNames(y))))

  # diagnostic features
  expect_equal(diagnostic_hz(z)[[idname(z)]], 'P001-copy')

})

test_that("non-conformal combination tests", {

  # random data
  ids <- sprintf("%02d", 1:5)
  x <- do.call('rbind', lapply(ids, random_profile, n=c(6, 7, 8), n_prop=1, method='LPP',
                   lpp.a=5, lpp.b=15, lpp.d=5, lpp.e=5, lpp.u=25))

  depths(x) <- id ~ top + bottom

  # more random data
  ids <- sprintf("%s", letters[1:5])
  y <- do.call('rbind', lapply(ids, random_profile, n=c(6, 7, 8), n_prop=4, method='LPP',
                   lpp.a=5, lpp.b=15, lpp.d=5, lpp.e=5, lpp.u=25))

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
  expect_error(combine(x, y), "inconsistent depth units")

  # reset depth units
  depth_units(y) <- 'cm'

  # attempt combine
  z <- combine(list(x, y, jacobs2000))

  # there should be a total of 17 profiles in the result
  expect_equal(sum(sapply(list(x, y, jacobs2000), length)), 17)
  expect_equal(length(z), sum(sapply(list(x, y, jacobs2000), length)))

  # see https://github.com/ncss-tech/aqp/issues/163
  data(sp4)
  depths(sp4) <- id ~ top + bottom

  # profile to general realizations of
  p.idx <- 1

  # spike profile
  spike.idx <- 6

  horizons(sp4)$bdy <- 4
  p <- perturb(sp4[p.idx, ], n = 10, 
               boundary.attr = 'bdy', min.thickness = 2)

  site(p)$id <- NULL

  # these calls should produce same order result
  #  .:. combine uses depths<- internally
  z.1 <- combine(list(sp4[c(p.idx, spike.idx), ], p))
  z.2 <- combine(list(p, sp4[c(p.idx, spike.idx), ]))

  expect_true(spc_in_sync(z.1)$valid)
  expect_true(spc_in_sync(z.2)$valid)
  expect_true(all(profile_id(z.1) == profile_id(z.2)))
})


test_that("combine with non-conformal spatial data", {

  # test data
  x <- sp1
  y <- sp1
  z <- sp1

  # alter CRS, this generates an sp warning
  # 2020-07-12: now caught with all other rgdal 1.5-8+ warnings in proj4string,SoilProfileCollection-method
  prj(y) <- 'EPSG:26910' # NAD83 UTM Zone 10

  # this should not work, IDs aren't unique
  expect_error(expect_message(combine(list(x, y)),
                              "inconsistent CRS, dropping spatial metadata"),
               "non-unique profile IDs detected")

  # make IDs unique
  profile_id(y) <- sprintf("%s-copy", profile_id(y))
  profile_id(z) <- sprintf("%s-copy-copy", profile_id(z))

  res <- combine(list(x, y, z))
  
  # remove CRS (avoids "inconsistent CRS, dropping spatial metadata")
  # equivalent ways to "unset" CRS: "", NA, NULL
  prj(x) <- ''
  prj(y) <- NULL
  
  expect_message(res <- combine(list(x, y, z)), "inconsistent CRS, dropping spatial metadata")
  expect_true(inherits(res, 'SoilProfileCollection'))

  # coordinates are preserved, but projection is not
  expect_null(metadata(res)$projection)
  expect_null(metadata(res)$coordinates)
  
  ## TODO: different coordinate names
})


test_that("filtering NULL/NA elements", {

  # test data
  x <- sp1
  y <- sp1
  profile_id(y) <- sprintf("%s-copy", profile_id(y))

  # add NULLs
  s <- list(NULL, x, y, NULL)

  # this should work
  res <- combine(s)
  expect_true(inherits(res, 'SoilProfileCollection'))


  # add NULLs, different arrangement
  s <- list(x, NULL)

  # should work
  res <- combine(s)
  expect_true(inherits(res, 'SoilProfileCollection'))


  # add NAs
  s <- list(NA, x, y, NA)

  # this should work
  res <- combine(s)
  expect_true(inherits(res, 'SoilProfileCollection'))

  # this should NOT work
  expect_error(combine(s, na.rm = FALSE))

  # all NA ---> result is NULL
  s <- list(NA, NA, NA)
  expect_null(combine(s))

  # all NULL ---> result is NULL
  s <- list(NULL, NULL, NULL)
  expect_null(combine(s))
})

