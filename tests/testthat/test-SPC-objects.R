context("SoilProfileCollection init, methods, coercion")

## make sample data
data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

# add real coordinates
sp1$x <- seq(-119, -120, length.out = length(sp1))
sp1$y <- seq(38, 39, length.out = length(sp1))



## tests

test_that("SPC construction from a data.frame", {
  
  # did it work?
  expect_true(inherits(sp1, 'SoilProfileCollection'))
  
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
  expect_true(inherits(sp1.dh, 'data.frame'))
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
  
  expect_true(inherits(h, 'data.frame'))
  expect_true(inherits(s, 'data.frame'))
  expect_true(inherits(d, 'data.frame'))
})


test_that("SPC deconstruction into a list", {
  
  # do it here
  l <- as(sp1, 'list')
  
  # result should be a list
  expect_true(inherits(l, 'list'))
  
  # there should be no NULL data, e.g. missing slots
  res <- sapply(l, is.null)
  expect_false(any(res))
  
  # check internals
  expect_equivalent(l$idcol, idname(sp1))
  expect_equivalent(l$hzidcol, hzidname(sp1))
  expect_equivalent(l$depthcols, horizonDepths(sp1))
  expect_equivalent(l$metadata, metadata(sp1))
  expect_equivalent(l$horizons, horizons(sp1))
  expect_equivalent(l$site, site(sp1))
  expect_equivalent(l$sp, sp1@sp)
  expect_equivalent(l$diagnostic, diagnostic_hz(sp1))
  
})


test_that("SPC subsetting ", {
  
  # profile subsets
  expect_true(inherits(sp1[1, ], 'SoilProfileCollection'))
  expect_true(inherits(sp1[1:5, ], 'SoilProfileCollection'))
  
  # profile and horizon subsets
  expect_true(inherits(sp1[1, 1], 'SoilProfileCollection'))
  
  # there should only be 1 profile and 1 horizon
  expect_equal(length(sp1[1, 1]), 1)
  expect_equal(nrow(sp1[1, 1]), 1)
  
  # there should be 5 profiles and 1 horizon / profile
  expect_equal(length(sp1[1:5, 1]), 5)
  expect_equal(nrow(sp1[1:5, 1]), 5)
})



test_that("SPC graceful failure of spatial operations when data are missing", {
  
  # @sp has not been initialized
  expect_false(validSpatialData(sp1))
  
  # coercion should not work
  expect_error(as(sp1, 'SpatialPoints'))
  expect_error(as(sp1, 'SpatialPointsDataFrame'))
  
  # square-bracket indexing should work with n = 1
  # https://github.com/ncss-tech/aqp/issues/85
  s <- sp1[1, 1]
  expect_true(inherits(s, 'SoilProfileCollection'))
  
})


test_that("SPC spatial operations ", {
  
  # init / extract coordinates
  sp::coordinates(sp1) <- ~ x + y
  co <- sp::coordinates(sp1)
  
  # these are valid coordinates
  expect_true(validSpatialData(sp1))
  
  # coordinates should be a matrix
  expect_true(inherits(co, 'matrix'))
  # as many rows as length and 2 columns
  expect_equal(dim(co), c(length(sp1), 2))
  
  # coordinate columns should be removed from @site
  expect_true(all( ! dimnames(co)[[2]] %in% siteNames(sp1)))
  
  # CRS
  sp::proj4string(sp1) <- '+proj=longlat +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0'
  
  # we should get back the same thing we started with
  expect_equal(sp::proj4string(sp1), '+proj=longlat +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0')
  
  # basic coercion
  expect_true(inherits(as(sp1, 'SpatialPoints'), 'SpatialPoints'))
  
  # down-grade to {site + sp} = SpatialPointsDataFrame
  expect_message(as(sp1, 'SpatialPointsDataFrame'), 'only site data are extracted')
  sp1.spdf <- suppressMessages(as(sp1, 'SpatialPointsDataFrame'))
  expect_true(inherits(sp1.spdf, 'SpatialPointsDataFrame'))
  
  # implicity down-grade to SPDF via hz-subsetting
  sp1.spdf <- suppressMessages(sp1[, 1])
  expect_true(inherits(sp1.spdf, 'SpatialPointsDataFrame'))
  
  # again, with profile indexing
  sp1.spdf <- suppressMessages(sp1[1, 1])
  expect_true(inherits(sp1.spdf, 'SpatialPointsDataFrame'))
  
})


test_that("SPC misc. ", {
  
  # units
  depth_units(sp1) <- 'inches' 
  expect_equal(depth_units(sp1), 'inches')
  
  # metadata
  m <- metadata(sp1)
  m$citation <- 'this is a citation'
  metadata(sp1) <- m
  expect_true(inherits(metadata(sp1), 'data.frame'))
  expect_equal(ncol(metadata(sp1)), 2)
  
})


test_that("SPC depth columns get/set ", {
  
  # getting 
  hd <- horizonDepths(sp1)
  expect_equal(hd, c('top', 'bottom'))
  
  # setting
  hd.new <- c('t', 'b')
  horizonDepths(sp1) <- hd.new
  expect_equal(horizonDepths(sp1), hd.new)
  
  # error conditions
  expect_error(horizonDepths(sp1) <- NA)
  expect_error(horizonDepths(sp1) <- NULL)
  expect_error(horizonDepths(sp1) <- c(1,2,3))
  expect_error(horizonDepths(sp1) <- c('t'))
  expect_error(horizonDepths(sp1) <- c('t', NA))
  
  # warnings
  expect_warning(horizonDepths(sp1) <- c('t', '2342sdrse'))
  
})


test_that("SPC horizonNames get/set ", {
  
  # getting 
  hn <- horizonNames(sp1)
  expect_equal(hn, c("id", "top", "bottom", "bound_distinct", "bound_topography",
                     "name", "texture", "prop", "structure_grade", "structure_size", 
                     "structure_type", "stickiness", "plasticity", "field_ph", "hue", 
                     "value", "chroma", "hzID"))
  
  # setting
  idx <- match('chroma', hn)
  hn[idx] <- 'g'
  horizonNames(sp1) <- hn
  expect_equal(horizonNames(sp1), hn)
  
  # error conditions
  expect_error(horizonNames(sp1) <- NA)
  expect_error(horizonNames(sp1) <- NULL)
  expect_error(horizonNames(sp1) <- c(1,2,3))
  expect_error(horizonNames(sp1) <- c('t'))
  expect_error(horizonNames(sp1) <- c('t', NA))
  expect_error(horizonNames(sp1) <- hn[-1])
  
  # warnings
  hn[idx] <- '   g'
  expect_warning(horizonNames(sp1) <- hn)
  
})


test_that("SPC horizon ID get/set ", {
  
  # automatically generated horizon IDs
  auto.hz.ids <- hzID(sp1)
  
  # should be 1:nrow(sp1)
  expect_equivalent(auto.hz.ids, seq_len(nrow(sp1)))
  
  # try replacing with reasonable IDs
  hzID(sp1) <- rev(hzID(sp1))
  expect_equivalent(hzID(sp1), rev(seq_len(nrow(sp1))))
  
  # try replacing with bogus values
  expect_error(hzID(sp1) <- 1)
  # non-unique
  expect_error(hzID(sp1) <- sample(hzID(sp1), replace = TRUE))
  
})


test_that("SPC horizon ID name get/set ", {
  
  # check default
  expect_equivalent(hzidname(sp1), 'hzID')
  
  # make a new horizon ID
  sp1$junk <- 1:nrow(sp1)
  hzidname(sp1) <- 'junk'
  expect_equivalent(hzidname(sp1), 'junk')
  
  # error conditions:
  # no column
  expect_error(hzidname(sp1) <- 'xxx')
  
  # not unique
  expect_error(hzidname(sp1) <- 'top')
  
})

test_that("SPC horizon ID get/set ", {
  
  # automatically generated horizon IDs
  auto.hz.ids <- hzID(sp1)
  
  # should be 1:nrow(sp1)
  expect_equivalent(auto.hz.ids, seq_len(nrow(sp1)))
  
  # try replacing with reasonable IDs
  hzID(sp1) <- rev(hzID(sp1))
  expect_equivalent(hzID(sp1), rev(seq_len(nrow(sp1))))
  
  # try replacing with bogus values
  expect_error(hzID(sp1) <- 1)
  # non-unique
  expect_error(hzID(sp1) <- sample(hzID(sp1), replace = TRUE))
  
})


test_that("SPC profile ID get/set ", {
  
  # original
  pIDs <- profile_id(sp1)
  
  # new
  pIDs.new <- sprintf("%s-copy", pIDs)
  
  # try re-setting
  profile_id(sp1) <- pIDs.new
  
  # were the IDs altered?
  expect_equivalent(profile_id(sp1), pIDs.new)
  
  # bogus edits
  expect_error(profile_id(sp1) <- 1)
  expect_error(profile_id(sp1) <- sample(pIDs, replace = TRUE))
  expect_error(profile_id(sp1) <- c(NA, pIDs[-1]))
  
})

test_that("SPC profile ID reset integrity: site", {
  
  # test site 
  data(sp4)
  depths(sp4) <- id ~ top + bottom
  
  # save old ID and replace with known pattern
  sp4$old_id <- profile_id(sp4)
  profile_id(sp4) <- sprintf("%s-zzz", profile_id(sp4))
  
  # stripping the pattern should return original labels, in order
  expect_equal(sp4$old_id, gsub('-zzz', '', profile_id(sp4)))
  
})


test_that("SPC profile ID reset integrity: horizon", {

  # test hz
  data(sp4)
  depths(sp4) <- id ~ top + bottom
  
  # save old ID and replace with known pattern
  sp4$old_id <- as.vector(unlist(horizons(sp4)[idname(sp4)]))
  profile_id(sp4) <- sprintf("%s-zzz", profile_id(sp4))
  
  # stripping the pattern should return original labels, in order
  new.ids <- as.vector(unlist(horizons(sp4)[idname(sp4)]))
  new.ids <- gsub(pattern='-zzz', replacement = '', x = new.ids)
  expect_equal(sp4$old_id, new.ids)
  
})

test_that("SPC horizon ID init conflicts", {
  
  # decompose, re-init and test for message
  x <- sp1
  x <- as(x, 'data.frame')
  expect_message(depths(x) <- id ~ top + bottom, "^using")
  expect_equivalent(hzidname(x), 'hzID')
  
  # decompose, add non-unique column conflicing with hzID
  x <- sp1
  x <- as(x, 'data.frame')
  x$hzID <- 1
  expect_warning(depths(x) <- id ~ top + bottom, "not a unique horizon ID, using")
  # test backup name
  expect_equivalent(hzidname(x), 'hzID_')
  
  # special case: IDs resulting from slice()
  s <- slice(sp1, 0:100 ~ .)
  expect_equivalent(hzidname(s), 'sliceID')
  # check to make sure hzID and sliceID are present
  expect_equal(grep('hzID|sliceID', horizonNames(s)), c(18, 20))
  
})

test_that("horizon slot set/merge", {
  x <- sp1
  
  # take unique site ID, horizon ID, and corresponding property (clay)
  hnew <- horizons(x)[, c(idname(x), hzidname(x), 'prop')]
  
  # do some calculation, create a few new variables
  hnew$prop100 <- hnew$prop / 100
  hnew$prop200 <- hnew$prop / 200
  hnew$prop300 <- hnew$prop / 300
  
  hnew$prop[1] <- 50
  
  # utilize horizons() merge() functionality to add all new variables in hnew to horizons
  horizons(x) <- hnew
  
  # verify new columns have been added
  expect_equivalent(horizons(x)[1,c('prop100','prop200','prop300')], c(0.13, 0.13 / 2, 0.13 / 3))
  
  # verify old columns have same names (i.e. no issues with duplication of column names in merge)
  expect_true(all(c(idname(x), hzidname(x), 'prop') %in% names(horizons(x))))
  
  # verify old columns have been updated
  expect_equivalent(horizons(x)[1,c('prop')], c(50))
})




