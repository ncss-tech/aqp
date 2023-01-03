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

  # test construction with disordered ID and top depths
  daf <- data.frame(id = c(2,2,2,1,1,1), top = c(4,3,2,4,3,2), bottom = c(5,4,3,5,4,3))

  # the input data profiles both have bad "top depth logic" (reversed order of horizons)
  expect_true(hzDepthTests(daf$top[1:3], daf$bottom[1:3])["depthLogic"])

  expect_silent({depths(daf) <- id ~ top + bottom})
  # inspect

  # plot(df) # plot "works" even with invalid depth logic

  # whole SPC is valid, regardless of whether order is corrected
  expect_true(spc_in_sync(daf)$valid)

  # however, after promotion, the depth logic from input data has been corrected
  expect_true(all(checkHzDepthLogic(daf)$valid))

  # the numeric IDs from the input data are in order
  expect_true(all(profile_id(daf) == as.character(1:2)))

})

test_that("SPC diagnostics and restrictions", {
  # diagnostic & restriction slot should be initialized as an empty data.frame
  sp1.dh <- diagnostic_hz(sp1)
  expect_true(inherits(sp1.dh, 'data.frame'))
  expect_equal(nrow(sp1.dh), 0)

  sp1.rh <- restrictions(sp1)
  expect_true(inherits(sp1.rh, 'data.frame'))
  expect_equal(nrow(sp1.rh), 0)
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
  expect_equivalent(names(l$metadata), names(metadata(sp1)))

  expect_equivalent(l$horizons, horizons(sp1))
  expect_equivalent(l$site, site(sp1))
  expect_equivalent(l$sp, sp1@sp)
  expect_equivalent(l$diagnostic, diagnostic_hz(sp1))
  expect_equivalent(l$restrictions, restrictions(sp1))

  # check internals after [-subsetting
  sp1.sub <- sp1[1:2,]
  # none of these slots should change, the others will be subset
  # verifying these are transferred ensures key info slots are handled
  # by the SPC subset method
  expect_equivalent(l$idcol, idname(sp1.sub))
  expect_equivalent(l$hzidcol, hzidname(sp1.sub))
  expect_equivalent(l$depthcols, horizonDepths(sp1.sub))
  expect_equivalent(names(l$metadata), names(metadata(sp1.sub)))
})


test_that("SPC subsetting ", {

  # profile subsets
  expect_true(inherits(sp1[1, ], 'SoilProfileCollection'))
  expect_true(inherits(sp1[1:5, ], 'SoilProfileCollection'))

  # profile and horizon subsets
  expect_true(inherits(sp1[1, 1], 'SoilProfileCollection'))

  # horizon subsets
  expect_true(inherits(sp1[, 2], 'SoilProfileCollection'))

  # there should only be 1 profile and 1 horizon
  expect_equal(length(sp1[1, 1]), 1)
  expect_equal(nrow(sp1[1, 1]), 1)

  # there should be 5 profiles and 1 horizon / profile
  expect_equal(length(sp1[1:5, 1]), 5)
  expect_equal(nrow(sp1[1:5, 1]), 5)

})

test_that("SPC subsetting with tidy verbs ", {

  # filter works as expected
  expect_equal(length(subset(sp1, structure_type == "PL")), 1)

  # ensure multiple expressions yields same result as single expression
  l1 <- subset(sp1, !is.na(texture), prop > mean(prop, na.rm=TRUE))
  l2 <- subset(sp1, !is.na(texture) & prop > mean(prop, na.rm=TRUE))
  expect_equivalent(length(l1), length(l2))

  # mixing of site and horizon level expressions is the intersection
  l1 <- subset(sp1, group == 2, prop > mean(prop, na.rm=TRUE))
  expect_equivalent(length(l1), 4)

  # grepSPC works as expected
  expect_equal(length(grepSPC(sp1, texture, "SCL")), 1)

  # subApply works as expected
  expect_equal(length(subApply(sp1, function(p) TRUE)), length(sp1))
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
  
  skip_if_not_installed("sf")
  
  # init / extract coordinates
  coordinates(sp1) <- ~ x + y
  co <- coordinates(sp1)
  
  # these are valid coordinates
  expect_true(validSpatialData(sp1))
  
  # coordinates should be a matrix
  expect_true(inherits(co, 'matrix'))
  
  # as many rows as length and 2 columns
  expect_equal(dim(co), c(length(sp1), 2))
  
  # coordinate columns should be removed from @site
  # expect_true(all(!dimnames(co)[[2]] %in% siteNames(sp1)))

  # set CRS
  expect_silent(aqp::crs(sp1) <- "OGC:CRS84")
  
  # get CRS (via crs(<SPC>) method)
  expect_true(nchar(crs(sp1)) > 0)
  
  # # basic coercion
  expect_true(inherits(as(sp1, 'SpatialPoints'), 'SpatialPoints'))

  # # down-grade to {site + sp} = SpatialPointsDataFrame
  expect_message(as(sp1, 'SpatialPointsDataFrame'), 'only site data are extracted')

  # retain SPC object when using unit-length j index
  sp1.spc <- suppressMessages(sp1[, 1])
  expect_true(inherits(sp1.spc, 'SoilProfileCollection'))

  # again, with profile indexing
  sp1.spc <- suppressMessages(sp1[1, 1])
  expect_true(inherits(sp1.spc, 'SoilProfileCollection'))

})


test_that("SPC misc. ", {

  # units
  depth_units(sp1) <- 'inches'
  expect_equal(depth_units(sp1), 'inches')

  # metadata
  m <- metadata(sp1)
  m$citation <- 'this is a citation'
  metadata(sp1) <- m
  expect_true(is.list(metadata(sp1)))


  expect_equal(length(metadata(sp1)$citation), 1)

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

test_that("SPC min/max overrides work as expected", {

  set.seed(20202)
  df <- lapply(1:10, random_profile, SPC = TRUE)
  df <- pbindlist(df)

  ## visually inspect output
  # profileApply(df, min)
  # profileApply(df, max)

  # both min and max should return 10cm
  expect_equal(min(df), 44)
  expect_equal(max(df), 134)

  expect_equal(min(df, v = "p2"), 44)
  expect_equal(max(df, v = "p2"), 134)
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
  expect_equivalent(auto.hz.ids, as.character(seq_len(nrow(sp1))))

  # try replacing with reasonable IDs
  hzID(sp1) <- rev(hzID(sp1))
  expect_equivalent(hzID(sp1), as.character(rev(seq_len(nrow(sp1)))))

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

  # warning conditions:
  # not unique, keep default
  expect_warning(hzidname(sp1) <- 'top')

})

test_that("SPC horizon designation/texcl name get/set ", {

  # check intended behavior of setters
  hzdesgnname(sp1) <- 'name'
  hztexclname(sp1) <- 'texture'

  expect_equivalent(hzdesgnname(sp1), 'name')
  expect_equivalent(hztexclname(sp1), 'texture')

  # check handy accessor for hz designations
  designations <- hzDesgn(sp1)
  expect_type(designations, 'character')
  expect_equal(length(designations), 60)

  # make a new horizon column
  sp1$junk <- rep("foo", nrow(sp1))
  hzdesgnname(sp1) <- 'junk'
  hztexclname(sp1) <- 'junk'
  expect_equivalent(hzdesgnname(sp1), 'junk')
  expect_equivalent(hztexclname(sp1), 'junk')

  # error conditions:
  # no column in horizon table 'xxx'
  expect_error(hzdesgnname(sp1) <- 'xxx')

  # set to empty
  expect_silent(hzdesgnname(sp1) <- '')

  # null when cannot find the column name
  expect_silent(designations <- hzDesgn(sp1))
  expect_true(is.null(designations))
})

test_that("SPC horizon ID get/set ", {

  # automatically generated horizon IDs
  auto.hz.ids <- hzID(sp1)

  # should be 1:nrow(sp1)
  expect_equivalent(auto.hz.ids, as.character(seq_len(nrow(sp1))))

  # try replacing with reasonable IDs
  hzID(sp1) <- rev(hzID(sp1))
  expect_equivalent(hzID(sp1), as.character(rev(seq_len(nrow(sp1)))))

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

context("SoilProfileCollection integrity")

test_that("SPC profile ID reset integrity: site", {

  # test site
  data(sp4)

  # message due to unordered site IDs
  expect_silent(depths(sp4) <- id ~ top + bottom)

  # save old ID and replace with known pattern
  sp4$old_id <- profile_id(sp4)
  profile_id(sp4) <- sprintf("%s-zzz", profile_id(sp4))

  # stripping the pattern should return original labels, in order
  expect_equal(sp4$old_id, gsub('-zzz', '', profile_id(sp4)))

})


test_that("SPC profile ID reset integrity: horizon", {

  # test hz
  data(sp4)

  # message due to unordered site IDs
  expect_silent(depths(sp4) <- id ~ top + bottom)

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
  expect_true(checkSPC(x))

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

test_that("horizons<- left-join", {
  x <- sp1

  # take unique site ID, horizon ID, and corresponding property (clay)
  hnew <- horizons(x)[, c(idname(x), hzidname(x), 'prop')]

  # do some calculation, create a few new variables
  hnew$prop100 <- hnew$prop / 100
  hnew$prop200 <- hnew$prop / 200
  hnew$prop300 <- hnew$prop / 300

  # change a value of existing variable
  hnew$prop[1] <- 50

  # utilize horizons<- left join
  expect_silent(horizons(x) <- hnew)

  # verify old columns have same names
  # (i.e. no issues with duplication of column names in merge)
  expect_true(all(c(idname(x), hzidname(x), 'prop') %in% names(horizons(x))))

  # verify old columns have same value
  clay_prop <- horizons(sp1)[2,'prop']
  expect_equivalent(horizons(x)[2, c('prop')], clay_prop)

  # verify new columns have been added
  # now with proper sorting; first profile, first horizon
  expect_equivalent(horizons(x)[2, c('prop100','prop200','prop300')],
                    c(clay_prop /  100, clay_prop / 200, clay_prop / 300))


})

test_that("ordering of profiles and horizons is retained after left-join", {
  # IDs that when sorted will not be in this order
  s <- c('a', "1188707", "1188710", "120786", "1207894", 'z')

  l <- lapply(s, random_profile)

  d <- do.call('rbind', l)

  # init SPC
  # message due to unordered site IDs
  expect_silent({depths(d) <- id ~ top + bottom})

  ## former bug on set of a new horizon-level attr
  d$zzz <- rep(NA, times = nrow(d))

  # previously mysterious warning message
  z <- d[1:5, ]

  # ordering of profile IDs (unique, from @horizons) != ordering of IDs in @site
  expect_true(all(profile_id(d) == site(d)[[idname(d)]]))
})

test_that("replaceHorizons<- works as expected", {
  x <- sp1

  # replacement with existing value -- works
  hz.before <- horizons(x)
  replaceHorizons(x) <- hz.before
  expect_equal(hz.before, horizons(x))

  # works when hzidname is missing, defaults to hzID
  expect_message(replaceHorizons(x) <- horizons(x)[,c(idname(x),
                                                      horizonDepths(x))])
  expect_equal(x$hzID, as.character(1:nrow(x)))

  # missing idname = error
  expect_error(replaceHorizons(x) <- horizons(x)[,c(horizonDepths(x))])

  # missing depths = error
  expect_error(replaceHorizons(x) <- horizons(x)[,c(idname(x))])
})

spc <- data.frame(id = do.call('c', as.list((lapply(1:4, function(i) rep(i, 10))))),
                  top = rep(0:9, 4), bottom = rep(1:10, 4))

depths(spc) <- id ~ top+bottom

rev.ord <- rev(1:nrow(spc@horizons))

test_that("basic integrity checks", {

  # a new SPC is valid
  expect_true(spc_in_sync(spc)$valid)

  spc@horizons <- spc@horizons[rev.ord,]

  # inverting the horizon order makes it invalid
  expect_true(!spc_in_sync(spc)$valid)

  # an empty spc derived from invalid spc is valid
  expect_true(spc_in_sync(spc[0,])$valid)

  # reordering the horizons with reorderHorizons resolves integrity issues
  expect_true(spc_in_sync(reorderHorizons(spc))$valid)

  # default reordering uses metadata$original.order, here we override and reverse it
  expect_false(spc_in_sync(reorderHorizons(spc, rev(spc@metadata$original.order)))$valid)

  # removing the metadata works because target order matches sequential order
  #  this cannot be guaranteed to be the case in general but is a reasonable default
  spc@metadata$target.order <- NULL
  expect_true(spc_in_sync(reorderHorizons(spc))$valid)

  # reordering horizons with any order works, even if invalid
  spc <- reorderHorizons(spc, target.order = c(20:40,1:19))
  expect_true(!spc_in_sync(spc)$valid)

  # inspect the hzids -- in this case we know they should be 20:40 then 1:19
  expect_true(all(spc[[hzidname(spc)]] == c(20:40,1:19)))

  # subset the broken SPC to get the 4th profile
  spc4 <- subset(spc, id == "4")

  # the target order reflects a reasonable result for the single profile SPC
  expect_true(all(metadata(spc4)$target.order == 1:10))

  # the subset of the broken SPC is valid
  expect_true(spc_in_sync(spc4)$valid)

})
