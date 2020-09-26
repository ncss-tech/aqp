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
})

test_that("depthWeights parity", {
  # depthWeights calculates contributing fractions within glom intervals for a profile p
  wts <- depthWeights(p$top, p$bottom, 25, 100)

  # wrong length errors
  expect_error(depthWeights(p$top, p$bottom, c(25,50), 100))
  expect_error(depthWeights(p$top[1], p$bottom, 25, 100))

  expect_equal(wts, c(0, 0, 0.32, 0.186666666666667, 0.36, 0.133333333333333, 0, 0, 0, 0))
  expect_equal(sum(wts), 1)
  glmp <- glom(p, 25, 100, truncate = TRUE)
  glmp$thk <- glmp$bottom - glmp$top
  glmp$relthk <- glmp$thk / sum(glmp$thk)
  expect_equal(glmp$relthk, wts[wts > 0])
})

test_that("degenerate single horizon case", {
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

test_that("glomApply works as expected", {

  # using same depth returns a single horizon from every profile
  # glomApply with same z1 as z2
  expect_silent(res <- glomApply(sp1, function(p) return(c(25,25))))

  # above is the same as calling glom with just z1 specified (for first profile in sp1)

  # there are slight differences in hzID due to glomApply using pbindlist internally after glom-ing
  expect_equivalent(res[1,], glom(sp1[1,], 25))

  # they are equivalent but not equal
  expect_error(expect_equal(res[1,], glom(sp1[1,], 25)))

  # after pbindlist hzID 3 becomes 1 (since sp1 does not have a true hzID specified)
  tdepths <- horizonDepths(sp1)
  expect_equal(horizons(res[1,])[,tdepths], horizons(glom(sp1[1,], 25))[,tdepths])

  # every profile returns one horizon (all profiles at least 25cm deep)
  expect_true(all(profileApply(res, nrow) == 1))

  # glom returns empty results for some of these profiles at 200cm
  #  glom will produce a NULL, and pbindlist will drop that profile
  expect_warning(res2 <- glomApply(sp1, function(p) return(c(200,200))))

    ## 7 profiles have 200 as an invalid upper bound
    ## note that P006 and P008 end EXACTLY at 200 and are NOT included
    # plot(sp1)
    # abline(h = 200)
  expect_equal(names(profileApply(res2, nrow)), c("P007", "P009"))

  test_that("realistic glomApply scenarios", {
    data(sp3)
    depths(sp3) <- id ~ top + bottom

    # constant depths, whole horizon returns by default
    expect_warning(glomApply(sp3, function(p) c(25,100)))

    # constant depths, truncated
    #(see aqp::trunc for helper function)
    expect_silent(glomApply(sp3, function(p) c(25,30), truncate = TRUE))

    # constant depths, inverted
    expect_warning(glomApply(sp3, function(p) c(25,100), invert = TRUE))

    # constant depths, inverted + truncated (same as above)
    expect_silent(res <- glomApply(sp3, function(p) c(25,30), invert = TRUE, truncate = TRUE))

    # before invert, 46 horizons
    expect_equal(nrow(sp3), 46)

    # after glom invert, some horizons are split, increasing the total number
    expect_equal(nrow(res), 52)

    # random boundaries in each profile the specific warnings are a product of pseudorandom numbers
    set.seed(100)
    expect_warning(glomApply(sp3, function(p) round(sort(runif(2, 0, max(sp3))))))

    # random boundaries in each profile (truncated)
    expect_warning(glomApply(sp3, function(p) round(sort(runif(2, 0, max(sp3)))), truncate = TRUE))

    # calculate some boundaries as site level attribtes
    expect_warning(sp3$glom_top <- profileApply(sp3, getMineralSoilSurfaceDepth))
    expect_silent(sp3$glom_bottom <- profileApply(sp3, estimateSoilDepth))

    # use site level attributes for glom intervals for each profile
    expect_silent(glomApply(sp3, function(p) return(c(p$glom_top, p$glom_bottom))))
  })

  # trunc wrapper function for constant depths works as expected
  expect_equal({
      glomApply(sp1, function(p) return(c(25,36)), truncate = TRUE)
    },{
      trunc(sp1, 25, 36)
    })
})


