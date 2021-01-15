context("segment")


test_that("data.frame interface works as expected", {
  
  # init local copy of sample data
  data(sp1)
  
  # trimming
  z <- segment(sp1, intervals = c(0, 10, 20, 30), trim = TRUE, hzdepcols = c('top', 'bottom'))
  
  # correct object type and segment label
  expect_true(inherits(z, 'data.frame'))
  expect_true('segment_id' %in% names(z))
  
  # label class
  expect_true(inherits(z[['segment_id']], 'character'))
  
  # no triming
  z <- segment(sp1, intervals = c(0, 10, 20, 30), trim = FALSE, hzdepcols = c('top', 'bottom'))
  
  # correct object type and segment label
  expect_true(inherits(z, 'data.frame'))
  expect_true('segment_id' %in% names(z))
  
  # label class
  expect_true(inherits(z[['segment_id']], 'character'))
})


test_that("SPC interface works as expected", {
  
  # init local copy of sample data
  data(sp1)
  depths(sp1) <- id ~ top + bottom
  
  # trimming
  z <- segment(sp1, intervals = c(0, 10, 20, 30), trim = TRUE)
  
  expect_true(inherits(z, 'SoilProfileCollection'))
  expect_true('segment_id' %in% horizonNames(z))
  
  # label class
  expect_true(inherits(z[['segment_id']], 'character'))
  
  # no trimming
  z <- segment(sp1, intervals = c(0, 10, 20, 30), trim = FALSE)
  
  expect_true(inherits(z, 'SoilProfileCollection'))
  expect_true('segment_id' %in% horizonNames(z))
  
  # label class
  expect_true(inherits(z[['segment_id']], 'character'))
  
})



test_that("expected outcome with NA horizon depths", {
  
  # init local copy of sample data
  data(sp1)
  
  # copies
  good <- sp1
  bad <- sp1
  
  # add NA to horizon depths
  bad$top[c(1, 5)] <- NA
  
  # segment
  z.bad <- segment(bad, intervals = c(0, 10, 20, 30), trim = TRUE, hzdepcols = c('top', 'bottom'))
  z.good <- segment(good, intervals = c(0, 10, 20, 30), trim = TRUE, hzdepcols = c('top', 'bottom'))
  
  # label class
  expect_true(inherits(z.good[['segment_id']], 'character'))
  expect_true(inherits(z.bad[['segment_id']], 'character'))
  
  ## TODO: is this expected?
  # row count
  expect_false(nrow(z.good) == nrow(z.bad))
  
  # same values
  # expect_false(all(z.good$segment_id == z.bad$segment_id))
  
})


test_that("expected outcome with bogus horizon depths", {
  
  # init local copy of sample data
  data(sp1)
  
  # copies
  good <- sp1
  bad <- sp1
  
  # add NA to horizon depths
  bad$top[c(1, 5)] <- bad$bottom[c(1, 5)]
  
  # segment
  z.bad <- segment(bad, intervals = c(0, 10, 20, 30), trim = TRUE, hzdepcols = c('top', 'bottom'))
  z.good <- segment(good, intervals = c(0, 10, 20, 30), trim = TRUE, hzdepcols = c('top', 'bottom'))
  
  # label class
  expect_true(inherits(z.good[['segment_id']], 'character'))
  expect_true(inherits(z.bad[['segment_id']], 'character'))
  
  ## TODO: is this expected?
  # row count
  expect_false(nrow(z.good) == nrow(z.bad))
  
  # same values
  # expect_false(all(z.good$segment_id == z.bad$segment_id))
  
})




test_that("same results as weighted mean via slab", {

  # 100 random data
  s <- lapply(1:100, random_profile, n_prop = 1, SPC = TRUE, method = 'random_walk')
  s <- combine(s)
  
  # weighted mean via slab
  a.slab <- slab(s, fm = ~ p1, slab.structure = c(0, 10, 20, 30), slab.fun = mean, na.rm = TRUE)
  
  # segment
  z <- segment(s, intervals = c(0, 10, 20, 30), trim = TRUE)
  
  # compute horizon thickness weights
  z <- horizons(z)
  z$thick <- z$bottom - z$top
  
  # weighted mean from segment output
  a.segment <- sapply(split(z, z$segment_id), function(i) {
    weighted.mean(i$p1, i$thick)
  })
  
  # inspect as needed
  res <- data.frame(
    slab = a.slab$value,
    segment = a.segment,
    diff = a.slab$value - a.segment
  )
  
  expect_true(all(res$diff < 0.001))
  
})




