context("SoilProfileCollection depth error checking")

## sample data
data(sp3)

expect_silent({depths(sp3) <- id ~ top + bottom})

test_that("hzDepthTests works as expected", {
  hdep <- horizonDepths(sp3)

  # vector of top and bottom depths -> 4 logical test results
  res <- hzDepthTests(sp3[[hdep[1]]], sp3[[hdep[2]]])
  expect_equal(names(res)[res], c("depthLogic","overlapOrGap")) # depthLogic & overlapOrGap errors
  expect_equal(length(res), 4)

  # mismatched lengths (top and bottom must have same number of values)
  expect_error(hzDepthTests(sp3[[hdep[1]]], sp3[[hdep[2]]][1]))
})

test_that("checkHzDepthLogic() works as expected", {

  # these data should be clean
  res <- checkHzDepthLogic(sp3)

  # result is an data.frame
  expect_true(inherits(res, 'data.frame'))

  # number of rows should match length(SPC)
  expect_true(nrow(res) == length(sp3))

  # all clear
  expect_true(all( ! res$depthLogic))
  expect_true(all( ! res$sameDepth))
  expect_true(all( ! res$missingDepth))
  expect_true(all( ! res$overlapOrGap))
  expect_true(all(res$valid))
  
  # works on a horizons()-like data.frame
  h <- horizons(sp3)
  h$foo <- h$id
  h$id <- NULL
  h$hzdept <- h$top
  h$top <- NULL
  h$hzdepb <- h$bottom
  h$bottom <- NULL
  expect_true(all(checkHzDepthLogic(h, c("hzdept","hzdepb"), "foo", fast = TRUE)$valid))
})


test_that("checkHzDepthLogic() depth logic errors", {

  # local copy
  x <- sp3[1, ]
  x$top[1] <- 10
  res <- checkHzDepthLogic(x)

  # errors only affect the first profile in this set
  expect_true(res$depthLogic[1])
  expect_false(res$valid[1])
})

test_that("checkHzDepthLogic() same top / bottom depths", {

  # local copy
  x <- sp3[7, ]
  x$bottom[3] <- x$top[3]
  res <- checkHzDepthLogic(x)

  # errors only affect the first profile in this set
  expect_true(res$sameDepth[1])
  expect_false(res$valid[1])
})


test_that("checkHzDepthLogic() NA in depths", {

  # local copy
  x <- sp3[4, ]
  x$bottom[3] <- NA
  res <- checkHzDepthLogic(x)

  # errors only affect the first profile in this set
  expect_true(res$missingDepth[1])
  expect_false(res$valid[1])
})


test_that("checkHzDepthLogic() gap", {

  # local copy
  x <- sp3[8, ]
  # create a gap
  x$top[4] <- 82
  res <- checkHzDepthLogic(x)

  # errors only affect the first profile in this set
  expect_true(res$overlapOrGap[1])
  expect_false(res$valid[1])
})


test_that("checkHzDepthLogic() overlap", {

  # local copy
  x <- sp3[8, ]
  # create a gap
  x$top[4] <- 75
  res <- checkHzDepthLogic(x)

  # errors only affect the first profile in this set
  expect_true(res$overlapOrGap[1])
  expect_false(res$valid[1])
})

test_that("splitLogicErrors", {
  data(sp4)
  depths(sp4) <- id ~ top + bottom

  # no errors (all list elements return NULL)
  expect_equal(unlist(splitLogicErrors(sp4)), c(NULL, NULL, NULL, NULL))

  # NA in top depth triggers depth logic and missing depth errors
  data(sp4)
  sp4$top[1] <- NA
  expect_message(depths(sp4) <- id ~ top + bottom)

  res <- splitLogicErrors(sp4)
  
  # the same profile occurs in two groups, since NA causes depth logic and missingDepth errors
  expect_true(profile_id(res$depthLogic) == profile_id(res$missingDepth))

  # interact = TRUE gets these in the same (interaction) group
  #  each SPC profile occurs once, name/number elements varies with your data
  #  (and whether or not you use split.default(..., drop = TRUE))
  res2 <- splitLogicErrors(sp4, interact = TRUE, sep = "_", drop = TRUE)
  expect_true(length(res2$depthLogic__missingDepth_) == 1)
})

