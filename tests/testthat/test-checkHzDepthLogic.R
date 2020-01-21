context("SoilProfileCollection depth error checking")

## sample data
data(sp3)
depths(sp3) <- id ~ top + bottom

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


