context("warpHorizons()")


test_that("warping factor", {
  
  s <- quickSPC('p1:AA|Bt1Bt1Bt1|Bt2Bt2B|Bt3|Cr|RRRRR')
  
  # identity
  expect_message(w <- warpHorizons(s))
  expect_true(inherits(w, 'SoilProfileCollection'))
  expect_equal(s[, , .BOTTOM], w[, , .BOTTOM])
  
  w <- warpHorizons(s, fact = 2)
  expect_true(inherits(w, 'SoilProfileCollection'))
  expect_equal(max(w), 280)
  
  w <- warpHorizons(s, fact = 0.5)
  expect_true(inherits(w, 'SoilProfileCollection'))
  expect_equal(max(w), 70)
  
  w <- warpHorizons(s, fact = c(1.3, 0.7, 0.8, 1, 1, 1))
  expect_true(inherits(w, 'SoilProfileCollection'))
  expect_equal(max(w), 133)
  
})

test_that("scaleTo", {
  
  s <- quickSPC('p1:AA|Bt1Bt1Bt1|Bt2Bt2B|Bt3|Cr|RRRRR')
  
  # exact target depth currently impossible due to rounding
  w <- warpHorizons(s, scaleTo = 100, soilDepthFun = estimateSoilDepth)
  expect_true(inherits(w, 'SoilProfileCollection'))
  expect_true(
    abs(estimateSoilDepth(w) - 100) < 2
  )
  
  # exact target depth currently impossible due to rounding
  w <- warpHorizons(s, scaleTo = 100, soilDepthFun = max)
  expect_true(inherits(w, 'SoilProfileCollection'))
  expect_true(
    abs(max(w) - 100) < 2
  )

})

test_that("warping factor is a horizon level attribute", {
  
  s <- quickSPC('p1:AA|Bt1Bt1Bt1|Bt2Bt2B|Bt3|Cr|RRRRR')
  s$wf <- runif(nrow(s), min = 0.5, max = 1.5)
  
  w <- warpHorizons(s, fact = 'wf')
  expect_true(inherits(w, 'SoilProfileCollection'))
})


