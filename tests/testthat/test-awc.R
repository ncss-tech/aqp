context("estimateAWC and correctAWC - legacy AWC lookup tables")

test_that('basic functionality', {
  
  # test range of organic matter, loam FEF textures
  base.awc <- estimateAWC(c("l","l","l"), c(3, 2, 1), na.rm = TRUE)
  expect_equal(base.awc, c(0.21, 0.18, 0.17))
  
  # auto-recycling of singletons, 3 AWCs all corrected with 25% rf
  corrected.awc <- correctAWC(base.awc, total_rf = 25)
  expect_equal(corrected.awc, c(0.18, 0.15, 0.14))
  
  # using gravel-size fragments often gives somewhat more restrictive AWC 
  corrected.awc2 <- correctAWC(base.awc, total_rf = 25, gravel = 25)
  expect_equal(corrected.awc2, c(0.16, 0.15, 0.14))
  
  salty.awc <- correctAWC(base.awc, total_rf = 0, ec = 8)
  expect_equal(salty.awc, c(0.17, 0.14, 0.14))
})
