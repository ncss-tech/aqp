test_that('estimateAWC/correctAWC', {
  
  # test range of organic matter, loam FEF textures
  base.awc <- estimateAWC(texcl = c("l","l","l"), omcl = c(3, 2, 1), na.rm = TRUE)
  expect_equal(base.awc, c(0.21, 0.18, round(0.175, 2)))
  
  # assume base AWC for testing corrections independently
  base.expected <- c(0.21, 0.18, 0.17)
  
  # auto-recycling of singletons, 3 AWCs all corrected with 25% rf
  corrected.awc <- correctAWC(base.expected, total_rf = 25)
  expect_equal(corrected.awc, c(0.18, 0.15, 0.14))
  
  # using gravel-size fragments often gives somewhat more restrictive AWC 
  corrected.awc2 <- correctAWC(base.expected, total_rf = 25, gravel = 25)
  expect_equal(corrected.awc2, c(0.16, 0.15, 0.14))
  
  salty.awc <- correctAWC(base.expected, total_rf = 0, ec = 8)
  expect_equal(salty.awc, c(0.17, 0.14, 0.14))
  
  # NA handling 
  corrected.awc <- correctAWC(base.expected, total_rf = c(25, NA, 25), nullFragsAreZero = TRUE)
  expect_equal(corrected.awc, c(0.18, 0.18, 0.14))
  
  corrected.awc <- correctAWC(base.expected, total_rf = c(25, NA, 25), nullFragsAreZero = FALSE)
  expect_equal(corrected.awc, c(0.18, NA, 0.14))
  
  corrected.awc <- correctAWC(base.expected, ec = c(8, NA, 8), nullFragsAreZero = TRUE)
  expect_equal(corrected.awc, c(0.17, 0.18, 0.14))
  
  corrected.awc <- correctAWC(base.expected, ec = c(8, NA, 8), nullFragsAreZero = FALSE)
  expect_equal(corrected.awc, c(0.17, NA, 0.14))
  
  # multiple inputs (fragments + salts)
  corrected.awc <- correctAWC(base.expected, total_rf = c(25, 25, NA), ec = c(8, NA, 8), nullFragsAreZero = TRUE)
  expect_equal(corrected.awc, c(0.14, 0.15, 0.14))
  
  corrected.awc <- correctAWC(base.expected, total_rf = c(25, 25, NA), ec = c(8, NA, 8), nullFragsAreZero = FALSE)
  expect_equal(corrected.awc, c(0.14, NA, NA))
  
  # more limiting `gravel` is used over NA `total_rf`, only NA is for 2nd texture with NA ec with nullFragsAreZero=F
  corrected.awc <- correctAWC(base.expected, total_rf = c(25, 25, NA), gravel = c(25, 25, 25), ec = c(8, NA, 8), nullFragsAreZero = FALSE)
  expect_equal(corrected.awc, c(0.13, NA, 0.11))
})
