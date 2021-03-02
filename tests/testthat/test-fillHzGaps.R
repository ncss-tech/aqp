context("fillHzGaps")

# sample data
data(sp4)
depths(sp4) <- id ~ top + bottom

# introduce logic errors
idx <- c(2, 6:7, 8, 12)
sp4$top[idx] <- NA

# remove logic errors / create gaps
x <- HzDepthLogicSubset(sp4, byhz = TRUE)

# # gaps and now problematic profiles
# par(mar = c(0, 0, 0, 1))
# plotSPC(x, width = 0.3, default.color = 'royalblue')

# basic functionality
test_that("fillHzGaps", {
  
  # CRAN safe
  
  # fill / flag
  z <- fillHzGaps(x, flag = TRUE)
  
  # result is always this
  expect_true(inherits(z, 'SoilProfileCollection'))
  
  # flag column present
  expect_true('.filledGap' %in% horizonNames(z))
  
  # correct gaps have been filled
  expect_true(all(which(z[['.filledGap']]) == c(2L, 5L, 7L, 8L, 10L, 
                                                13L, 18L, 21L, 24L, 
                                                27L, 33L, 37L)))
  
  # calculated hzIDs are in ascending order
  expect_equal(hzID(z), as.character(1:nrow(z)))
  
  # fill just top to 10 cm
  z2 <- fillHzGaps(x, flag = TRUE, to_top = 10, to_bottom = NULL)
  expect_equal(which(z2[['.filledGap']]), c(2L, 6L, 10L))
  expect_equal(z2[3, 1]$top, 10L)
  
  # fill just bottom to 200 cm
  z3 <- fillHzGaps(x, flag = TRUE, to_top = NULL, to_bottom = 200)
  expect_equal(z3[, , .LAST]$bottom, rep(200L, length(z3)))
  
  # just fill the gaps (no top or bottom)
  z4 <- fillHzGaps(x, flag = TRUE, to_top = NULL, to_bottom = NULL)
  expect_equal(which(z4[['.filledGap']]), c(2L, 9L))

})
