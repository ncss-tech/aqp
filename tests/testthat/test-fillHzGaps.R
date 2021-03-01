context("fillHzGaps")

# sample data
data(sp4)
depths(sp4) <- id ~ top + bottom

# introduce gaps
idx <- c(2, 8, 12)
sp4$top[idx] <- NA

# remove problematic horizons
# GAPS created
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
  expect_true(all(which(z[['.filledGap']]) == idx))
  
  # calculated hzIDs are in ascending order
  expect_equal(hzID(z), as.character(1:nrow(z)))
})
