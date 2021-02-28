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

test_that("fillHzGaps", {
  
  # CRAN safe
  
  z <- fillHzGaps(x, flag = TRUE)
  
  expect_true(inherits(z, 'SoilProfileCollection'))
  
  ## TODO: more tests
  
  # # BUG: plotSPC can't use logical data for color
  # z$.filledGap <- as.factor(z$.filledGap)
  # plotSPC(z, width = 0.3, color = '.filledGap', show.legend = FALSE)
  # 

})
