context("fillHzGaps")

# sample data
data(sp4)
depths(sp4) <- id ~ top + bottom


# basic functionality
test_that("fillHzGaps", {
  
  # CRAN safe
  
  # introduce logic errors
  idx <- c(2, 6:7, 8, 12)
  sp4$top[idx] <- NA
  
  # remove logic errors / create gaps
  x <- HzDepthLogicSubset(sp4, byhz = TRUE)
  
  # # gaps and now problematic profiles
  # par(mar = c(0, 0, 0, 1))
  # plotSPC(x, width = 0.3, default.color = 'royalblue')
  
  # fill / noflag
  z <- fillHzGaps(x, flag = FALSE)
  
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
  
  # filled horizons IDs start from the original max (30)
  expect_equal(hzID(z), c("1", "31", "3", "4", "32", "5", "33", "34", "9", "35", "10", 
                          "11", "36", "13", "14", "15", "16", "37", "17", "18", "38", "19", 
                          "20", "39", "21", "22", "40", "23", "24", "25", "26", "27", "41", 
                          "28", "29", "30", "42"))
  
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


test_that("multiple simultaneous arguments", {
  
  # remove 1st horizon for profiles 1:4
  y <- sp4
  idx <- y[,, .FIRST, .HZID]
  replaceHorizons(y) <- horizons(y)[-idx[1:4], ]
  
  # fill gaps AND NA-pad to top/bottom anchors
  z <- fillHzGaps(y, to_top = 0, to_bottom = 75, flag = TRUE)
  
  # result is always this
  expect_true(inherits(z, 'SoilProfileCollection'))
  
  # flag column present
  expect_true('.filledGap' %in% horizonNames(z))
  
  # test padding "up to" 0
  expect_true(min(z$top) == 0)
  
  # test padding "down to" 75
  expect_true(max(z) == 75)
  
})


test_that("ignore perfectly overlapping horizons", {
  
  y <- data.frame(
    id = 'SPC',
    top = c(0, 25, 25, 50, 75, 100, 100),
    bottom = c(25, 50, 50, 75, 100, 125, 125)
  )
  
  depths(y) <- id ~ top + bottom
  
  # fill gaps AND NA-pad to top/bottom anchors
  z <- fillHzGaps(y)
  
  # result is always this
  expect_true(inherits(z, 'SoilProfileCollection'))
  
  # flag column present
  expect_true('.filledGap' %in% horizonNames(z))
  
  # no filling required
  expect_true(all(!z$.filledGap))
  
})

test_that("gap + perfectly overlapping horizons", {
  
  y <- data.frame(
    id = 'SPC',
    top = c(0, 15, 25, 25, 50, 75, 100, 100),
    bottom = c(10, 25, 50, 50, 75, 100, 125, 125)
  )
  
  depths(y) <- id ~ top + bottom
  
  # fill gaps AND NA-pad to top/bottom anchors
  z <- fillHzGaps(y)
  
  # result is always this
  expect_true(inherits(z, 'SoilProfileCollection'))
  
  # flag column present
  expect_true('.filledGap' %in% horizonNames(z))
  
  # single filled horizons
  expect_true(z$.filledGap[2])
  
})

