# base non-standard eval of keyword in ... "k-index": SPC[i, j, ...]
#  support for .LAST, .FIRST, .HZID special keywords
test_that(".LAST, .FIRST, .HZID, .NHZ k-keywords for SoilProfileCollection objects", {
  # define special symbols in global env
  # (not needed for tests, but needed wherever they are used in package)
  .FIRST <- NULL
  .LAST <- NULL
  .HZID <- NULL
  .NHZ <- NULL
  
  data(sp4)
  depths(sp4) <- id ~ top + bottom
  
  # .LAST
  expect_equal(length(sp4[, , .LAST]), 10)
  
  # .NHZ
  expect_equal(sp4[, , .NHZ], c(4, 2, 3, 4, 3,
                                2, 2, 2, 5, 3))
  
  # .HZID
  expect_equal(sp4[, , .HZID], 1:30)
  
  # .LAST .HZID
  expect_equal(sp4[, , .LAST, .HZID], c(4, 6, 9, 13, 16,
                                        18, 20, 22, 27, 30))
  
  # .FIRST sets j <- 1
  expect_equal(sp4[, , .FIRST, .HZID], sp4[, 1, , .HZID])
  
  # .FIRST ignores j input if given
  expect_equal(sp4[, 1000, .FIRST, .HZID], sp4[, 1, , .HZID])
  
  # .LAST ignores j input if given
  expect_equal(sp4[, , .LAST, .HZID], sp4[, 1000, .LAST, .HZID])
  
  # horizon index of horizons in profiles 5 to 10
  expect_equal(sp4[5:10, , .HZID], 14:30)
  
  # horizon index of 2nd horizon in profiles 5 to 10
  expect_equal(sp4[5:10, 2, .HZID], c(15, 18, 20, 22, 24, 29) - 13)
  
  # number of horizons
  expect_equal(sp4[5:10, 2, .NHZ],  c(1, 1, 1, 1, 1, 1))
})
