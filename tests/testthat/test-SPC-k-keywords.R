# base non-standard eval of keyword in ... "k-index": SPC[i, j, ...]
#  support for .LAST, .FIRST, .HZID special keywords
context(".LAST, .FIRST, .HZID k-keywords for SoilProfileCollection objects")

# define special symbols in global env 
# (not needed for tests, but needed wherever they are used in package)
.FIRST <- NULL
.LAST <- NULL
.HZID <- NULL

data(sp4)
depths(sp4) <- id ~ top + bottom

# .LAST
expect_equal(length(sp4[, , .LAST]), 10)

expect_equal(sp4[, , .HZID], 1:30)

expect_equal(sp4[, , .LAST, .HZID], 
             c(4L, 6L, 9L, 13L, 16L, 18L, 20L, 22L, 27L, 30L))
# .FIRST sets j <- 1
expect_equal(sp4[, , .FIRST, .HZID], 
             sp4[, 1, , .HZID])

# .FIRST ignores j input if given
expect_equal(sp4[, 1000, .FIRST, .HZID], 
             sp4[, 1, , .HZID])

# .LAST ignores j input if given
expect_equal(sp4[, , .LAST, .HZID],
             sp4[, 1000, .LAST, .HZID])

# horizon index of 2nd horizon in each profile
expect_equal(sp4[5:10, 2, .HZID],
             c(15L, 18L, 20L, 22L, 24L, 29L))
