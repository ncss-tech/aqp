context("repairMissingHzDepths")


data(sp4)
depths(sp4) <- id ~ top + bottom

# copy to break
x <- sp4

## legal (deepest) horizons to repair:
# introduce NA
x$bottom[4] <- NA
# top == bottom
x$bottom[6] <- x$top[6]

## illegal horizons to repair
x$bottom[12] <- NA



test_that("works as expected", {
  
  a <- 10
  z <- repairMissingHzDepths(x, adj = a)
  
  # output is correct
  expect_true(inherits(z, 'SoilProfileCollection'))
  
  # legal hz have been repaired according to `a`
  expect_true(z$bottom[4] == z$top[4] + a)
  expect_true(z$bottom[6] == z$top[6] + a)
  
  # illegal hz have not
  expect_true(is.na(x$bottom[12]))
  
})
