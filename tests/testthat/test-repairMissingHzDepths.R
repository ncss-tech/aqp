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
  
  # output is an SPC
  expect_true(inherits(z, 'SoilProfileCollection'))
  
  # bottom depths repaired according to `a` [4,6] or adjacent top depths [12]
  expect_true(z$bottom[4] == z$top[4] + a)
  expect_true(z$bottom[6] == z$top[6] + a)
  expect_true(z$bottom[12] == sp4$bottom[12])
  
  # repair all missing bottom depths, existing data unaltered
  h <- data.frame(
    id = c(1, 1, 1, 2, 2, 2, 2, 3, 3),
    top = c(0:2, 0:3, 0:1) * 10,
    bottom = c(rep(NA_integer_, 7), c(10, 99))
  )
  
  # NA depths result in warnings
  expect_warning({
    depths(h) <- id ~ top + bottom
  })
  
  # all depth logic in resulting SPC is valid, existing data preserved
  g <- repairMissingHzDepths(h)
  expect_true(all(checkHzDepthLogic(g)$valid))
  expect_equal(g[3, ]$bottom[1:2], c(10, 99))
  expect_equal(min(g), 30)
  
  # no adj, max.depth only, existing data preserved
  f <- repairMissingHzDepths(h, adj = NA, max.depth = 200)
  expect_true(all(checkHzDepthLogic(f)$valid))
  expect_equal(f$bottom[c(3, 7, 8, 9)], c(200, 200, 10, 99))
  expect_equal(min(f), 99)
  
  # specifying max.depth too small, existing data preserved
  f$bottom[c(3,7)] <- NA
  d <- repairMissingHzDepths(f, max.depth = 20)
  expect_true(all(checkHzDepthLogic(d)$valid))
  expect_equal(d$bottom[c(3, 7, 8, 9)], c(30, 40, 10, 99))
  expect_equal(min(d), 30)
  
  # specifying max.depth too small with adj=NA gets an override to max(x), existing data preserved
  f$bottom[c(3,7)] <- NA
  d <- repairMissingHzDepths(f, adj = NA, max.depth = 20)
  expect_true(all(checkHzDepthLogic(d)$valid))
  expect_equal(d$bottom[c(3, 7, 8, 9)], c(99, 99, 10, 99))
  expect_equal(min(d), 99)
})
