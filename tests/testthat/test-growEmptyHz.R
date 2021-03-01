context("growEmptyHz")

data(sp4)
depths(sp4) <- id ~ top + bottom

x <- sp4

# remove 1st horizon
y <- x
idx <- y[,, .FIRST, .HZID]
replaceHorizons(y) <- horizons(y)[-idx[1:4], ]




test_that("grow empty horizons down", {
  
  # grow to 100cm, all profiles modified
  g <- growEmptyHz(x, z = 100, direction = 'down')
  expect_true(all(g[,, .LAST]$bottom == 100))
  
  # grow to 10cm, no profiles modified
  g <- growEmptyHz(x, z = 10, direction = 'down')
  expect_true(all(x[,, .LAST]$bottom == g[,, .LAST]$bottom))
  
  # grow to 45cm, not all profiles modified
  g <- growEmptyHz(x, z = 45, direction = 'down')
  expect_false(all(g[,, .LAST]$bottom == 45))
  
})


test_that("grow empty horizons up", {
  
  # grow up to 0cm
  g <- growEmptyHz(y, z = 0, direction = 'up')
  expect_true(all(g[,, .FIRST]$top == 0))
  
  # grow up to 15cm, no profiles modified
  g <- growEmptyHz(y, z = 15, direction = 'up')
  expect_true(all(y[,, .FIRST]$top == g[,, .FIRST]$top))
  
})

