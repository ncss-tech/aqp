context("flagOverlappingHz")

# two overlapping horizons
z <- data.frame(
  id = 'SPC',
  top = c(0, 25, 25, 50, 75, 100, 100),
  bottom = c(25, 50, 50, 75, 100, 125, 125)
)

depths(z) <- id ~ top + bottom

# basic functionality
test_that("flagOverlappingHz", {
  
  .overlapFlag <- flagOverlappingHz(z)
  
  # logical vector
  expect_true(length(.overlapFlag) == nrow(z))
  expect_true(inherits(.overlapFlag, 'logical'))
  
  # not overlapping horizons
  expect_true(all( !.overlapFlag[c(1, 4, 5)]))
  
  # overlapping horizons
  expect_true(all(.overlapFlag[c(2, 3, 6, 7)]))
  
})

