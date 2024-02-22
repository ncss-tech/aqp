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


# more complex edge case
x <- data.frame(peiid = c("1373969", "1373969", "1373969", "1373969", 
"1373969", "1373969", "1373969", "1373969", "1373969", "1373969", 
"1373969", "1373969"), hzdept = c(0L, 0L, 0L, 0L, 
26L, 45L, 77L, 102L, 102L, 102L, 185L, 205L), hzdepb = c(26L, 
26L, 26L, 26L, 45L, 77L, 102L, 185L, 185L, 185L, 205L, 220L), 
    hzname = c("Ap", "Ap", "Ap", "Ap", "Bt1", "Bt2", "BCt", "Cd", 
    "Cd", "Cd", "2C1", "2C2"), texture = c("L", "L", "L", "L", 
    "CL", "CL", "CL", "L", "L", "L", "SR- VFSL L", "S"))

depths(x) <- peiid ~ hzdept + hzdepb

test_that("edge case", {
  expect_equal(as.logical(flagOverlappingHz(x)), 
               c(TRUE, TRUE, TRUE, TRUE, 
                 FALSE, FALSE, FALSE, 
                 TRUE, TRUE, TRUE, 
                 FALSE, FALSE))
})
