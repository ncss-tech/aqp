context("flagOverlappingHz")

# two overlapping horizons
z <- data.frame(
  id = 'SPC',
  top = c(0, 25, 25, 50, 75, 100, 100),
  bottom = c(25, 50, 50, 75, 100, 125, 125)
)

depths(z) <- id ~ top + bottom

# basic functionality
test_that("flagOverlappingHz: perfect overlap in 2 horizons", {
  
  .overlapFlag <- flagOverlappingHz(z)
  
  # logical vector
  expect_true(length(.overlapFlag) == nrow(z))
  expect_true(inherits(.overlapFlag, 'logical'))
  
  # not overlapping horizons
  expect_true(all( !.overlapFlag[c(1, 4, 5)]))
  
  # overlapping horizons
  expect_true(all(.overlapFlag[c(2, 3, 6, 7)]))
  
})

z2 <- data.frame(
  id = 'SPC',
  top = c(0, 25, 25, 25, 50, 75, 100, 100),
  bottom = c(25, 45, 50, 50, 75, 100, 125, 125)
)

depths(z2) <- id ~ top + bottom

test_that("flagOverlappingHz: imperfect in 1 horizon, perfect in 2 horizons", {
  
  .overlapFlag <- flagOverlappingHz(z2)
  
  # logical vector
  expect_true(length(.overlapFlag) == nrow(z2))
  expect_true(inherits(.overlapFlag, 'logical'))
  
  # not overlapping horizons
  expect_true(all(!.overlapFlag[c(1, 2, 5, 6)]))
  
  # overlapping horizons
  expect_true(all(.overlapFlag[c(3, 4, 7, 8)]))
  
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


z3 <- data.frame(
  id = 'SPC1',
  top = c(0, 25, 25, 25, 50, 75, 100, 100),
  bottom = c(25, 45, 50, 50, 75, 100, 125, 125)
)

z4 <- data.frame(
  id = 'SPC2',
  top = c(0, 25, 50, 75, 100),
  bottom = c(25, 50, 75, 100, 125)
)

z5 <- rbind(z3, z4)
depths(z5) <- id ~ top + bottom

# test multiple profiles with some depths the same between both profiles
# but only the first profile has overlap (1 imperfect, 2 perfect)
test_that("multiple profiles, with and without overlap", {
 .overlapFlag <- flagOverlappingHz(z5)
 expect_false(any(.overlapFlag[9:13]))
})

z6 <- data.frame(
  id = 'SPC1',
  top = 0,
  bottom = 100
)

z7 <- data.frame(
  id = 'SPC2',
  top = c(0, 0, 100),
  bottom = c(100, 100, 200)
)

z8 <- rbind(z6, z7)
depths(z8) <- id ~ top + bottom

test_that("multiple profiles, edge case", {
  # first single horizon profile matches depths that overlap in next profile
  .overlapFlag <- flagOverlappingHz(z8)
  expect_false(.overlapFlag[1])
})
