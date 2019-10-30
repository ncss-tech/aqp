# getSurfaceHorizonDepth / Mineral Soil Surface, Organic Soil Horizons

context("surface horizon thickness, mineral soil surface, organic soil horizon")

data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

p <- sp1[1]
attr <- 'prop' # clay contents % 

q <- sp1[2]

test_that("getSurfaceHorizonDepth", {
  #all horizons containing A
  t1 <- getSurfaceHorizonDepth(p, pattern = 'A', hzdesgn = 'name')
  expect_equal(t1, 57)
  
  #all horizons that start with A and have a number from _2_ to 9
  t4 <- getSurfaceHorizonDepth(p, pattern = '^A[2-9]', hzdesgn = 'name')
  expect_equal(t4, 0)
})

test_that("getPlowLayerDepth()", {
  # matches first two horizons in fake Ap horizon data with "buried Ap"
  p$aphorizons <- c("Ap1","Ap2","AB", rep('C', nrow(p) - 4), "Apb")
  rez <- getPlowLayerDepth(p, hzdesgn = 'aphorizons')
  expect_equivalent(rez, 14)
})

test_that("getMineralSoilSurfaceDepth()", {
  # matches first 3 horizons in fake O horizon data
  p$ohorizons <- c("Oi1","Oi2","Oe", rep('C', nrow(p) - 4), "2C")
  rez <- getMineralSoilSurfaceDepth(p, hzdesgn='ohorizons')
  expect_equivalent(rez, 49)
  
  # matches first horizon with original horizon designations
  rez2 <- getMineralSoilSurfaceDepth(q, hzdesgn='name')
  expect_equivalent(rez2, 5)
})
