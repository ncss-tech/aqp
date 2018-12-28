# getSurfaceHorizonDepth / Mineral Soil Surface, Organic Soil Horizons

context("surface horizon thickness, mineral soil surface, organic soil horizon")

data(sp1, package = 'aqp')
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

p <- sp1[1]
attr <- 'prop' # clay contents % 

q <- sp1[2]

test_that("getPlowLayerDepth()", {
  p$aphorizons <- c("Ap1","Ap2","AB", rep('C', nrow(p) - 4), "Apb")
  rez <- getPlowLayerDepth(p, hzdesgn = 'aphorizons')
  expect_equivalent(rez, 14)
})

test_that("getMineralSoilSurfaceDepth()", {
  p$ohorizons <- c("Oi1","Oi2","Oe", rep('C', nrow(p) - 4), "2C")
  rez <- getMineralSoilSurfaceDepth(p, hzdesgn='ohorizons')
  expect_equivalent(rez, 49)
  
  rez2 <- getMineralSoilSurfaceDepth(q, hzdesgn='name')
  expect_equivalent(rez2, 5)
})