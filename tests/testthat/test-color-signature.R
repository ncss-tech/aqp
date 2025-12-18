context("color signature")

## sample data
# trivial example, not very interesting
data(sp1)
depths(sp1) <- id ~ top + bottom

# sRGB color coordinates in [0, 1]
.rgb <- munsell2rgb(sp1$hue, sp1$value, sp1$chroma, return_triplets = TRUE)
sp1$r <- .rgb$r
sp1$g <- .rgb$g
sp1$b <- .rgb$b

# CIELAB color
.lab <- munsell2rgb(sp1$hue, sp1$value, sp1$chroma, returnLAB = TRUE)
sp1$CIE_L <- .lab$L
sp1$CIE_A <- .lab$A
sp1$CIE_B <- .lab$B

# Munsell notation
sp1$m <- sprintf("%s %s/%s", sp1$hue, sp1$value, sp1$chroma)

# hex sRGB
sp1$hex <- munsell2rgb(sp1$hue, sp1$value, sp1$chroma)


## tests


## REMOVE this once arguments have been removed
test_that("deprecation of r, g, b arguments", {
  
  # 2025-12-15
  expect_warning(
    pig <- soilColorSignature(sp1, r = 'r', g = 'g', b = 'b'), 
    regexp = 'deprecated'
  )
  
})



test_that("color specification detection / interpretation", {
  
  # sRGB coordinates
  pig <- soilColorSignature(sp1, color = c('r', 'g', 'b'))
  expect_equal(attr(pig, 'colorspec'), 'color-coordinate-data.frame')
  
  # error condition
  # CIELAB coordinates, without correct space argument
  expect_error(soilColorSignature(sp1, color = c('CIE_L', 'CIE_A', 'CIE_B')))
  
  # CIELAB coordinates
  pig <- soilColorSignature(sp1, color = c('CIE_L', 'CIE_A', 'CIE_B'), space = 'CIELAB')
  expect_equal(attr(pig, 'colorspec'), 'color-coordinate-data.frame')
  
  # sRGB hex
  pig <- soilColorSignature(sp1, color = 'hex')
  expect_equal(attr(pig, 'colorspec'), 'hex-sRGB')
  
  # Munsell notation
  pig <- soilColorSignature(sp1, color = 'm')
  expect_equal(attr(pig, 'colorspec'), 'munsell')
  
})



test_that("colorBucket", {
  
  # extract color signature
  pig <- soilColorSignature(sp1, color = 'm', method = 'colorBucket')
  
  # expected output
  expect_true(inherits(pig, 'data.frame'))
  expect_true(nrow(pig) == length(profile_id(sp1)))
  expect_true(ncol(pig) == 6)
  
  # add more of these
  expect_equal(pig$id[1], 'P001')
  expect_equal(pig$.white.pigment[1], 0.635, tolerance = 0.001)
  expect_equal(pig$.red.pigment[1], 0.103, tolerance = 0.001)
  expect_equal(pig$.green.pigment[1], 0, tolerance = 0.001)
  expect_equal(pig$.yellow.pigment[1], 0.261, tolerance = 0.001)
  expect_equal(pig$.blue.pigment[1], 0, tolerance = 0.001)
  
})

test_that("depthSlices", {
  
  # extract color signature
  pig <- soilColorSignature(sp1, color = 'm', method = 'depthSlices')
  
  # expected output
  expect_true(inherits(pig, 'data.frame'))
  expect_true(nrow(pig) == length(profile_id(sp1)))
  expect_true(ncol(pig) == 10)
  
  # add more of these
  expect_equal(pig$id[1], 'P001')
  expect_equal(pig$A.1[1], 5.87, tolerance = 0.01)
  expect_equal(pig$A.2[1], 5.57, tolerance = 0.01)
  expect_equal(pig$B.1[1], 11.07, tolerance = 0.01)
  expect_equal(pig$B.2[1], 17.86, tolerance = 0.01)
  expect_equal(pig$L.1[1], 30.25, tolerance = 0.01)
  
})


test_that("perceptualDistMat = TRUE", {
  
  # result is a distance matrix
  d <- soilColorSignature(sp1, color = 'm', method = 'depthSlices', perceptualDistMat = TRUE)
  
  # expected output
  expect_true(inherits(d, 'dist'))
  
  # all profiles IDs should be present
  expect_equal(length(setdiff(dimnames(d), profile_id(sp1))), 0)
  
})




## TODO: create / use local data to check ordering
# using OSDs right now for a diverse range of colors
test_that("expected order from OSDs, depthSlices", {
  
  # only run this locally
  skip_on_cran()
  
  # curl is required to check if offline
  skip_if_not_installed("curl")
  
  skip_if_offline()
  
  # TODO: consider not using soilDB for testing soilColorSignature()
  skip_if_not_installed("soilDB")
  
  suppressWarnings(library(soilDB, quietly = TRUE))
  suppressWarnings(library(cluster, quietly = TRUE))
  
  s.list <- c('amador', 'redding', 'pentz', 'willows', 'pardee', 'yolo', 'hanford', 'cecil', 'sycamore', 'KLAMATH', 'MOGLIA', 'vleck', 'drummer', 'CANEYHEAD', 'musick', 'sierra', 'HAYNER', 'zook', 'argonaut', 'PALAU')
  
  # get these soil series
  s <- soilDB::fetchOSD(s.list)
  
  if (!is.null(s)) {
    # use hex sRGB, provided by fetchOSD()
    pig <- soilColorSignature(s, color = 'soil_color', method = 'depthSlices')
    row.names(pig) <- pig[, 1]
    d <- daisy(pig[, -1])
    dd <- diana(d)
    
    # expected ordering
    o <- c("AMADOR", "MOGLIA", "VLECK", "HANFORD", "PARDEE", "CANEYHEAD", 
           "HAYNER", "ARGONAUT", "MUSICK", "CECIL", "PALAU", "REDDING", 
           "SIERRA", "DRUMMER", "ZOOK", "PENTZ", "YOLO", "SYCAMORE", "WILLOWS", 
           "KLAMATH")
    
    
    expect_true(
      all(profile_id(s)[dd$order] ==  o)
    )
  }
  
})


