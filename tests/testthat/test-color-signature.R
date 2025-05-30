context("color signature")

## sample data
# trivial example, not very interesting
data(sp1)
depths(sp1) <- id ~ top + bottom

# convert Munsell -> sRGB triplets
rgb.data <- munsell2rgb(sp1$hue, sp1$value, sp1$chroma, return_triplets = TRUE)
sp1$r <- rgb.data$r
sp1$g <- rgb.data$g
sp1$b <- rgb.data$b

## tests


test_that("colorBucket", {
  
  # extract color signature
  pig <- soilColorSignature(sp1, method = 'colorBucket')
  
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
  pig <- soilColorSignature(sp1, method = 'depthSlices')
  
  # expected output
  expect_true(inherits(pig, 'data.frame'))
  expect_true(nrow(pig) == length(profile_id(sp1)))
  expect_true(ncol(pig) == 10)
  
  # add more of these
  expect_equal(pig$id[1], 'P001')
  expect_equal(pig$A.0.1[1], 5.87, tolerance = 0.01)
  expect_equal(pig$A.0.5[1], 5.57, tolerance = 0.01)
  expect_equal(pig$B.0.1[1], 11.07, tolerance = 0.01)
  expect_equal(pig$B.0.5[1], 17.86, tolerance = 0.01)
  expect_equal(pig$L.0.1[1], 30.25, tolerance = 0.01)
  
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
    ## TODO: this will be simplified soon
    # manually convert Munsell -> sRGB
    rgb.data <- munsell2rgb(s$hue, s$value, s$chroma, return_triplets = TRUE)
    s$r <- rgb.data$r
    s$g <- rgb.data$g
    s$b <- rgb.data$b
    
    # 
    pig <- soilColorSignature(s, RescaleLightnessBy = 5, method = 'depthSlices')
    row.names(pig) <- pig[, 1]
    d <- daisy(pig[, -1])
    dd <- diana(d)
    
    # expected ordering
    o <- c("AMADOR", "VLECK", "PENTZ", "YOLO", "HANFORD", "MOGLIA", "PARDEE", 
           "HAYNER", "CANEYHEAD", "DRUMMER", "WILLOWS", "ZOOK", "SYCAMORE", 
           "KLAMATH", "ARGONAUT", "REDDING", "MUSICK", "CECIL", "SIERRA", 
           "PALAU")
    
    
    expect_true(
      all(profile_id(s)[dd$order] ==  o)
    )
  }
    
})


