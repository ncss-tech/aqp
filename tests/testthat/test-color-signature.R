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
  
  # parsing bogus notation generates NA
  expect_equal(pig$id[1], 'P001')
  expect_equal(pig$.white.pigment[1], 0.6360583, tolerance=0.0001)
  expect_equal(pig$.red.pigment[1], 0.10280513, tolerance=0.0001)
  expect_equal(pig$.green.pigment[1], 0, tolerance=0.0001)
  expect_equal(pig$.yellow.pigment[1], 0.2611365, tolerance=0.0001)
  expect_equal(pig$.blue.pigment[1], 0, tolerance=0.0001)
  
})
