context("color conversion")

## sample data
x <- '10YR 3/4'
x.p <- parseMunsell(x, convertColors = FALSE)

# Munsell --> sRGB conversion
m <- munsell2rgb(x.p$hue, x.p$value, x.p$chroma)
m.rgb <- munsell2rgb(x.p$hue, x.p$value, x.p$chroma, return_triplets = TRUE)

# sRGB --> Munsell
x.back <- rgb2munsell(color = m.rgb, colorSpace = 'LAB', nClosest = 1)
# using trunctacted sRGB values
x.back.trunc <- rgb2munsell(data.frame(r=0.36, g=0.26, b=0.13))

# neutral colors map to shades of grey
x.neutral <- parseMunsell('N 2/', return_triplets=TRUE)

## tests

test_that("parsing Munsell notation", {

  # parsing bogus notation generates NA
  expect_equal(parseMunsell('10YZ 4/5'), as.character(NA))
  expect_equal(parseMunsell('10YR /5'), as.character(NA))
  expect_equal(parseMunsell('10YR '), as.character(NA))
  expect_equal(parseMunsell('10YR 4/'), as.character(NA))
  
  # neutral colors
  expect_equal(class(parseMunsell('N 2/', convertColors = FALSE)), 'data.frame')
  
  # splitting of text into colums within data.frame
  expect_identical(x.p, data.frame(hue = "10YR", value = "3", chroma = "4", stringsAsFactors = FALSE))
  
})


test_that("Munsell <--> sRGB and back again", {
  
  # sRGB in hex notation
  expect_equal(m, '#5E4323FF')
  expect_equal(parseMunsell(x), m)
  
  # sRGB triplets
  expect_equal(m.rgb$r, 0.3679063, tolerance=0.0001)
  expect_equal(m.rgb$g, 0.2644507, tolerance=0.0001)
  expect_equal(m.rgb$b, 0.1364835, tolerance=0.0001)
  
  # neutral colors
  expect_equal(x.neutral$r, 0.2, tolerance=0.01)
  expect_equal(x.neutral$g, 0.2, tolerance=0.01)
  expect_equal(x.neutral$b, 0.2, tolerance=0.01)
  
  # sRGB --> Munsell
  expect_equal(x.back, data.frame(hue='10YR', value=3, chroma=4, sigma=0, stringsAsFactors = FALSE))
  expect_equal(x.back.trunc$hue, '10YR')
  expect_equal(x.back.trunc$value, 3)
  expect_equal(x.back.trunc$chroma, 4)
  })


test_that("closest Munsell chip based on sRGB coordinates", {
  
  # closest chip in aqp LUT
  expect_equal(getClosestMunsellChip('10YR 3.3/5', convertColors = FALSE), '10YR 3/5')
  expect_equal(getClosestMunsellChip('9YR 3.8/3', convertColors = FALSE), '10YR 4/3')
  expect_equal(getClosestMunsellChip('7.9YR 2.7/2.0', convertColors = FALSE), '7.5YR 3/2')
})


