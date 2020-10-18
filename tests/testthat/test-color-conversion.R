context("color conversion")

## sample data
x <- '10YR 3/4'
x.p <- parseMunsell(x, convertColors = FALSE)

# Munsell --> sRGB conversion
m <- munsell2rgb(x.p$hue, x.p$value, x.p$chroma)
m.rgb <- munsell2rgb(x.p$hue, x.p$value, x.p$chroma, return_triplets = TRUE)

# sRGB --> Munsell
x.back <- rgb2munsell(color = m.rgb, colorSpace = 'LAB', nClosest = 1)
# using truncated sRGB values
x.back.trunc <- rgb2munsell(data.frame(r=0.36, g=0.26, b=0.13))

# neutral colors map to shades of grey
x.neutral <- parseMunsell('N 2/', return_triplets=TRUE)

## tests

test_that("parsing Munsell notation", {

  # parsing bogus notation generates NA
  # will also generate a warning from munsell2rgb()
  expect_equal(suppressWarnings(parseMunsell('10YZ 4/5')), as.character(NA))
  expect_equal(suppressWarnings(parseMunsell('10YR /5')), as.character(NA))
  expect_equal(suppressWarnings(parseMunsell('10YR ')), as.character(NA))
  expect_equal(suppressWarnings(parseMunsell('10YR 4/')), as.character(NA))
  expect_equal(suppressWarnings(parseMunsell('G1 6/N')), as.character(NA))
  
  # parsing bogus notation without conversion
  # doesn't replace with NA
  bogus <- parseMunsell('G1 3/X', convertColors = FALSE)
  expect_equal(bogus$hue, 'G1')
  expect_equal(bogus$value, '3')
  
  # neutral colors
  expect_true(inherits(parseMunsell('N 2/', convertColors = FALSE), 'data.frame'))
  
  # splitting of text into colums within data.frame
  expect_identical(x.p, data.frame(hue = "10YR", value = "3", chroma = "4", stringsAsFactors = FALSE))
  
})


# addresses #66 (https://github.com/ncss-tech/aqp/issues/66)
test_that("Munsell hue parsing", {
  
  # normal operation
  res <- aqp:::.parseMunsellHue('10YR')
  expect_true(inherits(res, 'data.frame'))
  expect_equal(res$hue.numeric, 10L)
  expect_equal(res$hue.character, 'YR')
  expect_equal(nrow(res), 1)
  
  # bogus hue
  res <- aqp:::.parseMunsellHue('G1 ')
  expect_true(inherits(res, 'data.frame'))
  expect_true(is.na(res$hue.numeric))
  expect_true(is.na(res$hue.character))
  expect_equal(nrow(res), 1)
})


test_that("non-integer value and chroma are rounded", {
  
  # rounding of value, throws warning
  expect_warning(res <- parseMunsell('10YR 3.3/4'), regexp = 'rounded')
  # this will not throw a warning
  res <- parseMunsell('10YR 3.3/4', convertColors = FALSE)
  # results should be the same
  expect_equal(
    suppressWarnings(parseMunsell('10YR 3.3/4')),
    parseMunsell('10YR 3/4')
  )
  
  # rounding of chroma, throws warning
  expect_warning(res <- parseMunsell('10YR 3/4.6'), regexp = 'rounded')
  # this will not throw a warning
  res <- parseMunsell('10YR 3/4.6', convertColors = FALSE)
  # results should be the same
  expect_equal(
    suppressWarnings(parseMunsell('10YR 3/4.6')),
    parseMunsell('10YR 3/5')
  )
  
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
  expect_equal(x.back$hue, '10YR')
  expect_equal(x.back$value, 3)
  expect_equal(x.back$chroma, 4)
  expect_equal(x.back$sigma, 0)
  
  expect_equal(x.back.trunc$hue, '10YR')
  expect_equal(x.back.trunc$value, 3)
  expect_equal(x.back.trunc$chroma, 4)
  })


test_that("missing data", {
  
  # data with missing sRGB coordinates
  color <- rbind(
    cbind(NA, NA, NA),
    cbind(0.5, 0.2, 0.2),
    cbind(1, 1, 1),
    cbind(NA, NA, NA)
  )
  
  # conversion should work without error
  res <- rgb2munsell(color)
  
  # same number of rows in / out
  expect_true(nrow(res) == nrow(color))
  
  # row order preserved
  expect_true(is.na(res$hue[1]) & is.na(res$hue[4]))
  
})



test_that("closest Munsell chip based on sRGB coordinates", {
  
  # closest chip in aqp LUT
  expect_equal(getClosestMunsellChip('10YR 3.3/5', convertColors = FALSE), '10YR 3/5')
  expect_equal(getClosestMunsellChip('9YR 3.8/3', convertColors = FALSE), '10YR 4/3')
  expect_equal(getClosestMunsellChip('7.9YR 2.7/2.0', convertColors = FALSE), '7.5YR 3/2')
})


# https://github.com/ncss-tech/aqp/issues/69
test_that("Munsell --> LAB + sRGB coordinates", {
  
  # sRGB
  test.1 <- parseMunsell("10YR 3/5", return_triplets=TRUE)
  expect_equal(names(test.1), c('r', 'g', 'b'))
  
  
  # sRGB and LAB
  test.2 <- parseMunsell("10YR 3/5", return_triplets=TRUE, returnLAB=TRUE)
  expect_equal(names(test.2), c('r', 'g', 'b', 'L', 'A', 'B'))
  
  # LAB
  test.3 <- parseMunsell("10YR 3/5", return_triplets=FALSE, returnLAB=TRUE)
  expect_equal(names(test.3), c('L', 'A', 'B'))
  
  # test the LAB ---> sRGB is close
  test.4 <- grDevices::convertColor(test.3, from = 'Lab', to='sRGB')
  
  # sRGB (r)
  expect_equal(test.1[, 1], test.4[, 1], tolerance=0.1)
  # sRGB (g)
  expect_equal(test.1[, 2], test.4[, 2], tolerance=0.1)
  # sRGB (b)
  expect_equal(test.1[, 3], test.4[, 3], tolerance=0.1)
})

test_that("similar colors result in same, closest chip", {
  
  cols <- t(col2rgb(c('#5F5345', '#554636'))) / 255
  res <-  rgb2munsell(cols)
  
  expect_equal(res$hue[1], res$hue[2])
  expect_equal(res$value[1], res$value[2])
  expect_equal(res$chroma[1], res$chroma[2])
})

test_that("munsell2SPC wrapper method works as expected", {

  data(sp3)
  depths(sp3) <- id ~ top + bottom

  # inspect input data
  # horizons(sp3)[,c("hue","value","chroma")]

  # do color conversions to sRGB and LAB, join into horizon data
  expect_silent( {sp3 <- munsell2spc(sp3)})
  expect_true(inherits(sp3, 'SoilProfileCollection'))

  # # plot rgb "R" coordinate by horizon
  # plot(sp3, color = "rgb_R")
  # 
  # # plot lab "A" coordinate by horizon
  # plot(sp3, color = "lab_A")
  
  # test returning profile+horizon ID data.frame with results
  expect_silent( {dftest <- munsell2spc(sp3, as.spc = FALSE)})
  expect_true(inherits(dftest, 'data.frame'))
  
  # foo is not a column in horizons()
  expect_error( {err1 <- munsell2spc(sp3, hue = "foo")} )
})
