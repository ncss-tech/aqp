context("mixing Munsell colors")

# reference spectra
data("munsell.spectra.wide")


test_that("spec2Munsell works as expected", {
  
  # try a color, result should be identical chip
  # depends on SO and illuminant
  chip <- '10YR 3/3'
  m <- spec2Munsell(munsell.spectra.wide[, chip], SO = 'CIE1931', illuminant = 'D65')  
  
  # object structure / contents
  expect_true(inherits(m, 'data.frame'))
  
  # same output as col2Munsell
  expect_true(
    all(
      names(m) == c('hue', 'value', 'chroma', 'sigma')
        )
  )

  # input == output
  expect_true(
    sprintf("%s %s/%s", m$hue, m$value, m$chroma) == chip
    )
  
  # expected dE00
  expect_equal(m$sigma, 0.64, tolerance = 0.01)
      
})

test_that("neutral chips", {
  
  # try a color, result should be identical chip
  # depends on SO and illuminant
  chip <- 'N 4/'
  m <- spec2Munsell(munsell.spectra.wide[, chip], SO = 'CIE1931', illuminant = 'D65')  
  
  # object structure / contents
  expect_true(inherits(m, 'data.frame'))
  
  # same output as col2Munsell
  expect_true(
    all(
      names(m) == c('hue', 'value', 'chroma', 'sigma')
    )
  )
  
  # input == output
  expect_true(
    sprintf("%s %s/", m$hue, m$value) == chip
  )
  
  # expected dE00
  expect_equal(m$sigma, 0.36, tolerance = 0.01)
  
})



test_that("interpolated / extrapolated spectra", {
  
  # 1-chroma chips are extrapolated for some hues
  # depends on SO and illuminant
  chip <- '7.5YR 2/1'
  m <- spec2Munsell(munsell.spectra.wide[, chip], SO = 'CIE1931', illuminant = 'D65')  
  
  # object structure / contents
  expect_true(inherits(m, 'data.frame'))
  
  # same output as col2Munsell
  expect_true(
    all(
      names(m) == c('hue', 'value', 'chroma', 'sigma')
    )
  )
  
  # input == output
  expect_true(
    sprintf("%s %s/%s", m$hue, m$value, m$chroma) == chip
  )
  
  # expected dE00
  expect_equal(m$sigma, 3, tolerance = 0.1)
  
  
  # 8.5-value chips are interpolated for some hues
  chip <- '7.5YR 8.5/6'
  m <- spec2Munsell(munsell.spectra.wide[, chip], SO = 'CIE1931', illuminant = 'D65') 
  
  # input == output
  expect_true(
    sprintf("%s %s/%s", m$hue, m$value, m$chroma) == chip
  )
  
  # expected dE00
  expect_equal(m$sigma, 0.13, tolerance = 0.01)
  
})


test_that("supporting data match range of Munsell spectra", {
  
  # standard illuminants + observers
  data("spectral.reference")
  
  expect_true(
    all(
      range(spectral.reference$w) == range(munsell.spectra.wide$wavelength)
    )
  )
  
})



