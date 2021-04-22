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
  
  # same output as rgb2Munsell
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
  expect_equal(m$sigma, 0.6454, tolerance = 1e-4)
      
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



