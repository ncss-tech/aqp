

# https://rip94550.wordpress.com/2012/05/21/color-from-spectrum-to-xyz-and-beyond/



## TODO: allow for more flexibility in spectra min/max/res wavelength

#' @title Convert reflectance spectra to closest Munsell chip
#' 
#' @note D65 illuminant spectra and CIE1931 color matching functions derived from CIE reference data.
#' 
#' @param x reflectance spectra, (380nm to 730nm, 10nm resolution)
#' @param ... further arguments to `rgb2Munsell`
#'
#' @return output from `rgb2Munsell`
#' @export
#'
#' @references 
#' Marcus, R.T. (1998). The Measurement of Color. In K. Nassau (Ed.), Color for Science, Art, and Technology (pp. 32-96). North-Holland.
#' 
#' "Selected colorimetric tables in Excel" http://files.cie.co.at/204.xls
#' 
spec2Munsell <- function(x, ...) {
  
  # D65 and CIE1931 reference data at 5nm
  spectral.reference <- NULL
  load(system.file("data/spectral.reference.rda", package="aqp")[1])
  
  
  # this is the range of wavelengths we have to work with
  # in the Munsell spectra libraries 
  #   * munsell.spectra
  #   * munsell.spectra.wide
  w.10 <- seq(from = 380, to = 730, by = 10)
  
  # spline interpolator
  f <- splinefun(w.10, x)
  
  # interpolate to 5nm res
  w.5 <- seq(from = 380, to = 730, by = 5)
  R <- data.frame(
    w = w.5, 
    x = f(w.5)
  )
  
  # reflectance spectra * illuminant
  S <- R$x * spectral.reference$D65
  
  # A-matrix: these are the color-matching functions
  A <- spectral.reference[, c('xbar', 'ybar', 'zbar')]
  
  # apply A-matrix to spectra
  XYZ <- t(A) %*% S
  # convert to vector
  XYZ <- t(XYZ)[1, ]
  
  # apply A-matrix to illuminant
  k <- t(A) %*% spectral.reference$D65
  # convert to vector
  k <- t(k)[1, ]
  
  # re-scale by "photopic response"
  col.XYZ <- XYZ / k[2]
  
  # convert to sRGB
  col.srgb <- convertColor(
    rbind(col.XYZ), 
    from = 'XYZ', 
    to = 'sRGB', 
    from.ref.white = 'D65', 
    to.ref.white = 'D65'
  )
  
  # convert sRGB to closest Munsell chip
  m <- rgb2munsell(col.srgb, ...)
  
  return(m)
}

