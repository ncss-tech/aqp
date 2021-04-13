

# https://rip94550.wordpress.com/2012/05/21/color-from-spectrum-to-xyz-and-beyond/

# https://cran.r-project.org/web/packages/colorscience/index.html

# library(colorscience)
# 
# D65 <- illuminantD65[illuminantD65$wlnm >= 380 & illuminantD65$wlnm <= 730, ]
# CIE1931 <- ciexyz31[ciexyz31$wlnm >= 380 & ciexyz31$wlnm <= 730, ]
# 
# names(D65) <- c('w', 'x')
# names(CIE1931)[1] <- 'w'
# 
# save(D65, file = 'data/D65.rda')
# save(CIE1931, file = 'data/CIE1931.rda')
# 


## TODO: allow for more flexibility in spectra min/max/res wavelength

#' @title Convert reflectance spectra to closest Munsell chip
#' 
#' @note D65 illuminant spectra and CIE1931 color matching functions subset from data provided by the {colorscience} package.
#' 
#' @param x reflectance spectra, (380nm to 730nm, 10nm resolution)
#' @param ... further arguments to `rgb2Munsell`
#'
#' @return output from `rgb2Munsell`
#' @export
#'
#' @references 
#' Wyszecki, G., & Stiles, W. S., 1982 Color Science: concepts and methods, quantitative data and formulae (2nd ed.). New York: Wiley.
#' 
#' Jose Gama and Glenn Davis (2019). colorscience: Color Science Methods and Data. R package version 1.0.8. 
#' https://CRAN.R-project.org/package=colorscience
#' 
spec2Munsell <- function(x, ...) {
  
  # D65 illuminant (1nm resolution)
  load(system.file("data/D65.rda", package="aqp")[1])
  # CIE 1931 standard observer color matching functions (xbar, ybar, zbar)
  load(system.file("data/CIE1931.rda", package="aqp")[1])
  
  # this is the range of wavelengths we have to work with
  # in the Munsell spectra libraries 
  #   * munsell.spectra
  #   * munsell.spectra.wide
  w <- seq(from = 380, to = 730, by = 10)
  
  # spline interpolator
  f <- splinefun(w, x)
  
  # interpolat to 1nm res
  R <- data.frame(
    w = 380:730, 
    x = f(380:730)
  )
  
  # reflectance spectra * illuminant
  S <- R$x * D65$x
  
  # A-matrix: these are the color-matching functions
  A <- CIE1931[, -1]
  
  # apply A-matrix to spectra
  XYZ <- t(A) %*% S
  # convert to vector
  XYZ <- t(XYZ)[1, ]
  
  # apply A-matrix to illuminant
  k <- t(A) %*% D65$x
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

