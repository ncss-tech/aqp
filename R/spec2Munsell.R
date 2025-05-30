

# https://rip94550.wordpress.com/2012/05/21/color-from-spectrum-to-xyz-and-beyond/



## TODO: allow for more flexibility in spectra min/max/res wavelength

#' @title Convert reflectance spectra to closest Munsell chip
#' 
#' @details See the [expanded tutorial](https://ncss-tech.github.io/AQP/aqp/mix-colors.html) for additional examples.
#' 
#' @param x reflectance spectra, must range from 380nm to 730nm with resolution specified in `res`
#' 
#' @param res spectra resolution in nm, typically 5nm or 10nm
#' 
#' @param convert logical, convert sRGB coordinates to closest Munsell chip (see `?munsell`)
#' 
#' @param SO CIE standard observer: these are the color matching functions defined by CIE and used to represent "average" human color perception. CIE1931 is the 2 degree standard observer more useful for describing color perception over very small areas or at distance. CIE1964 is the 10 degree standard observer, used for most industrial color matching applications.
#' 
#' @param illuminant CIE standard illuminants: 
#'    * D65 represents average daylight
#'    * F2 represents typical fluorescent lighting
#' 
#' @param ... further arguments to [col2Munsell()]
#'
#' @return output from [col2Munsell()]
#' @export
#'
#' @references 
#' Marcus, R.T. (1998). The Measurement of Color. In K. Nassau (Ed.), Color for Science, Art, and Technology (pp. 32-96). North-Holland.
#' 
#' CIE Colorimetry – Part 1: CIE standard colorimetric observers. CIES014-1/E:2006 – ISO 11664-1:2007(E)
#' 
#' CIE. (n.d.). CIE 15:2004 Tables Data. Retrieved from https://law.resource.org/pub/us/cfr/ibr/003/cie.15.2004.tables.xls
#' 
#' 
spec2Munsell <- function(x, res = 10, convert = TRUE, SO = c('CIE1931', 'CIE1964'), illuminant = c('D65', 'F2'), ...) {
  
  # D65 and CIE1931 reference data at 5nm
  spectral.reference <- NULL
  load(system.file("data/spectral.reference.rda", package="aqp")[1])
  
  # select standard observer
  SO <- match.arg(SO)
  
  # select illuminant
  illuminant <- match.arg(illuminant)
  
  # this is the range of wavelengths we have to work with
  # in the Munsell spectra libraries 
  #   * munsell.spectra
  #   * munsell.spectra.wide
  .wl <- seq(from = 380, to = 730, by = res)
  
  
  ## 10nm res: length(x) == 36
  ## 5nm res: length(x) == 71
  # sanity check
  if(length(.wl) != length(x)) {
    stop('inconsistent spectral limits / resolution', call. = FALSE)
  }
  
  # spline interpolator: reflectance ~ wavelength
  .sf <- splinefun(.wl, x)
  
  ## TODO: interpolate spectral.refernce to match resolution of x
  
  # spectral reference is 5nm resolution
  # interpolate to 5nm res
  w.5 <- seq(from = 380, to = 730, by = 5)
  R <- data.frame(
    w = w.5, 
    x = .sf(w.5)
  )
  
  # reflectance spectra * illuminant
  S <- R$x * spectral.reference[[illuminant]]
  
  
  # select the appropriate standard observer
  SO.vars <- switch(SO,
    'CIE1931' = c('xbar_2', 'ybar_2', 'zbar_2'),
    'CIE1964' = c('xbar_10', 'ybar_10', 'zbar_10')
  )
  
  # A-matrix: these are the color-matching functions of the standard observer
  A <- spectral.reference[, SO.vars]
  
  # apply A-matrix to spectra
  XYZ <- t(A) %*% S
  # convert to vector
  XYZ <- t(XYZ)[1, ]
  
  # apply A-matrix to illuminant
  k <- t(A) %*% spectral.reference[[illuminant]]
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
  if(convert) {
    m <- col2Munsell(col.srgb, ...)
    return(m)
  } else {
    # return sRGB coordinates
    return(col.srgb)
  }
  
  
  
}

