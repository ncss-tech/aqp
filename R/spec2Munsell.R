

# https://rip94550.wordpress.com/2012/05/21/color-from-spectrum-to-xyz-and-beyond/



## TODO: allow for more flexibility in spectra min/max/res wavelength

#' @title Convert reflectance spectra to closest Munsell chip
#' @param x reflectance spectra, (380nm to 730nm, 10nm resolution)
#' 
#' @param convert logical, convert sRGB coordinates to closest Munsell chip (see `?munsell`)
#' 
#' @param SO CIE standard observer: these are the color matching functions defined by CIE and used to represent "average" human color perception. CIE1931 is the 2 degree standard observer more useful for describing color perception over very small areas or at distance. CIE1964 is the 10 degree standard observer, used for most industrial color matching applications.
#' 
#' @param illuminant CIE standard illuminants: 
#'    * D65 represents average daylight
#'    * F2 represents typical fluorescent lighting
#' 
#' @param ... further arguments to [`rgb2munsell`]
#'
#' @return output from [`rgb2munsell`]
#' @export
#'
#' @references 
#' Marcus, R.T. (1998). The Measurement of Color. In K. Nassau (Ed.), Color for Science, Art, and Technology (pp. 32-96). North-Holland.
#' 
#' CIE Colorimetry – Part 1: CIE standard colorimetric observers. CIES014-1/E:2006 – ISO 11664-1:2007(E)
#' 
#' CIE. (n.d.). CIE 15:2004 Tables Data. Retrieved from https://law.resource.org/pub/us/cfr/ibr/003/cie.15.2004.tables.xls
#' 
#' @examples 
#' 
#' # Munsell reference spectra
#' data("munsell.spectra.wide")
#' 
#' # convert to closest Munsell chip
#' # sRGB -> Munsell conversion via rgb2Munsell()
#' spec2Munsell(munsell.spectra.wide[, '10YR 3/3'])
#' 
#' # attempt several
#' cols <- c('10YR 6/2', '5YR 5/6', '10B 4/4', '5G 4/4', '2.5Y 8/2', '10YR 3/3', '7.5YR 2.5/2')
#' 
#' # most are exact or very close
#' z <- do.call(
#'   'rbind',
#'   lapply(cols, function(i) {
#'     spec2Munsell(munsell.spectra.wide[, i])  
#'   })
#' )
#' 
#' # format Munsell notation from pieces
#' z$m <- sprintf("%s %s/%s", z$hue, z$value, z$chroma)
#' 
#' # compare
#' colorContrastPlot(
#'   m1 = cols, 
#'   m2 = z$m, 
#'   labels = c('original', 'spectral\ninterpretation')
#' )
#' 
#' if(requireNamespace("gower")) {
#' # mix colors, return spectra, convert to color
#' cols <- c('10YR 6/2', '5YR 5/6', '10B 4/4')
#' res <- mixMunsell(cols, keepMixedSpec = TRUE, mixingMethod = 'reference')
#' 
#' # note that they are slightly different
#' res$mixed
#' spec2Munsell(res$spec)
#' 
#' }
#' 
spec2Munsell <- function(x, convert = TRUE, SO = c('CIE1931', 'CIE1964'), illuminant = c('D65', 'F2'), ...) {
  
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
    m <- rgb2munsell(col.srgb, ...)
    return(m)
  } else {
    # return sRGB coordinates
    return(col.srgb)
  }
  
  
  
}

