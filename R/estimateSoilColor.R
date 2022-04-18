
## TODO: optimize


#' @title Estimate dry soil colors from moist soil colors and vice versa.
#' 
#' @description Soil color is typically described at dry and moist conditions. This function attempts to estimate soil color at dry or moist condition when one is missing. Estimation proceeds as:
#'   * convert Munsell notation to CIELAB color coordinates via `munsell2rgb()`
#'   * apply scaling, rotation, and translation parameters in CIELAB color space
#'   * convert CIELAB to sRGB coordinates
#'   * locate closest Munsell chip to sRGB coordinates via `rgb2munsell()`
#'   
#' Estimation of dry from moist soil color state is not guaranteed to be symmetric with estimation of moist from dry.
#' 
#' @details Scaling, rotation, and translation parameters for shifting between dry <--> moist CIELAB coordinates was determined using `vegan::procrustes()`, from those official series descriptions (OSD) where moist and dry soil colors were available.
#' 
#' This is still a work in progress.
#' 
#' @author D.E. Beaudette
#' 
#' @param hue vector of Munsell hue ('10YR', '2.5Y', etc.)
#' @param value vector of Munsell value (2,2.5 2.5, 3, 5, 6, etc.)
#' @param chroma vector of Munsell chroma (2, 3, 4, etc.)
#' @param sourceMoistureState character, source colors are either 'dry' or 'moist' 
#'
#' @return `data.frame` of estimated colors in Munsell notation. The `sigma` column contains CIE2000 color contrast metric values describing the perceptual distance between estimated color in CIELAB coordinates and closest Munsell chip.
#' 
#' @export
#'
#' @examples
#' 
#' estimateSoilColor(hue = '10YR', value = 3, chroma = 3, sourceMoistureState = 'moist')
#' 
#' # note that estimation is not symmetric
#' estimateSoilColor(hue = '10YR', value = 5, chroma = 3, sourceMoistureState = 'dry')
#' 
estimateSoilColor <- function(hue, value, chroma, sourceMoistureState = c('dry', 'moist')) {
  
  ## sanity checks
  sourceMoistureState <- match.arg(sourceMoistureState)
  
  ## convert input to CIELAB
  z <- munsell2rgb(hue, value, chroma, returnLAB = TRUE)
  
  # select transformation
  # transformation parameters via vegan::procrustes()
  params <- switch(sourceMoistureState,
         dry = {
           # dry -> moist
           list(
             scale = 0.813113909258287,
             
             rotation = structure(c(0.995726413877236, 0.0257258575317077, -0.0886966118937879, 
                                    -0.0198592989419549, 0.997595348898966, 0.0664012658063297, 0.0901915569923982, 
                                    -0.0643560417475089, 0.993842936755039), .Dim = c(3L, 3L)),
             
             translation = structure(c(-4.1036881559009, 1.62003171022114, -0.738786964568888
             ), .Dim = c(1L, 3L))
           )
         },
         
         moist = {
           # moist -> dry  
           list(
             scale = 0.832382710140909,
             
             rotation = structure(c(0.995726413877236, -0.0198592989419543, 0.0901915569923983, 
                                    0.0257258575317069, 0.997595348898967, -0.0643560417475067, -0.0886966118937878, 
                                    0.0664012658063277, 0.993842936755039), .Dim = c(3L, 3L)),
             
             translation = structure(c(21.1881839195852, 0.403136273628388, 5.99548096284415
             ), .Dim = c(1L, 3L))
           )
           }
  )
  
  
  ## apply transformation
  Y <- as.matrix(z)
  Y <- params$scale * Y %*% params$rotation
  Y <- sweep(Y, MARGIN = 2, STATS = params$translation, FUN = "+")
  
  ## CIELAB -> sRGB
  ## TODO: why does farver give slightly different results?
  
  ## farver: results are scaled 0-255
  # .srgb <- farver::convert_colour(Y, from = 'lab', to = 'rgb', white_from = 'D65', white_to = 'D65')
  
  ## grDevices: results are scaled 0-1
  .srgb <- convertColor(Y, from = 'Lab', to = 'sRGB', from.ref.white='D65', to.ref.white = 'D65')
  
  ## sRGB -> Munsell
  res <- rgb2munsell(color = .srgb, colorSpace = 'CIE2000', nClosest = 1)
  
  ## additional diagnostics... ?
  
  return(res)
  
}
