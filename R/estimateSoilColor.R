
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
#' Estimates for colors having a (dry or moist) Munsell value of 10 are not likely correct.
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
  
  # latest models
  # load('../../SoilWeb-data/OSD/models/procrustes-models.rda')
  
  # select transformation
  # transformation parameters via vegan::procrustes()
  params <- switch(sourceMoistureState,
         dry = {
           # dry -> moist
           list(
             scale = 0.812898619142037,
             
             rotation = structure(c(0.995675622919053, 0.0257258722497405, -0.0892649618929251, 
                                    -0.0197691154694856, 0.997558105087928, 0.0669851404935216, 0.0907702374036196, 
                                    -0.0649307821481665, 0.993752865420432), dim = c(3L, 3L)),
             
             translation = structure(c(-4.08321328666085, 1.61159422497282, -0.753817567156023
             ), dim = c(1L, 3L))
           )
         },
         
         moist = {
           # moist -> dry  
           list(
             scale = 0.832203042606691,
             
             rotation = structure(c(0.995675622919054, -0.0197691154694851, 0.0907702374036196, 
                                    0.0257258722497396, 0.997558105087928, -0.0649307821481645, -0.0892649618929251, 
                                    0.0669851404935191, 0.993752865420432), dim = c(3L, 3L)),
             
             translation = structure(c(21.1915203051648, 0.411048926770148, 6.01556883263775
             ), dim = c(1L, 3L))
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
