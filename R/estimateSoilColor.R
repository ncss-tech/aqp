
## TODO: optimize


#' @title Estimate dry soil colors from moist soil colors and vice versa.
#' 
#' @description Soil color is typically described at dry and moist conditions. This function attempts to estimate soil color at dry or moist condition when one is missing. Estimation proceeds as:
#'   * convert Munsell notation to CIELAB color coordinates via `munsell2rgb()`
#'   * apply scaling, rotation, and translation parameters in CIELAB color space
#'   * locate closest Munsell chip to CIELAB coordinates via `col2munsell()`
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
#' # keep examples from using more than 2 cores
#' data.table::setDTthreads(Sys.getenv("OMP_THREAD_LIMIT", unset = 2))
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
  # updated 2026-01-02
  params <- switch(sourceMoistureState,
                   dry = {
                     # dry -> moist
                     list(
                       scale = 0.817488,
                       
                       rotation = structure(
                         c(0.99556760, -0.01633258, 0.09261960, 
                           0.02238987, 0.99765080, -0.06474236, 
                           -0.09134461, 0.06652914, 0.99359450
                         ), dim = c(3L, 3L)),
                       
                       translation = structure(c(-4.306294, 1.424194, -0.9546662), dim = c(1L, 3L))
                     )
                   },
                   
                   moist = {
                     # moist -> dry  
                     list(
                       scale = 0.8329229,
                       
                       rotation = structure(
                         c(0.99556760, 0.02238987, -0.09134461,
                           -0.01633258, 0.99765080, 0.06652914, 
                           0.09261960, -0.06474236, 0.99359450
                         ), dim = c(3L, 3L)),
                       
                       translation = structure(c(21.16093, 0.5293671, 6.101675), dim = c(1L, 3L))
                     )
                   }
  )
  
  
  # apply transformation
  Y <- as.matrix(z)
  Y <- params$scale * Y %*% params$rotation
  Y <- sweep(Y, MARGIN = 2, STATS = params$translation, FUN = "+")
  
  # CIELAB -> closest Munsel
  res <- col2Munsell(Y, space = 'CIELAB', nClosest = 1)
  
  
  ## TODO: post-processing or additional diagnostics?
  
  return(res)
  
}
