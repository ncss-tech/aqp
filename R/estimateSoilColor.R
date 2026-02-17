

## TODO:
##      * accept colors in any format via aqp:::.detectColorSpec()
##      * stratified model by major hz types
##      * information about expected prediction error
##      * add `sameHue` argument to enforce no change in hue


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
#' # estimation is not always symmetric
#' estimateSoilColor(hue = '10YR', value = 4, chroma = 3, sourceMoistureState = 'dry')
#' 
#' # more examples
#' estimateSoilColor(hue = '2.5Y', value = 8, chroma = 2, sourceMoistureState = 'dry')
#' estimateSoilColor(hue = '2.5YR', value = 3, chroma = 4, sourceMoistureState = 'moist')
#' 
#' estimateSoilColor(hue = 'N', value = 2, chroma = 0, sourceMoistureState = 'moist')
#' 
#' estimateSoilColor(hue = '7.5YR', value = 2, chroma = 2, sourceMoistureState = 'moist')
#' 
#' # resulting hue is not always the same
#' estimateSoilColor(hue = '5G', value = 6, chroma = 6, sourceMoistureState = 'dry')
#' 
estimateSoilColor <- function(hue, value, chroma, sourceMoistureState = c('dry', 'moist')) {
  
  # sanity checks
  sourceMoistureState <- match.arg(sourceMoistureState)
  
  # convert input to CIELAB
  z <- munsell2rgb(hue, value, chroma, returnLAB = TRUE)
  
  
  ## TODO: add logic to enforce same hue, not at important with latest model
  # # optionally keep track of original hues
  # if(sameHue) {
  #   o.hue <- hue
  # }
  
  
  # manuscript in progress
  # latest models: soil-color/moist-dry-model/
  
  # select transformation
  # transformation parameters via vegan::procrustes()
  # updated 2026-02-13 (NASIS pedon model)
  params <- switch(sourceMoistureState,
                   dry = {
                     # dry -> moist
                     list(
                       scale = 0.7728589,
                       
                       rotation = structure(
                         c(0.997992548998439, 0.0262814715398672, -0.0576207983049428, 
                           -0.0234753940832354, 0.998530378505848, 0.0488465871234181, 
                           0.0588198777305472, -0.047395859045575, 0.997142845598912), 
                         dim = c(3L, 3L)
                       ),
                       
                       translation = structure(
                         c(-1.36373229346336, 1.77648038381813, 0.205785719295584), 
                         dim = c(1L, 3L)
                       )
                     )
                   },
                   
                   moist = {
                     # moist -> dry  
                     list(
                       scale = 0.835203,
                       
                       rotation = structure(
                         c(0.997992548998438, -0.0234753940832355, 0.058819877730547, 
                           0.0262814715398672, 0.998530378505848, -0.0473958590455751, 
                           -0.0576207983049427, 0.048846587123418, 0.997142845598912), 
                         dim = c(3L, 3L)),
                       
                       translation = structure(
                         c(20.194756551203, -0.0896637372378102, 4.07182351403834), 
                         dim = c(1L, 3L)
                       )
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
