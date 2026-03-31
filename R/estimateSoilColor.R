

## TODO:
##      * accept colors in any format via aqp:::.detectColorSpec()
##      * stratified model by major hz types
##      * add `sameHue` argument to enforce no change in hue


#' @title Estimate dry soil colors from moist soil colors and vice versa.
#' 
#' @description All else equal, soil color will predictably shift in perceived lightness (change in Munsell value) as moisture content changes. Field-described soil colors are typically collected at approximately air dry ("dry") and field capacity ("moist") states. This function estimates "dry" soil colors from "moist" soil colors and vice versa. Two methods are available for estimation, both developed from a national collection of field-described soil colors (approx. 800k horizons). Estimates are only valid for mineral soil material, having Munsell values and chroma < 10. Estimation has a median error rate of approximately (CIE dE2000) 5. 
#' 
#' Available Methods:
#'  * "procrustes": soil colors are converted using scale, rotation, and translation parameters in CIELAB color space
#'  * "ols": soil colors are converted using 3 multiple linear regression models (CIELAB coordinates)
#' 
#' This is still a work in progress.
#' 
#' @details 
#' 
#' For both methods, estimation proceeds as:
#'   * convert Munsell notation to CIELAB color coordinates via `munsell2rgb()`
#'   * apply rotation or regression model to color coordinates in CIELAB space
#'   * locate closest Munsell chip to resulting CIELAB coordinates via `col2munsell()`
#'   
#' Estimation of dry from moist soil color state is not guaranteed to be symmetric with estimation of moist from dry.
#' 
#' Scaling, rotation, and translation parameters for shifting between dry <--> moist CIELAB coordinates were determined using `vegan::procrustes()`. Multiple linear regression models were fit using `rms::ols()`.
#' 
#' Estimation error for both methods, converting from either moisture states is, approximately 5 (CIE dE 2000).

#' 
#' @author D.E. Beaudette
#' 
#' @param hue vector of Munsell hue ('10YR', '2.5Y', etc.)
#' @param value vector of Munsell value (2,2.5 2.5, 3, 5, 6, etc.)
#' @param chroma vector of Munsell chroma (2, 3, 4, etc.)
#' @param method character, one of 'procrustes' or 'ols', see details
#' @param sourceMoistureState character, source colors are either 'dry' or 'moist' 
#' @param returnMunsell logical, `TRUE`: return closest Munsell chip, `FALSE`: return estimated CIELAB coordinates
#'
#' @return `data.frame` of estimated colors in Munsell notation. The `sigma` column contains CIE2000 color contrast metric values describing the perceptual distance between estimated color in CIELAB coordinates and closest Munsell chip.
#' 
#' @references
#' 
#' J. A. Shields, E. A. Paul, R. J. St. Arnaud, and W. K. Head. 1968. SPECTROPHOTOMETRY MEASUREMENT OF SOIL COLOR AND ITS RELATIONSHIP TO MOISTURE AND ORGANIC MATTER. Canadian Journal of Soil Science. 48(3): 271-280. https://doi.org/10.4141/cjss68-037
#' 
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
#' # return estimated CIELAB coordinates
#' estimateSoilColor(hue = '5G', value = 6, chroma = 6, sourceMoistureState = 'dry',
#'  returnMunsell = FALSE)
#' 
estimateSoilColor <- function(hue, value, chroma, method = c('procrustes', 'ols'), sourceMoistureState = c('dry', 'moist'), returnMunsell = TRUE) {
  
  # sanity checks
  sourceMoistureState <- match.arg(sourceMoistureState)
  method <- match.arg(method)
  
  # TODO: accept other color input formats
  # detect color spec
  # .spec <- .detectColorSpec(color)
  
  # build a cache so that only unique rows are processed
  # NA in any column is handled correctly
  .cache <- data.frame(hue, value, chroma)
  .cache$id <- sapply(1:nrow(.cache), function(i) {
    # hash should only include values
    digest::digest(
      unlist(.cache[i, 1:3], use.names = FALSE), algo = 'xxhash32'
    )
  })
  
  # vector of original IDs
  .in <- .cache$id
  
  # iterate over unique colors
  .cache <- unique(.cache)
  
  # reduction in records
  if(getOption('.aqp.verbose', default = FALSE)) {
    .rr <- 1 - (nrow(.cache) / length(.in))
    .rr <- round(100 * .rr)
    message(sprintf("cache performance: %s%% row reduction", .rr))
  }
  
  # convert input to CIELAB
  .lab <- munsell2rgb(.cache$hue, .cache$value, .cache$chroma, returnLAB = TRUE)
  
  # manuscript in progress
  # latest models: soil-color/moist-dry-model/
  
  if(method == 'procrustes') {
    .res <- .ESC_procrustes(.lab, .state = sourceMoistureState, .m = returnMunsell)
  }
  
  if(method == 'ols') {
    .res <- .ESC_OLS(.lab, .state = sourceMoistureState, .m = returnMunsell)
  }
  
  # expand unique records to original input stream
  .idx <- match(.in, .cache$id)
  .res <- .res[.idx, ]
  row.names(.res) <- NULL
  
  ## TODO: post-processing or additional diagnostics?
  
  return(.res)
}



.ESC_procrustes <- function(.lab, .state, .m) {
  
  # select transformation
  # transformation parameters via vegan::procrustes()
  # updated 2026-02-13 (NASIS pedon model)
  params <- switch(.state,
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
  Y <- as.matrix(.lab)
  Y <- params$scale * Y %*% params$rotation
  Y <- sweep(Y, MARGIN = 2, STATS = params$translation, FUN = "+")
  
  # CIELAB -> closest Munsell
  if(.m) {
    .res <- col2Munsell(Y, space = 'CIELAB', nClosest = 1)  
  } else {
    # CIELAB coordinate
    .res <- as.data.frame(Y)
    names(.res) <- c('L', 'A', 'B')
  }
  
  return(.res)
  
}


.ESC_OLS <- function(.lab, .state, .m) {
  
  # updated from NASIS pedons
  # ols(m_L ~ rcs(d_L, 3) + d_A + d_B, data = z)
  # 
  
  .dmL <- function(d_L = 51.396738, d_A = 2.8906427, d_B = 12.657859) {
    14.458147+0.34626849* d_L+0.00079258202*pmax(d_L-40.900674,0)^3-0.0012058348*pmax(d_L-51.396738,0)^3+0.00041325275*pmax(d_L-71.527256,0)^3+0.14117082*d_A+0.17977648*d_B 
  }
  
  .dmA <- function(d_A = 2.8906427,d_L = 51.396738,d_B = 12.657859) {
    -2.0541836+1.1614388* d_A-0.0028361881*pmax(d_A,0)^3+0.0043178943*pmax(d_A-2.8906427,0)^3-0.0014817062*pmax(d_A-8.423728,0)^3+0.052411824*d_L-0.05196947*d_B 
  }
  
  .dmB <- function(d_B = 12.657859,d_L = 51.396738,d_A = 2.8906427) {
    -5.5112278+0.73963288* d_B-0.00022874794*pmax(d_B-5.7294897,0)^3+0.00045814458*pmax(d_B-12.657859,0)^3-0.00022939663*pmax(d_B-19.566637,0)^3+0.1445493*d_L+0.21897497*d_A 
  }
  
  .mdL <- function(m_L = 40.90522,m_A = 3.2269026,m_B = 12.070734) {
    14.107465+1.030697* m_L-0.00062374023*pmax(m_L-30.240189,0)^3+0.0012570867*pmax(m_L-40.90522,0)^3-0.00063334643*pmax(m_L-51.40849,0)^3-0.55323226*m_A+0.23956231*m_B 
  }
  
  .mdA <- function(m_A = 3.2269026,m_L = 40.90522,m_B = 12.070734) {
    0.67518171+1.0529881* m_A-0.00046032631*pmax(m_A,0)^3+0.00071535947*pmax(m_A-3.2269026,0)^3-0.00025503316*pmax(m_A-9.0513538,0)^3-0.0037767466*m_L-0.089514303*m_B 
  }
  
  .mdB <- function(m_B = 12.070734,m_L = 40.90522,m_A = 3.2269026) {
    3.0242045+0.86434908* m_B-0.00088833302*pmax(m_B-5.5707716,0)^3+0.001658638*pmax(m_B-12.070734,0)^3-0.00077030501*pmax(m_B-19.566637,0)^3-0.038454218*m_L+0.28100782*m_A 
  }
  
  
  .P <- switch(
    .state, 
    'dry' = {
      data.frame(
        L = .dmL(d_L = .lab$L, d_A = .lab$A, d_B = .lab$B),
        A = .dmA(d_A = .lab$A, d_L = .lab$L, d_B = .lab$B),
        B = .dmB(d_B = .lab$B, d_L = .lab$L, d_A = .lab$A)
      )   
    }, 
    'moist' = {
      data.frame(
        L = .mdL(m_L = .lab$L, m_A = .lab$A, m_B = .lab$B),
        A = .mdA(m_A = .lab$A, m_L = .lab$L, m_B = .lab$B),
        B = .mdB(m_B = .lab$B, m_L = .lab$L, m_A = .lab$A)
      )
    })
  
  
  # CIELAB -> closest Munsell
  if(.m) {
    .res <- col2Munsell(.P, space = 'CIELAB', nClosest = 1)  
  } else {
    # CIELAB coordinate
    .res <- .P
  }
  
  return(.res)
}






