
## TODO: 
##       * optionally return wt. mean dH, dV, dC | most frequent contrast class
##         metric = c('dE00', 'dV', 'dH, 'dC', 'CC')
##
##       * allow various specifications for color
##
##       * unify with colorQuantiles()



#' @title Quantitative Description of Color Variation
#' 
#' @description
#' This function computes several measures of "color variation", typically associated with soil colors described in the Munsell system, using the CIE2000 dE (dE00) color contrast metric. The resulting dE00 summaries map closely to color differences as perceived by "average human vision".
#' 
#' @details
#' dE00 values are computed according to `method`:
#' 
#'   * 'frequency': relative to most frequent color in `m`
#'   * 'centroid': relative to centroid (CIELAB coordinates) of colors specified in `m`
#'   * 'L1': relative to L1-median (geometric median) CIELAB coordinates of colors specified in `m`, via `Gmedian::Gmedian()`
#'   * 'reference': relative to color specified in `ref`
#' 
#' 
#' The `L1` method is more robust to outliers in `m` as compared to other methods.
#' 
#' @param m character vector of colors, described using the Munsell system e.g. `c('10YR 3/3', '5YR 4/6')`
#' @param method character, one of `c('frequency', 'centroid', 'reference')`, see Details
#' @param ref character, a reference color specified in the Munsell system when `method = 'reference'`
#'
#' @returns numeric, dE00 summary of color variation along with group centroid for `method = c('frequency', 'centroid', 'L1')`
#' @export
#'
#' @examples
#' 
#' # some brownish colors with a wild outlier
#' m <- c('10YR 3/3', '10YR 4/4', '10YR 4/4', '5GY 6/8')
#' 
#' # useful when there may be a lot of duplicates
#' # error when there is not a single, most-frequent color
#' colorVariation(m, method = 'frequency')
#' 
#' # statistical "centroid" of colors, not robust to outliers
#' # result may not match any color in `m`
#' colorVariation(m, method = 'centroid')
#' 
#' # deviation from a known reference
#' colorVariation(m, method = 'reference', ref = '10YR 2/1')
#' 
#' # L1-median (requires Gmedian package) like 'centroid'
#' # more robust to outliers
#' # result will usually be very close to a color in `m`
#' if(requireNamespace('Gmedian')) {
#'   colorVariation(m, method = 'L1')
#' }
#' 
#' 
#' # compare methods using a range of colors on a 
#' # single hue page
#' x <- expand.grid(
#'   hue = '10YR', 
#'   value = 2:7, 
#'   chroma = 2:7
#' )
#' 
#' x$m <- sprintf("%s %s/%s", x$hue, x$value, x$chroma)
#' 
#' colorChart(x$m)
#' 
#' (v <- colorVariation(x$m, method = 'centroid'))
#' contrastChart(attr(v, 'centroid'), hues = x$hue[1], thresh = v)
#' 
#' if(requireNamespace('Gmedian')) {
#'   (v <- colorVariation(x$m, method = 'L1'))
#'   contrastChart(attr(v, 'L1'), hues = x$hue[1], thresh = v)
#' }
#' 
#' 
#' # attempt to simulate colors from a group centroid
#' \dontrun{
#' v <- colorVariation(x$m, method = 'centroid')
#' 
#' s <- simulateColor(
#' method = 'dE00', 
#' n = 200, 
#' parameters = list(m = attr(v, 'centroid'), thresh = v * 1.96, hues = x$hue[1])
#' )
#' colorChart(s[[1]])
#' }
#' 
#' 
colorVariation <- function(m, method = c('frequency', 'centroid', 'L1', 'reference'), ref = NULL) {
  
  method <- match.arg(method)
  
  # dep check
  if(method == 'L1') {
    if(!requireNamespace('Gmedian')) {
      stop('package `Gmedian` is required', call. = FALSE)
    }
  }
  
  # filter Munsell colors:
  #  * NA in vector, or any position of hue value/chroma
  #  * mis-specified
  #  * non-standard notation
  idx <- which(validateMunsell(m))
  m <- m[idx]
  
  # trap all NA
  if(length(m) < 1) {
    return(NA)
  }
  
  # sorted weights and names
  wt <- sort(table(m), decreasing = TRUE)
  wt.m <- names(wt)
  
  # single color, variation is 0 
  if(length(wt) < 2) {
    message('fewer than 2 unique colors')
    return(0)
  }
  
  
  # D(colors, most frequent color)
  if(method == 'frequency') {
    
    # sanity check: there must be a single most-frequent color
    .maxwt <- wt[which.max(wt)]
    if(length(which(wt == .maxwt)) > 1) {
      stop('no single, most-frequent color: select a different method')
    }
    
    # color contrast vs. most frequent color
    cc <- colorContrast(m1 = wt.m, m2 = rep(wt.m[1], times = length(wt)))
    
    if(length(wt) < 3) {
      # unweighted
      res <- mean(cc$dE00, na.rm = TRUE)
    } else {
      # weighted
      res <- weighted.mean(cc$dE00, w = wt, na.rm = TRUE)
    }
    
    attr(res, 'most frequent') <- wt.m[1]
  }
  
  
  # D(colors, wt. color-centroid)
  if(method == 'centroid') {
    
    # centroid is wt. mean in CIELAB space
    lab <- parseMunsell(wt.m, returnLAB = TRUE)
    
    # account for colors which cannot be converted from Munsell -> CIELAB
    not.na <- which(! apply(apply(lab, 2, is.na), 1, any))
    
    lab.centroid <- apply(lab[not.na, ], 2, function(i) {
      sum(wt[not.na] * i) / sum(wt[not.na])
    })
    
    # convert back to Munsell notation for colorContrast()
    m.centroid <- col2Munsell(t(lab.centroid), space = 'CIELAB')
    m.centroid <- formatMunsell(m.centroid$hue, m.centroid$value, m.centroid$chroma)
    
    # color contrast vs. centroid
    cc <- colorContrast(m1 = wt.m, m2 = rep(m.centroid, times = length(wt)))
    
    if(length(wt) < 3) {
      # unweighted
      res <- mean(cc$dE00, na.rm = TRUE)
    } else {
      # weighted
      res <- weighted.mean(cc$dE00, w = wt, na.rm = TRUE)
    }
    
    attr(res, 'centroid') <- m.centroid
    
  }
  
  # D(colors, L1 median)
  if(method == 'L1') {
    
    # use all colors, not unique colors with weights
    lab <- parseMunsell(m, returnLAB = TRUE)
    
    # automatically removes NA
    lab.centroid <- Gmedian::Gmedian(lab)
    
    # convert back to Munsell notation for colorContrast()
    m.centroid <- col2Munsell(lab.centroid, space = 'CIELAB')
    m.centroid <- formatMunsell(m.centroid$hue, m.centroid$value, m.centroid$chroma)
    
    # color contrast vs. centroid
    cc <- colorContrast(m1 = wt.m, m2 = rep(m.centroid, times = length(wt)))
    
    if(length(wt) < 3) {
      # unweighted
      res <- mean(cc$dE00, na.rm = TRUE)
    } else {
      # weighted
      res <- weighted.mean(cc$dE00, w = wt, na.rm = TRUE)
    }
    
    attr(res, 'L1') <- m.centroid
    
  }
  
  # D(color, reference color)
  if(method == 'reference') {
    
    # color contrast vs. reference color
    cc <- colorContrast(m1 = wt.m, m2 = rep(ref, times = length(wt)))
    
    if(length(wt) < 3) {
      # unweighted
      res <- mean(cc$dE00, na.rm = TRUE)
    } else {
      # weighted
      res <- weighted.mean(cc$dE00, w = wt)
    }
    
  }
  
  return(res)
}

