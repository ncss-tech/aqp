
## TODO: optionally return wt. mean dH, dV, dC | most frequent contrast class
##       metric = c('dE00', 'dV', 'dH, 'dC', 'CC')

## TODO: allow various specifications for color

## TODO: add method = 'L1 median'
##       --> requires updates to colorQuantiles() to accept Munsell notation


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
#' @param m character vector of colors, described using the Munsell system e.g. `c('10YR 3/3', '5YR 4/6')`
#' @param method character, one of `c('frequency', 'centroid', 'reference')`, see Details
#' @param ref character, a reference color specified in the Munsell system when `method = 'reference'`
#'
#' @returns numeric dE00 summary of color variation
#' @export
#'
#' @examples
#' 
#' m <- c('10YR 3/3', '10YR 4/4', '10YR 4/4', '5GY 6/8')
#' colorVariation(m)
#' 
colorVariation <- function(m, method = c('frequency', 'centroid', 'reference', 'L1'), ref = NULL) {
  
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
    
    # color contrast vs. most frequent color
    cc <- colorContrast(m1 = wt.m, m2 = rep(wt.m[1], times = length(wt)))
    
    if(length(wt) < 3) {
      # unweighted
      res <- mean(cc$dE00, na.rm = TRUE)
    } else {
      # weighted
      res <- weighted.mean(cc$dE00, w = wt)
    }
    
    attr(res, 'most frequent') <- wt.m[1]
    
  }
  
  
  # D(colors, wt. color-centroid)
  if(method == 'centroid') {
    
    # centroid is wt. mean in CIELAB space
    lab <- parseMunsell(wt.m, returnLAB = TRUE)
    
    lab.centroid <- apply(lab, 2, function(i) {
      sum(wt * i) / sum (wt)
    })
    
    # convert back to Munsell notation for colorContrast()
    m.centroid <- col2Munsell(t(lab.centroid), space = 'CIELAB')
    m.centroid <- sprintf("%s %s/%s", m.centroid$hue, m.centroid$value, m.centroid$chroma)
    
    # color contrast vs. centroid
    cc <- colorContrast(m1 = wt.m, m2 = rep(m.centroid, times = length(wt)))
    
    if(length(wt) < 3) {
      # unweighted
      res <- mean(cc$dE00, na.rm = TRUE)
    } else {
      # weighted
      res <- weighted.mean(cc$dE00, w = wt)
    }
    
    attr(res, 'centroid') <- m.centroid
    
  }
  
  # D(colors, L1 median)
  if(method == 'L1') {
    
    # 
    lab <- parseMunsell(m, returnLAB = TRUE)
    
    lab.centroid <- Gmedian::Gmedian(lab)
    
    # convert back to Munsell notation for colorContrast()
    m.centroid <- col2Munsell(lab.centroid, space = 'CIELAB')
    m.centroid <- sprintf("%s %s/%s", m.centroid$hue, m.centroid$value, m.centroid$chroma)
    
    # color contrast vs. centroid
    cc <- colorContrast(m1 = wt.m, m2 = rep(m.centroid, times = length(wt)))
    
    if(length(wt) < 3) {
      # unweighted
      res <- mean(cc$dE00, na.rm = TRUE)
    } else {
      # weighted
      res <- weighted.mean(cc$dE00, w = wt)
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
  
  
  # ## TODO: return to this someday
  # # wt. mean of D(colors)
  # if(method == 'pairs') {
  #   
  #   # all pair-wise dE00
  #   cc <- colorContrast(wt.m)
  #   
  #   # sum of weights of pair-wise comparisons
  #   s <- vector(mode = 'numeric', length = nrow(cc))
  #   
  #   # TODO: check this
  #   
  #   # iterate over rows of cc
  #   # combine weights for each color
  #   for(j in seq_along(s)) {
  #     .idx1 <- match(cc[j, ]$m1, wt.m)
  #     .idx2 <- match(cc[j, ]$m2, wt.m)
  #     
  #     # sum of W1 + W2 for pariwise combination m1, m2
  #     s[j] <- wt[.idx1] + wt[.idx2]
  #   }
  #   
  #   # use regular SD
  #   if(length(wt) < 3) {
  #     # unweighted
  #     res <- mean(cc$dE00, na.rm = TRUE)
  #   } else {
  #     # weighted
  #     res <- weighted.mean(cc$dE00, w = s)
  #   }
  #   
  # }
  
  return(res)
  
}

