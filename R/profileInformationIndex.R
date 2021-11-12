

# x: single profile SPC
# vars: columns to consider
# method: aggregation over columns
# baseline: toggle for baseline comparison
# numericDigits: number of digits to retain in numeric -> character conversion
.pii_by_profile <- function(x, vars, method, baseline, numericDigits) {
  
  # SPC -> DF
  h <- as(x, 'data.frame')[, vars, drop = FALSE]
  
  # select variables
  # iterate over columns and compute column-wise PII
  h <- lapply(h[, vars, drop = FALSE], FUN = .pii, baseline = baseline, numericDigits = numericDigits)
  
  # each list element is a 1-length numeric
  h <- unlist(h)
  
  # unless it is all NA
  if(all(is.na(h))) {
    return(NA)
  }
  
  # reduce to single number
  res <- switch(method,
                mean = {
                  mean(h, na.rm = TRUE) - 1  
                },
                median = {
                  median(h, na.rm = TRUE) - 1
                },
                sum = {
                  sum(h, na.rm = TRUE) 
                }
  )
  
  return(res)
}


# main algorithm
# i: vector of values
# baseline: toggle for baseline comparison
# numericDigits: number of digits to retain in numeric -> character conversion
.pii <- function(i, baseline, numericDigits) {
  
  if(all(is.na(i))) {
    return(NA)
  }
  
  # baseline is mean(i)
  if(is.numeric(i)) {
    
    v <- as.character(signif(na.omit(i), digits = numericDigits))
    b <- as.character(signif(rep(mean(i, na.rm = TRUE), times = length(v)), digits = numericDigits))
    
    v <- memCompress(v, type = 'gzip')
    b <- memCompress(b, type = 'gzip')
    
  } else {
    
    # treating all categorical variables as nominal for now
    v <- as.character(na.omit(i))
    
    # baseline is the most frequent
    b <- names(sort(table(v), decreasing = TRUE))[1]
    b <- rep(b, times = length(v))
    
    # compress values, baseline: smallest possible representation
    v <- memCompress(v, type = 'gzip')
    b <- memCompress(b, type = 'gzip')
    
  }
  
  # compare vs. baseline
  if(baseline) {
    res <- length(v) / length(b)
  } else {
    # no comparison
    res <- length(v)
  }
  
  return(res)
}



#' @title Soil Profile Information Index
#' 
#' @description A simple index of "information" content associated with individuals in a `SoilProfileCollection` object. Information content is quantified by number of bytes after gzip compression via `memCompress()`.
#'
#' @param x `SoilProfileCollection` object
#' @param vars character vector of site or horizon level attributes to consider
#' @param method character: aggregation method, information content evaluated over `vars`: 'median', 'mean', or 'sum'
#' @param baseline logical, compute ratio to "baseline" information content, see details
#' @param useDepths logical, include horizon depths in `vars`
#' @param numericDigits integer, number of significant digits to retain in numeric -> character conversion
#'
#' @return a numeric vector of the same length as `length(x)` and in the same order, suitable for direct assignment to a new site-level attribute 
#' @export
#'
#' @author D.E. Beaudette
#' 
#' @details Information content via compression (gzip) is the central assumption behind this function: the values associated with a simple soil profile having few horizons and little variation between horizons (isotropic depth-functions) will compress to a much smaller size than a complex profile (many horizons, strong anisotropy). Information content is evaluated a profile at a time, over each site or horizon level attribute specified in `vars`. Values are aggregated to the profile level by `method`: median, mean, or sum. The `baseline` argument invokes a comparison to the simplest possible representation of each depth-function:
#' 
#'    * `numeric`: replication of the mean value to match the number of horizons with non-NA values
#'    * `character` or `factor`: replication of the most frequent value to match the number of horizons with non-NA values
#'
#' The ratios computed against a "simple" baseline represent something like "information gain", ranging from 0 to 1. Larger baseline ratios suggest more complexity (more information) associated with a soil profile's depth-functions. Alternatively, the total quantity of information (in bytes) can be determined by setting `baseline = FALSE` and `method = 'sum'`.
#'
profileInformationIndex <- function(x, vars, method = c('median', 'mean', 'sum'), baseline = TRUE, useDepths = TRUE, numericDigits = 4) {
  
  # method
  method <- match.arg(method)
  
  # depths
  if(useDepths) {
    vars <- unique(c(vars, horizonDepths(x)))
  }
  
  ## TODO: think about how to make this more efficient when n > 1000
  
  # iterate over profiles
  # result is a vector suitable for site-level attribute
  res <- profileApply(x, simplify = TRUE, FUN = .pii_by_profile, vars = vars, method = method, baseline = baseline, numericDigits = numericDigits)
  
  # done
  return(res)
}

