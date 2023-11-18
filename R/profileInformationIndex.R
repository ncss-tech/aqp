
# x: vector of any type, NA removed
# d: number of significant digits to retain for numeric x
.prepareVector <- function(x, d) {
  
  if(!inherits(x, 'character')) {
    
    # format numeric values to a fixed precision
    if(inherits(x, 'numeric')) {
      x <- format(round(x, digits = d), digits = d)
    }
    
    # convert logical to integers
    if(inherits(x, 'logical')) {
      x <- as.integer(x)
    }
    
    # convert factors to integers
    if(inherits(x, 'factor')) {
      x <- as.integer(x)
    }
    
    # to character
    x <- as.character(x)
  }
  
  # collapse to single string
  # otherwise, memCompress will add '\n' between vector elements
  x <- paste0(x, collapse = '')
  
  return(x)
}

# x: vector of any type, ignoring NA
# d: number of significant digits to retain for numeric x
.prepareBaseline <- function(x, d, type) {
  
  if(type == 'numeric') {
    # replicate simple mean over all horizons
    # weighted by 1cm slices
    b <- format(
      round(
        rep(
          mean(x, na.rm = TRUE), 
          times = length(x)
        ), 
        digits = d
      ),
      digits = d
    )
  } else {
    # logical and factor -> convert to integer codes
    if(!inherits(x, 'character')) {
      x <- as.integer(x)
    }
    
    # baseline is the most frequent, 
    # integer-coded (factor, logical)
    # frequency weighted by hz thickness, 
    # non-NA value
    b <- names(sort(table(x, useNA = 'no'), decreasing = TRUE))[1]
    b <- rep(b, times = length(x))
  }
  
  
  # collapse to single string
  # otherwise, memCompress will add '\n' between vector elements
  b <- paste0(b, collapse = '')
  
  return(b)
}



# x: character, length 1 output from .prepareVector()
# d: number of significant digits to retain for numeric x
# n: compression method
.compressedLength <- function(x, d = 10, m = 'gzip') {
  
  # ideally, numeric data have mean of 0 and sd of 1
  # otherwise, 1, 10, 100 will have different compressed lengths
  
  # memCompress() requires character data
  # this algorithm is based on character vectors flattened to single string
  # output from .prepareVector() is ideal
  if(!inherits(x, 'character') || length(x) > 1) {
    message('flattening character vector')
    x <- .prepareVector(x, d = d)
  }
  
  # otherwise using a single string, with NA removed
  
  # length of compressed vector is the metric
  res <- length(memCompress(x, type = m))
  
  # length of empty string ('') is 8 bits
  res <- res - 8
  
  return(res)
}



# i: vector, any type
# numericDigits: requested precision for numeric data
.prepareVariable <- function(i, numericDigits) {
  
  # remove all NA
  i <- as.vector(na.omit(i))
  
  # short-circuit for all NA
  if(all(is.na(i))) {
    return(list(v = NULL, b = NULL))
  }
  
  # baseline is mean(i)
  if(is.numeric(i)) {
    
    # baseline for numeric data: 1cm slice wt. mean
    b <- .prepareBaseline(i, d = numericDigits, type = 'numeric')
    
    # prepare for compression
    v <- .prepareVector(i, d = numericDigits)
    
  } else {
    # categorical or logical
    # baseline is most-frequent, non-NA value
    
    # treating all categorical variables as nominal for now
    
    # baseline is the most frequent, weighted by hz thickness, non-NA value
    b <- .prepareBaseline(i, d = numericDigits, type = 'other')
    
    
    # prepare for compression
    v <- .prepareVector(i, d = numericDigits)
  }
  
  return(list(v = v, b = b))
}


.PII_by_profile <- function(x, vars, baseline, method, numericDigits, compression) {
  
  # diced SPC -> DF of horizon data
  # just variables of interest
  h <- as(x, 'data.frame')[, vars, drop = FALSE]
  
  # iterate over columns and prepare data + baseline for PII
  h <- lapply(
    h[, vars, drop = FALSE], 
    FUN = .prepareVariable, 
    numericDigits = numericDigits
  )
  
  # extract variables
  v <- lapply(h, '[[', 'v')
  
  # extract baseline
  b <- lapply(h, '[[', 'b')
  
  res <- switch(
    method,
    'joint' = {
      
      # joint complexity
      
      # concatenate prepared variables, by flattening character vectors
      v <- as.vector(do.call('paste0', list(v, collapse = '')))
      # concatenate prepared baseline, by flattening character vectors
      b <- as.vector(do.call('paste0', list(b, collapse = '')))
      
      # compress entire collection of data
      v <- .compressedLength(v, m = compression)
      
      # this is the joint baseline complexity
      b <- .compressedLength(b, m = compression)
      
      # ratio complexity vs. baseline complexity
      if(baseline) {
        v <- v / b 
      }
      v
    },
    'individual' = {
      
      ## TODO: flattening by .compressedLength() or here?
      # sum of individual complexities
      v <- suppressMessages(sum(sapply(v, .compressedLength, m = compression), na.rm = TRUE))
      b <- suppressMessages(sum(sapply(b, .compressedLength, m = compression), na.rm = TRUE))
      
      # ratio complexity vs. baseline complexity
      if(baseline) {
        v <- v / b 
      }
      v
    }
  )
  
  return(res)
}


#' @title Soil Profile Information Index
#' 
#' @description A simple index of "information" content associated with individuals in a `SoilProfileCollection` object. Information content is quantified by number of bytes after compression via `memCompress()`.
#'
#' @param x `SoilProfileCollection` object
#' 
#' @param vars character vector of site or horizon level attributes to consider
#' 
#' @param method character, 'individual' or 'joint' complexity
#' 
#' @param baseline logical, compute ratio to "baseline" information content, see details
#' 
#' @param numericDigits integer, number of significant digits to retain in numeric -> character conversion
#' 
#' @param padNA logical, pad depths to `max(x)`, supplied to `dice(fill = padNA)`
#' 
#' @param scaleNumeric logical, `scale()` each numeric variable, causing "profile information" to vary based on other profiles in the collection
#' 
#' @param compression character, compression method as used by [memCompress()]: 'gzip', 'bzip2', 'xz', 'none'
#'
#' @return a numeric vector of the same length as `length(x)` and in the same order, suitable for direct assignment to a new site-level attribute 
#' 
#' @export
#'
#' @author D.E. Beaudette
#' 
#' @details Information content via compression (gzip) is the central assumption behind this function: the values associated with a simple soil profile having few horizons and little variation between horizons (isotropic depth-functions) will compress to a much smaller size than a complex profile (many horizons, strong anisotropy). Information content is evaluated a profile at a time, over each site or horizon level attribute specified in `vars`. The `baseline` argument invokes a comparison to the simplest possible representation of each depth-function:
#' 
#'    * `numeric`: replication of the mean value to match the number of horizons with non-NA values
#'    * `character` or `factor`: replication of the most frequent value to match the number of horizons with non-NA values
#'
#' The ratios computed against a "simple" baseline represent something like "information gain". Larger baseline ratios suggest more complexity (more information) associated with a soil profile's depth-functions. Alternatively, the total quantity of information (in bytes) can be determined by setting `baseline = FALSE`.
#'
#'
#'
#' @examples 
#' 
#' # single horizon, constant value
#' p1 <- data.frame(id = 1, top = 0, bottom = 100, p = 5, name = 'H')
#' 
#' # multiple horizons, constant value
#' p2 <- data.frame(
#'   id = 2, top = c(0, 10, 20, 30, 40, 50),
#'   bottom = c(10, 20, 30, 40, 50, 100),
#'   p = rep(5, times = 6),
#'   name = c('A1', 'A2', 'Bw', 'Bt1', 'Bt2', 'C')
#' )
#' 
#' # multiple horizons, random values
#' p3 <- data.frame(
#'   id = 3, top = c(0, 10, 20, 30, 40, 50),
#'   bottom = c(10, 20, 30, 40, 50, 100),
#'   p = c(1, 5, 10, 35, 6, 2),
#'   name = c('A1', 'A2', 'Bw', 'Bt1', 'Bt2', 'C')
#' )
#' 
#' # multiple horizons, mostly NA
#' p4 <- data.frame(
#'   id = 4, top = c(0, 10, 20, 30, 40, 50),
#'   bottom = c(10, 20, 30, 40, 50, 100),
#'   p = c(1, NA, NA, NA, NA, NA),
#'   name = c('A1', 'A2', 'Bw', 'Bt1', 'Bt2', 'C')
#' )
#' 
#' # shallower version of p1
#' p5 <- data.frame(id = 5, top = 0, bottom = 50, p = 5, name = 'H')
#' 
#' # combine and upgrade to SPC
#' z <- rbind(p1, p2, p3, p4, p5)
#' depths(z) <- id ~ top + bottom
#' hzdesgnname(z) <- 'name'
#' 
#' z <- fillHzGaps(z)
#' 
#' # visual check
#' par(mar = c(1, 0, 3, 3))
#' plotSPC(z, color = 'p', name.style = 'center-center', cex.names = 0.8, max.depth = 110)
#' 
#' # factor version of horizon name
#' z$fname <- factor(z$name)
#' 
#' vars <- c('p', 'name')
#' # result is total bytes
#' pi <- profileInformationIndex(z, vars = vars, method = 'joint', baseline = FALSE)
#' 
#' text(x = 1:5, y = 105, labels = pi, cex = 0.85)
#' mtext('Profile Information Index (bytes)', side = 1, line = -1)
#'
profileInformationIndex <- function(x, vars, method = c('joint', 'individual'), baseline = FALSE, numericDigits = 8, padNA = FALSE, scaleNumeric = FALSE, compression = 'gzip') {
  
  # sanity check
  method <- match.arg(method)
  
  ## TODO: think more about this
  # scaling means that PII depends on collection and is not absolute
  if(scaleNumeric) {
    # scale numeric variables to mean of 0 and SD of 1
    for(i in vars) {
      if(inherits(x[[i]], 'numeric')) {
        
        # scale() will return NaN when SD() is NA or 0
        .sd_i <- sd(x[[i]], na.rm = TRUE)
        
        # threshold for very small numbers 
        if(!is.na(.sd_i)) {
          if(.sd_i > 0.0001) {
            x[[i]] <- scale(x[[i]])
          }
        }
        
        # otherwise, use as-is
      }
    }
  }
  
  
  ## TODO: this will error / drop profiles in the presence of bad horizonation
  
  # dice() to 1cm intervals for common baseline
  #  -> 10 horizons of the same data NOT more informative than 1 horizon 
  #  -> causes data corruption when bad hz depths present (lots of messages)
  x <- dice(x, fill = padNA)
  
  ## TODO: convert to data.table
  # SPC = FALSE
  # data.table()
  # dt[, ]
  # as.vector()
  
  # iterate over profiles
  # result is a vector suitable for site-level attribute
  res <- profileApply(
    x, 
    simplify = TRUE, 
    FUN = .PII_by_profile, 
    vars = vars, 
    baseline = baseline, 
    method = method,
    numericDigits = numericDigits, 
    compression = compression
  )
  
  # done
  return(res)
}

