
# x: vector of any type
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
  
  return(x)
}


# x: vector of any type
# d: number of significant digits to retain for numeric x
# n: compression method
.compressedLength <- function(x, d = 10, m = 'gzip') {
  
  # ideally, numeric data have mean of 0 and sd of 1
  # otherwise, 1, 10, 100 will have different compressed lengths
  
  # memCompress() requires character data
  # convert all types to character, preserving 'd' significant digits
  if(!inherits(x, 'character')) {
    x <- .prepareVector(x, d = d)
  } 
  
  # length of compressed vector is the metric
  res <- length(memCompress(x, type = m))
  return(res)
}

# 
# # x: single profile SPC
# # vars: columns to consider
# # method: aggregation over columns
# # baseline: toggle for baseline comparison
# # numericDigits: number of digits to retain in numeric -> character conversion
# # removeNA: do not include NA in gz(data)
# .pii_by_profile <- function(x, vars, method, baseline, numericDigits, removeNA) {
#   
#   # SPC -> DF
#   h <- as(x, 'data.frame')[, vars, drop = FALSE]
#   
#   
#   ## TODO: computing PII by column removes possible redundancy created by correlation within
#   ##       -> PII as currently computed is an over-estimate
#   ##       -> joint PII via: compress(c(v1, v2, v3))
#   
#   
#   # iterate over columns and compute column-wise PII
#   h <- lapply(
#     h[, vars, drop = FALSE], 
#     FUN = .pii, 
#     baseline = baseline, 
#     numericDigits = numericDigits,
#     removeNA = removeNA
#   )
#   
#   # each list element is a 1-length numeric
#   h <- unlist(h)
#   
#   # unless it is all NA
#   if(all(is.na(h))) {
#     return(NA)
#   }
#   
#   # aggregate PII by column to single value / profile
#   res <- switch(method,
#                 mean = {
#                   mean(h, na.rm = TRUE)
#                 },
#                 median = {
#                   median(h, na.rm = TRUE)
#                 },
#                 sum = {
#                   sum(h, na.rm = TRUE)
#                 }
#   )
#   
#   return(res)
# }



## TODO: consider "max entropy" base line of runif(length(i))
##       PII = 1 - I/Ib

# 
# # main algorithm
# # i: vector of values
# # baseline: toggle for baseline comparison
# # numericDigits: number of digits to retain in numeric -> character conversion
# # removeNA: do not include NA in gz(data)
# .pii <- function(i, baseline, numericDigits, removeNA) {
#   
#   if(all(is.na(i))) {
#     return(NA)
#   }
#   
#   # baseline is mean(i)
#   if(is.numeric(i)) {
#     # optionally remove NA from length(gz())
#     if(removeNA) {
#       i <- na.omit(i)
#     }
#     
#     # baseline for numeric data:
#     # replicate simple mean over all horizons (no weighting)
#     b <- as.character(
#       signif(
#         rep(
#           mean(i, na.rm = TRUE), 
#           times = length(i)
#         ), 
#         digits = numericDigits
#       )
#     )
#     
#     # return compressed length
#     # any data type
#     v <- .compressedLength(i, d = numericDigits)
#     b <- .compressedLength(b)
#     
#   } else {
#     # categorical data
#     # baseline is most-frequent, non-NA value
#     
#     # optionally remove NA from length(gz())
#     if(removeNA) {
#       i <- na.omit(i)
#     }
#     
#     # treating all categorical variables as nominal for now
#     
#     # baseline is the most frequent, weighted by hz thickness, non-NA value
#     b <- names(sort(table(i), decreasing = TRUE))[1]
#     b <- rep(b, times = length(i))
#     
#     # return compressed length
#     # any data type
#     v <- .compressedLength(i)
#     b <- .compressedLength(b)
#   }
#   
#   # optionally compare vs. baseline
#   if(baseline) {
#     res <- v / b
#   } else {
#     # no comparison
#     res <- v
#   }
#   
#   return(res)
# }


## TODO: think more about NA handling

# i: vector, any type
# numericDigits: requested precision for numeric data
# removeNA: remove NA before preparing
.prepareVariable <- function(i, numericDigits, removeNA) {
  
  ## TODO: what is appropriate return value when all NA?
  
  # short-circuit for all NA
  if(all(is.na(i))) {
    return(list(v = NULL, b = NULL))
  }
  
  # baseline is mean(i)
  if(is.numeric(i)) {
    # optionally remove NA from length(gz())
    if(removeNA) {
      i <- na.omit(i)
    }
    
    # baseline for numeric data:
    # replicate simple mean over all horizons (no weighting)
    b <- format(
      round(
        rep(
          mean(i, na.rm = TRUE), 
          times = length(i)
        ), 
        digits = numericDigits
      ),
      digits = numericDigits
    )
    
    # prepare for compression
    v <- .prepareVector(i, d = numericDigits)
    
  } else {
    # categorical or logical
    # baseline is most-frequent, non-NA value
    
    # optionally remove NA from length(gz())
    if(removeNA) {
      i <- na.omit(i)
    }
    
    # treating all categorical variables as nominal for now
    
    # baseline is the most frequent, weighted by hz thickness, non-NA value
    b <- names(sort(table(i), decreasing = TRUE))[1]
    b <- rep(b, times = length(i))
    
    # prepare for compression
    v <- .prepareVector(i, d = numericDigits)
  }
  
  return(list(v = v, b = b))
}


.PII_by_profile <- function(x, vars, baseline, method, numericDigits, removeNA) {
  
  # diced SPC -> DF of horizon data
  # just variables of interest
  h <- as(x, 'data.frame')[, vars, drop = FALSE]
  
  # iterate over columns and prepare data + baseline for PII
  h <- lapply(
    h[, vars, drop = FALSE], 
    FUN = .prepareVariable, 
    numericDigits = numericDigits,
    removeNA = removeNA
  )
  
  # extract variables
  v <- lapply(h, '[[', 'v')
  
  # extract baseline
  b <- lapply(h, '[[', 'b')
  
  res <- switch(
    method,
    'joint' = {
      
      # joint complexity
      
      # concatenate prepared variables, as formatted text
      v <- as.vector(do.call('c', v))
      # concatenate prepared baseline, as formatted text
      b <- as.vector(do.call('c', b))
      
      # compress entire collection of data
      v <- .compressedLength(v)
      
      # this is the joint baseline complexity
      b <- .compressedLength(b)
      
      # ratio complexity vs. baseline complexity
      if(baseline) {
       v <- v / b 
      }
      v
    },
    'individual' = {
      
      # sum of individual complexities
      v <- sum(sapply(v, .compressedLength), na.rm = TRUE)
      b <- sum(sapply(b, .compressedLength), na.rm = TRUE)
      
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
#' @param removeNA remove NA before compression
#' 
#' @param padNA pad all profiles with NA to deepest lower depth in `x` (`max(x)`)
#'
#' @return a numeric vector of the same length as `length(x)` and in the same order, suitable for direct assignment to a new site-level attribute 
#' 
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
#' pi <- profileInformationIndex(z, vars = vars, method = 'sum', baseline = FALSE)
#' 
#' text(x = 1:5, y = 105, labels = pi, cex = 0.85)
#' mtext('Profile Information Index (bytes)', side = 1, line = -1)

#'
profileInformationIndex <- function(x, vars, method = c('joint', 'individual'), baseline = TRUE, numericDigits = 8, removeNA = FALSE, padNA = TRUE) {
  
  method <- match.arg(method)
  
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
  
  # dice() to 1cm intervals for common baseline
  #  -> 10 horizons of the same data NOT more informative than 1 horizon 
  #  -> causes data corruption when bad hz depths present (lots of messages)
  x <- dice(x, fill = padNA)
  
  ## TODO: think about how to make this more efficient when n > 1000
  
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
    removeNA = removeNA
  )
  
  # done
  return(res)
}

