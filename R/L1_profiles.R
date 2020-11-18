

.Gmedian.chunk <- function(i, id = '.chunk') {
  
  # iterate over chunks
  depth.list <- split(i, i[[id]])
  
  res <- lapply(depth.list, function(j) {
    
    # structure: group, chunk, top, bottom, var1, var2, var3, ...
    v <- 5:ncol(j)
    
    # NA not allowed, keep track and filter
    idx <- which(complete.cases(j[, v, drop = FALSE]))
    
    # catch conditions where there are no data
    # TODO: what is the min number of required records?
    if(length(idx) < 1)
      return(NULL)
    
    ## TODO: how do arguments to Gmedian() affect the results?
    # L1 median for non-NA records, variables of interest only
    G <- data.frame(Gmedian::Gmedian(X = j[idx, v, drop = FALSE]))
    
    # retain original names
    names(G) <- names(j)[v]
    
    # package group + top + bottom + L1 data
    d <- data.frame(group = j[1, 1], top = j[1, 3], bottom = j[1, 4], G, stringsAsFactors = FALSE)
    
    return(d)
  })
  
  # list -> DF
  res <- do.call('rbind', res)
  
  return(res)
}


## TODO:
# best practices for variable name compatibility (slab-style vs. original names)
# slab-style support for arbitrary depth intervals via basis argument
# most-likely horizon designation by chunk
# parallel operation -> see dice()
# parallel Gmedian computation
# add principal components for viz
# document!

#' @title Create Representative Soil Profiles via L1 Estimator
#' 
#' @description The L1 estimator, or \href{https://en.wikipedia.org/wiki/Geometric_median}{geometric median}, is a multivariate generalization of the (univariate) \href{https://en.wikipedia.org/wiki/Median}{median} concept. This function performs a multivariate aggregation (via L1 estimator) according to a suite of ratio-scale soil properties. The L1 estimator is applied to soil profile data that have been sliced to a 1-depth-unit basis.
#' 
#' See the \href{https://ncss-tech.github.io/AQP/aqp/L1-profiles.html}{L1 Profiles Tutoral} for additional examples.
#'
#' @note This function requires the `Gmedian` package.
#' 
#' @references Cardot, H., Cenac, P. and Zitt, P-A. (2013). Efficient and fast estimation of the geometric median in Hilbert spaces with an averaged stochastic gradient algorithm. Bernoulli, 19, 18-43.
#'
#' @param x \code{SoilProfileCollection} object
#' @param fm formula
#' @param basis aggregation basis
#' @param method soil depth evaluation method: regular expression, simple, constant. See details.
#' @param maxDepthRule maximum depth rule: min | max. See details.
#' @param maxDepthConstant depth when \code{maxDepthRule = 'constant'}
#' @param strict passed to \code{slice}
#'
#' @return a \code{SoilProfileCollection} object
#' @export
#'
L1_profiles <- function(x, fm, basis = 1, method = c('regex', 'simple', 'constant'), maxDepthRule = c('max', 'min'), maxDepthConstant = NULL, strict = FALSE) {
  
  # sanity check, need this for L1 median
  if(!requireNamespace('Gmedian'))
    stop('package `Gmedian` is required', call.=FALSE)
  
  
  # sanity checks: is this an SPC?
  if(! inherits(x, 'SoilProfileCollection')) {
    stop('`object` should be a SoilProfileCollection', call. = FALSE)
  }
  
  # extract components of the formula:
  g <- all.vars(update(fm, .~0)) # left-hand side
  vars <- all.vars(update(fm, 0~.)) # right-hand side
  
  # sanity check: do the variables specified in fm exist in the correct site / hz slots?
  if(
    ! all(vars %in% horizonNames(x)) |
    ! g %in% siteNames(x)
  ) {
    stop('`fm` must reference one or more horizon-level attributes and a single site-level attribute', call. = FALSE)
  }
  
  # arguments
  method <- match.arg(method)
  maxDepthRule <- match.arg(maxDepthRule)
  
  # multi-argument sanity
  if(method == 'constant' & !is.numeric(maxDepthConstant)) {
    stop('contant max depth must be specified by single numeric value', call. = FALSE)
  }
  
  # SPC metadata
  hztb <- horizonDepths(x)
  
  # upper depth logic, usually 0
  collection.top <- min(x[[hztb[1]]], na.rm = TRUE)
  
  # lower depth logic
  collection.bottom <- switch(method,
         regex = {
           x.depths <- profileApply(x, estimateSoilDepth, name = hzdesgnname(x))
           switch(maxDepthRule, 
                  min = {
                    min(x.depths, na.rm = TRUE)
                  },
                  max = {
                    max(x.depths, na.rm = TRUE)
                  })
         },
         simple = {
           switch(maxDepthRule,
                  min = {
                    min(x)
                  },
                  max = {
                    max(x)
                  })
         },
         constant = {
           maxDepthConstant
         }
         
  )
   
  
  # create slice formula
  # this only needs the top/bottom depths
  slice.fm <- as.formula(sprintf("%s:%s ~ %s", collection.top, collection.bottom, paste(vars, collapse = ' + ')))
  
  # re-format for slice-wise L1 median
  s <- slice(x, slice.fm, strict = strict)
  
  # work on de-normalized data as a DF
  h <- as(s, 'data.frame')
  
  ## TODO: interpret basis as either slab thickness, or like slab.structure
  
  # determine which via length(basis)
  
  # assign slab IDs
  
  
  # simplest case: working with raw slices, use top depth
  if(basis == 1) {
    h$.chunk <- h[[hztb[1]]]
  }
  
  
  # retain only those columns we need
  h <- h[, c(g, '.chunk', hztb, vars)] 
  
  # iterate over groups
  group.list <- split(h, h[[g]])
  
  ## TODO: parallel
  agg <- lapply(group.list, .Gmedian.chunk)
  agg <- do.call('rbind', agg)
  
  # init SPC with new ID, top, bottom names
  depths(agg) <- group ~ top + bottom
  
  return(agg)
}


