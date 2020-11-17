

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
    
    # L1 median for non-NA records, variables of interest only
    G <- data.frame(Gmedian(j[idx, v, drop = FALSE]))
    
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

#' @title Create Representative Profiles via L1 Median
#' 
#'
#' @param x \code{SoilProfileCollection} object
#' @param fm formula
#' @param basis aggregation basis
#' @param method soil depth evaluation method
#' @param maxDepthRule min | max
#' @param strict passed to \code{slice}
#'
#' @return
#' @export
#'
L1_profiles <- function(x, fm, basis = 1, method = c('regex', 'simple'), maxDepthRule = c('max', 'min'), strict = FALSE) {
  
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
         })
   
  
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


