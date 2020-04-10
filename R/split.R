
.splitSPC <- function(x, f=NULL, drop=TRUE, ...) {
  
  # identity split, use idname
  if(is.null(f)) {
    
    # grouping factor, make sure to use original ordering
    fg <- site(x)[[idname(x)]]
    fg <- factor(fg, levels=fg)
    
  } else {
    # standard, site-level group split
    if(! f %in% siteNames(x)) {
      stop(sprintf('%s must be site-level attribute', f), call. = FALSE)
    }
    
    # no NA allowed
    if(any(is.na(x[[f]]))) {
      stop(sprintf('NA not allowed in %s', f), call. = FALSE)
    }
    
    # extract to local variable, so as not to modify original data
    fg <- x[[f]]
    
    # splitting variable should be a factor
    if(! inherits(fg, 'factor')) {
      fg <- factor(fg)
      message(sprintf('converting %s to a factor', f))
    }
  }
  
  
  ## TODO: test this
  # is this really neccessary?
  if(drop) {
    fg <- droplevels(fg)
  }
  
  # iterate over levels
  lv <- levels(fg)  
  
  # index and split
  res <- lapply(lv, function(i) {
    
    # simple indexing on site-level data only
    rr <- x[which(fg == i), ]
    
    # c/o AGB: this should work, but will require fancier imports from rlang
    # filter(x, !!sym(f) == i)
    
    return(rr)
  })
  
  # save names
  names(res) <- lv
  
  # result is a list
  return(res)
}



## S4 magic
# already exists, but we are modifying it... good idea or bad idea?
setGeneric("split", function(x, f=NULL, drop=TRUE, ...) standardGeneric("split"))

setMethod(f='split', signature='SoilProfileCollection', .splitSPC)


