
.splitSPC <- function(x, f, drop=TRUE, ...) {
  
  ## TODO: maybe safe for hz attr?
  # for now this is only safe for site-level attr
  if(! f %in% siteNames(x)) {
    stop(sprintf('%s must be site-level attribute', f), call. = FALSE)
  }
  
  # no NA allowed
  if(any(is.na(x[[f]]))) {
    stop(sprintf('NA not allowed in %s', f), call. = FALSE)
  }
  
  # splitting variable should be a factor
  if(! inherits(x[[f]], 'factor')) {
    x[[f]] <- factor(x[[f]])
    message(sprintf('converting %s to a factor', f))
  }
  
  ## TODO: test this
  # is this really neccessary?
  if(drop) {
    x[[f]] <- droplevels(x[[f]])
  }
  
  # iterate over levels
  lv <- levels(x[[f]])  
  
  # index and split
  res <- lapply(lv, function(i) {
    
    # simple indexing on site-level data only
    x[which(x[[f]] == i), ]
    
    # c/o AGB: this should work, but will require fancier imports from rlang
    # filter(x, !!sym(f) == i)
  })
  
  # save names
  names(res) <- lv
  
  # result is a list
  return(res)
}



# S4 magic
if (!isGeneric("split"))
  setGeneric("split", function(x, f, drop=TRUE, ...) standardGeneric("split"))

setMethod(f='split', signature='SoilProfileCollection', .splitSPC)


