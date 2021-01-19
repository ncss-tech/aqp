if (!isGeneric("group_by"))
  setGeneric("group_by", function(object, ...) 
    standardGeneric("group_by"))

#' (EXPERIMENTAL) Store groupings within a profile collection.  
#' @name group_by
#' @param object SoilProfileCollection. 
#' @param ... One or more expressions evaluated within the context of \code{object} that resolve to vectors that can be coerced to factor "groups."
#' @aliases group_by,SoilProfileCollection-method
#' @rdname group_by
setMethod("group_by", signature(object = "SoilProfileCollection"), 
          function(object, ...) { 
  if(requireNamespace("rlang", quietly = TRUE)) {
    # capture expression(s) at function
    x <- rlang::enquos(..., .named = TRUE)    
    
    # create composite object to facilitate eval_tidy
    data <- compositeSPC(object)
   
    # evaluate n expressions to get n grouping vars
    foo <- NULL
    
    for(n in names(x)) {
      bar <- try(as.character(rlang::eval_tidy(x[[n]], data)))
      
      if(inherits(foo, 'try-error'))
        stop("group_by expects expressions that resolve to one or more site or horizon level attributes", call.=FALSE)
      
      if(any(is.na(bar))) {
        stop(sprintf("group_by expects no NA values in '%s'", n), call.=FALSE)
      } else if(length(bar) == length(object)){ 
        foo <- c(foo, n)
      } else if(bar %in% siteNames(object)) {
        foo <- c(foo, bar)
      } else {
        stop("group_by expects expressions that resolve to one or more site or horizon level attributes", call.=FALSE)
      }
    }  
    
    # TODO: safe getter/setter metod
    # set value of metadata "SPC group" - which defines one or more 
    #   site (or horizon) level attrs that are used for grouping in certain functions
    #   in order of precedence, pipe-delimited.
    object@metadata$aqp_group_by <- paste0(foo, collapse = "|")
    
    return(object)
  } else {
    stop("package 'rlang' is required for group_by", .call=FALSE)
  }
})
