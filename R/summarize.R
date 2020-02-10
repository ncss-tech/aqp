#' @title Reduce multiple site or horizon values to a single site value
#' @name summarize
#' @aliases summarize,SoilProfileCollection-method
#' @description \code{summarize()} is a function used for summarizing SoilProfileCollections. Each summary expression is applied to individual profiles. In the future, higher tiers of grouping, based on one or more variables, will be possible. 
#' @param object A SoilProfileCollection
#' @param ... A set of comma-delimited R expressions that resolve to a summary value; may be named. e.g \code{mean = mean(clay, na.rm=TRUE)}
#' @return A SoilProfileCollection
#' @author Andrew G. Brown.
#' 
#' @rdname summarize
#' @export summarize
summarize <- function(object, ...) {
  if(requireNamespace("rlang")) {
    
    # capture expression(s) at function
    x <- rlang::enquos(..., .named = TRUE)
  
    # TODO: group_by iterator would operate above profile?
    #       is it safe to assume operations, in general, would
    #       be on the profile basis, and then aggregated by group?
    
    # apply expressions to each profile, frameify results
    res <- profileApply(object, function(o) {
      # create composite object to facilitate eval_tidy
      # TODO: abstract
      
      h <- horizons(o)
      s <- as.list(site(o))
      h <- as.list(h[,!horizonNames(o) %in% siteNames(o)])
      .data <- c(s, h)
      
      buf <- data.frame(1)
      for(n in names(x)) {
        foo <- rlang::eval_tidy(x[[n]], .data)
        buf[[n]] <- foo
      }
      buf <- as.data.frame(buf[,names(x)])
      names(buf) <- names(x)
      return(buf)
      
    }, frameify = TRUE)
    
    # would include other grouping vars here
    dfid <- data.frame(profile_id(object))
    
    names(dfid) <- idname(object)
    rownames(res) <- NULL
    
    site(object) <- cbind(dfid, res)
    
    return(object)
  }
}

