#' @title Transform a SPC with expressions based on site or horizon level attributes
#' @name mutate
#' @aliases mutate,SoilProfileCollection-method
#' @description \code{mutate()} is a function used for transforming SoilProfileCollections. It allows the user to specify an arbitrary number of expressions that resolve to the (re-)calculation of one or more site or horizon level attributes. For instance: \code{mutate(spc, thickness = hzdepb - hzdept)}. The expressions may depend on one another, and are evaluated from left to right.
#' @param object A SoilProfileCollection
#' @param ... Comma-separated set of R expressions e.g. \code{thickness = hzdepb - hzdept, hzdepm = hzdept + round(thk / 2)}
#' @return A SoilProfileCollection.
#' @author Andrew G. Brown.
#' 
#' @rdname mutate
#' @export mutate
mutate <- function(object, ...) {
  #if(requireNamespace("rlang")) {
    
    # capture expression(s) at function
    x <- rlang::enquos(..., .named = TRUE)
    
    # create composite object to facilitate eval_tidy
    data <- compositeSPC(object)
    
    for(n in names(x)) {
      foo <- rlang::eval_tidy(x[[n]], data)
      object[[n]] <- foo
      data[[n]] <- foo
    }
    
    return(object)
  #}
}
