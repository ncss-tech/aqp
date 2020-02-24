#' @title Subset an SPC by applying glom to each profile
#' @name glomApply
#' @aliases glomApply,SoilProfileCollection-method
#' @description \code{glomApply()} is a function used for subsetting SoilProfileCollections. A function \code{.fun} is supplied which is expected to return a top and bottom depth when supplied with a single-profile SPC and ... (any additional parameters). This numeric vector of length two is used by \code{glom()} to produce a subset SPC that is just the horizons overlapping the specified interval.
#' @param object A SoilProfileCollection
#' @param .fun A function that returns vector with top and bottom depth (z1 and z2 arguments to \code{glom}) for a single profile `p` (as passed by \code{profileApply})
#' @param truncate Truncate horizon top and bottom depths to z1 and z2? 
#' @param ... A set of comma-delimited R expressions that resolve to a transformation to be applied to a single profile e.g \code{glomApply(hzdept = max(hzdept) - hzdept)}
#' @param chunk.size Chunk size parameter for \code{profileApply}
#' @return A SoilProfileCollection.
#' @author Andrew G. Brown.
#' 
#' @rdname glomApply
#' @export glomApply
glomApply <- function(object, .fun=NULL, truncate = FALSE, ..., chunk.size = 100) {
  if(is.null(.fun) | !inherits(.fun, 'function'))
    stop("function `.fun`` to return glom boundaries for profiles is missing", call. = FALSE)
  aqp::union(profileApply(object, function(p, ...) {
    dep <- .fun(p, ...)
    return(glom(p, dep[1], dep[2], truncate = truncate))
  }, simplify = FALSE, chunk.size = chunk.size))
}
