#' @title Return a list representation of site and horizon level data
#' @name compositeSPC
#' @aliases compositeSPC,SoilProfileCollection-method
#' @description \code{compositeSPC()} is a convenience function that returns a named list representation of the columns from the \code{@site} and \code{@horizons} slots.
#' @param object A SoilProfileCollection
#' @return A list.
#' @author Andrew G. Brown.
#' 
#' @rdname compositeSPC
#' @export compositeSPC
compositeSPC <- function(object) {
  # create composite object to facilitate eval_tidy
  h <- object@horizons
  s <- as.list(object@site)
  h <- as.list(h[,!colnames(h) %in% names(s)])
  return(c(s, h))
}