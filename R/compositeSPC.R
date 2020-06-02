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
  
  # create composite horizon/site list object to facilitate eval_tidy
  h <- object@horizons
  s <- object@site
  h[[idname(object)]] <- NULL
  
  # convert all factors to character for logical comparison/matching?
  .unfactor <-  function(x) {
    if (is.factor(x))
      return(as.character(x))
    return(x)
  }
  h[] <- lapply(h, .unfactor)
  s[] <- lapply(s, .unfactor)
  
  res <- c(as.list(s), as.list(h))
  return(res)
}