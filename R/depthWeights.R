#' @title Return a named list representation of site and horizon level data
#' @name depthWeights
#' @aliases depthWeights,SoilProfileCollection-method
#' @description \code{depthWeights()} calculates the contributing fraction for each pair of horizon top and bottom depths, given an upper and lower boundary. 
#' @param top A numeric vector of horizon top depths.
#' @param bottom A numeric vector of horizon bottom depths.
#' @param upper A unit length numeric vector with upper boundary.
#' @param lower A unit length numeric vector with lower boundary.
#' @return A named list.
#' @author Andrew G. Brown.
#' 
#' @rdname depthWeights
#' @export depthWeights
depthWeights <- function(top, bottom, upper, lower) {
  if(length(upper) > 1 | length(lower) > 1)
    stop("upper and lower boundary must have length of one")
  if(length(top) != length(bottom))
    stop("lengths of top and bottom depth vectors must match")
  top[top < upper] <- bottom[bottom < upper] <- upper
  top[top > lower] <- bottom[bottom > lower] <- lower
  return((bottom - top) / (lower - upper))
}