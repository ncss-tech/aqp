# get.increase.matrix()
#' compute pair-wise distances to determine where an attribute increases within
#' a vertical distance specified
#'
#' Uses matrix outer product to determine all pair-wise differences in `attr`
#' for the horizons of `p`. Supplies `attr` to `threshold.fun` to determine the
#' minimum value criterion to return TRUE in output matrix for an "increase".
#' Also, computes all pair-wise distances in depth dimension to determine
#' whether the vertical distance criteria have been met simultaneously with
#' `attr` increase.
#'
#' This function assumes that the `threshold.fun` supplied by the user returns
#' either a constant or a vector of equal length to its input.
#'
#' Note that the `threshold.fun` result is allowed to contain NA, but that will
#' result in no output for affected cells.
#'
#' \code{get.increase.depths} performs the conversion of the square matrix
#' output of \code{get.increase.matrix} back to horizon top depth for where
#' criteria were met.
#'
#'
#' @param p a SoilProfileCollection, containing a single profile
#' @param attr horizon attribute name to get the "increase" of
#' @param threshold.fun a function that returns the threshold (as a function of
#' attr); may return a constant single value
#' @param vertical.distance the vertical distance (determined from difference
#' SPC top depth variable) within which increase must be met
#' @return Returns a square logical matrix reflecting where the increase
#' criteria were met.
#'
#' \code{get.increase.depths} converts to horizon dop depth by using above
#' matrix output to determine depths where increase is met.
#' @author Andrew Gene Brown
#' @seealso \code{getArgillicBounds}, \code{crit.clay.argillic}
#' @keywords manip
#' @examples
#'
#' data(sp1, package = 'aqp')
#' depths(sp1) <- id ~ top + bottom
#' site(sp1) <- ~ group
#'
#' p <- sp1[1]
#' attr <- 'prop' # clay contents
#' foo <- get.increase.matrix(p, threshold.fun = crit.clay.argillic,
#'                            attr = attr, vertical.distance = 30)
#' foo
#'
get.increase.matrix <- function(p, attr, threshold.fun, vertical.distance) {
  # p - a SoilProfileCollection, containing a single profile
  # attr - attribute name to get the "increase" of
  # threshold.fun - a function that returns the threshold (as a function of attr); may return a constant single value
  # vertical distance - the vertical distance (determined from difference SPC top depth variable) within which increase must be met

  # NOTE: should topdepths or midpoints be used? typically the same horizon index is chosen
  #
  #       with thick horizons/diffuse transitions -- the midpoint is probably more appropriate.
  #
  #       I originally chose top depth because it is more straightforward to validate against known data
  #       whereas the midpoints required some more thought. now that I am pleased with stability of algo,
  #       I think it is worth switching to midpoint given it probably gives a better estimate of
  #       the thickness of transitional zone between horizons when it matters
  #
  h <- horizons(p)
  depthz <- horizonDepths(p)
  middepth <- h[[depthz[1]]] + (h[[depthz[2]]] - h[[depthz[1]]]) / 2 # TODO: evaluate

  increase.var <- horizons(p)[[attr]]

  threshold.vector <- threshold.fun(increase.var)

  if(length(threshold.vector) == 1) {
    # if threshold.fun() returns a constant value, expand it to match the length of the attribute
    threshold.vector <- rep(threshold.vector, length(increase.var))

  } else if(length(threshold.vector) != length(increase.var)) {
    # this function assumes that the threshold.fun() supplied by the user returns either a constant,
    # or a vector of equal length to `increase.var` when called above. otherwise, we cannot calculate the result.
    # note that the threshold.fun() result _is_ allowed to contain NA, but that will result in no output for affected cells
    stop(paste0('profile ID:', profile_id(site(p)),
                " - threshold.fun() result should be length 1 or equal to length of attribute \'",attr,"\' (n=",length(increase.var),")."))
  }

  # repeat attr content by horizon in columns; number of columns = number of horizons
  attr.mat <- outer(increase.var, rep(1, length(increase.var)))

  # create a matrix of corresponding thrsholds
  thresh.mat <- outer(threshold.vector, rep(1, length(increase.var)))

  # calculate a matrix of attr differences (between all horizons, not just adjacent)
  attr.inc.mat <- outer(increase.var, increase.var, `-`)

  # calculate a vertical distance matrix (between all horizons)
  vdist.mat <- outer(middepth, c(0, middepth[-length(middepth)]), `-`)

  # crit1 "an increase of at least [thresh.mat]"
  increase.met <- (attr.mat - thresh.mat) > (attr.inc.mat * upper.tri(attr.inc.mat))
  increase.met[is.na(increase.met)] <- FALSE

  # crit2 "within a vertical distance of [vertical.distance]"
  vdist.met <- abs(vdist.mat) <= vertical.distance

  # are crit 1 and crit 2 met?
  criteria.met <- increase.met & vdist.met
  return(criteria.met)
}

#' Return the horizon top depths from a call to get.increase.matrix()
#'
#' \code{get.increase.depths} performs the conversion of the square matrix
#' output of \code{get.increase.matrix} back to horizon top depth for where
#' criteria were met.
#'
#' Note that the `threshold.fun` result is allowed to contain NA, but that will
#' result in no output for affected cells.
#'
#'
#' @param p a SoilProfileCollection, containing a single profile
#' @param attr horizon attribute name to get the "increase" of
#' @param threshold.fun a function that returns the threshold (as a function of
#' attr); may return a constant single value
#' @param vertical.distance the vertical distance (determined from difference
#' SPC top depth variable) within which increase must be met
#' @return Returns a numeric vector of depths where the increase requirement is
#' met. For the argillic, the first is the one of interest.
#'
#' \code{get.increase.depths} converts to horizon dop depth by using above
#' matrix output to determine depths where increase is met.
#' @author Andrew Gene Brown
#' @seealso \code{getArgillicBounds}, \code{crit.clay.argillic}
#' @keywords manip
#' @examples
#'
#' data(sp1, package = 'aqp')
#' depths(sp1) <- id ~ top + bottom
#' site(sp1) <- ~ group
#'
#' p <- sp1[1]
#' attr <- 'prop' # clay contents
#' foo <- get.increase.depths(p, threshold.fun = crit.clay.argillic,
#'                            attr = attr, vertical.distance = 30)
#' foo
#'
get.increase.depths <- function(p, attr, threshold.fun, vertical.distance) {
  topdepth <- horizons(p)[[horizonDepths(p)[1]]]
  #TODO: is there interesting pedogenic information that can be derived
  # from the lower triangle of the criteria.met matrix we omit from the
  # "increase" matrix?) -- i.e. pale/haplo clay decrease at depth?
  criteria.met <- get.increase.matrix(p, attr, threshold.fun, vertical.distance)
  # get the index of  _first_ column from matrix criteria.met that has nonzero sum,
  # this column index is the index of horizon where the attr increase is met
  # then return the top depth.
  return(topdepth[which(colSums(criteria.met) > 0)])
}
