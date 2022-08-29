# x: SoilProfileCollection
# h: horizon-level attribute, typically a GHL
# result: list of levels and median depths
#' Guess Appropriate Ordering for Generalized Horizon Labels
#'
#' This function makes an (educated) guess at an appropriate set of levels for
#' generalized horizon labels using the median of horizon depth mid-points.
#'
#' This function is useful when groups of horizons have been generalized via
#' some method other than \code{generalize.hz}. For example, it may be useful
#' to generalize horizons using labels derived from slice depths. The default
#' sorting of these labels will not follow a logical depth-wise sorting when
#' converted to a factor. \code{guessGenHzLevels} does a good job of "guessing"
#' the proper ordering of these labels based on median horizon depth mid-point.
#'
#' @param x a \code{SoilProfileCollection} object
#' @param hz name of horizon-level attribute containing generalized horizon
#' labels, see details
#' @return a list: \item{levels}{a vector of levels sorted by median horizon
#' depth mid-point} \item{median.depths}{a vector of median horizon mid-points}
#' @author D.E. Beaudette
#' @seealso \code{\link{generalize.hz}}
#' @keywords manip
#' @examples
#'
#' # load some example data
#' data(sp1, package='aqp')
#'
#' # upgrade to SoilProfileCollection
#' depths(sp1) <- id ~ top + bottom
#'
#' # generalize horizon names
#' n <- c('O', 'A', 'B', 'C')
#' p <- c('O', 'A', 'B', 'C')
#' sp1$genhz <- generalize.hz(sp1$name, n, p)
#'
#' # note: levels are in the order in which originally defined:
#' levels(sp1$genhz)
#'
#' # generalize horizons by depth slice
#' s <- dice(sp1, c(5, 10, 15, 25, 50, 100, 150) ~ .)
#' s$slice <- paste0(s$top, ' cm')
#' # not a factor
#' levels(s$slice)
#'
#' # the proper ordering of these new labels can be guessed from horizon depths
#' guessGenHzLevels(s, 'slice')
#'
#' # convert to factor, and set proper order
#' s$slice <- factor(s$slice, levels=guessGenHzLevels(s, 'slice')$levels)
#'
#' # that is better
#' levels(s$slice)
#'
guessGenHzLevels <- function(x, hz='genhz') {
  tb <- horizonDepths(x)
  h <- horizons(x)
  # compute horizon mid-point
  m <- (h[[tb[1]]] + h[[tb[2]]]) / 2
  # median mid-point is probably a good indicator of depth-wise ordering
  m.med <- tapply(m, h[[hz]], median, na.rm=TRUE)
  # sort and return
  s <- sort(m.med)
  return(list(levels=names(s), median.depths=s))
}
