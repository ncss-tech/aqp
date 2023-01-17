#'
#' Unroll Genetic Horizons
#'
#' Generate a discretized vector of genetic horizons along a user-defined
#' pattern.
#'
#' This function is used internally by several higher-level components of the
#' \code{aqp} package. Basic error checking is performed to make sure that
#' bottom and top horizon boundaries make sense. Note that the horizons should
#' be sorted according to depth before using this function. The
#' \code{max_depth} argument is used to specify the maximum depth of profiles
#' within a collection, so that data from any profile shallower than this depth
#' is padded with NA.
#'
#' @param top vector of upper horizon boundaries, must be an integer
#' @param bottom vector of lower horizon boundaries, must be an integer
#' @param prop vector of some property to be "unrolled" over a regular sequence
#' @param max_depth maximum depth to which missing data is padded with NA
#' @param bottom_padding_value value to use when padding missing data
#' @param strict should horizons be strictly checked for self-consistency?
#' defaults to FALSE
#' @return a vector of "unrolled" property values
#' @author Dylan E. Beaudette
#' @references http://casoilresource.lawr.ucdavis.edu/
#' @keywords manip
#' @export
#' @examples
#'
#' data(sp1)
#'
#' # subset a single soil profile:
#' sp1.1 <- subset(sp1, subset=id == 'P001')
#'
#' # demonstrate how this function works
#' x <- with(sp1.1, unroll(top, bottom, prop, max_depth=50))
#' plot(x, 1:length(x), ylim=c(90,0), type='b', cex=0.5)
#'
# convert a set of horizon depths and property into a continuous sequence
# returning a vector of standardized length
# suitable for slotting
# horizons must be in order by depth!
unroll <- function(top,
                   bottom,
                   prop,
                   max_depth,
                   bottom_padding_value = NA,
                   strict = FALSE) {
    # currently this will only work with integer depths
    if (any(!as.integer(top[top != 0]) == top[top != 0]) |
        any(!as.integer(bottom) == bottom))
      stop('this function can only accept integer horizon depths', call. = FALSE)

    # are horizons in the correct order?
    if (!all.equal(top, sort(top)))
      stop('Error: horizons are not sorted by depth', call. = FALSE)

    # number of horizons
    n.horizons <- length(top)

    # all bottom values above the last horizon should be in the SET of top values below the first horizon
    hz.test.bottom_hz_in_top <- bottom[-n.horizons] %in% top[-1]
    if (length(which(hz.test.bottom_hz_in_top)) != (n.horizons - 1)) {
      if (strict) {
        stop('error unrolling profile')
      }
      else {
        warning('error unrolling profile, stop execution with strict=TRUE')

      }
    }

    # inverse RLE, to generate repeating sequence of property, n times
    p <- inverse.rle(list(lengths = bottom - top, values = prop))

    # total depth, in unit length
    p.len <- length(p)

    # number of NAs to prepend, in case our profile does not start at 0
    num.NA.prepend <- abs(0 - min(top))

    # number of NAs we need to append to match the deepest profile
    num.NA.append <- max_depth - (p.len + num.NA.prepend)

    # debug
    # print(paste(max_depth, num.NA.prepend, p.len, num.NA.append))

    # padd the result with NA: from the top down
    p.pad <- c(rep(NA, times = num.NA.prepend), p)

    # but only if the number of NA to append is positive
    if (sign(num.NA.append) == 1)
      p.pad <- c(p.pad, rep(bottom_padding_value, times = num.NA.append))

    # return vector, padded and truncated to max_depth
    return(as.vector(p.pad)[1:max_depth])
  }
